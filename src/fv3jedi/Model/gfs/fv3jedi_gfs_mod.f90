! (C) Copyright 2017-2018 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module fv3jedi_gfs_mod

use iso_c_binding
use fckit_configuration_module, only: fckit_configuration
use datetime_mod
use duration_mod
use netcdf

use fckit_mpi_module, only: fckit_mpi_comm

use fv3jedi_kinds_mod
use fv3jedi_field_mod, only: fv3jedi_field, pointer_field
use fv3jedi_geom_mod, only: fv3jedi_geom
use fv3jedi_state_mod, only: fv3jedi_state
use fv3jedi_increment_mod, only: fv3jedi_increment

use time_manager_mod,   only: time_type, set_calendar_type, set_time,    &
                              set_date, days_in_month, month_name,       &
                              operator(+), operator (<), operator (>),   &
                              operator (/=), operator (/), operator (==),&
                              operator (*), THIRTY_DAY_MONTHS, JULIAN,   &
                              NOLEAP, NO_CALENDAR, date_to_string,       &
                              get_date, operator(-)

use  atmos_model_mod,   only: atmos_model_init, atmos_model_end,         &
                              get_atmos_model_ungridded_dim,             &
                              update_atmos_model_dynamics,               &
                              update_atmos_radiation_physics,            &
                              update_atmos_model_state,                  &
                              atmos_data_type, atmos_model_restart,      &
                              atmos_model_exchange_phase_1,              &
                              atmos_model_exchange_phase_2,              &
                              addLsmask2grid, IPD_Data, Atm_block, IPD_Control

use constants_mod,      only: constants_init
use       fms_mod,      only: open_namelist_file, file_exist, check_nml_error, &
                              error_mesg, fms_init, fms_end, close_file,       &
                              write_version_number, uppercase

use mpp_mod,            only: mpp_init, mpp_pe, mpp_root_pe, mpp_npes, mpp_get_current_pelist, &
                              mpp_set_current_pelist, stdlog, mpp_error, NOTE, FATAL, WARNING
use mpp_mod,            only: mpp_clock_id, mpp_clock_begin, mpp_clock_end, mpp_sync

use mpp_io_mod,         only: mpp_open, mpp_close, MPP_NATIVE, MPP_RDONLY, MPP_DELETE

use mpp_domains_mod,    only: mpp_get_global_domain, mpp_global_field, CORNER, domain2d
use mpp_domains_mod,    only: mpp_get_compute_domains, mpp_update_domains
use mpp_domains_mod,    only: mpp_get_boundary, DGRID_NE

use memutils_mod,       only: print_memuse_stats
use sat_vapor_pres_mod, only: sat_vapor_pres_init

use diag_manager_mod,   only: diag_manager_init, diag_manager_end, &
                              diag_manager_set_time_end

use data_override_mod,  only: data_override_init


use atmosphere_mod,     only: Atm

use module_fv3_io_def, only:  iau_offset
use module_fv3_config, only:  dt_atmos, calendar, restart_interval,             &
                              quilting, calendar_type, cpl,                     &
                              cplprint_flag, force_date_from_configure

use tracer_manager_mod, only: get_tracer_index
use field_manager_mod,  only: MODEL_ATMOS

use fv_prec_mod, only: kind_fv3

use fv3jedi_sfcprop_type_mod, only: GFSJEDI_sfcprop_type
use fv3jedi_cldprop_type_mod, only: GFSJEDI_cldprop_type
use fv3jedi_tbdprop_type_mod, only: GFSJEDI_tbdprop_type

implicit none
private

public :: gfs_model
public :: gfs_create
public :: gfs_delete
public :: gfs_initialize
public :: gfs_step
public :: gfs_finalize

! --------------------------------------------------------------------------------------------------

type atmos_internalstate_type
  type(atmos_data_type)  :: Atm
  type(time_type)        :: Time_atmos, Time_init, Time_end,  &
                            Time_step_atmos, Time_step_ocean, &
                            Time_restart, Time_step_restart
  integer :: num_atmos_calls, ret, intrm_rst
end type

!> Fortran derived type to hold model definition
type :: gfs_model
  type(fckit_mpi_comm) :: f_comm
  type(atmos_internalstate_type) :: atm_int_state
  integer :: date_init(6)
  logical :: forecast_complete
  integer :: write_gfs_restart
  integer :: na
  integer :: sphum
  integer :: liq_wat
  integer :: ice_wat
  integer :: o3mr
  integer :: rainwat
  integer :: snowwat
  integer :: graupel
  integer :: cld_amt

  integer :: nblks
  type(GFSJEDI_sfcprop_type), allocatable :: GFSJEDI_sfcprop1(:)
  type(GFSJEDI_sfcprop_type), allocatable :: GFSJEDI_sfcprop2(:)
  type(GFSJEDI_cldprop_type), allocatable :: GFSJEDI_cldprop1(:)
  type(GFSJEDI_cldprop_type), allocatable :: GFSJEDI_cldprop2(:)
  type(GFSJEDI_tbdprop_type), allocatable :: GFSJEDI_tbdprop1(:)
  type(GFSJEDI_tbdprop_type), allocatable :: GFSJEDI_tbdprop2(:)

  integer :: GFSsubsteps

end type gfs_model

! --------------------------------------------------------------------------------------------------

contains

! --------------------------------------------------------------------------------------------------

subroutine gfs_create(self, geom, jediconf, gfsmconf)

implicit none
type(gfs_model),           intent(inout) :: self
type(fv3jedi_geom),        intent(in)    :: geom
type(fckit_configuration), intent(in)    :: jediconf
type(fckit_configuration), intent(in)    :: gfsmconf

integer :: rc
character(len=:), allocatable :: str
character(len=20) :: dt_jedi_str
type(duration) :: dtstep
integer :: dt_jedi
character(len=20) :: date_start
integer :: date_end(6)
integer, allocatable :: res_intvl(:)
integer :: unit, ix, i, j, nb
character(len=9)  :: month

! Get communicator
! ----------------
self%f_comm = geom%f_comm

! Check for same time steps
! -------------------------
call jediconf%get_or_die("tstep",str); dt_jedi_str = str; deallocate(str)
dtstep = trim(dt_jedi_str)
dt_jedi = int(duration_seconds(dtstep))
call gfsmconf%get_or_die("dt_atmos",dt_atmos)

self%write_gfs_restart = 0
if (jediconf%has("write_gfs_restart")) &
  call jediconf%get_or_die("write_gfs_restart",self%write_gfs_restart)

! Determine substeps of GFS needed for one JEDI step
! --------------------------------------------------
if (dt_jedi < dt_atmos) then
  call abor1_ftn("fv3jedi_gfs_mod.create: JEDI model time step should not be less than GFS time step")
elseif (mod(dt_jedi,dt_atmos) .ne. 0) then
  call abor1_ftn("fv3jedi_gfs_mod.create: JEDI time step needs to be divisible by GFS time step")
endif

self%GFSsubsteps = dt_jedi/dt_atmos

if (self%f_comm%rank() == 0) then
   print*, "There are ", int(self%GFSsubsteps,2), " time steps of GFS for each time step of JEDI"
endif


!GFS model_config
call gfsmconf%get_or_die("restart_interval",restart_interval)
call gfsmconf%get_or_die("calendar",str); calendar = trim(str); deallocate(str)

force_date_from_configure = .true.
open(unit=1000, file=trim('INPUT/coupler.res'),status="old",err=998 )
read (1000,*,err=999) calendar_type
close(1000)
force_date_from_configure = .false.
998 continue
999 continue

call constants_init
call sat_vapor_pres_init

if ( force_date_from_configure ) then
 select case( uppercase(trim(calendar)) )
 case( 'JULIAN' )
     calendar_type = JULIAN
 case( 'NOLEAP' )
     calendar_type = NOLEAP
 case( 'THIRTY_DAY' )
     calendar_type = THIRTY_DAY_MONTHS
 case( 'NO_CALENDAR' )
     calendar_type = NO_CALENDAR
 case default
     call mpp_error ( FATAL, 'COUPLER_MAIN: coupler_nml entry calendar must '// &
                             'be one of JULIAN|NOLEAP|THIRTY_DAY|NO_CALENDAR.' )
 end select
endif

call set_calendar_type (calendar_type         )

! Current time
! ------------
call jediconf%get_or_die("date_start",str); date_start = str; deallocate(str)

read(date_start(1 :4 ),*) self%date_init(1)
read(date_start(6 :7 ),*) self%date_init(2)
read(date_start(9 :10),*) self%date_init(3)
read(date_start(12:13),*) self%date_init(4)
read(date_start(15:16),*) self%date_init(5)
read(date_start(18:19),*) self%date_init(6)

self%atm_int_state%Time_atmos = set_date (self%date_init(1), self%date_init(2), self%date_init(3), &
                                          self%date_init(4), self%date_init(5), self%date_init(6))
if(self%f_comm%rank()==0) write(*,'(A,6I5)') 'CurrTime =', self%date_init


! Start time
! ----------
self%atm_int_state%Time_init = set_date (self%date_init(1), self%date_init(2), self%date_init(3),  &
                                        self%date_init(4), self%date_init(5), self%date_init(6))

if(self%f_comm%rank()==0) write(*,'(A,6I5)') 'StartTime=',self%date_init


! End time
! --------
date_end(1) = 9999 ! Jedi determines the stopping so set to extreme future
date_end(2) = 12
date_end(3) = 31
date_end(4) = 23
date_end(5) = 59
date_end(6) = 59

if ( date_end(1) == 0 ) date_end = self%date_init
self%atm_int_state%Time_end = set_date (date_end(1), date_end(2), date_end(3),  &
                                        date_end(4), date_end(5), date_end(6))

if(self%f_comm%rank()==0) write(*,'(A,6I5)') 'StopTime =',date_end


! Diag manager
! ------------
call diag_manager_set_time_end(self%atm_int_state%Time_end)
call diag_manager_init (TIME_INIT=self%date_init)
call diag_manager_set_time_end(self%atm_int_state%Time_end)


! Internal clock info
! -------------------
self%atm_int_state%Time_step_atmos = set_time (dt_atmos,0)
self%atm_int_state%num_atmos_calls = 10000 !Run_length / dt_atmos
if (self%f_comm%rank() == 0) write(0,*)'num_atmos_calls=',self%atm_int_state%num_atmos_calls,&
                                       'time_init=', self%date_init, &
                                       'time_atmos=',self%date_init, &
                                       'time_end=', date_end, &
                                       'dt_atmos=', dt_atmos!, &
                                       !'Run_length=',Run_length
allocate(res_intvl(size(restart_interval)))
res_intvl = restart_interval*3600
self%atm_int_state%Time_step_restart = set_time (res_intvl(1), 0)
self%atm_int_state%Time_restart      = self%atm_int_state%Time_atmos + &
                                       self%atm_int_state%Time_step_restart
self%atm_int_state%intrm_rst         = 0
if (res_intvl(1)>0) self%atm_int_state%intrm_rst = 1
self%atm_int_state%Atm%iau_offset    = iau_offset

! TODO; check dimension of res_intvl

! Write time stamp
! ----------------
call mpp_open( unit, 'time_stamp.out', nohdrs=.TRUE. )
month = month_name(self%date_init(2))
if ( self%f_comm%rank() == 0 ) write (unit,20) self%date_init, month(1:3)
month = month_name(date_end(2))
if ( self%f_comm%rank() == 0 ) write (unit,20) date_end, month(1:3)
call mpp_close (unit)
20  format (6i4,2x,a3)


! Initialize model fields
! -----------------------
call  atmos_model_init (self%atm_int_state%Atm,        self%atm_int_state%Time_init, &
                        self%atm_int_state%Time_atmos, self%atm_int_state%Time_step_atmos)

call data_override_init ( )

if ( self%f_comm%rank() == 0 ) then
  call mpp_open( unit, 'RESTART/file' )
  call mpp_close(unit, MPP_DELETE)
endif

! Check the fv3-jedi geom matches gfs geom
! ----------------------------------------
call geom_handshake(geom)

! Track forecast runs
! -------------------
self%forecast_complete = .false.

! Tracer indices
! --------------
self%sphum   = get_tracer_index (MODEL_ATMOS, 'sphum')
self%ice_wat = get_tracer_index (MODEL_ATMOS, 'ice_wat')
self%liq_wat = get_tracer_index (MODEL_ATMOS, 'liq_wat')
self%o3mr    = get_tracer_index (MODEL_ATMOS, 'o3mr')
self%rainwat = get_tracer_index (MODEL_ATMOS, 'rainwat')
self%snowwat = get_tracer_index (MODEL_ATMOS, 'snowwat')
self%graupel = get_tracer_index (MODEL_ATMOS, 'graupel')
self%cld_amt = get_tracer_index (MODEL_ATMOS, 'cld_amt')

! Create initial time model check point
! -------------------------------------

! NB the GFS physics state is pointers, thus we need two copies of the initial state. The first
! remains fixed, the second is the one pointed to by GFS whcih can change as the model runs.

self%nblks = size(Atm_block%blksz)
allocate(self%GFSJEDI_sfcprop1(self%nblks))
allocate(self%GFSJEDI_sfcprop2(self%nblks))
allocate(self%GFSJEDI_cldprop1(self%nblks))
allocate(self%GFSJEDI_cldprop2(self%nblks))
allocate(self%GFSJEDI_tbdprop1(self%nblks))
allocate(self%GFSJEDI_tbdprop2(self%nblks))

do nb = 1, self%nblks
  ix = Atm_block%blksz(nb)
  call self%GFSJEDI_sfcprop1(nb)%create(ix, IPD_Control)
  call self%GFSJEDI_sfcprop1(nb)%gfs_to_jedi( IPD_Data(nb)%Sfcprop )
  call self%GFSJEDI_sfcprop2(nb)%create(ix, IPD_Control)
  call self%GFSJEDI_sfcprop2(nb)%copy( self%GFSJEDI_sfcprop1(nb) )
  call self%GFSJEDI_cldprop1(nb)%create(ix, IPD_Control)
  call self%GFSJEDI_cldprop1(nb)%gfs_to_jedi( IPD_Data(nb)%Cldprop )
  call self%GFSJEDI_cldprop2(nb)%create(ix, IPD_Control)
  call self%GFSJEDI_cldprop2(nb)%copy( self%GFSJEDI_cldprop1(nb) )
  call self%GFSJEDI_tbdprop1(nb)%create(ix, nb, IPD_Control)
  call self%GFSJEDI_tbdprop1(nb)%gfs_to_jedi( IPD_Data(nb)%tbd )
  call self%GFSJEDI_tbdprop2(nb)%create(ix, nb, IPD_Control)
  call self%GFSJEDI_tbdprop2(nb)%copy( self%GFSJEDI_tbdprop1(nb) )
enddo

end subroutine gfs_create

! --------------------------------------------------------------------------------------------------

subroutine gfs_delete(self)

implicit none
type(gfs_model), intent(inout) :: self

integer :: rc, nb

call atmos_model_end (self%atm_int_state%atm)

call diag_manager_end(self%atm_int_state%Time_atmos )

do nb = 1, self%nblks
  call self%GFSJEDI_sfcprop1(nb)%delete()
  call self%GFSJEDI_sfcprop2(nb)%delete()
enddo
deallocate(self%GFSJEDI_sfcprop1, self%GFSJEDI_sfcprop2)

end subroutine gfs_delete

! --------------------------------------------------------------------------------------------------

subroutine gfs_initialize(self, state, sdate)

implicit none
type(gfs_model),     intent(inout) :: self
type(fv3jedi_state), intent(inout) :: state
type(datetime),      intent(in)    :: sdate

integer :: nb

! Reset the clocks with initialization time
! -----------------------------------------
self%atm_int_state%Time_atmos = set_date (self%date_init(1), &
                                          self%date_init(2), &
                                          self%date_init(3), &
                                          self%date_init(4), &
                                          self%date_init(5), &
                                          self%date_init(6))
self%atm_int_state%Time_init  = set_date (self%date_init(1), &
                                          self%date_init(2), &
                                          self%date_init(3), &
                                          self%date_init(4), &
                                          self%date_init(5), &
                                          self%date_init(6))

self%atm_int_state%Atm%Time_init = self%atm_int_state%Time_init
self%atm_int_state%Atm%Time      = self%atm_int_state%Time_atmos

! Check JEDI and GFS times are equivalent
! ---------------------------------------
call datetime_handshake(self, sdate)

! Track whether this model has been run before
! --------------------------------------------
if (self%forecast_complete) then

  self%forecast_complete = .false.

  ! Overwrite GFS state with saved state
  ! ------------------------------------
  call state_to_gfs( self, state )

endif

end subroutine gfs_initialize

! --------------------------------------------------------------------------------------------------

subroutine gfs_step(self, state, sdate)

implicit none
type(gfs_model),     intent(inout) :: self
type(fv3jedi_state), intent(inout) :: state
type(datetime),      intent(in)    :: sdate

integer :: n, rc
character(len=64) :: timestamp

! Check JEDI and GFS times are equivalent
! ---------------------------------------
call datetime_handshake(self, sdate)


! Step the GFS model
! ------------------

do n = 1, self%GFSsubsteps

  self%atm_int_state%Time_atmos = self%atm_int_state%Time_atmos + self%atm_int_state%Time_step_atmos

  call update_atmos_model_dynamics (self%atm_int_state%Atm)

  call update_atmos_radiation_physics (self%atm_int_state%Atm)

  call atmos_model_exchange_phase_1 (self%atm_int_state%Atm, rc=rc)

  call atmos_model_exchange_phase_2 (self%atm_int_state%Atm, rc=rc)

  call update_atmos_model_state (self%atm_int_state%Atm)

  ! For testing: internal GFS restart
  ! ---------------------------------
  if (self%write_gfs_restart == 1 .and. self%atm_int_state%intrm_rst>0) then
    if ((self%na /= self%atm_int_state%num_atmos_calls) .and.   &
       (self%atm_int_state%Time_atmos == self%atm_int_state%Time_restart)) then
      timestamp = date_to_string (self%atm_int_state%Time_restart)
      call atmos_model_restart(self%atm_int_state%Atm, timestamp)
      call wrt_atmres_timestamp(self, timestamp)
      self%atm_int_state%Time_restart = self%atm_int_state%Time_restart + &
                                        self%atm_int_state%Time_step_restart
    endif
  endif

enddo

! Copy the GFS state to the fv3jedi state
! ---------------------------------------
call gfs_to_state( self, state )

end subroutine gfs_step

! --------------------------------------------------------------------------------------------------

subroutine gfs_finalize(self, state, sdate)

implicit none
type(gfs_model),     intent(inout) :: self
type(fv3jedi_state), intent(inout) :: state
type(datetime),      intent(in)    :: sdate

! Check JEDI and GFS times are equivalent
! ---------------------------------------
call datetime_handshake(self, sdate)


! Record complete run of the forecast
! -----------------------------------
self%forecast_complete = .true.

end subroutine gfs_finalize

! --------------------------------------------------------------------------------------------------

subroutine state_to_gfs( self, state )

implicit none
type(gfs_model),     intent(inout) :: self
type(fv3jedi_state), intent(in)    :: state

integer :: isc, iec, jsc, jec, npz
integer :: i, j, var, nb
type(fv3jedi_field), pointer :: field_pointer

! Grid shortcuts
isc = state%isc
iec = state%iec
jsc = state%jsc
jec = state%jec

! Copy Atm into to JEDI state
do var = 1, state%nf

  !Get pointer to state
  call pointer_field(state%fields, state%fields(var)%fv3jedi_name, field_pointer)

  ! Levels for this field
  npz = state%fields(var)%npz

  select case (trim(state%fields(var)%fv3jedi_name))

    ! Dynamics fields
    ! ---------------

    case("ud")
      Atm(1)%u   (isc:iec,jsc:jec,1:npz) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("vd")
      Atm(1)%v   (isc:iec,jsc:jec,1:npz) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("ua")
      Atm(1)%ua  (isc:iec,jsc:jec,1:npz) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("va")
      Atm(1)%va  (isc:iec,jsc:jec,1:npz) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("t")
      Atm(1)%pt  (isc:iec,jsc:jec,1:npz) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("delp")
      Atm(1)%delp(isc:iec,jsc:jec,1:npz) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("q")
      Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%sphum  ) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("qi")
      Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%ice_wat) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("ql")
      Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%liq_wat) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("o3")
      Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%o3mr   ) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("qr")
      Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%rainwat) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("qs")
      Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%snowwat) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("graupel")
      Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%graupel) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("cld_amt")
      Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%cld_amt) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("phis")
      Atm(1)%phis(isc:iec,jsc:jec) = real(field_pointer%array(isc:iec,jsc:jec,1),kind_fv3)
    case("w")
      Atm(1)%w   (isc:iec,jsc:jec,1:npz) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("delz")
      Atm(1)%delz(isc:iec,jsc:jec,1:npz) = real(field_pointer%array(isc:iec,jsc:jec,1:npz),kind_fv3)
    case("u_srf")
      Atm(1)%u_srf(isc:iec,jsc:jec) = real(field_pointer%array(isc:iec,jsc:jec,1),kind_fv3)
    case("v_srf")
      Atm(1)%v_srf(isc:iec,jsc:jec) = real(field_pointer%array(isc:iec,jsc:jec,1),kind_fv3)
    case default
      !nothing to do
  end select

  nullify(field_pointer)

enddo

! Halo filling for dynamics variables
! -----------------------------------

call fill_dgrid_winds(Atm(1)%u,Atm(1)%v,fillhalo=.true.)
call mpp_update_domains (Atm(1)%ua   , Atm(1)%domain, complete=.true.)
call mpp_update_domains (Atm(1)%va   , Atm(1)%domain, complete=.true.)
call mpp_update_domains (Atm(1)%pt   , Atm(1)%domain, complete=.true.)
call mpp_update_domains (Atm(1)%delp , Atm(1)%domain, complete=.true.)
call mpp_update_domains (Atm(1)%q    , Atm(1)%domain, complete=.true.)
call mpp_update_domains (Atm(1)%phis , Atm(1)%domain, complete=.true.)
call mpp_update_domains (Atm(1)%w    , Atm(1)%domain, complete=.true.)
call mpp_update_domains (Atm(1)%delz , Atm(1)%domain, complete=.true.)


! Parts of the state only internal to GFS
! ---------------------------------------
do nb = 1, self%nblks
  ! Copy static
  call self%GFSJEDI_sfcprop2(nb)%copy( self%GFSJEDI_sfcprop1(nb) )
  call self%GFSJEDI_cldprop2(nb)%copy( self%GFSJEDI_cldprop1(nb) )
  call self%GFSJEDI_tbdprop2(nb)%copy( self%GFSJEDI_tbdprop1(nb) )
  ! Set GFS pointers to second
  call self%GFSJEDI_sfcprop2(nb)%jedi_to_gfs( IPD_Data(nb)%Sfcprop )
  call self%GFSJEDI_cldprop2(nb)%jedi_to_gfs( IPD_Data(nb)%Cldprop )
  call self%GFSJEDI_tbdprop2(nb)%jedi_to_gfs( IPD_Data(nb)%Tbd )
enddo

end subroutine state_to_gfs

! --------------------------------------------------------------------------------------------------

subroutine gfs_to_state( self, state )

implicit none
type(gfs_model),     intent(in)    :: self
type(fv3jedi_state), intent(inout) :: state

integer :: isc, iec, jsc, jec, npz
integer :: i, j, var, nb, ix
type(fv3jedi_field), pointer :: field_pointer

! Grid shortcuts
isc = state%isc
iec = state%iec
jsc = state%jsc
jec = state%jec

! Copy Atm into to JEDI state
do var = 1, state%nf

  !Get pointer to state
  call pointer_field(state%fields, state%fields(var)%fv3jedi_name, field_pointer)

  ! Levels for this field
  npz = state%fields(var)%npz

  select case (trim(state%fields(var)%fv3jedi_name))

    ! Dynamics fields
    ! ---------------

    case("ud")
      field_pointer%array(isc:iec,jsc:jec+1,1:npz) = real(Atm(1)%u   (isc:iec,jsc:jec+1,1:npz),kind_real)
    case("vd")
      field_pointer%array(isc:iec+1,jsc:jec,1:npz) = real(Atm(1)%v   (isc:iec+1,jsc:jec,1:npz),kind_real)
    case("ua")
      field_pointer%array(isc:iec,jsc:jec,1:npz) = real(Atm(1)%ua  (isc:iec,jsc:jec,1:npz),kind_real)
    case("va")
      field_pointer%array(isc:iec,jsc:jec,1:npz) = real(Atm(1)%va  (isc:iec,jsc:jec,1:npz),kind_real)
    case("t")
      field_pointer%array(isc:iec,jsc:jec,1:npz) = real(Atm(1)%pt  (isc:iec,jsc:jec,1:npz),kind_real)
    case("delp")
      field_pointer%array(isc:iec,jsc:jec,1:npz) = real(Atm(1)%delp(isc:iec,jsc:jec,1:npz),kind_real)
    case("q")
      field_pointer%array(isc:iec,jsc:jec,1:npz) = real(Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%sphum  ),kind_real)
    case("qi")
      field_pointer%array(isc:iec,jsc:jec,1:npz) = real(Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%ice_wat),kind_real)
    case("ql")
      field_pointer%array(isc:iec,jsc:jec,1:npz) = real(Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%liq_wat),kind_real)
    case("o3")
      field_pointer%array(isc:iec,jsc:jec,1:npz) = real(Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%o3mr   ),kind_real)
    case("qr")
      field_pointer%array(isc:iec,jsc:jec,1:npz) = real(Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%rainwat),kind_real)
    case("qs")
      field_pointer%array(isc:iec,jsc:jec,1:npz) = real(Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%snowwat),kind_real)
    case("graupel")
      field_pointer%array(isc:iec,jsc:jec,1:npz) = real(Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%graupel),kind_real)
    case("cld_amt")
      field_pointer%array(isc:iec,jsc:jec,1:npz) = real(Atm(1)%q   (isc:iec,jsc:jec,1:npz,self%cld_amt),kind_real)
    case("phis")
      field_pointer%array(isc:iec,jsc:jec,1)     = real(Atm(1)%phis(isc:iec,jsc:jec),kind_real)
    case("w")
      field_pointer%array(isc:iec,jsc:jec,1:npz) = real(Atm(1)%w   (isc:iec,jsc:jec,1:npz),kind_real)
    case("delz")
      field_pointer%array(isc:iec,jsc:jec,1:npz) = real(Atm(1)%delz(isc:iec,jsc:jec,1:npz),kind_real)
    case("u_srf")
      field_pointer%array(isc:iec,jsc:jec,1)     = real(Atm(1)%u_srf(isc:iec,jsc:jec),kind_real)
    case("v_srf")
      field_pointer%array(isc:iec,jsc:jec,1)     = real(Atm(1)%v_srf(isc:iec,jsc:jec),kind_real)

   ! Surface fields
   ! --------------
   case("slmsk")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%slmsk(ix),kind_real)
       enddo
     enddo
   case("sheleg")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%weasd(ix),kind_real)
       enddo
     enddo
   case("tsea")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%tsfc(ix),kind_real)
       enddo
     enddo
   case("vtype")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%vtype(ix),kind_real)
       enddo
     enddo
   case("stype")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%stype(ix),kind_real)
       enddo
     enddo
   case("vfrac")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%vfrac(ix),kind_real)
       enddo
     enddo
   case("stc")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,:) = real(IPD_Data(nb)%Sfcprop%stc(ix,:),kind_real)
       enddo
     enddo
   case("smc")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,:) = real(IPD_Data(nb)%Sfcprop%smc(ix,:),kind_real)
       enddo
     enddo
   case("snwdph")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%snowd(ix),kind_real)
       enddo
     enddo
   case("f10m")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%f10m(ix),kind_real)
       enddo
     enddo
   case("tg3")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%tg3(ix),kind_real)
       enddo
     enddo
   case("zorl")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%zorl(ix),kind_real)
       enddo
     enddo
   case("alvsf")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%alvsf(ix),kind_real)
       enddo
     enddo
   case("alvwf")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%alvwf(ix),kind_real)
       enddo
     enddo
   case("alnsf")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%alnsf(ix),kind_real)
       enddo
     enddo
   case("alnwf")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%alnwf(ix),kind_real)
       enddo
     enddo
   case("facsf")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%facsf(ix),kind_real)
       enddo
     enddo
   case("facwf")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%facwf(ix),kind_real)
       enddo
     enddo
   case("canopy")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%canopy(ix),kind_real)
       enddo
     enddo
   case("t2m")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%t2m(ix),kind_real)
       enddo
     enddo
   case("q2m")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%q2m(ix),kind_real)
       enddo
     enddo
   case("uustar")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%uustar(ix),kind_real)
       enddo
     enddo
   case("ffmm")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%ffmm(ix),kind_real)
       enddo
     enddo
   case("ffhh")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%ffhh(ix),kind_real)
       enddo
     enddo
   case("hice")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%hice(ix),kind_real)
       enddo
     enddo
   case("fice")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%fice(ix),kind_real)
       enddo
     enddo
   case("tisfc")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%tisfc(ix),kind_real)
       enddo
     enddo
   case("tprcp")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%tprcp(ix),kind_real)
       enddo
     enddo
   case("srflag")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%srflag(ix),kind_real)
       enddo
     enddo
   case("shdmin")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%shdmin(ix),kind_real)
       enddo
     enddo
   case("shdmax")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%shdmax(ix),kind_real)
       enddo
     enddo
   case("slope")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%slope(ix),kind_real)
       enddo
     enddo
   case("snoalb")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%snoalb(ix),kind_real)
       enddo
     enddo
   case("sncovr")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%sncovr(ix),kind_real)
       enddo
     enddo
   case("tref")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%tref(ix),kind_real)
       enddo
     enddo
   case("z_c")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%z_c(ix),kind_real)
       enddo
     enddo
   case("c_0")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%c_0(ix),kind_real)
       enddo
     enddo
   case("c_d")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%c_d(ix),kind_real)
       enddo
     enddo
   case("w_0")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%w_0(ix),kind_real)
       enddo
     enddo
   case("w_d")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%w_d(ix),kind_real)
       enddo
     enddo
   case("xt")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%xt(ix),kind_real)
       enddo
     enddo
   case("xs")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%xs(ix),kind_real)
       enddo
     enddo
   case("xu")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%xu(ix),kind_real)
       enddo
     enddo
   case("xv")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%xv(ix),kind_real)
       enddo
     enddo
   case("xz")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%xz(ix),kind_real)
       enddo
     enddo
   case("zm")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%zm(ix),kind_real)
       enddo
     enddo
   case("xtts")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%xtts(ix),kind_real)
       enddo
     enddo
   case("xzts")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%xzts(ix),kind_real)
       enddo
     enddo
   case("d_conv")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%d_conv(ix),kind_real)
       enddo
     enddo
   case("ifd")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%ifd(ix),kind_real)
       enddo
     enddo
   case("dt_cool")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%dt_cool(ix),kind_real)
       enddo
     enddo
   case("qrain")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,1) = real(IPD_Data(nb)%Sfcprop%qrain(ix),kind_real)
       enddo
     enddo
   case("slc")
     do j=jsc,jec
       do i=isc,iec
         nb = Atm_block%blkno(i,j)
         ix = Atm_block%ixp(i,j)
         field_pointer%array(i,j,:) = real(IPD_Data(nb)%Sfcprop%slc(ix,:),kind_real)
       enddo
     enddo

    !Physics
    case("cv")
      do j=jsc,jec
        do i=isc,iec
          nb = Atm_block%blkno(i,j)
          ix = Atm_block%ixp(i,j)
          field_pointer%array(i,j,1) = real(IPD_Data(nb)%Cldprop%cv(ix),kind_real)
        enddo
      enddo
    case("cvt")
      do j=jsc,jec
        do i=isc,iec
          nb = Atm_block%blkno(i,j)
          ix = Atm_block%ixp(i,j)
          field_pointer%array(i,j,1) = real(IPD_Data(nb)%Cldprop%cvt(ix),kind_real)
        enddo
      enddo
    case("cvb")
      do j=jsc,jec
        do i=isc,iec
          nb = Atm_block%blkno(i,j)
          ix = Atm_block%ixp(i,j)
          field_pointer%array(i,j,1) = real(IPD_Data(nb)%Cldprop%cvb(ix),kind_real)
        enddo
      enddo
    case("phy_f2d_01")
      do j=jsc,jec
        do i=isc,iec
          nb = Atm_block%blkno(i,j)
          ix = Atm_block%ixp(i,j)
          field_pointer%array(i,j,1) = real(IPD_Data(nb)%Tbd%phy_f2d(ix,1),kind_real)
        enddo
      enddo
    case("phy_fctd_01")
      if (associated(IPD_Data(nb)%Tbd%phy_fctd)) then
        do j=jsc,jec
          do i=isc,iec
            nb = Atm_block%blkno(i,j)
            ix = Atm_block%ixp(i,j)
            field_pointer%array(i,j,1) = real(IPD_Data(nb)%Tbd%phy_fctd(ix,1),kind_real)
          enddo
        enddo
      else
        field_pointer%array = 0.0_kind_real
      endif
    case("phy_f3d_01")
      do j=jsc,jec
        do i=isc,iec
          nb = Atm_block%blkno(i,j)
          ix = Atm_block%ixp(i,j)
          field_pointer%array(i,j,1:npz) = real(IPD_Data(nb)%Tbd%phy_f3d(ix,:,1),kind_real)
        enddo
      enddo
    case("phy_f3d_02")
      do j=jsc,jec
        do i=isc,iec
          nb = Atm_block%blkno(i,j)
          ix = Atm_block%ixp(i,j)
          field_pointer%array(i,j,1:npz) = real(IPD_Data(nb)%Tbd%phy_f3d(ix,:,2),kind_real)
        enddo
      enddo
    case default
      call abor1_ftn("fv3jedi_gfs_mod.gfs_to_state: Can't retrive variable"//&
                     trim(state%fields(var)%fv3jedi_name)//"from the GFS model state")
  end select

  nullify(field_pointer)

enddo

end subroutine gfs_to_state

! --------------------------------------------------------------------------------------------------

subroutine wrt_atmres_timestamp(self,timestamp)

  implicit none
  type(gfs_model),   intent(in) :: self
  character(len=32), intent(in) :: timestamp

  integer :: unit, date(6)

 ! Compute current date
 ! --------------------
  call get_date (self%atm_int_state%Time_atmos, date(1), date(2), date(3),  &
                                                date(4), date(5), date(6))

 ! Write coupler.res file
 ! ----------------------

  if (self%f_comm%rank() == 0)then
      call mpp_open( unit, 'RESTART/'//trim(timestamp)//'.coupler.res', nohdrs=.TRUE. )
      write( unit, '(i6,8x,a)' )calendar_type, &
           '(Calendar: no_calendar=0, thirty_day_months=1, julian=2, gregorian=3, noleap=4)'

      write( unit, '(6i6,8x,a)' )self%date_init, &
           'Model start time:   year, month, day, hour, minute, second'
      write( unit, '(6i6,8x,a)' )date, &
           'Current model time: year, month, day, hour, minute, second'
      call mpp_close(unit)
  endif
end subroutine wrt_atmres_timestamp

! --------------------------------------------------------------------------------------------------

subroutine datetime_handshake(self, sdate)

implicit none
type(gfs_model), intent(in) :: self
type(datetime),  intent(in)    :: sdate

! Locals
integer :: n, failed
character(len=20) :: sdatec
integer :: date_jedi(6), date_gfs(6)

! Convert Jedi datetime to string
call datetime_to_string(sdate, sdatec)

! Convert Jedi datetime to integers
read(sdatec(1 :4 ),*) date_jedi(1)
read(sdatec(6 :7 ),*) date_jedi(2)
read(sdatec(9 :10),*) date_jedi(3)
read(sdatec(12:13),*) date_jedi(4)
read(sdatec(15:16),*) date_jedi(5)
read(sdatec(18:19),*) date_jedi(6)

! Convert GFS datetime to integers
call get_date (self%atm_int_state%Time_atmos, date_gfs(1), date_gfs(2), date_gfs(3),  &
                                              date_gfs(4), date_gfs(5), date_gfs(6))

! Compare each element
failed = 0
do n = 1,6
  if (date_jedi(n) .ne. date_gfs(n)) then
    failed = failed + 1
  endif
enddo

! Throw failure if necessary
if (failed > 0) then
  if (self%f_comm%rank() == 0) then
    print*, "fv3jedi_gfs_mod.datetime_handshake Jedi datetime does not match GFS datetime"
    print*, "Jedi time: ", date_jedi
    print*, "GFS  time: ", date_gfs
    print*, "Aborting..."
  endif
  call abor1_ftn("fv3jedi_gfs_mod.datetime_handshake")
endif

end subroutine datetime_handshake

! --------------------------------------------------------------------------------------------------

subroutine geom_handshake(geom)

type(fv3jedi_geom), intent(in) :: geom

if (geom%isc .ne. Atm(1)%bd%isc) call abor1_ftn("fv3jedi_gfs_mod.geom_handshake, isc mismatch")
if (geom%iec .ne. Atm(1)%bd%iec) call abor1_ftn("fv3jedi_gfs_mod.geom_handshake, iec mismatch")
if (geom%jsc .ne. Atm(1)%bd%jsc) call abor1_ftn("fv3jedi_gfs_mod.geom_handshake, jsc mismatch")
if (geom%jec .ne. Atm(1)%bd%jec) call abor1_ftn("fv3jedi_gfs_mod.geom_handshake, jec mismatch")

if (minval(abs(geom%grid_lon-real(Atm(1)%gridstruct%agrid_64(:,:,1),kind_real))) > 1.0e-9_kind_real) &
  call abor1_ftn("fv3jedi_gfs_mod.geom_handshake, grid_lon mismatch")

if (minval(abs(geom%grid_lat-real(Atm(1)%gridstruct%agrid_64(:,:,2),kind_real))) > 1.0e-9_kind_real) &
  call abor1_ftn("fv3jedi_gfs_mod.geom_handshake, grid_lat mismatch")

end subroutine geom_handshake

! --------------------------------------------------------------------------------------------------

subroutine fill_dgrid_winds(u,v,fillhalo)

implicit none
real(kind=kind_real), intent(inout) :: u(Atm(1)%bd%isd:Atm(1)%bd%ied  ,Atm(1)%bd%jsd:Atm(1)%bd%jed+1,1:Atm(1)%npz)
real(kind=kind_real), intent(inout) :: v(Atm(1)%bd%isd:Atm(1)%bd%ied+1,Atm(1)%bd%jsd:Atm(1)%bd%jed  ,1:Atm(1)%npz)
logical, optional,    intent(in)    :: fillhalo

integer :: isc, iec, jsc, jec, npz, i, j, k
real(kind=kind_real) :: ebuffery(Atm(1)%bd%jsc:Atm(1)%bd%jec,1:Atm(1)%npz)
real(kind=kind_real) :: nbufferx(Atm(1)%bd%isc:Atm(1)%bd%iec,1:Atm(1)%npz)
real(kind=kind_real) :: wbuffery(Atm(1)%bd%jsc:Atm(1)%bd%jec,1:Atm(1)%npz)
real(kind=kind_real) :: sbufferx(Atm(1)%bd%isc:Atm(1)%bd%iec,1:Atm(1)%npz)

! ---------------------------------------- !
! Fill edge and then halo of  D-grid winds !
! ---------------------------------------- !

! Shortcuts
! ---------
isc = Atm(1)%bd%isc
iec = Atm(1)%bd%iec
jsc = Atm(1)%bd%jsc
jec = Atm(1)%bd%jec
npz = Atm(1)%npz

! Fill north/east edges
! ---------------------
ebuffery = 0.0_kind_real
nbufferx = 0.0_kind_real
wbuffery = 0.0_kind_real
sbufferx = 0.0_kind_real
call mpp_get_boundary( u, v, Atm(1)%domain, &
                       wbuffery=wbuffery, ebuffery=ebuffery, &
                       sbufferx=sbufferx, nbufferx=nbufferx, &
                       gridtype=DGRID_NE, complete=.true. )
do k=1,npz
   do i=isc,iec
      u(i,jec+1,k) = nbufferx(i,k)
   enddo
enddo
do k=1,npz
   do j=jsc,jec
      v(iec+1,j,k) = ebuffery(j,k)
   enddo
enddo

! Fill halos
! ----------
if (present(fillhalo)) then
  if (fillhalo) then
    call mpp_update_domains(u, v, Atm(1)%domain, gridtype=DGRID_NE)
  endif
endif

end subroutine fill_dgrid_winds

! --------------------------------------------------------------------------------------------------

end module fv3jedi_gfs_mod
