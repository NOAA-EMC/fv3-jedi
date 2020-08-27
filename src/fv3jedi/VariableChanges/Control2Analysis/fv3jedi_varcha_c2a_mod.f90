! (C) Copyright 2018-2019 UCAR
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

module fv3jedi_varcha_c2a_mod

! iso
use iso_c_binding

! fckit
use fckit_configuration_module, only: fckit_configuration

! oops
use datetime_mod

! femps
use femps_grid_mod
use femps_operators_mod
use femps_testgrid_mod
use femps_solve_mod
use femps_fv3_mod

! fv3jedi
use fv3jedi_kinds_mod,   only: kind_real
use fv3jedi_geom_mod,    only: fv3jedi_geom
use fv3jedi_state_mod,   only: fv3jedi_state
use fv3jedi_field_mod,   only: copy_subset, has_field, pointer_field_array, field_clen, allocate_copy_field_array

use pressure_vt_mod
use temperature_vt_mod
use moisture_vt_mod
use wind_vt_mod

implicit none
private

public :: fv3jedi_varcha_c2a
public :: create
public :: delete
public :: changevar
public :: changevarinverse

type :: fv3jedi_varcha_c2a
  type(fempsgrid) :: grid
  type(fempsoprs) :: oprs
  integer :: lprocs
  integer, allocatable :: lev_start(:), lev_final(:)
end type fv3jedi_varcha_c2a

! ------------------------------------------------------------------------------

contains

! ------------------------------------------------------------------------------

subroutine create(self, geom, conf)

implicit none
type(fv3jedi_varcha_c2a),  intent(inout) :: self
type(fv3jedi_geom),        intent(inout) :: geom
type(fckit_configuration), intent(in)    :: conf

integer :: ngrids, niter, lprocs, lstart
logical :: check_convergence
character(len=:), allocatable :: str
character(len=2055) :: path2fv3gridfiles

integer :: n, levs_per_proc

! Grid and operators for the femps Poisson solver
! -----------------------------------------------

! Configuration
call conf%get_or_die("femps_iterations",niter)
call conf%get_or_die("femps_ngrids",ngrids)
call conf%get_or_die("femps_path2fv3gridfiles",str); path2fv3gridfiles = str
if( .not. conf%get('femps_levelprocs',lprocs) ) then
  lprocs = -1
endif
if( .not. conf%get('femps_checkconvergence',check_convergence) ) then
  check_convergence = .false.
endif

! Processors that will do the work
! --------------------------------
lprocs = min(lprocs,geom%f_comm%size())
lprocs = min(lprocs,geom%npz)

if (lprocs == -1) then
  self%lprocs = min(geom%npz,geom%f_comm%size())
else
  self%lprocs = lprocs
endif

if (geom%f_comm%rank() == 0 ) print*, "Running femps with ", self%lprocs, " processors."

allocate(self%lev_start(self%lprocs))
allocate(self%lev_final(self%lprocs))

if (self%lprocs == geom%npz) then
  do n = 1,self%lprocs
    self%lev_start(n) = n
    self%lev_final(n) = n
  enddo
else
  levs_per_proc = floor(real(geom%npz,kind_real)/real(self%lprocs,kind_real))
  lstart = 0
  do n = 1,self%lprocs
    self%lev_start(n) = lstart+1
    self%lev_final(n) = self%lev_start(n) + levs_per_proc - 1
    if (n .le. mod(geom%npz, self%lprocs)) self%lev_final(n) = self%lev_final(n) + 1
    lstart = self%lev_final(n)
  enddo
endif

if (self%lev_final(self%lprocs) .ne. geom%npz) &
  call abor1_ftn("fv3jedi_varcha_c2a_mod.create: last level not equal to number of levels.")

! Processors doing the work need grid and operators
if (geom%f_comm%rank() < self%lprocs ) then

  if (geom%f_comm%rank() == 0 ) print*, 'Creating FEMPS grid object'
  call self%grid%setup('cs',ngrids=ngrids,cube=geom%npx-1,niter=niter,&
                       comm = geom%f_comm%communicator(), &
                       rank = geom%f_comm%rank(), &
                       csize = geom%f_comm%size(), &
                       check_convergence = check_convergence )

  if (geom%f_comm%rank() == 0 ) print*, 'Creating FEMPS grid hierarchy from files'
  call fv3grid_to_ugrid(self%grid,path2fv3gridfiles)

  ! Build the connectivity and extra geom
  if (geom%f_comm%rank() == 0 ) print*, 'Creating FEMPS cubed-sphere connectivity'
  call self%grid%build_cs(1,1)

  ! Perform all the setup
  if (geom%f_comm%rank() == 0 ) print*, 'Creating FEMPS static operators'
  call preliminary(self%grid,self%oprs)

  ! Partial delete of operators not needed
  if (geom%f_comm%rank() == 0 ) print*, 'FEMPS partial deallocate'
  call self%oprs%pdelete()

endif

end subroutine create

! ------------------------------------------------------------------------------

subroutine delete(self)

implicit none
type(fv3jedi_varcha_c2a), intent(inout) :: self

call self%oprs%delete()
call self%grid%delete()

deallocate(self%lev_start,self%lev_final)

end subroutine delete

! ------------------------------------------------------------------------------

subroutine changevar(self,geom,xctl,xana)

implicit none
type(fv3jedi_varcha_c2a), intent(inout) :: self
type(fv3jedi_geom),       intent(inout) :: geom
type(fv3jedi_state),      intent(in)    :: xctl
type(fv3jedi_state),      intent(inout) :: xana

integer :: f
character(len=field_clen), allocatable :: fields_to_do(:)
real(kind=kind_real), pointer :: field_ptr(:,:,:)

! Stream function/velocity potential
logical :: have_udvd, have_vodi
real(kind=kind_real), pointer     ::   psi(:,:,:)     ! Stream function
real(kind=kind_real), pointer     ::   chi(:,:,:)     ! Velocity potentail
real(kind=kind_real), allocatable ::    ud(:,:,:)     ! D-grid u wind
real(kind=kind_real), allocatable ::    vd(:,:,:)     ! D-grid v wind
real(kind=kind_real), allocatable ::  vort(:,:,:)     ! Vorticity
real(kind=kind_real), allocatable ::  divg(:,:,:)     ! Divergence

! Pressure
logical :: have_pres
real(kind=kind_real), pointer     ::    ps(:,:,:)     ! Surface pressure
real(kind=kind_real), allocatable ::  delp(:,:,:)     ! Pressure thickness
real(kind=kind_real), allocatable ::    pe(:,:,:)     ! Pressure edges
real(kind=kind_real), allocatable ::     p(:,:,:)     ! Pressure mid
real(kind=kind_real), allocatable ::   pkz(:,:,:)     ! Pressure ^ kappa

! Temperaure
logical :: have_t, have_pt
real(kind=kind_real), pointer     ::    t (:,:,:)     ! Temperature
real(kind=kind_real), allocatable ::   pt (:,:,:)     ! Potential temperature

! Clouds
logical :: have_cld4
real(kind=kind_real), pointer     :: qi   (:,:,:)     ! Cloud liquid ice
real(kind=kind_real), pointer     :: ql   (:,:,:)     ! Cloud liquid water
real(kind=kind_real), pointer     :: qilsf(:,:,:)     ! Fraction ice large scale
real(kind=kind_real), pointer     :: qicnf(:,:,:)     ! Fraction ice convective
real(kind=kind_real), allocatable :: qils (:,:,:)     ! Cloud liquid ice large scale
real(kind=kind_real), allocatable :: qicn (:,:,:)     ! Cloud liquid ice convective
real(kind=kind_real), allocatable :: qlls (:,:,:)     ! Cloud liquid water large scale
real(kind=kind_real), allocatable :: qlcn (:,:,:)     ! Cloud liquid water convective

! Copy fields that are the same in both
! -------------------------------------
call copy_subset(xctl%fields, xana%fields, fields_to_do)

! If variable change is the identity early exit
! ---------------------------------------------
if (.not.allocated(fields_to_do)) return

! Wind variables
! --------------
have_udvd = .false.
have_vodi = .false.
if (has_field(xctl%fields, 'psi') .and. has_field(xctl%fields, 'chi')) then
  call pointer_field_array(xctl%fields, 'psi', psi)
  call pointer_field_array(xctl%fields, 'chi', chi)
  allocate(ud(geom%isc:geom%iec  ,geom%jsc:geom%jec+1,1:geom%npz))
  allocate(vd(geom%isc:geom%iec+1,geom%jsc:geom%jec  ,1:geom%npz))
  call psichi_to_udvd(geom, psi, chi, ud, vd)
  have_udvd = .true.
  allocate(vort(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  allocate(divg(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  call psichi_to_vortdivg(geom, self%grid, self%oprs, psi, chi, self%lprocs, self%lev_start, &
                          self%lev_final, vort, divg)
  have_vodi = .true.
endif

! Pressure
! --------
have_pres = .false.
if (has_field(xctl%fields, 'ps')) then
  call pointer_field_array(xctl%fields, 'ps', ps)
  allocate(delp(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  allocate(  pe(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz+1))
  allocate(   p(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  allocate( pkz(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  call ps_to_delp(geom, ps, delp)
  call delp_to_pe_p_logp(geom, delp, pe, p)
  call pe_to_pkz(geom, pe, pkz)
  have_pres = .true.
endif

! Temperature
! -----------
have_t  = .false.
have_pt = .false.
if (has_field(xctl%fields, 't')) then
  call pointer_field_array(xctl%fields, 't', t)
  have_t = .true.
  if (have_pres) then
    allocate(pt(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
    call t_to_pt(geom, pkz, t, pt)
    have_pt = .true.
  endif
endif

! Clouds
! ------
have_cld4 = .false.
if ( has_field(xctl%fields, 'ice_wat') .and. has_field(xctl%fields, 'liq_wat') .and. &
     has_field(xctl%fields, 'qilsf') .and. has_field(xctl%fields, 'qicnf')) then
  call pointer_field_array(xctl%fields, 'ice_wat', qi)
  call pointer_field_array(xctl%fields, 'liq_wat', ql)
  call pointer_field_array(xctl%fields, 'qilsf', qilsf)
  call pointer_field_array(xctl%fields, 'qicnf', qicnf)
  allocate(qils(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  allocate(qicn(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  allocate(qlls(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  allocate(qlcn(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  call q2_to_q4(geom, qi, ql, qilsf, qicnf, qils, qicn, qlls, qlcn)
  have_cld4 = .true.
endif

! Loop over the fields not found in the input state and work through cases
! ------------------------------------------------------------------------
do f = 1, size(fields_to_do)

  call pointer_field_array(xana%fields, trim(fields_to_do(f)),  field_ptr)

  select case (trim(fields_to_do(f)))

  case ("ud")

    if (.not. have_udvd) call field_fail(fields_to_do(f))
    field_ptr = ud

  case ("vd")

    if (.not. have_udvd) call field_fail(fields_to_do(f))
    field_ptr = vd

  case ("vort")

    if (.not. have_vodi) call field_fail(fields_to_do(f))
    field_ptr = vort

  case ("divg")

    if (.not. have_vodi) call field_fail(fields_to_do(f))
    field_ptr = divg

  case ("ps")

    if (.not. have_pres) call field_fail(fields_to_do(f))
    field_ptr = ps

  case ("delp")

    if (.not. have_pres) call field_fail(fields_to_do(f))
    field_ptr = delp

  case ("pkz")

    if (.not. have_pres) call field_fail(fields_to_do(f))
    field_ptr = pkz

  case ("qils")

    if (.not. have_pres) call field_fail(fields_to_do(f))
    field_ptr = qils

  case ("qicn")

    if (.not. have_pres) call field_fail(fields_to_do(f))
    field_ptr = qicn

  case ("qlls")

    if (.not. have_pres) call field_fail(fields_to_do(f))
    field_ptr = qlls

  case ("qlcn")

    if (.not. have_pres) call field_fail(fields_to_do(f))
    field_ptr = qlcn

  end select

enddo

! Copy calendar infomation
xana%calendar_type = xctl%calendar_type
xana%date_init = xctl%date_init

end subroutine changevar

! ------------------------------------------------------------------------------

subroutine changevarinverse(self,geom,xana,xctl)

implicit none
type(fv3jedi_varcha_c2a), intent(inout) :: self
type(fv3jedi_geom),       intent(inout) :: geom
type(fv3jedi_state),      intent(in)    :: xana
type(fv3jedi_state),      intent(inout) :: xctl

integer :: f
character(len=field_clen), allocatable :: fields_to_do(:)
real(kind=kind_real), pointer :: field_ptr(:,:,:)

! Stream function/velocity potential
logical :: have_pcvd
real(kind=kind_real), pointer     ::    ud(:,:,:)     ! D-grid u wind
real(kind=kind_real), pointer     ::    vd(:,:,:)     ! D-grid v wind
real(kind=kind_real), allocatable ::   psi(:,:,:)     ! Stream function
real(kind=kind_real), allocatable ::   chi(:,:,:)     ! Velocity potentail
real(kind=kind_real), allocatable ::  vort(:,:,:)     ! Vorticity
real(kind=kind_real), allocatable ::  divg(:,:,:)     ! Divergence

! Pressure
logical :: have_pres
real(kind=kind_real), allocatable ::  delp(:,:,:)     ! Pressure thickness
real(kind=kind_real), pointer     ::    ps(:,:,:)     ! Pressure edges
real(kind=kind_real), pointer     ::    pe(:,:,:)     ! Pressure mid
real(kind=kind_real), allocatable ::     p(:,:,:)     ! Pressure mid

! Temperature
logical :: have_temp
real(kind=kind_real), pointer     ::    pt(:,:,:)     ! Potential temperature
real(kind=kind_real), allocatable ::   pkz(:,:,:)     ! Pressure ^ kappa
real(kind=kind_real), allocatable ::     t(:,:,:)     ! Temperature

! Humidity
logical :: have_rhum
real(kind=kind_real), pointer     ::     q(:,:,:)     ! Specific humidity
real(kind=kind_real), allocatable ::  qsat(:,:,:)     ! Saturation specific humidity
real(kind=kind_real), allocatable ::    rh(:,:,:)     ! Relative humidity

! Clouds
logical :: have_qiql, have_cfrc
real(kind=kind_real), allocatable :: qi   (:,:,:)     ! Cloud liquid ice
real(kind=kind_real), allocatable :: ql   (:,:,:)     ! Cloud liquid water
real(kind=kind_real), pointer     :: qils (:,:,:)     ! Cloud liquid ice large scale
real(kind=kind_real), pointer     :: qicn (:,:,:)     ! Cloud liquid ice convective
real(kind=kind_real), pointer     :: qlls (:,:,:)     ! Cloud liquid water large scale
real(kind=kind_real), pointer     :: qlcn (:,:,:)     ! Cloud liquid water convective
real(kind=kind_real), allocatable :: qilsf(:,:,:)     ! Fraction ice large scale
real(kind=kind_real), allocatable :: qicnf(:,:,:)     ! Fraction ice convective

! Virtual temperature
logical :: have_virt
real(kind=kind_real), allocatable ::   tv(:,:,:)      ! Virtual temperature

! Copy fields that are the same in both
! -------------------------------------
call copy_subset(xana%fields, xctl%fields, fields_to_do)

! If variable change is the identity early exit
! ---------------------------------------------
if (.not.allocated(fields_to_do)) return

! Wind variables
! --------------
have_pcvd = .false.
if (has_field(xana%fields, 'ud') .and. has_field(xana%fields, 'vd')) then
  call pointer_field_array(xana%fields, 'ud', ud)
  call pointer_field_array(xana%fields, 'vd', vd)
  allocate( psi(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  allocate( chi(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  allocate(vort(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  allocate(divg(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  call udvd_to_psichi(geom, self%grid, self%oprs, ud, vd, psi, chi, &
                      self%lprocs, self%lev_start, self%lev_final, vort, divg)
  have_pcvd = .true.
endif

! Pressure
! --------
have_pres = .false.
if (has_field(xana%fields, 'delp')) then
  call allocate_copy_field_array(xana%fields, 'delp', delp)
  allocate(ps(geom%isc:geom%iec,geom%jsc:geom%jec,1))
  ps(:,:,1) = sum(delp,3)
  have_pres = .true.
elseif (has_field(xana%fields, 'pe')) then
  call pointer_field_array(xana%fields, 'pe', pe )
  allocate(  ps(geom%isc:geom%iec,geom%jsc:geom%jec,1))
  allocate(delp(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  call pe_to_delp(geom, pe, delp)
  ps(:,:,1) = pe(:,:,geom%npz+1)
  have_pres = .true.
endif

! Temperature
! -----------
have_temp = .false.
if (has_field(xana%fields, 't')) then
  call allocate_copy_field_array(xana%fields, 't', t)
  have_temp = .true.
elseif (has_field(xana%fields, 'pt')) then
  allocate(t(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  call pointer_field_array(xana%fields, 'pt', pt)
  if (has_field(xana%fields, 'pkz')) then
    call allocate_copy_field_array(xana%fields, 'pkz', pkz)
    have_temp = .true.
  elseif (have_pres) then
    allocate( pkz(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
    call ps_to_pkz(geom, ps, pkz)
    have_temp = .true.
  endif
  if (have_temp) call pt_to_t(geom, pkz, pt, t)
endif

! Humidity
! --------
have_rhum = .false.
if (has_field(xana%fields, 'sphum') .and. have_temp .and. have_pres) then
  allocate(qsat(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  allocate(  rh(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  call pointer_field_array(xana%fields, 'sphum', q)
  call get_qsat(geom, delp, t, q, qsat)
  call q_to_rh(geom, qsat, q, rh)
  have_rhum = .true.
endif

! Clouds
! ------
have_qiql = .false.
have_cfrc = .false.
if (has_field(xana%fields, 'ice_wat') .and. has_field(xana%fields, 'liq_wat')) then
  call allocate_copy_field_array(xana%fields, 'ice_wat', qi)
  call allocate_copy_field_array(xana%fields, 'liq_wat', ql)
  have_qiql = .true.
elseif (has_field(xana%fields, 'qils') .and. has_field(xana%fields, 'qicn') .and. &
        has_field(xana%fields, 'qlls') .and. has_field(xana%fields, 'qlcn')) then
  call pointer_field_array(xana%fields, 'qils', qils)
  call pointer_field_array(xana%fields, 'qicn', qicn)
  call pointer_field_array(xana%fields, 'qlls', qlls)
  call pointer_field_array(xana%fields, 'qlcn', qlcn)
  allocate(qi(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  allocate(ql(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  allocate(qilsf(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  allocate(qicnf(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  call q4_to_q2(geom, qils, qicn, qlls, qlcn, qi, ql, qilsf, qicnf)
  have_cfrc = .true.
  have_qiql = .true.
endif

! Virtual temperature
! -------------------
have_virt = .false.
if (have_temp .and. has_field(xana%fields, 'sphum')) then
  allocate(tv(geom%isc:geom%iec,geom%jsc:geom%jec,1:geom%npz))
  call pointer_field_array(xana%fields, 'sphum', q)
  call t_to_tv(geom, t, q, tv)
  have_virt = .true.
endif

! Loop over the fields not found in the input state and work through cases
! ------------------------------------------------------------------------
do f = 1, size(fields_to_do)

  call pointer_field_array(xctl%fields, trim(fields_to_do(f)),  field_ptr)

  select case (trim(fields_to_do(f)))

  case ("psi")

    if (.not. have_pcvd) call field_fail(fields_to_do(f))
    field_ptr = psi

  case ("chi")

    if (.not. have_pcvd) call field_fail(fields_to_do(f))
    field_ptr = chi

  case ("vort")

    if (.not. have_pcvd) call field_fail(fields_to_do(f))
    field_ptr = vort

  case ("divg")

    if (.not. have_pcvd) call field_fail(fields_to_do(f))
    field_ptr = divg

  case ("ps")

    if (.not. have_pres) call field_fail(fields_to_do(f))
    field_ptr = ps

  case ("delp")

    if (.not. have_pres) call field_fail(fields_to_do(f))
    field_ptr = delp

  case ("t")

    if (.not. have_temp) call field_fail(fields_to_do(f))
    field_ptr = t

  case ("tv")

    if (.not. have_virt) call field_fail(fields_to_do(f))
    field_ptr = tv

  case ("rh")

    if (.not. have_rhum) call field_fail(fields_to_do(f))
    field_ptr = rh

  case ("ice_wat")

    if (.not. have_qiql) call field_fail(fields_to_do(f))
    field_ptr = qi

  case ("liq_wat")

    if (.not. have_qiql) call field_fail(fields_to_do(f))
    field_ptr = ql

  case ("qilsf")

    if (.not. have_cfrc) call field_fail(fields_to_do(f))
    field_ptr = qilsf

  case ("qicnf")

    if (.not. have_cfrc) call field_fail(fields_to_do(f))
    field_ptr = qicnf

  end select

enddo

! Copy calendar infomation
xctl%calendar_type = xana%calendar_type
xctl%date_init = xana%date_init

end subroutine changevarinverse

! --------------------------------------------------------------------------------------------------

subroutine field_fail(field)

implicit none
character(len=*), intent(in) :: field

call abor1_ftn("fv3jedi_vc_model2geovals_mod.field_fail: Field "//trim(field)//&
               " cannot be obtained from input fields.")

end subroutine field_fail

! ------------------------------------------------------------------------------

end module fv3jedi_varcha_c2a_mod
