! (C) Copyright 2017-2018 UCAR
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 

module fv3jedi_io_geos_mod

use config_mod
use datetime_mod
use iso_c_binding

use fckit_mpi_module

use fv3jedi_constants_mod,    only: rad2deg
use fv3jedi_geom_mod,         only: fv3jedi_geom
use fv3jedi_field_mod,        only: fv3jedi_field
use fv3jedi_kinds_mod,        only: kind_real
use fv3jedi_netcdf_utils_mod, only: nccheck

use mpi
use netcdf

implicit none
private
public read_geos, write_geos

! ------------------------------------------------------------------------------

contains

! ------------------------------------------------------------------------------

subroutine read_geos(geom, fields, vdate, filename_in)

implicit none

!Arguments
type(fv3jedi_geom),         intent(in)    :: geom        !< Geom
type(fv3jedi_field),        intent(inout) :: fields(:)   !< Fields
type(datetime),             intent(inout) :: vdate       !< DateTime
character(len=*), optional, intent(in)    :: filename_in !< Filename

logical :: iam_io_proc
character(len=255) :: filename
integer :: ncid, dimid, varid
integer :: im, jm, lm, nm
integer :: date(6), intdate, inttime
character(len=8) :: cdate
character(len=6) :: ctime
integer :: idate, isecs
integer(kind=c_int) :: cidate, cisecs
integer :: ncdim3, ncdim2, lev, vindex
integer, target, allocatable :: istart3(:), icount3(:)
integer, target, allocatable :: istart2(:), icount2(:)
integer, pointer :: istart(:), icount(:)
integer :: n, geostiledim, tileoff
real(kind=kind_real), allocatable :: arrayg(:,:)
type(fckit_mpi_comm) :: f_comm
integer :: commtile, ranktile, ierr

! Processors that will handle IO
f_comm = fckit_mpi_comm()

! Split comm and set flag for IO procs
iam_io_proc = .true.

if (f_comm%size() > 6) then

  call mpi_comm_split(f_comm%communicator(), geom%ntile, f_comm%rank(), commtile, ierr)

  call mpi_comm_rank(commtile, ranktile, ierr);
  if (ranktile .ne. 0) iam_io_proc = .false.

endif


idate = 0
isecs = 0

if (iam_io_proc) then

  ! Set filenames
  if (.not.present(filename_in)) then
     filename = 'Data/GEOS.bkg.eta.nc4'
  else
     filename = trim(filename_in)
  endif
  
  ! Open the file
  call nccheck ( nf90_open(trim(filename), NF90_NOWRITE, ncid), "nf90_open"//trim(filename) )
  
  ! Get dimensions, XDim,YDim,lev,time
  call nccheck ( nf90_inq_dimid(ncid, "Xdim", dimid), "nf90_inq_dimid Xdim" )
  call nccheck ( nf90_inquire_dimension(ncid, dimid, len = im), "nf90_inquire_dimension Xdim" )
  
  call nccheck ( nf90_inq_dimid(ncid, "Ydim", dimid), "nf90_inq_dimid YDim" )
  call nccheck ( nf90_inquire_dimension(ncid, dimid, len = jm), "nf90_inquire_dimension YDim" )
  
  call nccheck ( nf90_inq_dimid(ncid, "lev",  dimid), "nf90_inq_dimid lev" )
  call nccheck ( nf90_inquire_dimension(ncid, dimid, len = lm), "nf90_inquire_dimension lev" )
  
  call nccheck ( nf90_inq_dimid(ncid, "time", dimid), "nf90_inq_dimid time" )
  call nccheck ( nf90_inquire_dimension(ncid, dimid, len = nm), "nf90_inquire_dimension time" )
  
  ! Get time attributes
  call nccheck ( nf90_inq_varid(ncid, "time", varid), "nf90_inq_varid time" )
  call nccheck ( nf90_get_att(ncid, varid, "begin_date", intdate), "nf90_get_att begin_date" )
  call nccheck ( nf90_get_att(ncid, varid, "begin_time", inttime), "nf90_get_att begin_time" )
  
  ! Pad with leading zeros if need be
  write(cdate,"(I0.8)") intdate
  write(ctime,"(I0.6)") inttime
  
  ! Back to integer
  read(cdate(1:4),*) date(1)
  read(cdate(5:6),*) date(2)
  read(cdate(7:8),*) date(3)
  read(ctime(1:2),*) date(4)
  read(ctime(3:4),*) date(5)
  read(ctime(5:6),*) date(6)
  
  ! To idate/isecs for Jedi
  idate = date(1)*10000 + date(2)*100 + date(3)
  isecs = date(4)*3600  + date(5)*60  + date(6)
  
  ! Make sure file dimensions equal to geometry
  if ( im /= geom%npx-1 .or. lm /= geom%npz) then
    call abor1_ftn("GEOS restarts: restart dimension not compatible with geometry")
  endif
  
  ! GEOS can use concatenated tiles or tile as a dimension
  if ( (im == geom%npx-1) .and. (jm == 6*(geom%npy-1) ) ) then
    geostiledim = 0
    tileoff = (geom%ntile-1)*(jm/geom%ntiles)
    ncdim3 = 4
    ncdim2 = 3
  else
    geostiledim = 1
    tileoff = 0
    ncdim3 = 5
    ncdim2 = 4
  endif
  
  allocate(istart3(ncdim3),icount3(ncdim3))
  allocate(istart2(ncdim2),icount2(ncdim2))
  
  ! Create local to this proc start/count
  if (geostiledim == 1) then
    istart3(1) = 1;           icount3(1) = geom%npx-1  !X
    istart3(2) = 1;           icount3(2) = geom%npy-1  !Y
    istart3(3) = geom%ntile;  icount3(3) = 1           !Tile
    istart3(4) = 1;           icount3(4) = 1           !Lev
    istart3(5) = 1;           icount3(5) = 1           !Time
    istart2(1) = 1;           icount2(1) = geom%npx-1
    istart2(2) = 1;           icount2(2) = geom%npy-1
    istart2(3) = geom%ntile;  icount2(3) = 1
    istart2(4) = 1;           icount2(4) = 1
    vindex = 4
  else
    istart3(1) = 1;           icount3(1) = geom%npx-1
    istart3(2) = tileoff+1;   icount3(2) = geom%npy-1
    istart3(3) = 1;           icount3(3) = 1
    istart3(4) = 1;           icount3(4) = 1
    istart2(1) = 1;           icount2(1) = geom%npx-1
    istart2(2) = tileoff+1;   icount2(2) = geom%npy-1
    istart2(3) = 1;           icount2(3) = 1
    vindex = 3
  endif
  
endif

allocate(arrayg(1:geom%npx-1,1:geom%npy-1)) !Whole tile array

do n = 1,size(fields)

  if (iam_io_proc) then
    if (fields(n)%npz == 1) then
      istart => istart2; icount => icount2
    elseif (fields(n)%npz == geom%npz) then
      istart => istart3; icount => icount3
    else
      call abor1_ftn("read_geos: vertical dimension not supported")
    endif
  endif

  do lev = 1,fields(n)%npz

    arrayg = 0.0_kind_real    
    if (iam_io_proc) then
      istart3(vindex) = lev
      call nccheck ( nf90_inq_varid (ncid, trim(fields(n)%short_name), varid), &
                    "nf90_inq_varid "//trim(fields(n)%short_name) )
      call nccheck ( nf90_get_var( ncid, varid, arrayg, istart, icount), &
                    "nf90_get_var "//trim(fields(n)%short_name) )
    endif

    if (f_comm%size() > 6) then
      call scatter_tile(geom, commtile, arrayg, fields(n)%array(geom%isc:geom%iec,geom%jsc:geom%jec,lev))
    else
      fields(n)%array(geom%isc:geom%iec,geom%jsc:geom%jec,lev) = arrayg(geom%isc:geom%iec,geom%jsc:geom%jec) !1 proc per tile already
    endif
  enddo

  if (iam_io_proc) then
    nullify(istart,icount)
  endif

enddo
  
if (iam_io_proc) then
  !Close this file
  call nccheck ( nf90_close(ncid), "nf90_close" )
  
  ! Deallocate
  deallocate ( istart2, icount2 )
  deallocate ( istart3, icount3 )
endif

deallocate(arrayg)

! Set the object date from the date of the file
call f_comm%broadcast(idate,0)
call f_comm%broadcast(isecs,0)
cidate = idate
cisecs = isecs
call datetime_from_ifs(vdate, cidate, cisecs)

! Release split comms
if (f_comm%size() > 6) then
  call MPI_Comm_free(commtile, ierr)
endif

end subroutine read_geos

! ------------------------------------------------------------------------------

subroutine write_geos(geom, fields, c_conf, vdate)

implicit none

! Arguments
type(fv3jedi_geom),  intent(in)    :: geom       !< Geom
type(fv3jedi_field), intent(in)    :: fields(:)  !< Fields
type(c_ptr),         intent(in)    :: c_conf     !< Configuration
type(datetime),      intent(in)    :: vdate      !< DateTime

! Locals
logical :: iam_io_proc

character(len=255) :: datapath, filename
character(len=64)  :: datefile
character(len=8)   :: date8s
character(len=6)   :: time6s
integer :: date8, time6
integer :: ncid, varid(1000), vc
integer :: date(6)
integer(kind=c_int) :: idate, isecs
integer :: n
integer :: x_dimid, y_dimid, n_dimid, z_dimid, t_dimid
integer :: ncdim3, ncdim2
integer :: ndimidsv, ndimidsg, ndimids2, ndimids3
integer, allocatable :: dimidsv(:), dimidsg(:), dimids2(:), dimids3(:)
integer, allocatable :: istart2(:), icount2(:)
integer, allocatable :: istart3(:), icount3(:)
integer, allocatable :: istart(:), icount(:), dimids(:), intarray(:)
real(kind=kind_real), allocatable :: arrayg(:,:), realarray(:)
type(fckit_mpi_comm) :: f_comm
integer :: lev, commtile, ranktile, commwrite, ierr

! Processors that will handle IO
f_comm = fckit_mpi_comm()

iam_io_proc = .true.

! Split the comm, one per tile
if (f_comm%size() > 6) then
  call mpi_comm_split(f_comm%communicator(), geom%ntile, f_comm%rank(), commtile, ierr)
  call mpi_comm_rank(commtile, ranktile, ierr);

  ! Write communicator
  call mpi_comm_split(f_comm%communicator(), ranktile, geom%ntile, commwrite, ierr)

  if (ranktile .ne. 0) iam_io_proc = .false.

else

  call mpi_comm_dup(f_comm%communicator(), commwrite, ierr)

endif

! Arrays to write from
allocate(arrayg(1:geom%npx-1,1:geom%npy-1)) !Whole tile array

if (iam_io_proc) then
  
  ! Place to save restarts
  datapath = "Data/"
  if (config_element_exists(c_conf,"datapath")) then
     datapath = config_get_string(c_conf,len(datapath),"datapath")
  endif
    
  ! Base filename
  filename = 'GEOS.eta.'
  if (config_element_exists(c_conf,"filename")) then
     filename = config_get_string(c_conf,len(filename),"filename")
  endif
  
  ! Append with the date
  call datetime_to_ifs(vdate, idate, isecs)
  date(1) = idate/10000
  date(2) = idate/100 - date(1)*100
  date(3) = idate - (date(1)*10000 + date(2)*100)
  date(4) = isecs/3600
  date(5) = (isecs - date(4)*3600)/60
  date(6) = isecs - (date(4)*3600 + date(5)*60)
  write(datefile,'(I4,I0.2,I0.2,A1,I0.2,I0.2,I0.2)') date(1),date(2),date(3),"_",date(4),date(5),date(6)
  filename = trim(datapath)//trim(filename)//trim(datefile)//trim("z.nc4")

  write(datefile,'(I4,A1,I0.2,A1,I0.2,A1,I0.2,A1,I0.2,A1,I0.2)') date(1),"-",date(2),"-",date(3)," "&
                                                    ,date(4),":",date(5),":",date(6)
  
  write(date8s,'(I4,I0.2,I0.2)')   date(1),date(2),date(3)
  write(time6s,'(I0.2,I0.2,I0.2)') date(4),date(5),date(6)
  read(date8s,*)  date8
  read(time6s,*)  time6
  
  ! Create the file
  call nccheck( nf90_create( filename, ior(NF90_NETCDF4, NF90_MPIIO), ncid, &
                             comm = commwrite, info = MPI_INFO_NULL), "nf90_create" )
 
  ! Create dimensions
  call nccheck ( nf90_def_dim(ncid, "Xdim", geom%npx-1,  x_dimid), "nf90_def_dim Xdim" )
  call nccheck ( nf90_def_dim(ncid, "Ydim", geom%npy-1,  y_dimid), "nf90_def_dim Ydim" )
  call nccheck ( nf90_def_dim(ncid, "nf",   geom%ntiles, n_dimid), "nf90_def_dim nf"   )
  call nccheck ( nf90_def_dim(ncid, "lev",  geom%npz,    z_dimid), "nf90_def_dim lev"  )
  call nccheck ( nf90_def_dim(ncid, "time", 1,           t_dimid), "nf90_def_dim time" )
  
  ! DimId arrays
  ndimidsv = 1
  ndimidsg = 3
  ndimids2 = 4
  ndimids3 = 5

  allocate(dimidsv(ndimidsv))
  dimidsv =  (/ z_dimid /)
  allocate(dimidsg(ndimidsg))
  dimidsg =  (/ x_dimid, y_dimid, n_dimid /)
  allocate(dimids2(ndimids2))
  dimids2 =  (/ x_dimid, y_dimid, n_dimid, t_dimid /)
  allocate(dimids3(ndimids3))
  dimids3 =  (/ x_dimid, y_dimid, n_dimid, z_dimid, t_dimid /)
  
  ! Define fields to be written (geom) 
  vc=1;
  call nccheck( nf90_def_var(ncid, "nf", NF90_INT, n_dimid, varid(vc)), "nf90_def_var nf" )
  call nccheck( nf90_put_att(ncid, varid(vc), "long_name", "cubed-sphere face") )
  call nccheck( nf90_put_att(ncid, varid(vc), "axis", "e") )
  call nccheck( nf90_put_att(ncid, varid(vc), "grads_dim", "e") )

  vc=vc+1;
  call nccheck( nf90_def_var(ncid, "Xdim", NF90_DOUBLE, x_dimid, varid(vc)), "nf90_def_var Xdim" )
  call nccheck( nf90_put_att(ncid, varid(vc), "long_name", "Fake Longitude for GrADS Compatibility") )
  call nccheck( nf90_put_att(ncid, varid(vc), "units", "degrees_east") )

  vc=vc+1;
  call nccheck( nf90_def_var(ncid, "Ydim", NF90_DOUBLE, y_dimid, varid(vc)), "nf90_def_var Ydim" )
  call nccheck( nf90_put_att(ncid, varid(vc), "long_name", "Fake Latitude for GrADS Compatibility") )
  call nccheck( nf90_put_att(ncid, varid(vc), "units", "degrees_north") )

  vc=vc+1;
  call nccheck( nf90_def_var(ncid, "lons", NF90_DOUBLE, dimidsg, varid(vc)), "nf90_def_var lons" )
  call nccheck( nf90_put_att(ncid, varid(vc), "long_name", "longitude") )
  call nccheck( nf90_put_att(ncid, varid(vc), "units", "degrees_east") )
  
  vc=vc+1;
  call nccheck( nf90_def_var(ncid, "lats", NF90_DOUBLE, dimidsg, varid(vc)), "nf90_def_var lats" )
  call nccheck( nf90_put_att(ncid, varid(vc), "long_name", "latitude") )
  call nccheck( nf90_put_att(ncid, varid(vc), "units", "degrees_north") )
  
  vc=vc+1;
  call nccheck( nf90_def_var(ncid, "lev", NF90_DOUBLE, z_dimid, varid(vc)), "nf90_def_var lev" )
  call nccheck( nf90_put_att(ncid, varid(vc), "long_name", "vertical level") )
  call nccheck( nf90_put_att(ncid, varid(vc), "units", "layer") )
  call nccheck( nf90_put_att(ncid, varid(vc), "positive", "down") )
  call nccheck( nf90_put_att(ncid, varid(vc), "coordinate", "eta") )
  call nccheck( nf90_put_att(ncid, varid(vc), "standard_name", "model_layers") )

  vc=vc+1;
  call nccheck( nf90_def_var(ncid, "time", NF90_INT, t_dimid, varid(vc)), "nf90_def_var time" )
  call nccheck( nf90_put_att(ncid, varid(vc), "long_name", "time"), "nf90_def_var time long_name" )
  call nccheck( nf90_put_att(ncid, varid(vc), "begin_date", date8), "nf90_def_var time begin_date" )
  call nccheck( nf90_put_att(ncid, varid(vc), "begin_time", time6), "nf90_def_var time begin_time" )
  

  ! Define fields to be written
  do n = 1,size(fields)
  
    if (fields(n)%npz == 1) then
      allocate(dimids(ndimids2))
      dimids = dimids2
    elseif (fields(n)%npz == geom%npz) then
      allocate(dimids(ndimids3))
      dimids = dimids3
    else
      call abor1_ftn("read_geos: vertical dimension not supported")
    endif
  
    vc=vc+1
    call nccheck( nf90_def_var(ncid, trim(fields(n)%short_name), NF90_DOUBLE, dimids, varid(vc)), &
                  "nf90_def_var"//trim(fields(n)%short_name)   )
    call nccheck( nf90_put_att(ncid, varid(vc), "long_name"    , trim(fields(n)%long_name) ), "nf90_put_att" )
    call nccheck( nf90_put_att(ncid, varid(vc), "units"        , trim(fields(n)%units)     ), "nf90_put_att" )
    call nccheck( nf90_put_att(ncid, varid(vc), "standard_name", trim(fields(n)%long_name) ), "nf90_put_att" )
    call nccheck( nf90_put_att(ncid, varid(vc), "coordinates"  , "lons lats"               ), "nf90_put_att" )
    call nccheck( nf90_put_att(ncid, varid(vc), "grid_mapping" , "cubed_sphere"            ), "nf90_put_att" )
  
    deallocate(dimids)
  
  enddo
  
  ! End define mode
  call nccheck( nf90_enddef(ncid), "nf90_enddef" )
  
  ncdim3 = 5
  ncdim2 = 4

  allocate(istart3(ncdim3),icount3(ncdim3))
  allocate(istart2(ncdim2),icount2(ncdim2))
  
  ! Create local to this proc start/count
  istart3(1) = 1;           icount3(1) = geom%npx-1  !X
  istart3(2) = 1;           icount3(2) = geom%npy-1  !Y
  istart3(3) = geom%ntile;  icount3(3) = 1           !Tile
  istart3(4) = 1;           icount3(4) = 1           !Lev
  istart3(5) = 1;           icount3(5) = 1           !Time

  istart2(1) = 1;           icount2(1) = geom%npx-1
  istart2(2) = 1;           icount2(2) = geom%npy-1
  istart2(3) = geom%ntile;  icount2(3) = 1
  istart2(4) = 1;           icount2(4) = 1
 
  vc=0

  allocate(intarray(6))
  do n = 1,6
    intarray(n) = n
  enddo
  vc=vc+1;call nccheck( nf90_put_var( ncid, varid(vc), intarray ), "nf90_put_var nf" )  
  deallocate(intarray)

  allocate(realarray(geom%npx-1))
  do n = 1,geom%npx-1
    realarray(n) = real(n,kind_real)
  enddo

  vc=vc+1;call nccheck( nf90_put_var( ncid, varid(vc), realarray ), "nf90_put_var Xdim" )  
  vc=vc+1;call nccheck( nf90_put_var( ncid, varid(vc), realarray ), "nf90_put_var Ydim" )  

  deallocate(realarray)

endif

! Gather longitudes to write
if (f_comm%size() > 6) then
  call gather_tile(geom, commtile, rad2deg*geom%grid_lon(geom%isc:geom%iec,geom%jsc:geom%jec), arrayg)
else
  arrayg = rad2deg*geom%grid_lon(geom%isc:geom%iec,geom%jsc:geom%jec)
endif

if (iam_io_proc) then

  vc=vc+1;call nccheck( nf90_put_var( ncid, varid(vc), arrayg, &
                                      start = istart2(1:3), count = icount2(1:3) ), "nf90_put_var lons" )

endif

! Gather latitudes to write
if (f_comm%size() > 6) then
  call gather_tile(geom, commtile, rad2deg*geom%grid_lat(geom%isc:geom%iec,geom%jsc:geom%jec), arrayg)
else
  arrayg = rad2deg*geom%grid_lat(geom%isc:geom%iec,geom%jsc:geom%jec)
endif

if (iam_io_proc) then

  vc=vc+1;call nccheck( nf90_put_var( ncid, varid(vc), arrayg, &
                                      start = istart2(1:3), count = icount2(1:3) ), "nf90_put_var lats" )

  allocate(intarray(geom%npz))
  do n = 1,geom%npz
    intarray(n) = n
  enddo
  vc=vc+1;call nccheck( nf90_put_var( ncid, varid(vc), intarray ), "nf90_put_var lev" )  
  deallocate(intarray)

  vc=vc+1;call nccheck( nf90_put_var( ncid, varid(vc), 0 ), "nf90_put_var time" )  

endif


! Write fields 
do n = 1,size(fields)
  
  if (iam_io_proc) then
    if (fields(n)%npz == 1) then
      allocate(istart(ncdim2))
      allocate(icount(ncdim2))
      istart = istart2
      icount = icount2
    elseif (fields(n)%npz == geom%npz) then
      allocate(istart(ncdim3))
      allocate(icount(ncdim3))
      istart = istart3
      icount = icount3
    else
      call abor1_ftn("read_geos: vertical dimension not supported")
    endif
    vc = vc + 1
  endif 
 
  do lev = 1,fields(n)%npz

    if (f_comm%size() > 6) then
      call gather_tile(geom, commtile, fields(n)%array(geom%isc:geom%iec,geom%jsc:geom%jec,lev), arrayg)
    else
      arrayg = fields(n)%array(:,:,lev)
    endif

    if (iam_io_proc) then

      if (fields(n)%npz == geom%npz) istart(4) = lev

      call nccheck( nf90_put_var( ncid, varid(vc), arrayg, start = istart, count = icount ), &
                                  "nf90_put_var "//trim(fields(n)%short_name) )
 
    endif

  enddo

  if (iam_io_proc) deallocate(istart,icount)

enddo

if (iam_io_proc) then

  ! Close file
  call nccheck( nf90_close(ncid), "nf90_close" )

  ! Deallocate
  deallocate ( dimidsv, dimidsg, dimids2, dimids3 )
  deallocate ( istart2, icount2 )
  deallocate ( istart3, icount3 )

endif

deallocate(arrayg)

! Release split comms
if (f_comm%size() > 6) then
  call MPI_Comm_free(commtile, ierr)
endif

call MPI_Comm_free(commwrite, ierr)

end subroutine write_geos

! ------------------------------------------------------------------------------

subroutine gather_tile(geom, comm, array_l, array_g)

type(fv3jedi_geom),   intent(in)    :: geom
integer,              intent(in)    :: comm
real(kind=kind_real), intent(in)    :: array_l(geom%isc:geom%iec,geom%jsc:geom%jec)  ! Local array
real(kind=kind_real), intent(inout) :: array_g(1:geom%npx-1,1:geom%npy-1)            ! Gathered array (only valid on root)

integer :: comm_size
integer :: n, ierr, npx_l, npy_l, subarray, resized_subarray
integer :: sizes_g(2), sizes_l(2), start_l(2), arraydispls_me
integer, allocatable :: counts(:), displs(:), arraydispls(:)
integer(kind=MPI_ADDRESS_KIND) :: extent, lb
real(kind=kind_real) :: forsize

call mpi_comm_size(comm, comm_size, ierr)

npx_l = geom%iec-geom%isc+1
npy_l = geom%jec-geom%jsc+1

sizes_g = [geom%npx-1, geom%npy-1]
sizes_l = [npx_l, npy_l]
start_l = [geom%isc-1, geom%jsc-1]

! Create recieving array
call mpi_type_create_subarray(2, sizes_g, sizes_l, start_l, mpi_order_fortran, mpi_double_precision, subarray, ierr)
call mpi_type_commit(subarray, ierr)

! Perform resizing
extent = sizeof(forsize)
lb = 0
call mpi_type_create_resized(subarray, lb, extent, resized_subarray, ierr)
call mpi_type_commit(resized_subarray,ierr)

! Set counts and displacement and gather
allocate(counts(comm_size), arraydispls(comm_size), displs(comm_size))

do n = 1,comm_size
   displs(n) = n-1
   counts(n) = 1
enddo

arraydispls = 0
arraydispls_me = (geom%isc - 1) + (geom%jsc - 1) * (geom%npy-1)
call mpi_allgatherv(arraydispls_me, 1, mpi_int, arraydispls, counts, displs, mpi_int, comm, ierr)

! Gather the full field
call mpi_gatherv( array_l, npx_l*npy_l, mpi_double_precision, &
                  array_g, counts, arraydispls, resized_subarray, &
                  0, comm, ierr)

! Deallocate
deallocate(counts,displs,arraydispls)

end subroutine gather_tile

! ------------------------------------------------------------------------------

subroutine scatter_tile(geom, comm, array_g, array_l)

type(fv3jedi_geom),   intent(in)    :: geom
integer,              intent(in)    :: comm
real(kind=kind_real), intent(in)    :: array_g(1:geom%npx-1,1:geom%npy-1)            ! Gathered array (only valid on root)
real(kind=kind_real), intent(inout) :: array_l(geom%isc:geom%iec,geom%jsc:geom%jec)  ! Local array

integer :: comm_size
integer :: n, ierr, npx_l, npy_l, subarray, resized_subarray
integer :: sizes_g(2), sizes_l(2), start_l(2), arraydispls_me
integer, allocatable :: counts(:), displs(:), arraydispls(:)
integer(kind=MPI_ADDRESS_KIND) :: extent, lb
real(kind=kind_real) :: forsize

call mpi_comm_size(comm, comm_size, ierr)

npx_l = geom%iec-geom%isc+1
npy_l = geom%jec-geom%jsc+1

sizes_g = [geom%npx-1, geom%npy-1]
sizes_l = [npx_l, npy_l]
start_l = [geom%isc-1, geom%jsc-1]

! Create recieving array
call mpi_type_create_subarray(2, sizes_g, sizes_l, start_l, mpi_order_fortran, mpi_double_precision, subarray, ierr)
call mpi_type_commit(subarray, ierr)

! Perform resizing
extent = sizeof(forsize)
lb = 0
call mpi_type_create_resized(subarray, lb, extent, resized_subarray, ierr)
call mpi_type_commit(resized_subarray,ierr)

! Set counts and displacement and gather
allocate(counts(comm_size), arraydispls(comm_size), displs(comm_size))

do n = 1,comm_size
   displs(n) = n-1
   counts(n) = 1
enddo

arraydispls = 0
arraydispls_me = (geom%isc - 1) + (geom%jsc - 1) * (geom%npy-1)
call mpi_allgatherv(arraydispls_me, 1, mpi_int, arraydispls, counts, displs, mpi_int, comm, ierr)

! Scatter the full field
call mpi_scatterv( array_g, counts, arraydispls, resized_subarray, &
                   array_l, npx_l*npy_l, mpi_double_precision, &
                   0, comm, ierr )

! Deallocate
deallocate(counts,displs,arraydispls)

end subroutine scatter_tile

! ------------------------------------------------------------------------------

end module fv3jedi_io_geos_mod
