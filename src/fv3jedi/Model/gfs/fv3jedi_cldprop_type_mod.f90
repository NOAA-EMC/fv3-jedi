module fv3jedi_cldprop_type_mod

use GFS_typedefs

implicit none

private
public GFSJEDI_cldprop_type

type GFSJEDI_cldprop_type

  real (kind=kind_phys), allocatable :: cv  (:)  !< fraction of convective cloud ; phys
  real (kind=kind_phys), allocatable :: cvt (:)  !< convective cloud top pressure in pa ; phys
  real (kind=kind_phys), allocatable :: cvb (:)  !< convective cloud bottom pressure in pa ; phys, cnvc90

  contains
    procedure :: create
    procedure :: delete
    procedure :: copy
    procedure :: gfs_to_jedi
    procedure :: jedi_to_gfs

end type GFSJEDI_cldprop_type

! --------------------------------------------------------------------------------------------------

contains

! --------------------------------------------------------------------------------------------------
subroutine create (cldprop_gfsjedi, IM, Model)

implicit none
class(GFSJEDI_cldprop_type), intent(inout) :: cldprop_gfsjedi
integer,                     intent(in)    :: IM
type(GFS_control_type),      intent(in)    :: Model

allocate (cldprop_gfsjedi%cv  (IM))
allocate (cldprop_gfsjedi%cvt (IM))
allocate (cldprop_gfsjedi%cvb (IM))

end subroutine create

! --------------------------------------------------------------------------------------------------

subroutine delete (cldprop_gfsjedi)

implicit none
class(GFSJEDI_cldprop_type), intent(inout) :: cldprop_gfsjedi

deallocate (cldprop_gfsjedi%cv )
deallocate (cldprop_gfsjedi%cvt)
deallocate (cldprop_gfsjedi%cvb)

end subroutine delete

! --------------------------------------------------------------------------------------------------

subroutine copy(cldprop_gfsjedi, other)

implicit none

class(GFSJEDI_cldprop_type), intent(inout) :: cldprop_gfsjedi
type(GFSJEDI_cldprop_type),  intent(in)    :: other

cldprop_gfsjedi%cv  = other%cv
cldprop_gfsjedi%cvt = other%cvt
cldprop_gfsjedi%cvb = other%cvb

end subroutine copy

! --------------------------------------------------------------------------------------------------

subroutine gfs_to_jedi(cldprop_gfsjedi, cldprop)

implicit none

class(GFSJEDI_cldprop_type), intent(inout) :: cldprop_gfsjedi
type(GFS_cldprop_type),      intent(in)    :: cldprop

cldprop_gfsjedi%cv  = cldprop%cv
cldprop_gfsjedi%cvt = cldprop%cvt
cldprop_gfsjedi%cvb = cldprop%cvb

end subroutine gfs_to_jedi

! --------------------------------------------------------------------------------------------------

subroutine jedi_to_gfs(cldprop_gfsjedi, cldprop)

implicit none

class(GFSJEDI_cldprop_type), intent(in)    :: cldprop_gfsjedi
type(GFS_cldprop_type),      intent(inout) :: cldprop

cldprop%cv  = cldprop_gfsjedi%cv
cldprop%cvt = cldprop_gfsjedi%cvt
cldprop%cvb = cldprop_gfsjedi%cvb

end subroutine jedi_to_gfs

! --------------------------------------------------------------------------------------------------

end module fv3jedi_cldprop_type_mod
