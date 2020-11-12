module fv3jedi_tbdprop_type_mod

use GFS_typedefs

implicit none

private
public GFSJEDI_tbdprop_type

type GFSJEDI_tbdprop_type

  !--- radiation random seeds
      integer,               allocatable :: icsdsw   (:)       !< (rad. only) auxiliary cloud control arrays passed to main
      integer,               allocatable :: icsdlw   (:)       !< (rad. only) radiations. if isubcsw/isubclw (input to init)
                                                                    !< (rad. only) are set to 2, the arrays contains provided
                                                                    !< (rad. only) random seeds for sub-column clouds generator

  !--- In
      real (kind=kind_phys), allocatable :: ozpl     (:,:,:)   !< ozone forcing data
      real (kind=kind_phys), allocatable :: h2opl    (:,:,:)   !< water forcing data
      real (kind=kind_phys), allocatable :: in_nm    (:,:)     !< IN number concentration
      real (kind=kind_phys), allocatable :: ccn_nm   (:,:)     !< CCN number concentration
      real (kind=kind_phys), allocatable :: aer_nm   (:,:,:)   !< GOCART aerosol climo

      !--- active when ((.not. newsas .or. cal_pre) .and. random_clds)
      real (kind=kind_phys), allocatable :: rann     (:,:)     !< random number array (0-1)

  !--- In/Out
      real (kind=kind_phys), allocatable :: acv      (:)       !< array containing accumulated convective clouds
      real (kind=kind_phys), allocatable :: acvb     (:)       !< arrays used by cnvc90 bottom
      real (kind=kind_phys), allocatable :: acvt     (:)       !< arrays used by cnvc90 top (cnvc90.f)

  !--- Stochastic physics properties calculated in physics_driver
      real (kind=kind_phys), allocatable :: dtdtr     (:,:)    !< temperature change due to radiative heating per time step (K)
      real (kind=kind_phys), allocatable :: dtotprcp  (:)      !< change in totprcp  (diag_type)
      real (kind=kind_phys), allocatable :: dcnvprcp  (:)      !< change in cnvprcp  (diag_type)
      real (kind=kind_phys), allocatable :: drain_cpl (:)      !< change in rain_cpl (coupling_type)
      real (kind=kind_phys), allocatable :: dsnow_cpl (:)      !< change in show_cpl (coupling_type)

  !--- phy_f*d variables needed for seamless restarts and moving data between grrad and gbphys
      real (kind=kind_phys), allocatable :: phy_fctd (:,:)     !< cloud base mass flux for CS convection
      real (kind=kind_phys), allocatable :: phy_f2d  (:,:)     !< 2d arrays saved for restart
      real (kind=kind_phys), allocatable :: phy_f3d  (:,:,:)   !< 3d arrays saved for restart

  !--- for explicit data blocking
      integer                        :: blkno                       !< block number of this block

  contains
    procedure :: create
    procedure :: delete
    procedure :: copy
    procedure :: gfs_to_jedi
    procedure :: jedi_to_gfs

end type GFSJEDI_tbdprop_type

! --------------------------------------------------------------------------------------------------

contains

! --------------------------------------------------------------------------------------------------
subroutine create (tbdprop_gfsjedi, IM, BLKNO, Model)

implicit none
class(GFSJEDI_tbdprop_type), intent(inout) :: tbdprop_gfsjedi
integer,                     intent(in)    :: IM
integer,                     intent(in)    :: BLKNO
type(GFS_control_type),      intent(in)    :: Model

    !--- In
    !--- sub-grid cloud radiation
        if ( Model%isubc_lw == 2 .or. Model%isubc_sw == 2 ) then
          allocate (tbdprop_gfsjedi%icsdsw (IM))
          allocate (tbdprop_gfsjedi%icsdlw (IM))
        endif

    !--- ozone and stratosphere h2o needs
        ! DH* oz_coeff is set to zero if both ozphys options are false,
        ! better to use conditional allocations here for ozpl (and h2opl)? *DH
        allocate (tbdprop_gfsjedi%ozpl  (IM,levozp,oz_coeff))
        allocate (tbdprop_gfsjedi%h2opl (IM,levh2o,h2o_coeff))

    !--- ccn and in needs
        ! DH* allocate only for MG? *DH
        allocate (tbdprop_gfsjedi%in_nm  (IM,Model%levs))
        allocate (tbdprop_gfsjedi%ccn_nm (IM,Model%levs))

    !--- aerosol fields
        ! DH* allocate only for MG? *DH
        allocate (tbdprop_gfsjedi%aer_nm  (IM,Model%levs,ntrcaer))

        allocate (tbdprop_gfsjedi%rann (IM,Model%nrcm))
        tbdprop_gfsjedi%rann = 0.6_kind_phys

    !--- In/Out
        allocate (tbdprop_gfsjedi%acv  (IM))
        allocate (tbdprop_gfsjedi%acvb (IM))
        allocate (tbdprop_gfsjedi%acvt (IM))

        if (Model%do_sppt) then
          allocate (tbdprop_gfsjedi%dtdtr     (IM,Model%levs))
          allocate (tbdprop_gfsjedi%dtotprcp  (IM))
          allocate (tbdprop_gfsjedi%dcnvprcp  (IM))
          allocate (tbdprop_gfsjedi%drain_cpl (IM))
          allocate (tbdprop_gfsjedi%dsnow_cpl (IM))
        endif

        allocate (tbdprop_gfsjedi%phy_f2d  (IM,Model%ntot2d))
        allocate (tbdprop_gfsjedi%phy_f3d  (IM,Model%levs,Model%ntot3d))

        if (Model%nctp > 0 .and. Model%cscnv) then
          allocate (tbdprop_gfsjedi%phy_fctd (IM,Model%nctp))
        endif

        tbdprop_gfsjedi%blkno = BLKNO

end subroutine create

! --------------------------------------------------------------------------------------------------

subroutine delete (tbdprop_gfsjedi)

implicit none
class(GFSJEDI_tbdprop_type), intent(inout) :: tbdprop_gfsjedi

if (allocated(tbdprop_gfsjedi%icsdsw   )) deallocate(tbdprop_gfsjedi%icsdsw   )
if (allocated(tbdprop_gfsjedi%icsdlw   )) deallocate(tbdprop_gfsjedi%icsdlw   )
if (allocated(tbdprop_gfsjedi%ozpl     )) deallocate(tbdprop_gfsjedi%ozpl     )
if (allocated(tbdprop_gfsjedi%h2opl    )) deallocate(tbdprop_gfsjedi%h2opl    )
if (allocated(tbdprop_gfsjedi%in_nm    )) deallocate(tbdprop_gfsjedi%in_nm    )
if (allocated(tbdprop_gfsjedi%ccn_nm   )) deallocate(tbdprop_gfsjedi%ccn_nm   )
if (allocated(tbdprop_gfsjedi%aer_nm   )) deallocate(tbdprop_gfsjedi%aer_nm   )
if (allocated(tbdprop_gfsjedi%rann     )) deallocate(tbdprop_gfsjedi%rann     )
if (allocated(tbdprop_gfsjedi%acv      )) deallocate(tbdprop_gfsjedi%acv      )
if (allocated(tbdprop_gfsjedi%acvb     )) deallocate(tbdprop_gfsjedi%acvb     )
if (allocated(tbdprop_gfsjedi%acvt     )) deallocate(tbdprop_gfsjedi%acvt     )
if (allocated(tbdprop_gfsjedi%dtdtr    )) deallocate(tbdprop_gfsjedi%dtdtr    )
if (allocated(tbdprop_gfsjedi%dtotprcp )) deallocate(tbdprop_gfsjedi%dtotprcp )
if (allocated(tbdprop_gfsjedi%dcnvprcp )) deallocate(tbdprop_gfsjedi%dcnvprcp )
if (allocated(tbdprop_gfsjedi%drain_cpl)) deallocate(tbdprop_gfsjedi%drain_cpl)
if (allocated(tbdprop_gfsjedi%dsnow_cpl)) deallocate(tbdprop_gfsjedi%dsnow_cpl)
if (allocated(tbdprop_gfsjedi%phy_fctd )) deallocate(tbdprop_gfsjedi%phy_fctd )
if (allocated(tbdprop_gfsjedi%phy_f2d  )) deallocate(tbdprop_gfsjedi%phy_f2d  )
if (allocated(tbdprop_gfsjedi%phy_f3d  )) deallocate(tbdprop_gfsjedi%phy_f3d  )

end subroutine delete

! --------------------------------------------------------------------------------------------------

subroutine copy(tbdprop_gfsjedi, other)

implicit none

class(GFSJEDI_tbdprop_type), intent(inout) :: tbdprop_gfsjedi
type(GFSJEDI_tbdprop_type),  intent(in)    :: other

if (allocated(tbdprop_gfsjedi%icsdsw   )) tbdprop_gfsjedi%icsdsw    = other%icsdsw
if (allocated(tbdprop_gfsjedi%icsdlw   )) tbdprop_gfsjedi%icsdlw    = other%icsdlw
if (allocated(tbdprop_gfsjedi%ozpl     )) tbdprop_gfsjedi%ozpl      = other%ozpl
if (allocated(tbdprop_gfsjedi%h2opl    )) tbdprop_gfsjedi%h2opl     = other%h2opl
if (allocated(tbdprop_gfsjedi%in_nm    )) tbdprop_gfsjedi%in_nm     = other%in_nm
if (allocated(tbdprop_gfsjedi%ccn_nm   )) tbdprop_gfsjedi%ccn_nm    = other%ccn_nm
if (allocated(tbdprop_gfsjedi%aer_nm   )) tbdprop_gfsjedi%aer_nm    = other%aer_nm
if (allocated(tbdprop_gfsjedi%rann     )) tbdprop_gfsjedi%rann      = other%rann
if (allocated(tbdprop_gfsjedi%acv      )) tbdprop_gfsjedi%acv       = other%acv
if (allocated(tbdprop_gfsjedi%acvb     )) tbdprop_gfsjedi%acvb      = other%acvb
if (allocated(tbdprop_gfsjedi%acvt     )) tbdprop_gfsjedi%acvt      = other%acvt
if (allocated(tbdprop_gfsjedi%dtdtr    )) tbdprop_gfsjedi%dtdtr     = other%dtdtr
if (allocated(tbdprop_gfsjedi%dtotprcp )) tbdprop_gfsjedi%dtotprcp  = other%dtotprcp
if (allocated(tbdprop_gfsjedi%dcnvprcp )) tbdprop_gfsjedi%dcnvprcp  = other%dcnvprcp
if (allocated(tbdprop_gfsjedi%drain_cpl)) tbdprop_gfsjedi%drain_cpl = other%drain_cpl
if (allocated(tbdprop_gfsjedi%dsnow_cpl)) tbdprop_gfsjedi%dsnow_cpl = other%dsnow_cpl
if (allocated(tbdprop_gfsjedi%phy_fctd )) tbdprop_gfsjedi%phy_fctd  = other%phy_fctd
if (allocated(tbdprop_gfsjedi%phy_f2d  )) tbdprop_gfsjedi%phy_f2d   = other%phy_f2d
if (allocated(tbdprop_gfsjedi%phy_f3d  )) tbdprop_gfsjedi%phy_f3d   = other%phy_f3d

tbdprop_gfsjedi%blkno     = other%blkno

end subroutine copy

! --------------------------------------------------------------------------------------------------

subroutine gfs_to_jedi(tbdprop_gfsjedi, tbdprop)

implicit none

class(GFSJEDI_tbdprop_type), intent(inout) :: tbdprop_gfsjedi
type(GFS_tbd_type),      intent(in)    :: tbdprop

if (allocated(tbdprop_gfsjedi%icsdsw   )) tbdprop_gfsjedi%icsdsw    = tbdprop%icsdsw
if (allocated(tbdprop_gfsjedi%icsdlw   )) tbdprop_gfsjedi%icsdlw    = tbdprop%icsdlw
if (allocated(tbdprop_gfsjedi%ozpl     )) tbdprop_gfsjedi%ozpl      = tbdprop%ozpl
if (allocated(tbdprop_gfsjedi%h2opl    )) tbdprop_gfsjedi%h2opl     = tbdprop%h2opl
if (allocated(tbdprop_gfsjedi%in_nm    )) tbdprop_gfsjedi%in_nm     = tbdprop%in_nm
if (allocated(tbdprop_gfsjedi%ccn_nm   )) tbdprop_gfsjedi%ccn_nm    = tbdprop%ccn_nm
if (allocated(tbdprop_gfsjedi%aer_nm   )) tbdprop_gfsjedi%aer_nm    = tbdprop%aer_nm
if (allocated(tbdprop_gfsjedi%rann     )) tbdprop_gfsjedi%rann      = tbdprop%rann
if (allocated(tbdprop_gfsjedi%acv      )) tbdprop_gfsjedi%acv       = tbdprop%acv
if (allocated(tbdprop_gfsjedi%acvb     )) tbdprop_gfsjedi%acvb      = tbdprop%acvb
if (allocated(tbdprop_gfsjedi%acvt     )) tbdprop_gfsjedi%acvt      = tbdprop%acvt
if (allocated(tbdprop_gfsjedi%dtdtr    )) tbdprop_gfsjedi%dtdtr     = tbdprop%dtdtr
if (allocated(tbdprop_gfsjedi%dtotprcp )) tbdprop_gfsjedi%dtotprcp  = tbdprop%dtotprcp
if (allocated(tbdprop_gfsjedi%dcnvprcp )) tbdprop_gfsjedi%dcnvprcp  = tbdprop%dcnvprcp
if (allocated(tbdprop_gfsjedi%drain_cpl)) tbdprop_gfsjedi%drain_cpl = tbdprop%drain_cpl
if (allocated(tbdprop_gfsjedi%dsnow_cpl)) tbdprop_gfsjedi%dsnow_cpl = tbdprop%dsnow_cpl
if (allocated(tbdprop_gfsjedi%phy_fctd )) tbdprop_gfsjedi%phy_fctd  = tbdprop%phy_fctd
if (allocated(tbdprop_gfsjedi%phy_f2d  )) tbdprop_gfsjedi%phy_f2d   = tbdprop%phy_f2d
if (allocated(tbdprop_gfsjedi%phy_f3d  )) tbdprop_gfsjedi%phy_f3d   = tbdprop%phy_f3d

tbdprop_gfsjedi%blkno     = tbdprop%blkno

end subroutine gfs_to_jedi

! --------------------------------------------------------------------------------------------------

subroutine jedi_to_gfs(tbdprop_gfsjedi, tbdprop)

implicit none

class(GFSJEDI_tbdprop_type), intent(in)    :: tbdprop_gfsjedi
type(GFS_tbd_type),      intent(inout) :: tbdprop

if (allocated(tbdprop_gfsjedi%icsdsw   )) tbdprop%icsdsw    = tbdprop_gfsjedi%icsdsw
if (allocated(tbdprop_gfsjedi%icsdlw   )) tbdprop%icsdlw    = tbdprop_gfsjedi%icsdlw
if (allocated(tbdprop_gfsjedi%ozpl     )) tbdprop%ozpl      = tbdprop_gfsjedi%ozpl
if (allocated(tbdprop_gfsjedi%h2opl    )) tbdprop%h2opl     = tbdprop_gfsjedi%h2opl
if (allocated(tbdprop_gfsjedi%in_nm    )) tbdprop%in_nm     = tbdprop_gfsjedi%in_nm
if (allocated(tbdprop_gfsjedi%ccn_nm   )) tbdprop%ccn_nm    = tbdprop_gfsjedi%ccn_nm
if (allocated(tbdprop_gfsjedi%aer_nm   )) tbdprop%aer_nm    = tbdprop_gfsjedi%aer_nm
if (allocated(tbdprop_gfsjedi%rann     )) tbdprop%rann      = tbdprop_gfsjedi%rann
if (allocated(tbdprop_gfsjedi%acv      )) tbdprop%acv       = tbdprop_gfsjedi%acv
if (allocated(tbdprop_gfsjedi%acvb     )) tbdprop%acvb      = tbdprop_gfsjedi%acvb
if (allocated(tbdprop_gfsjedi%acvt     )) tbdprop%acvt      = tbdprop_gfsjedi%acvt
if (allocated(tbdprop_gfsjedi%dtdtr    )) tbdprop%dtdtr     = tbdprop_gfsjedi%dtdtr
if (allocated(tbdprop_gfsjedi%dtotprcp )) tbdprop%dtotprcp  = tbdprop_gfsjedi%dtotprcp
if (allocated(tbdprop_gfsjedi%dcnvprcp )) tbdprop%dcnvprcp  = tbdprop_gfsjedi%dcnvprcp
if (allocated(tbdprop_gfsjedi%drain_cpl)) tbdprop%drain_cpl = tbdprop_gfsjedi%drain_cpl
if (allocated(tbdprop_gfsjedi%dsnow_cpl)) tbdprop%dsnow_cpl = tbdprop_gfsjedi%dsnow_cpl
if (allocated(tbdprop_gfsjedi%phy_fctd )) tbdprop%phy_fctd  = tbdprop_gfsjedi%phy_fctd
if (allocated(tbdprop_gfsjedi%phy_f2d  )) tbdprop%phy_f2d   = tbdprop_gfsjedi%phy_f2d
if (allocated(tbdprop_gfsjedi%phy_f3d  )) tbdprop%phy_f3d   = tbdprop_gfsjedi%phy_f3d

tbdprop%blkno     = tbdprop_gfsjedi%blkno

end subroutine jedi_to_gfs

! --------------------------------------------------------------------------------------------------

end module fv3jedi_tbdprop_type_mod
