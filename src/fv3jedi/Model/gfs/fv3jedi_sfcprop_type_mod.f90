module fv3jedi_sfcprop_type_mod

use GFS_typedefs

implicit none

private
public GFSJEDI_sfcprop_type

type GFSJEDI_sfcprop_type

!--- In (radiation and physics)
  real (kind=kind_phys), allocatable :: slmsk  (:)     !< sea/land mask array (sea:0,land:1,sea-ice:2)
  real (kind=kind_phys), allocatable :: oceanfrac(:)   !< ocean fraction [0:1]
  real (kind=kind_phys), allocatable :: landfrac(:)    !< land  fraction [0:1]
  real (kind=kind_phys), allocatable :: lakefrac(:)    !< lake  fraction [0:1]
  real (kind=kind_phys), allocatable :: tsfc   (:)     !< surface air temperature in K
                                                              !< [tsea in gbphys.f]
  real (kind=kind_phys), allocatable :: tsfco  (:)     !< sst in K
  real (kind=kind_phys), allocatable :: tsfcl  (:)     !< surface land temperature in K
  real (kind=kind_phys), allocatable :: tisfc  (:)     !< surface temperature over ice fraction
  real (kind=kind_phys), allocatable :: snowd  (:)     !< snow depth water equivalent in mm ; same as snwdph
  real (kind=kind_phys), allocatable :: zorl   (:)     !< composite surface roughness in cm
  real (kind=kind_phys), allocatable :: zorlo  (:)     !< ocean surface roughness in cm
  real (kind=kind_phys), allocatable :: zorll  (:)     !< land surface roughness in cm
  real (kind=kind_phys), allocatable :: fice   (:)     !< ice fraction over open water grid
  !real (kind=kind_phys), allocatable :: hprim  (:)     !< topographic standard deviation in m
  real (kind=kind_phys), allocatable :: hprime (:,:)   !< orographic metrics

!--- In (radiation only)
  real (kind=kind_phys), allocatable :: sncovr (:)     !< snow cover in fraction
  real (kind=kind_phys), allocatable :: snoalb (:)     !< maximum snow albedo in fraction
  real (kind=kind_phys), allocatable :: alvsf  (:)     !< mean vis albedo with strong cosz dependency
  real (kind=kind_phys), allocatable :: alnsf  (:)     !< mean nir albedo with strong cosz dependency
  real (kind=kind_phys), allocatable :: alvwf  (:)     !< mean vis albedo with weak cosz dependency
  real (kind=kind_phys), allocatable :: alnwf  (:)     !< mean nir albedo with weak cosz dependency
  real (kind=kind_phys), allocatable :: facsf  (:)     !< fractional coverage with strong cosz dependency
  real (kind=kind_phys), allocatable :: facwf  (:)     !< fractional coverage with   weak cosz dependency

!--- In (physics only)
  real (kind=kind_phys), allocatable :: slope  (:)     !< sfc slope type for lsm
  real (kind=kind_phys), allocatable :: shdmin (:)     !< min fractional coverage of green veg
  real (kind=kind_phys), allocatable :: shdmax (:)     !< max fractnl cover of green veg (not used)
  real (kind=kind_phys), allocatable :: tg3    (:)     !< deep soil temperature
  real (kind=kind_phys), allocatable :: vfrac  (:)     !< vegetation fraction
  real (kind=kind_phys), allocatable :: vtype  (:)     !< vegetation type
  real (kind=kind_phys), allocatable :: stype  (:)     !< soil type
  real (kind=kind_phys), allocatable :: uustar (:)     !< boundary layer parameter
  real (kind=kind_phys), allocatable :: oro    (:)     !< orography
  real (kind=kind_phys), allocatable :: oro_uf (:)     !< unfiltered orography

!-- In/Out
  real (kind=kind_phys), allocatable :: hice   (:)     !< sea ice thickness
  real (kind=kind_phys), allocatable :: weasd  (:)     !< water equiv of accumulated snow depth (kg/m**2)
                                                            !< over land and sea ice
  real (kind=kind_phys), allocatable :: canopy (:)     !< canopy water
  real (kind=kind_phys), allocatable :: ffmm   (:)     !< fm parameter from PBL scheme
  real (kind=kind_phys), allocatable :: ffhh   (:)     !< fh parameter from PBL scheme
  real (kind=kind_phys), allocatable :: f10m   (:)     !< fm at 10m - Ratio of sigma level 1 wind and 10m wind
  real (kind=kind_phys), allocatable :: tprcp  (:)     !< sfc_fld%tprcp - total precipitation
  real (kind=kind_phys), allocatable :: srflag (:)     !< sfc_fld%srflag - snow/rain flag for precipitation
  real (kind=kind_phys), allocatable :: slc    (:,:)   !< liquid soil moisture
  real (kind=kind_phys), allocatable :: smc    (:,:)   !< total soil moisture
  real (kind=kind_phys), allocatable :: stc    (:,:)   !< soil temperature

!--- Out
  real (kind=kind_phys), allocatable :: t2m    (:)     !< 2 meter temperature
  real (kind=kind_phys), allocatable :: q2m    (:)     !< 2 meter humidity

! -- In/Out for Noah MP
  real (kind=kind_phys), allocatable  :: snowxy (:)    !<
  real (kind=kind_phys), allocatable :: tvxy    (:)    !< veg temp
  real (kind=kind_phys), allocatable :: tgxy    (:)    !< ground temp
  real (kind=kind_phys), allocatable :: canicexy(:)    !<
  real (kind=kind_phys), allocatable :: canliqxy(:)    !<
  real (kind=kind_phys), allocatable :: eahxy   (:)    !<
  real (kind=kind_phys), allocatable :: tahxy   (:)    !<
  real (kind=kind_phys), allocatable :: cmxy    (:)    !<
  real (kind=kind_phys), allocatable :: chxy    (:)    !<
  real (kind=kind_phys), allocatable :: fwetxy  (:)    !<
  real (kind=kind_phys), allocatable :: sneqvoxy(:)    !<
  real (kind=kind_phys), allocatable :: alboldxy(:)    !<
  real (kind=kind_phys), allocatable :: qsnowxy (:)    !<
  real (kind=kind_phys), allocatable :: wslakexy(:)    !<
  real (kind=kind_phys), allocatable :: zwtxy   (:)    !<
  real (kind=kind_phys), allocatable :: waxy    (:)    !<
  real (kind=kind_phys), allocatable :: wtxy    (:)    !<
  real (kind=kind_phys), allocatable :: lfmassxy(:)    !<
  real (kind=kind_phys), allocatable :: rtmassxy(:)    !<
  real (kind=kind_phys), allocatable :: stmassxy(:)    !<
  real (kind=kind_phys), allocatable :: woodxy  (:)    !<
  real (kind=kind_phys), allocatable :: stblcpxy(:)    !<
  real (kind=kind_phys), allocatable :: fastcpxy(:)    !<
  real (kind=kind_phys), allocatable :: xsaixy  (:)    !<
  real (kind=kind_phys), allocatable :: xlaixy  (:)    !<
  real (kind=kind_phys), allocatable :: taussxy (:)    !<
  real (kind=kind_phys), allocatable :: smcwtdxy(:)    !<
  real (kind=kind_phys), allocatable :: deeprechxy(:)  !<
  real (kind=kind_phys), allocatable :: rechxy  (:)    !<

  real (kind=kind_phys), allocatable :: snicexy   (:,:)   !<
  real (kind=kind_phys), allocatable :: snliqxy   (:,:)   !<
  real (kind=kind_phys), allocatable :: tsnoxy    (:,:)   !<
  real (kind=kind_phys), allocatable :: smoiseq   (:,:)   !<
  real (kind=kind_phys), allocatable :: zsnsoxy   (:,:)   !<

!--- NSSTM variables  (only allocated when [Model%nstf_name(1) > 0])
  real (kind=kind_phys), allocatable :: tref   (:)     !< nst_fld%Tref - Reference Temperature
  real (kind=kind_phys), allocatable :: z_c    (:)     !< nst_fld%z_c - Sub layer cooling thickness
  real (kind=kind_phys), allocatable :: c_0    (:)     !< nst_fld%c_0 - coefficient1 to calculate d(Tz)/d(Ts)
  real (kind=kind_phys), allocatable :: c_d    (:)     !< nst_fld%c_d - coefficient2 to calculate d(Tz)/d(Ts)
  real (kind=kind_phys), allocatable :: w_0    (:)     !< nst_fld%w_0 - coefficient3 to calculate d(Tz)/d(Ts)
  real (kind=kind_phys), allocatable :: w_d    (:)     !< nst_fld%w_d - coefficient4 to calculate d(Tz)/d(Ts)
  real (kind=kind_phys), allocatable :: xt     (:)     !< nst_fld%xt      heat content in DTL
  real (kind=kind_phys), allocatable :: xs     (:)     !< nst_fld%xs      salinity  content in DTL
  real (kind=kind_phys), allocatable :: xu     (:)     !< nst_fld%xu      u current content in DTL
  real (kind=kind_phys), allocatable :: xv     (:)     !< nst_fld%xv      v current content in DTL
  real (kind=kind_phys), allocatable :: xz     (:)     !< nst_fld%xz      DTL thickness
  real (kind=kind_phys), allocatable :: zm     (:)     !< nst_fld%zm      MXL thickness
  real (kind=kind_phys), allocatable :: xtts   (:)     !< nst_fld%xtts    d(xt)/d(ts)
  real (kind=kind_phys), allocatable :: xzts   (:)     !< nst_fld%xzts    d(xz)/d(ts)
  real (kind=kind_phys), allocatable :: d_conv (:)     !< nst_fld%d_conv  thickness of Free Convection Layer (FCL)
  real (kind=kind_phys), allocatable :: ifd    (:)     !< nst_fld%ifd     index to start DTM run or not
  real (kind=kind_phys), allocatable :: dt_cool(:)     !< nst_fld%dt_cool Sub layer cooling amount
  real (kind=kind_phys), allocatable :: qrain  (:)     !< nst_fld%qrain   sensible heat flux due to rainfall (watts)

  contains
    procedure :: create
    procedure :: delete
    procedure :: copy
    procedure :: gfs_to_jedi
    procedure :: jedi_to_gfs

end type GFSJEDI_sfcprop_type

! --------------------------------------------------------------------------------------------------

contains

! --------------------------------------------------------------------------------------------------

subroutine create (Sfcprop_gfsjedi, IM, Model)

implicit none
class(GFSJEDI_sfcprop_type), intent(inout) :: Sfcprop_gfsjedi
integer,                     intent(in)    :: IM
type(GFS_control_type),      intent(in)    :: Model

!--- physics and radiation
allocate (Sfcprop_gfsjedi%slmsk    (IM))
allocate (Sfcprop_gfsjedi%oceanfrac(IM))
allocate (Sfcprop_gfsjedi%landfrac (IM))
allocate (Sfcprop_gfsjedi%lakefrac (IM))
allocate (Sfcprop_gfsjedi%tsfc     (IM))
allocate (Sfcprop_gfsjedi%tsfco    (IM))
allocate (Sfcprop_gfsjedi%tsfcl    (IM))
allocate (Sfcprop_gfsjedi%tisfc    (IM))
allocate (Sfcprop_gfsjedi%snowd    (IM))
allocate (Sfcprop_gfsjedi%zorl     (IM))
allocate (Sfcprop_gfsjedi%zorlo    (IM))
allocate (Sfcprop_gfsjedi%zorll    (IM))
allocate (Sfcprop_gfsjedi%fice     (IM))
allocate (Sfcprop_gfsjedi%hprime   (IM,Model%nmtvr))

!--- In (radiation only)
allocate (Sfcprop_gfsjedi%sncovr (IM))
allocate (Sfcprop_gfsjedi%snoalb (IM))
allocate (Sfcprop_gfsjedi%alvsf  (IM))
allocate (Sfcprop_gfsjedi%alnsf  (IM))
allocate (Sfcprop_gfsjedi%alvwf  (IM))
allocate (Sfcprop_gfsjedi%alnwf  (IM))
allocate (Sfcprop_gfsjedi%facsf  (IM))
allocate (Sfcprop_gfsjedi%facwf  (IM))

!--- physics surface props
!--- In
allocate (Sfcprop_gfsjedi%slope   (IM))
allocate (Sfcprop_gfsjedi%shdmin  (IM))
allocate (Sfcprop_gfsjedi%shdmax  (IM))
allocate (Sfcprop_gfsjedi%tg3     (IM))
allocate (Sfcprop_gfsjedi%vfrac   (IM))
allocate (Sfcprop_gfsjedi%vtype   (IM))
allocate (Sfcprop_gfsjedi%stype   (IM))
allocate (Sfcprop_gfsjedi%uustar  (IM))
allocate (Sfcprop_gfsjedi%oro     (IM))
allocate (Sfcprop_gfsjedi%oro_uf  (IM))

!--- In/Out
allocate (Sfcprop_gfsjedi%hice   (IM))
allocate (Sfcprop_gfsjedi%weasd  (IM))
!allocate (Sfcprop_gfsjedi%sncovr (IM))
allocate (Sfcprop_gfsjedi%canopy (IM))
allocate (Sfcprop_gfsjedi%ffmm   (IM))
allocate (Sfcprop_gfsjedi%ffhh   (IM))
allocate (Sfcprop_gfsjedi%f10m   (IM))
allocate (Sfcprop_gfsjedi%tprcp  (IM))
allocate (Sfcprop_gfsjedi%srflag (IM))
allocate (Sfcprop_gfsjedi%slc    (IM,Model%lsoil))
allocate (Sfcprop_gfsjedi%smc    (IM,Model%lsoil))
allocate (Sfcprop_gfsjedi%stc    (IM,Model%lsoil))

!--- Out
allocate (Sfcprop_gfsjedi%t2m (IM))
allocate (Sfcprop_gfsjedi%q2m (IM))

if (Model%nstf_name(1) > 0) then
  allocate (Sfcprop_gfsjedi%tref   (IM))
  allocate (Sfcprop_gfsjedi%z_c    (IM))
  allocate (Sfcprop_gfsjedi%c_0    (IM))
  allocate (Sfcprop_gfsjedi%c_d    (IM))
  allocate (Sfcprop_gfsjedi%w_0    (IM))
  allocate (Sfcprop_gfsjedi%w_d    (IM))
  allocate (Sfcprop_gfsjedi%xt     (IM))
  allocate (Sfcprop_gfsjedi%xs     (IM))
  allocate (Sfcprop_gfsjedi%xu     (IM))
  allocate (Sfcprop_gfsjedi%xv     (IM))
  allocate (Sfcprop_gfsjedi%xz     (IM))
  allocate (Sfcprop_gfsjedi%zm     (IM))
  allocate (Sfcprop_gfsjedi%xtts   (IM))
  allocate (Sfcprop_gfsjedi%xzts   (IM))
  allocate (Sfcprop_gfsjedi%d_conv (IM))
  allocate (Sfcprop_gfsjedi%ifd    (IM))
  allocate (Sfcprop_gfsjedi%dt_cool(IM))
  allocate (Sfcprop_gfsjedi%qrain  (IM))
endif

! Noah MP allocate and init when used
if (Model%lsm == Model%lsm_noahmp ) then
  allocate (Sfcprop_gfsjedi%snowxy   (IM))
  allocate (Sfcprop_gfsjedi%tvxy     (IM))
  allocate (Sfcprop_gfsjedi%tgxy     (IM))
  allocate (Sfcprop_gfsjedi%canicexy (IM))
  allocate (Sfcprop_gfsjedi%canliqxy (IM))
  allocate (Sfcprop_gfsjedi%eahxy    (IM))
  allocate (Sfcprop_gfsjedi%tahxy    (IM))
  allocate (Sfcprop_gfsjedi%cmxy     (IM))
  allocate (Sfcprop_gfsjedi%chxy     (IM))
  allocate (Sfcprop_gfsjedi%fwetxy   (IM))
  allocate (Sfcprop_gfsjedi%sneqvoxy (IM))
  allocate (Sfcprop_gfsjedi%alboldxy (IM))
  allocate (Sfcprop_gfsjedi%qsnowxy  (IM))
  allocate (Sfcprop_gfsjedi%wslakexy (IM))
  allocate (Sfcprop_gfsjedi%zwtxy    (IM))
  allocate (Sfcprop_gfsjedi%waxy     (IM))
  allocate (Sfcprop_gfsjedi%wtxy     (IM))
  allocate (Sfcprop_gfsjedi%lfmassxy (IM))
  allocate (Sfcprop_gfsjedi%rtmassxy (IM))
  allocate (Sfcprop_gfsjedi%stmassxy (IM))
  allocate (Sfcprop_gfsjedi%woodxy   (IM))
  allocate (Sfcprop_gfsjedi%stblcpxy (IM))
  allocate (Sfcprop_gfsjedi%fastcpxy (IM))
  allocate (Sfcprop_gfsjedi%xsaixy   (IM))
  allocate (Sfcprop_gfsjedi%xlaixy   (IM))
  allocate (Sfcprop_gfsjedi%taussxy  (IM))
  allocate (Sfcprop_gfsjedi%smcwtdxy (IM))
  allocate (Sfcprop_gfsjedi%deeprechxy(IM))
  allocate (Sfcprop_gfsjedi%rechxy    (IM))
  allocate (Sfcprop_gfsjedi%snicexy   (IM,-2:0))
  allocate (Sfcprop_gfsjedi%snliqxy   (IM,-2:0))
  allocate (Sfcprop_gfsjedi%tsnoxy    (IM,-2:0))
  allocate (Sfcprop_gfsjedi%smoiseq   (IM, 1:4))
  allocate (Sfcprop_gfsjedi%zsnsoxy   (IM,-2:4))
endif

end subroutine create

! --------------------------------------------------------------------------------------------------

subroutine delete (Sfcprop_gfsjedi)

implicit none
class(GFSJEDI_sfcprop_type), intent(inout) :: Sfcprop_gfsjedi

if (allocated(Sfcprop_gfsjedi%slmsk     )) deallocate (Sfcprop_gfsjedi%slmsk     )
if (allocated(Sfcprop_gfsjedi%oceanfrac )) deallocate (Sfcprop_gfsjedi%oceanfrac )
if (allocated(Sfcprop_gfsjedi%landfrac  )) deallocate (Sfcprop_gfsjedi%landfrac  )
if (allocated(Sfcprop_gfsjedi%lakefrac  )) deallocate (Sfcprop_gfsjedi%lakefrac  )
if (allocated(Sfcprop_gfsjedi%tsfc      )) deallocate (Sfcprop_gfsjedi%tsfc      )
if (allocated(Sfcprop_gfsjedi%tsfco     )) deallocate (Sfcprop_gfsjedi%tsfco     )
if (allocated(Sfcprop_gfsjedi%tsfcl     )) deallocate (Sfcprop_gfsjedi%tsfcl     )
if (allocated(Sfcprop_gfsjedi%tisfc     )) deallocate (Sfcprop_gfsjedi%tisfc     )
if (allocated(Sfcprop_gfsjedi%snowd     )) deallocate (Sfcprop_gfsjedi%snowd     )
if (allocated(Sfcprop_gfsjedi%zorl      )) deallocate (Sfcprop_gfsjedi%zorl      )
if (allocated(Sfcprop_gfsjedi%zorlo     )) deallocate (Sfcprop_gfsjedi%zorlo     )
if (allocated(Sfcprop_gfsjedi%zorll     )) deallocate (Sfcprop_gfsjedi%zorll     )
if (allocated(Sfcprop_gfsjedi%fice      )) deallocate (Sfcprop_gfsjedi%fice      )
if (allocated(Sfcprop_gfsjedi%hprime    )) deallocate (Sfcprop_gfsjedi%hprime    )
if (allocated(Sfcprop_gfsjedi%sncovr    )) deallocate (Sfcprop_gfsjedi%sncovr    )
if (allocated(Sfcprop_gfsjedi%snoalb    )) deallocate (Sfcprop_gfsjedi%snoalb    )
if (allocated(Sfcprop_gfsjedi%alvsf     )) deallocate (Sfcprop_gfsjedi%alvsf     )
if (allocated(Sfcprop_gfsjedi%alnsf     )) deallocate (Sfcprop_gfsjedi%alnsf     )
if (allocated(Sfcprop_gfsjedi%alvwf     )) deallocate (Sfcprop_gfsjedi%alvwf     )
if (allocated(Sfcprop_gfsjedi%alnwf     )) deallocate (Sfcprop_gfsjedi%alnwf     )
if (allocated(Sfcprop_gfsjedi%facsf     )) deallocate (Sfcprop_gfsjedi%facsf     )
if (allocated(Sfcprop_gfsjedi%facwf     )) deallocate (Sfcprop_gfsjedi%facwf     )
if (allocated(Sfcprop_gfsjedi%slope     )) deallocate (Sfcprop_gfsjedi%slope     )
if (allocated(Sfcprop_gfsjedi%shdmin    )) deallocate (Sfcprop_gfsjedi%shdmin    )
if (allocated(Sfcprop_gfsjedi%shdmax    )) deallocate (Sfcprop_gfsjedi%shdmax    )
if (allocated(Sfcprop_gfsjedi%tg3       )) deallocate (Sfcprop_gfsjedi%tg3       )
if (allocated(Sfcprop_gfsjedi%vfrac     )) deallocate (Sfcprop_gfsjedi%vfrac     )
if (allocated(Sfcprop_gfsjedi%vtype     )) deallocate (Sfcprop_gfsjedi%vtype     )
if (allocated(Sfcprop_gfsjedi%stype     )) deallocate (Sfcprop_gfsjedi%stype     )
if (allocated(Sfcprop_gfsjedi%uustar    )) deallocate (Sfcprop_gfsjedi%uustar    )
if (allocated(Sfcprop_gfsjedi%oro       )) deallocate (Sfcprop_gfsjedi%oro       )
if (allocated(Sfcprop_gfsjedi%oro_uf    )) deallocate (Sfcprop_gfsjedi%oro_uf    )
if (allocated(Sfcprop_gfsjedi%hice      )) deallocate (Sfcprop_gfsjedi%hice      )
if (allocated(Sfcprop_gfsjedi%weasd     )) deallocate (Sfcprop_gfsjedi%weasd     )
if (allocated(Sfcprop_gfsjedi%canopy    )) deallocate (Sfcprop_gfsjedi%canopy    )
if (allocated(Sfcprop_gfsjedi%ffmm      )) deallocate (Sfcprop_gfsjedi%ffmm      )
if (allocated(Sfcprop_gfsjedi%ffhh      )) deallocate (Sfcprop_gfsjedi%ffhh      )
if (allocated(Sfcprop_gfsjedi%f10m      )) deallocate (Sfcprop_gfsjedi%f10m      )
if (allocated(Sfcprop_gfsjedi%tprcp     )) deallocate (Sfcprop_gfsjedi%tprcp     )
if (allocated(Sfcprop_gfsjedi%srflag    )) deallocate (Sfcprop_gfsjedi%srflag    )
if (allocated(Sfcprop_gfsjedi%slc       )) deallocate (Sfcprop_gfsjedi%slc       )
if (allocated(Sfcprop_gfsjedi%smc       )) deallocate (Sfcprop_gfsjedi%smc       )
if (allocated(Sfcprop_gfsjedi%stc       )) deallocate (Sfcprop_gfsjedi%stc       )
if (allocated(Sfcprop_gfsjedi%t2m       )) deallocate (Sfcprop_gfsjedi%t2m       )
if (allocated(Sfcprop_gfsjedi%q2m       )) deallocate (Sfcprop_gfsjedi%q2m       )
if (allocated(Sfcprop_gfsjedi%tref      )) deallocate (Sfcprop_gfsjedi%tref      )
if (allocated(Sfcprop_gfsjedi%z_c       )) deallocate (Sfcprop_gfsjedi%z_c       )
if (allocated(Sfcprop_gfsjedi%c_0       )) deallocate (Sfcprop_gfsjedi%c_0       )
if (allocated(Sfcprop_gfsjedi%c_d       )) deallocate (Sfcprop_gfsjedi%c_d       )
if (allocated(Sfcprop_gfsjedi%w_0       )) deallocate (Sfcprop_gfsjedi%w_0       )
if (allocated(Sfcprop_gfsjedi%w_d       )) deallocate (Sfcprop_gfsjedi%w_d       )
if (allocated(Sfcprop_gfsjedi%xt        )) deallocate (Sfcprop_gfsjedi%xt        )
if (allocated(Sfcprop_gfsjedi%xs        )) deallocate (Sfcprop_gfsjedi%xs        )
if (allocated(Sfcprop_gfsjedi%xu        )) deallocate (Sfcprop_gfsjedi%xu        )
if (allocated(Sfcprop_gfsjedi%xv        )) deallocate (Sfcprop_gfsjedi%xv        )
if (allocated(Sfcprop_gfsjedi%xz        )) deallocate (Sfcprop_gfsjedi%xz        )
if (allocated(Sfcprop_gfsjedi%zm        )) deallocate (Sfcprop_gfsjedi%zm        )
if (allocated(Sfcprop_gfsjedi%xtts      )) deallocate (Sfcprop_gfsjedi%xtts      )
if (allocated(Sfcprop_gfsjedi%xzts      )) deallocate (Sfcprop_gfsjedi%xzts      )
if (allocated(Sfcprop_gfsjedi%d_conv    )) deallocate (Sfcprop_gfsjedi%d_conv    )
if (allocated(Sfcprop_gfsjedi%ifd       )) deallocate (Sfcprop_gfsjedi%ifd       )
if (allocated(Sfcprop_gfsjedi%dt_cool   )) deallocate (Sfcprop_gfsjedi%dt_cool   )
if (allocated(Sfcprop_gfsjedi%qrain     )) deallocate (Sfcprop_gfsjedi%qrain     )
if (allocated(Sfcprop_gfsjedi%snowxy    )) deallocate (Sfcprop_gfsjedi%snowxy    )
if (allocated(Sfcprop_gfsjedi%tvxy      )) deallocate (Sfcprop_gfsjedi%tvxy      )
if (allocated(Sfcprop_gfsjedi%tgxy      )) deallocate (Sfcprop_gfsjedi%tgxy      )
if (allocated(Sfcprop_gfsjedi%canicexy  )) deallocate (Sfcprop_gfsjedi%canicexy  )
if (allocated(Sfcprop_gfsjedi%canliqxy  )) deallocate (Sfcprop_gfsjedi%canliqxy  )
if (allocated(Sfcprop_gfsjedi%eahxy     )) deallocate (Sfcprop_gfsjedi%eahxy     )
if (allocated(Sfcprop_gfsjedi%tahxy     )) deallocate (Sfcprop_gfsjedi%tahxy     )
if (allocated(Sfcprop_gfsjedi%cmxy      )) deallocate (Sfcprop_gfsjedi%cmxy      )
if (allocated(Sfcprop_gfsjedi%chxy      )) deallocate (Sfcprop_gfsjedi%chxy      )
if (allocated(Sfcprop_gfsjedi%fwetxy    )) deallocate (Sfcprop_gfsjedi%fwetxy    )
if (allocated(Sfcprop_gfsjedi%sneqvoxy  )) deallocate (Sfcprop_gfsjedi%sneqvoxy  )
if (allocated(Sfcprop_gfsjedi%alboldxy  )) deallocate (Sfcprop_gfsjedi%alboldxy  )
if (allocated(Sfcprop_gfsjedi%qsnowxy   )) deallocate (Sfcprop_gfsjedi%qsnowxy   )
if (allocated(Sfcprop_gfsjedi%wslakexy  )) deallocate (Sfcprop_gfsjedi%wslakexy  )
if (allocated(Sfcprop_gfsjedi%zwtxy     )) deallocate (Sfcprop_gfsjedi%zwtxy     )
if (allocated(Sfcprop_gfsjedi%waxy      )) deallocate (Sfcprop_gfsjedi%waxy      )
if (allocated(Sfcprop_gfsjedi%wtxy      )) deallocate (Sfcprop_gfsjedi%wtxy      )
if (allocated(Sfcprop_gfsjedi%lfmassxy  )) deallocate (Sfcprop_gfsjedi%lfmassxy  )
if (allocated(Sfcprop_gfsjedi%rtmassxy  )) deallocate (Sfcprop_gfsjedi%rtmassxy  )
if (allocated(Sfcprop_gfsjedi%stmassxy  )) deallocate (Sfcprop_gfsjedi%stmassxy  )
if (allocated(Sfcprop_gfsjedi%woodxy    )) deallocate (Sfcprop_gfsjedi%woodxy    )
if (allocated(Sfcprop_gfsjedi%stblcpxy  )) deallocate (Sfcprop_gfsjedi%stblcpxy  )
if (allocated(Sfcprop_gfsjedi%fastcpxy  )) deallocate (Sfcprop_gfsjedi%fastcpxy  )
if (allocated(Sfcprop_gfsjedi%xsaixy    )) deallocate (Sfcprop_gfsjedi%xsaixy    )
if (allocated(Sfcprop_gfsjedi%xlaixy    )) deallocate (Sfcprop_gfsjedi%xlaixy    )
if (allocated(Sfcprop_gfsjedi%taussxy   )) deallocate (Sfcprop_gfsjedi%taussxy   )
if (allocated(Sfcprop_gfsjedi%smcwtdxy  )) deallocate (Sfcprop_gfsjedi%smcwtdxy  )
if (allocated(Sfcprop_gfsjedi%deeprechxy)) deallocate (Sfcprop_gfsjedi%deeprechxy)
if (allocated(Sfcprop_gfsjedi%rechxy    )) deallocate (Sfcprop_gfsjedi%rechxy    )
if (allocated(Sfcprop_gfsjedi%snicexy   )) deallocate (Sfcprop_gfsjedi%snicexy   )
if (allocated(Sfcprop_gfsjedi%snliqxy   )) deallocate (Sfcprop_gfsjedi%snliqxy   )
if (allocated(Sfcprop_gfsjedi%tsnoxy    )) deallocate (Sfcprop_gfsjedi%tsnoxy    )
if (allocated(Sfcprop_gfsjedi%smoiseq   )) deallocate (Sfcprop_gfsjedi%smoiseq   )
if (allocated(Sfcprop_gfsjedi%zsnsoxy   )) deallocate (Sfcprop_gfsjedi%zsnsoxy   )

end subroutine delete

! --------------------------------------------------------------------------------------------------

subroutine copy(Sfcprop_gfsjedi, other)

implicit none

class(GFSJEDI_sfcprop_type), intent(inout) :: Sfcprop_gfsjedi
type(GFSJEDI_sfcprop_type),  intent(in)    :: other

if (allocated(Sfcprop_gfsjedi%slmsk     )) Sfcprop_gfsjedi%slmsk      = other%slmsk
if (allocated(Sfcprop_gfsjedi%oceanfrac )) Sfcprop_gfsjedi%oceanfrac  = other%oceanfrac
if (allocated(Sfcprop_gfsjedi%landfrac  )) Sfcprop_gfsjedi%landfrac   = other%landfrac
if (allocated(Sfcprop_gfsjedi%lakefrac  )) Sfcprop_gfsjedi%lakefrac   = other%lakefrac
if (allocated(Sfcprop_gfsjedi%tsfc      )) Sfcprop_gfsjedi%tsfc       = other%tsfc
if (allocated(Sfcprop_gfsjedi%tsfco     )) Sfcprop_gfsjedi%tsfco      = other%tsfco
if (allocated(Sfcprop_gfsjedi%tsfcl     )) Sfcprop_gfsjedi%tsfcl      = other%tsfcl
if (allocated(Sfcprop_gfsjedi%tisfc     )) Sfcprop_gfsjedi%tisfc      = other%tisfc
if (allocated(Sfcprop_gfsjedi%snowd     )) Sfcprop_gfsjedi%snowd      = other%snowd
if (allocated(Sfcprop_gfsjedi%zorl      )) Sfcprop_gfsjedi%zorl       = other%zorl
if (allocated(Sfcprop_gfsjedi%zorlo     )) Sfcprop_gfsjedi%zorlo      = other%zorlo
if (allocated(Sfcprop_gfsjedi%zorll     )) Sfcprop_gfsjedi%zorll      = other%zorll
if (allocated(Sfcprop_gfsjedi%fice      )) Sfcprop_gfsjedi%fice       = other%fice
if (allocated(Sfcprop_gfsjedi%hprime    )) Sfcprop_gfsjedi%hprime     = other%hprime
if (allocated(Sfcprop_gfsjedi%sncovr    )) Sfcprop_gfsjedi%sncovr     = other%sncovr
if (allocated(Sfcprop_gfsjedi%snoalb    )) Sfcprop_gfsjedi%snoalb     = other%snoalb
if (allocated(Sfcprop_gfsjedi%alvsf     )) Sfcprop_gfsjedi%alvsf      = other%alvsf
if (allocated(Sfcprop_gfsjedi%alnsf     )) Sfcprop_gfsjedi%alnsf      = other%alnsf
if (allocated(Sfcprop_gfsjedi%alvwf     )) Sfcprop_gfsjedi%alvwf      = other%alvwf
if (allocated(Sfcprop_gfsjedi%alnwf     )) Sfcprop_gfsjedi%alnwf      = other%alnwf
if (allocated(Sfcprop_gfsjedi%facsf     )) Sfcprop_gfsjedi%facsf      = other%facsf
if (allocated(Sfcprop_gfsjedi%facwf     )) Sfcprop_gfsjedi%facwf      = other%facwf
if (allocated(Sfcprop_gfsjedi%slope     )) Sfcprop_gfsjedi%slope      = other%slope
if (allocated(Sfcprop_gfsjedi%shdmin    )) Sfcprop_gfsjedi%shdmin     = other%shdmin
if (allocated(Sfcprop_gfsjedi%shdmax    )) Sfcprop_gfsjedi%shdmax     = other%shdmax
if (allocated(Sfcprop_gfsjedi%tg3       )) Sfcprop_gfsjedi%tg3        = other%tg3
if (allocated(Sfcprop_gfsjedi%vfrac     )) Sfcprop_gfsjedi%vfrac      = other%vfrac
if (allocated(Sfcprop_gfsjedi%vtype     )) Sfcprop_gfsjedi%vtype      = other%vtype
if (allocated(Sfcprop_gfsjedi%stype     )) Sfcprop_gfsjedi%stype      = other%stype
if (allocated(Sfcprop_gfsjedi%uustar    )) Sfcprop_gfsjedi%uustar     = other%uustar
if (allocated(Sfcprop_gfsjedi%oro       )) Sfcprop_gfsjedi%oro        = other%oro
if (allocated(Sfcprop_gfsjedi%oro_uf    )) Sfcprop_gfsjedi%oro_uf     = other%oro_uf
if (allocated(Sfcprop_gfsjedi%hice      )) Sfcprop_gfsjedi%hice       = other%hice
if (allocated(Sfcprop_gfsjedi%weasd     )) Sfcprop_gfsjedi%weasd      = other%weasd
!if (allocated(Sfcprop_gfsjedi%sncovr    )) Sfcprop_gfsjedi%sncovr     = other%sncovr
if (allocated(Sfcprop_gfsjedi%canopy    )) Sfcprop_gfsjedi%canopy     = other%canopy
if (allocated(Sfcprop_gfsjedi%ffmm      )) Sfcprop_gfsjedi%ffmm       = other%ffmm
if (allocated(Sfcprop_gfsjedi%ffhh      )) Sfcprop_gfsjedi%ffhh       = other%ffhh
if (allocated(Sfcprop_gfsjedi%f10m      )) Sfcprop_gfsjedi%f10m       = other%f10m
if (allocated(Sfcprop_gfsjedi%tprcp     )) Sfcprop_gfsjedi%tprcp      = other%tprcp
if (allocated(Sfcprop_gfsjedi%srflag    )) Sfcprop_gfsjedi%srflag     = other%srflag
if (allocated(Sfcprop_gfsjedi%slc       )) Sfcprop_gfsjedi%slc        = other%slc
if (allocated(Sfcprop_gfsjedi%smc       )) Sfcprop_gfsjedi%smc        = other%smc
if (allocated(Sfcprop_gfsjedi%stc       )) Sfcprop_gfsjedi%stc        = other%stc
if (allocated(Sfcprop_gfsjedi%t2m       )) Sfcprop_gfsjedi%t2m        = other%t2m
if (allocated(Sfcprop_gfsjedi%q2m       )) Sfcprop_gfsjedi%q2m        = other%q2m
if (allocated(Sfcprop_gfsjedi%tref      )) Sfcprop_gfsjedi%tref       = other%tref
if (allocated(Sfcprop_gfsjedi%z_c       )) Sfcprop_gfsjedi%z_c        = other%z_c
if (allocated(Sfcprop_gfsjedi%c_0       )) Sfcprop_gfsjedi%c_0        = other%c_0
if (allocated(Sfcprop_gfsjedi%c_d       )) Sfcprop_gfsjedi%c_d        = other%c_d
if (allocated(Sfcprop_gfsjedi%w_0       )) Sfcprop_gfsjedi%w_0        = other%w_0
if (allocated(Sfcprop_gfsjedi%w_d       )) Sfcprop_gfsjedi%w_d        = other%w_d
if (allocated(Sfcprop_gfsjedi%xt        )) Sfcprop_gfsjedi%xt         = other%xt
if (allocated(Sfcprop_gfsjedi%xs        )) Sfcprop_gfsjedi%xs         = other%xs
if (allocated(Sfcprop_gfsjedi%xu        )) Sfcprop_gfsjedi%xu         = other%xu
if (allocated(Sfcprop_gfsjedi%xv        )) Sfcprop_gfsjedi%xv         = other%xv
if (allocated(Sfcprop_gfsjedi%xz        )) Sfcprop_gfsjedi%xz         = other%xz
if (allocated(Sfcprop_gfsjedi%zm        )) Sfcprop_gfsjedi%zm         = other%zm
if (allocated(Sfcprop_gfsjedi%xtts      )) Sfcprop_gfsjedi%xtts       = other%xtts
if (allocated(Sfcprop_gfsjedi%xzts      )) Sfcprop_gfsjedi%xzts       = other%xzts
if (allocated(Sfcprop_gfsjedi%d_conv    )) Sfcprop_gfsjedi%d_conv     = other%d_conv
if (allocated(Sfcprop_gfsjedi%ifd       )) Sfcprop_gfsjedi%ifd        = other%ifd
if (allocated(Sfcprop_gfsjedi%dt_cool   )) Sfcprop_gfsjedi%dt_cool    = other%dt_cool
if (allocated(Sfcprop_gfsjedi%qrain     )) Sfcprop_gfsjedi%qrain      = other%qrain
if (allocated(Sfcprop_gfsjedi%snowxy    )) Sfcprop_gfsjedi%snowxy     = other%snowxy
if (allocated(Sfcprop_gfsjedi%tvxy      )) Sfcprop_gfsjedi%tvxy       = other%tvxy
if (allocated(Sfcprop_gfsjedi%tgxy      )) Sfcprop_gfsjedi%tgxy       = other%tgxy
if (allocated(Sfcprop_gfsjedi%canicexy  )) Sfcprop_gfsjedi%canicexy   = other%canicexy
if (allocated(Sfcprop_gfsjedi%canliqxy  )) Sfcprop_gfsjedi%canliqxy   = other%canliqxy
if (allocated(Sfcprop_gfsjedi%eahxy     )) Sfcprop_gfsjedi%eahxy      = other%eahxy
if (allocated(Sfcprop_gfsjedi%tahxy     )) Sfcprop_gfsjedi%tahxy      = other%tahxy
if (allocated(Sfcprop_gfsjedi%cmxy      )) Sfcprop_gfsjedi%cmxy       = other%cmxy
if (allocated(Sfcprop_gfsjedi%chxy      )) Sfcprop_gfsjedi%chxy       = other%chxy
if (allocated(Sfcprop_gfsjedi%fwetxy    )) Sfcprop_gfsjedi%fwetxy     = other%fwetxy
if (allocated(Sfcprop_gfsjedi%sneqvoxy  )) Sfcprop_gfsjedi%sneqvoxy   = other%sneqvoxy
if (allocated(Sfcprop_gfsjedi%alboldxy  )) Sfcprop_gfsjedi%alboldxy   = other%alboldxy
if (allocated(Sfcprop_gfsjedi%qsnowxy   )) Sfcprop_gfsjedi%qsnowxy    = other%qsnowxy
if (allocated(Sfcprop_gfsjedi%wslakexy  )) Sfcprop_gfsjedi%wslakexy   = other%wslakexy
if (allocated(Sfcprop_gfsjedi%zwtxy     )) Sfcprop_gfsjedi%zwtxy      = other%zwtxy
if (allocated(Sfcprop_gfsjedi%waxy      )) Sfcprop_gfsjedi%waxy       = other%waxy
if (allocated(Sfcprop_gfsjedi%wtxy      )) Sfcprop_gfsjedi%wtxy       = other%wtxy
if (allocated(Sfcprop_gfsjedi%lfmassxy  )) Sfcprop_gfsjedi%lfmassxy   = other%lfmassxy
if (allocated(Sfcprop_gfsjedi%rtmassxy  )) Sfcprop_gfsjedi%rtmassxy   = other%rtmassxy
if (allocated(Sfcprop_gfsjedi%stmassxy  )) Sfcprop_gfsjedi%stmassxy   = other%stmassxy
if (allocated(Sfcprop_gfsjedi%woodxy    )) Sfcprop_gfsjedi%woodxy     = other%woodxy
if (allocated(Sfcprop_gfsjedi%stblcpxy  )) Sfcprop_gfsjedi%stblcpxy   = other%stblcpxy
if (allocated(Sfcprop_gfsjedi%fastcpxy  )) Sfcprop_gfsjedi%fastcpxy   = other%fastcpxy
if (allocated(Sfcprop_gfsjedi%xsaixy    )) Sfcprop_gfsjedi%xsaixy     = other%xsaixy
if (allocated(Sfcprop_gfsjedi%xlaixy    )) Sfcprop_gfsjedi%xlaixy     = other%xlaixy
if (allocated(Sfcprop_gfsjedi%taussxy   )) Sfcprop_gfsjedi%taussxy    = other%taussxy
if (allocated(Sfcprop_gfsjedi%smcwtdxy  )) Sfcprop_gfsjedi%smcwtdxy   = other%smcwtdxy
if (allocated(Sfcprop_gfsjedi%deeprechxy)) Sfcprop_gfsjedi%deeprechxy = other%deeprechxy
if (allocated(Sfcprop_gfsjedi%rechxy    )) Sfcprop_gfsjedi%rechxy     = other%rechxy
if (allocated(Sfcprop_gfsjedi%snicexy   )) Sfcprop_gfsjedi%snicexy    = other%snicexy
if (allocated(Sfcprop_gfsjedi%snliqxy   )) Sfcprop_gfsjedi%snliqxy    = other%snliqxy
if (allocated(Sfcprop_gfsjedi%tsnoxy    )) Sfcprop_gfsjedi%tsnoxy     = other%tsnoxy
if (allocated(Sfcprop_gfsjedi%smoiseq   )) Sfcprop_gfsjedi%smoiseq    = other%smoiseq
if (allocated(Sfcprop_gfsjedi%zsnsoxy   )) Sfcprop_gfsjedi%zsnsoxy    = other%zsnsoxy

end subroutine copy

! --------------------------------------------------------------------------------------------------

subroutine gfs_to_jedi(Sfcprop_gfsjedi, Sfcprop)

implicit none

class(GFSJEDI_sfcprop_type), intent(inout) :: Sfcprop_gfsjedi
type(GFS_sfcprop_type),      intent(in)    :: Sfcprop

if (allocated(Sfcprop_gfsjedi%slmsk     )) Sfcprop_gfsjedi%slmsk      = Sfcprop%slmsk
if (allocated(Sfcprop_gfsjedi%oceanfrac )) Sfcprop_gfsjedi%oceanfrac  = Sfcprop%oceanfrac
if (allocated(Sfcprop_gfsjedi%landfrac  )) Sfcprop_gfsjedi%landfrac   = Sfcprop%landfrac
if (allocated(Sfcprop_gfsjedi%lakefrac  )) Sfcprop_gfsjedi%lakefrac   = Sfcprop%lakefrac
if (allocated(Sfcprop_gfsjedi%tsfc      )) Sfcprop_gfsjedi%tsfc       = Sfcprop%tsfc
if (allocated(Sfcprop_gfsjedi%tsfco     )) Sfcprop_gfsjedi%tsfco      = Sfcprop%tsfco
if (allocated(Sfcprop_gfsjedi%tsfcl     )) Sfcprop_gfsjedi%tsfcl      = Sfcprop%tsfcl
if (allocated(Sfcprop_gfsjedi%tisfc     )) Sfcprop_gfsjedi%tisfc      = Sfcprop%tisfc
if (allocated(Sfcprop_gfsjedi%snowd     )) Sfcprop_gfsjedi%snowd      = Sfcprop%snowd
if (allocated(Sfcprop_gfsjedi%zorl      )) Sfcprop_gfsjedi%zorl       = Sfcprop%zorl
if (allocated(Sfcprop_gfsjedi%zorlo     )) Sfcprop_gfsjedi%zorlo      = Sfcprop%zorlo
if (allocated(Sfcprop_gfsjedi%zorll     )) Sfcprop_gfsjedi%zorll      = Sfcprop%zorll
if (allocated(Sfcprop_gfsjedi%fice      )) Sfcprop_gfsjedi%fice       = Sfcprop%fice
if (allocated(Sfcprop_gfsjedi%hprime    )) Sfcprop_gfsjedi%hprime     = Sfcprop%hprime
if (allocated(Sfcprop_gfsjedi%sncovr    )) Sfcprop_gfsjedi%sncovr     = Sfcprop%sncovr
if (allocated(Sfcprop_gfsjedi%snoalb    )) Sfcprop_gfsjedi%snoalb     = Sfcprop%snoalb
if (allocated(Sfcprop_gfsjedi%alvsf     )) Sfcprop_gfsjedi%alvsf      = Sfcprop%alvsf
if (allocated(Sfcprop_gfsjedi%alnsf     )) Sfcprop_gfsjedi%alnsf      = Sfcprop%alnsf
if (allocated(Sfcprop_gfsjedi%alvwf     )) Sfcprop_gfsjedi%alvwf      = Sfcprop%alvwf
if (allocated(Sfcprop_gfsjedi%alnwf     )) Sfcprop_gfsjedi%alnwf      = Sfcprop%alnwf
if (allocated(Sfcprop_gfsjedi%facsf     )) Sfcprop_gfsjedi%facsf      = Sfcprop%facsf
if (allocated(Sfcprop_gfsjedi%facwf     )) Sfcprop_gfsjedi%facwf      = Sfcprop%facwf
if (allocated(Sfcprop_gfsjedi%slope     )) Sfcprop_gfsjedi%slope      = Sfcprop%slope
if (allocated(Sfcprop_gfsjedi%shdmin    )) Sfcprop_gfsjedi%shdmin     = Sfcprop%shdmin
if (allocated(Sfcprop_gfsjedi%shdmax    )) Sfcprop_gfsjedi%shdmax     = Sfcprop%shdmax
if (allocated(Sfcprop_gfsjedi%tg3       )) Sfcprop_gfsjedi%tg3        = Sfcprop%tg3
if (allocated(Sfcprop_gfsjedi%vfrac     )) Sfcprop_gfsjedi%vfrac      = Sfcprop%vfrac
if (allocated(Sfcprop_gfsjedi%vtype     )) Sfcprop_gfsjedi%vtype      = Sfcprop%vtype
if (allocated(Sfcprop_gfsjedi%stype     )) Sfcprop_gfsjedi%stype      = Sfcprop%stype
if (allocated(Sfcprop_gfsjedi%uustar    )) Sfcprop_gfsjedi%uustar     = Sfcprop%uustar
if (allocated(Sfcprop_gfsjedi%oro       )) Sfcprop_gfsjedi%oro        = Sfcprop%oro
if (allocated(Sfcprop_gfsjedi%oro_uf    )) Sfcprop_gfsjedi%oro_uf     = Sfcprop%oro_uf
if (allocated(Sfcprop_gfsjedi%hice      )) Sfcprop_gfsjedi%hice       = Sfcprop%hice
if (allocated(Sfcprop_gfsjedi%weasd     )) Sfcprop_gfsjedi%weasd      = Sfcprop%weasd
!if (allocated(Sfcprop_gfsjedi%sncovr    )) Sfcprop_gfsjedi%sncovr     = Sfcprop%sncovr
if (allocated(Sfcprop_gfsjedi%canopy    )) Sfcprop_gfsjedi%canopy     = Sfcprop%canopy
if (allocated(Sfcprop_gfsjedi%ffmm      )) Sfcprop_gfsjedi%ffmm       = Sfcprop%ffmm
if (allocated(Sfcprop_gfsjedi%ffhh      )) Sfcprop_gfsjedi%ffhh       = Sfcprop%ffhh
if (allocated(Sfcprop_gfsjedi%f10m      )) Sfcprop_gfsjedi%f10m       = Sfcprop%f10m
if (allocated(Sfcprop_gfsjedi%tprcp     )) Sfcprop_gfsjedi%tprcp      = Sfcprop%tprcp
if (allocated(Sfcprop_gfsjedi%srflag    )) Sfcprop_gfsjedi%srflag     = Sfcprop%srflag
if (allocated(Sfcprop_gfsjedi%slc       )) Sfcprop_gfsjedi%slc        = Sfcprop%slc
if (allocated(Sfcprop_gfsjedi%smc       )) Sfcprop_gfsjedi%smc        = Sfcprop%smc
if (allocated(Sfcprop_gfsjedi%stc       )) Sfcprop_gfsjedi%stc        = Sfcprop%stc
if (allocated(Sfcprop_gfsjedi%t2m       )) Sfcprop_gfsjedi%t2m        = Sfcprop%t2m
if (allocated(Sfcprop_gfsjedi%q2m       )) Sfcprop_gfsjedi%q2m        = Sfcprop%q2m
if (allocated(Sfcprop_gfsjedi%tref      )) Sfcprop_gfsjedi%tref       = Sfcprop%tref
if (allocated(Sfcprop_gfsjedi%z_c       )) Sfcprop_gfsjedi%z_c        = Sfcprop%z_c
if (allocated(Sfcprop_gfsjedi%c_0       )) Sfcprop_gfsjedi%c_0        = Sfcprop%c_0
if (allocated(Sfcprop_gfsjedi%c_d       )) Sfcprop_gfsjedi%c_d        = Sfcprop%c_d
if (allocated(Sfcprop_gfsjedi%w_0       )) Sfcprop_gfsjedi%w_0        = Sfcprop%w_0
if (allocated(Sfcprop_gfsjedi%w_d       )) Sfcprop_gfsjedi%w_d        = Sfcprop%w_d
if (allocated(Sfcprop_gfsjedi%xt        )) Sfcprop_gfsjedi%xt         = Sfcprop%xt
if (allocated(Sfcprop_gfsjedi%xs        )) Sfcprop_gfsjedi%xs         = Sfcprop%xs
if (allocated(Sfcprop_gfsjedi%xu        )) Sfcprop_gfsjedi%xu         = Sfcprop%xu
if (allocated(Sfcprop_gfsjedi%xv        )) Sfcprop_gfsjedi%xv         = Sfcprop%xv
if (allocated(Sfcprop_gfsjedi%xz        )) Sfcprop_gfsjedi%xz         = Sfcprop%xz
if (allocated(Sfcprop_gfsjedi%zm        )) Sfcprop_gfsjedi%zm         = Sfcprop%zm
if (allocated(Sfcprop_gfsjedi%xtts      )) Sfcprop_gfsjedi%xtts       = Sfcprop%xtts
if (allocated(Sfcprop_gfsjedi%xzts      )) Sfcprop_gfsjedi%xzts       = Sfcprop%xzts
if (allocated(Sfcprop_gfsjedi%d_conv    )) Sfcprop_gfsjedi%d_conv     = Sfcprop%d_conv
if (allocated(Sfcprop_gfsjedi%ifd       )) Sfcprop_gfsjedi%ifd        = Sfcprop%ifd
if (allocated(Sfcprop_gfsjedi%dt_cool   )) Sfcprop_gfsjedi%dt_cool    = Sfcprop%dt_cool
if (allocated(Sfcprop_gfsjedi%qrain     )) Sfcprop_gfsjedi%qrain      = Sfcprop%qrain
if (allocated(Sfcprop_gfsjedi%snowxy    )) Sfcprop_gfsjedi%snowxy     = Sfcprop%snowxy
if (allocated(Sfcprop_gfsjedi%tvxy      )) Sfcprop_gfsjedi%tvxy       = Sfcprop%tvxy
if (allocated(Sfcprop_gfsjedi%tgxy      )) Sfcprop_gfsjedi%tgxy       = Sfcprop%tgxy
if (allocated(Sfcprop_gfsjedi%canicexy  )) Sfcprop_gfsjedi%canicexy   = Sfcprop%canicexy
if (allocated(Sfcprop_gfsjedi%canliqxy  )) Sfcprop_gfsjedi%canliqxy   = Sfcprop%canliqxy
if (allocated(Sfcprop_gfsjedi%eahxy     )) Sfcprop_gfsjedi%eahxy      = Sfcprop%eahxy
if (allocated(Sfcprop_gfsjedi%tahxy     )) Sfcprop_gfsjedi%tahxy      = Sfcprop%tahxy
if (allocated(Sfcprop_gfsjedi%cmxy      )) Sfcprop_gfsjedi%cmxy       = Sfcprop%cmxy
if (allocated(Sfcprop_gfsjedi%chxy      )) Sfcprop_gfsjedi%chxy       = Sfcprop%chxy
if (allocated(Sfcprop_gfsjedi%fwetxy    )) Sfcprop_gfsjedi%fwetxy     = Sfcprop%fwetxy
if (allocated(Sfcprop_gfsjedi%sneqvoxy  )) Sfcprop_gfsjedi%sneqvoxy   = Sfcprop%sneqvoxy
if (allocated(Sfcprop_gfsjedi%alboldxy  )) Sfcprop_gfsjedi%alboldxy   = Sfcprop%alboldxy
if (allocated(Sfcprop_gfsjedi%qsnowxy   )) Sfcprop_gfsjedi%qsnowxy    = Sfcprop%qsnowxy
if (allocated(Sfcprop_gfsjedi%wslakexy  )) Sfcprop_gfsjedi%wslakexy   = Sfcprop%wslakexy
if (allocated(Sfcprop_gfsjedi%zwtxy     )) Sfcprop_gfsjedi%zwtxy      = Sfcprop%zwtxy
if (allocated(Sfcprop_gfsjedi%waxy      )) Sfcprop_gfsjedi%waxy       = Sfcprop%waxy
if (allocated(Sfcprop_gfsjedi%wtxy      )) Sfcprop_gfsjedi%wtxy       = Sfcprop%wtxy
if (allocated(Sfcprop_gfsjedi%lfmassxy  )) Sfcprop_gfsjedi%lfmassxy   = Sfcprop%lfmassxy
if (allocated(Sfcprop_gfsjedi%rtmassxy  )) Sfcprop_gfsjedi%rtmassxy   = Sfcprop%rtmassxy
if (allocated(Sfcprop_gfsjedi%stmassxy  )) Sfcprop_gfsjedi%stmassxy   = Sfcprop%stmassxy
if (allocated(Sfcprop_gfsjedi%woodxy    )) Sfcprop_gfsjedi%woodxy     = Sfcprop%woodxy
if (allocated(Sfcprop_gfsjedi%stblcpxy  )) Sfcprop_gfsjedi%stblcpxy   = Sfcprop%stblcpxy
if (allocated(Sfcprop_gfsjedi%fastcpxy  )) Sfcprop_gfsjedi%fastcpxy   = Sfcprop%fastcpxy
if (allocated(Sfcprop_gfsjedi%xsaixy    )) Sfcprop_gfsjedi%xsaixy     = Sfcprop%xsaixy
if (allocated(Sfcprop_gfsjedi%xlaixy    )) Sfcprop_gfsjedi%xlaixy     = Sfcprop%xlaixy
if (allocated(Sfcprop_gfsjedi%taussxy   )) Sfcprop_gfsjedi%taussxy    = Sfcprop%taussxy
if (allocated(Sfcprop_gfsjedi%smcwtdxy  )) Sfcprop_gfsjedi%smcwtdxy   = Sfcprop%smcwtdxy
if (allocated(Sfcprop_gfsjedi%deeprechxy)) Sfcprop_gfsjedi%deeprechxy = Sfcprop%deeprechxy
if (allocated(Sfcprop_gfsjedi%rechxy    )) Sfcprop_gfsjedi%rechxy     = Sfcprop%rechxy
if (allocated(Sfcprop_gfsjedi%snicexy   )) Sfcprop_gfsjedi%snicexy    = Sfcprop%snicexy
if (allocated(Sfcprop_gfsjedi%snliqxy   )) Sfcprop_gfsjedi%snliqxy    = Sfcprop%snliqxy
if (allocated(Sfcprop_gfsjedi%tsnoxy    )) Sfcprop_gfsjedi%tsnoxy     = Sfcprop%tsnoxy
if (allocated(Sfcprop_gfsjedi%smoiseq   )) Sfcprop_gfsjedi%smoiseq    = Sfcprop%smoiseq
if (allocated(Sfcprop_gfsjedi%zsnsoxy   )) Sfcprop_gfsjedi%zsnsoxy    = Sfcprop%zsnsoxy

end subroutine gfs_to_jedi

! --------------------------------------------------------------------------------------------------

subroutine jedi_to_gfs(Sfcprop_gfsjedi, Sfcprop)

implicit none

class(GFSJEDI_sfcprop_type), intent(in)    :: Sfcprop_gfsjedi
type(GFS_sfcprop_type),      intent(inout) :: Sfcprop

if (allocated(Sfcprop_gfsjedi%slmsk     )) Sfcprop%slmsk      = Sfcprop_gfsjedi%slmsk
if (allocated(Sfcprop_gfsjedi%oceanfrac )) Sfcprop%oceanfrac  = Sfcprop_gfsjedi%oceanfrac
if (allocated(Sfcprop_gfsjedi%landfrac  )) Sfcprop%landfrac   = Sfcprop_gfsjedi%landfrac
if (allocated(Sfcprop_gfsjedi%lakefrac  )) Sfcprop%lakefrac   = Sfcprop_gfsjedi%lakefrac
if (allocated(Sfcprop_gfsjedi%tsfc      )) Sfcprop%tsfc       = Sfcprop_gfsjedi%tsfc
if (allocated(Sfcprop_gfsjedi%tsfco     )) Sfcprop%tsfco      = Sfcprop_gfsjedi%tsfco
if (allocated(Sfcprop_gfsjedi%tsfcl     )) Sfcprop%tsfcl      = Sfcprop_gfsjedi%tsfcl
if (allocated(Sfcprop_gfsjedi%tisfc     )) Sfcprop%tisfc      = Sfcprop_gfsjedi%tisfc
if (allocated(Sfcprop_gfsjedi%snowd     )) Sfcprop%snowd      = Sfcprop_gfsjedi%snowd
if (allocated(Sfcprop_gfsjedi%zorl      )) Sfcprop%zorl       = Sfcprop_gfsjedi%zorl
if (allocated(Sfcprop_gfsjedi%zorlo     )) Sfcprop%zorlo      = Sfcprop_gfsjedi%zorlo
if (allocated(Sfcprop_gfsjedi%zorll     )) Sfcprop%zorll      = Sfcprop_gfsjedi%zorll
if (allocated(Sfcprop_gfsjedi%fice      )) Sfcprop%fice       = Sfcprop_gfsjedi%fice
if (allocated(Sfcprop_gfsjedi%hprime    )) Sfcprop%hprime     = Sfcprop_gfsjedi%hprime
if (allocated(Sfcprop_gfsjedi%sncovr    )) Sfcprop%sncovr     = Sfcprop_gfsjedi%sncovr
if (allocated(Sfcprop_gfsjedi%snoalb    )) Sfcprop%snoalb     = Sfcprop_gfsjedi%snoalb
if (allocated(Sfcprop_gfsjedi%alvsf     )) Sfcprop%alvsf      = Sfcprop_gfsjedi%alvsf
if (allocated(Sfcprop_gfsjedi%alnsf     )) Sfcprop%alnsf      = Sfcprop_gfsjedi%alnsf
if (allocated(Sfcprop_gfsjedi%alvwf     )) Sfcprop%alvwf      = Sfcprop_gfsjedi%alvwf
if (allocated(Sfcprop_gfsjedi%alnwf     )) Sfcprop%alnwf      = Sfcprop_gfsjedi%alnwf
if (allocated(Sfcprop_gfsjedi%facsf     )) Sfcprop%facsf      = Sfcprop_gfsjedi%facsf
if (allocated(Sfcprop_gfsjedi%facwf     )) Sfcprop%facwf      = Sfcprop_gfsjedi%facwf
if (allocated(Sfcprop_gfsjedi%slope     )) Sfcprop%slope      = Sfcprop_gfsjedi%slope
if (allocated(Sfcprop_gfsjedi%shdmin    )) Sfcprop%shdmin     = Sfcprop_gfsjedi%shdmin
if (allocated(Sfcprop_gfsjedi%shdmax    )) Sfcprop%shdmax     = Sfcprop_gfsjedi%shdmax
if (allocated(Sfcprop_gfsjedi%tg3       )) Sfcprop%tg3        = Sfcprop_gfsjedi%tg3
if (allocated(Sfcprop_gfsjedi%vfrac     )) Sfcprop%vfrac      = Sfcprop_gfsjedi%vfrac
if (allocated(Sfcprop_gfsjedi%vtype     )) Sfcprop%vtype      = Sfcprop_gfsjedi%vtype
if (allocated(Sfcprop_gfsjedi%stype     )) Sfcprop%stype      = Sfcprop_gfsjedi%stype
if (allocated(Sfcprop_gfsjedi%uustar    )) Sfcprop%uustar     = Sfcprop_gfsjedi%uustar
if (allocated(Sfcprop_gfsjedi%oro       )) Sfcprop%oro        = Sfcprop_gfsjedi%oro
if (allocated(Sfcprop_gfsjedi%oro_uf    )) Sfcprop%oro_uf     = Sfcprop_gfsjedi%oro_uf
if (allocated(Sfcprop_gfsjedi%hice      )) Sfcprop%hice       = Sfcprop_gfsjedi%hice
if (allocated(Sfcprop_gfsjedi%weasd     )) Sfcprop%weasd      = Sfcprop_gfsjedi%weasd
!if (allocated(Sfcprop_gfsjedi%sncovr    )) Sfcprop%sncovr     = Sfcprop_gfsjedi%sncovr
if (allocated(Sfcprop_gfsjedi%canopy    )) Sfcprop%canopy     = Sfcprop_gfsjedi%canopy
if (allocated(Sfcprop_gfsjedi%ffmm      )) Sfcprop%ffmm       = Sfcprop_gfsjedi%ffmm
if (allocated(Sfcprop_gfsjedi%ffhh      )) Sfcprop%ffhh       = Sfcprop_gfsjedi%ffhh
if (allocated(Sfcprop_gfsjedi%f10m      )) Sfcprop%f10m       = Sfcprop_gfsjedi%f10m
if (allocated(Sfcprop_gfsjedi%tprcp     )) Sfcprop%tprcp      = Sfcprop_gfsjedi%tprcp
if (allocated(Sfcprop_gfsjedi%srflag    )) Sfcprop%srflag     = Sfcprop_gfsjedi%srflag
if (allocated(Sfcprop_gfsjedi%slc       )) Sfcprop%slc        = Sfcprop_gfsjedi%slc
if (allocated(Sfcprop_gfsjedi%smc       )) Sfcprop%smc        = Sfcprop_gfsjedi%smc
if (allocated(Sfcprop_gfsjedi%stc       )) Sfcprop%stc        = Sfcprop_gfsjedi%stc
if (allocated(Sfcprop_gfsjedi%t2m       )) Sfcprop%t2m        = Sfcprop_gfsjedi%t2m
if (allocated(Sfcprop_gfsjedi%q2m       )) Sfcprop%q2m        = Sfcprop_gfsjedi%q2m
if (allocated(Sfcprop_gfsjedi%tref      )) Sfcprop%tref       = Sfcprop_gfsjedi%tref
if (allocated(Sfcprop_gfsjedi%z_c       )) Sfcprop%z_c        = Sfcprop_gfsjedi%z_c
if (allocated(Sfcprop_gfsjedi%c_0       )) Sfcprop%c_0        = Sfcprop_gfsjedi%c_0
if (allocated(Sfcprop_gfsjedi%c_d       )) Sfcprop%c_d        = Sfcprop_gfsjedi%c_d
if (allocated(Sfcprop_gfsjedi%w_0       )) Sfcprop%w_0        = Sfcprop_gfsjedi%w_0
if (allocated(Sfcprop_gfsjedi%w_d       )) Sfcprop%w_d        = Sfcprop_gfsjedi%w_d
if (allocated(Sfcprop_gfsjedi%xt        )) Sfcprop%xt         = Sfcprop_gfsjedi%xt
if (allocated(Sfcprop_gfsjedi%xs        )) Sfcprop%xs         = Sfcprop_gfsjedi%xs
if (allocated(Sfcprop_gfsjedi%xu        )) Sfcprop%xu         = Sfcprop_gfsjedi%xu
if (allocated(Sfcprop_gfsjedi%xv        )) Sfcprop%xv         = Sfcprop_gfsjedi%xv
if (allocated(Sfcprop_gfsjedi%xz        )) Sfcprop%xz         = Sfcprop_gfsjedi%xz
if (allocated(Sfcprop_gfsjedi%zm        )) Sfcprop%zm         = Sfcprop_gfsjedi%zm
if (allocated(Sfcprop_gfsjedi%xtts      )) Sfcprop%xtts       = Sfcprop_gfsjedi%xtts
if (allocated(Sfcprop_gfsjedi%xzts      )) Sfcprop%xzts       = Sfcprop_gfsjedi%xzts
if (allocated(Sfcprop_gfsjedi%d_conv    )) Sfcprop%d_conv     = Sfcprop_gfsjedi%d_conv
if (allocated(Sfcprop_gfsjedi%ifd       )) Sfcprop%ifd        = Sfcprop_gfsjedi%ifd
if (allocated(Sfcprop_gfsjedi%dt_cool   )) Sfcprop%dt_cool    = Sfcprop_gfsjedi%dt_cool
if (allocated(Sfcprop_gfsjedi%qrain     )) Sfcprop%qrain      = Sfcprop_gfsjedi%qrain
if (allocated(Sfcprop_gfsjedi%snowxy    )) Sfcprop%snowxy     = Sfcprop_gfsjedi%snowxy
if (allocated(Sfcprop_gfsjedi%tvxy      )) Sfcprop%tvxy       = Sfcprop_gfsjedi%tvxy
if (allocated(Sfcprop_gfsjedi%tgxy      )) Sfcprop%tgxy       = Sfcprop_gfsjedi%tgxy
if (allocated(Sfcprop_gfsjedi%canicexy  )) Sfcprop%canicexy   = Sfcprop_gfsjedi%canicexy
if (allocated(Sfcprop_gfsjedi%canliqxy  )) Sfcprop%canliqxy   = Sfcprop_gfsjedi%canliqxy
if (allocated(Sfcprop_gfsjedi%eahxy     )) Sfcprop%eahxy      = Sfcprop_gfsjedi%eahxy
if (allocated(Sfcprop_gfsjedi%tahxy     )) Sfcprop%tahxy      = Sfcprop_gfsjedi%tahxy
if (allocated(Sfcprop_gfsjedi%cmxy      )) Sfcprop%cmxy       = Sfcprop_gfsjedi%cmxy
if (allocated(Sfcprop_gfsjedi%chxy      )) Sfcprop%chxy       = Sfcprop_gfsjedi%chxy
if (allocated(Sfcprop_gfsjedi%fwetxy    )) Sfcprop%fwetxy     = Sfcprop_gfsjedi%fwetxy
if (allocated(Sfcprop_gfsjedi%sneqvoxy  )) Sfcprop%sneqvoxy   = Sfcprop_gfsjedi%sneqvoxy
if (allocated(Sfcprop_gfsjedi%alboldxy  )) Sfcprop%alboldxy   = Sfcprop_gfsjedi%alboldxy
if (allocated(Sfcprop_gfsjedi%qsnowxy   )) Sfcprop%qsnowxy    = Sfcprop_gfsjedi%qsnowxy
if (allocated(Sfcprop_gfsjedi%wslakexy  )) Sfcprop%wslakexy   = Sfcprop_gfsjedi%wslakexy
if (allocated(Sfcprop_gfsjedi%zwtxy     )) Sfcprop%zwtxy      = Sfcprop_gfsjedi%zwtxy
if (allocated(Sfcprop_gfsjedi%waxy      )) Sfcprop%waxy       = Sfcprop_gfsjedi%waxy
if (allocated(Sfcprop_gfsjedi%wtxy      )) Sfcprop%wtxy       = Sfcprop_gfsjedi%wtxy
if (allocated(Sfcprop_gfsjedi%lfmassxy  )) Sfcprop%lfmassxy   = Sfcprop_gfsjedi%lfmassxy
if (allocated(Sfcprop_gfsjedi%rtmassxy  )) Sfcprop%rtmassxy   = Sfcprop_gfsjedi%rtmassxy
if (allocated(Sfcprop_gfsjedi%stmassxy  )) Sfcprop%stmassxy   = Sfcprop_gfsjedi%stmassxy
if (allocated(Sfcprop_gfsjedi%woodxy    )) Sfcprop%woodxy     = Sfcprop_gfsjedi%woodxy
if (allocated(Sfcprop_gfsjedi%stblcpxy  )) Sfcprop%stblcpxy   = Sfcprop_gfsjedi%stblcpxy
if (allocated(Sfcprop_gfsjedi%fastcpxy  )) Sfcprop%fastcpxy   = Sfcprop_gfsjedi%fastcpxy
if (allocated(Sfcprop_gfsjedi%xsaixy    )) Sfcprop%xsaixy     = Sfcprop_gfsjedi%xsaixy
if (allocated(Sfcprop_gfsjedi%xlaixy    )) Sfcprop%xlaixy     = Sfcprop_gfsjedi%xlaixy
if (allocated(Sfcprop_gfsjedi%taussxy   )) Sfcprop%taussxy    = Sfcprop_gfsjedi%taussxy
if (allocated(Sfcprop_gfsjedi%smcwtdxy  )) Sfcprop%smcwtdxy   = Sfcprop_gfsjedi%smcwtdxy
if (allocated(Sfcprop_gfsjedi%deeprechxy)) Sfcprop%deeprechxy = Sfcprop_gfsjedi%deeprechxy
if (allocated(Sfcprop_gfsjedi%rechxy    )) Sfcprop%rechxy     = Sfcprop_gfsjedi%rechxy
if (allocated(Sfcprop_gfsjedi%snicexy   )) Sfcprop%snicexy    = Sfcprop_gfsjedi%snicexy
if (allocated(Sfcprop_gfsjedi%snliqxy   )) Sfcprop%snliqxy    = Sfcprop_gfsjedi%snliqxy
if (allocated(Sfcprop_gfsjedi%tsnoxy    )) Sfcprop%tsnoxy     = Sfcprop_gfsjedi%tsnoxy
if (allocated(Sfcprop_gfsjedi%smoiseq   )) Sfcprop%smoiseq    = Sfcprop_gfsjedi%smoiseq
if (allocated(Sfcprop_gfsjedi%zsnsoxy   )) Sfcprop%zsnsoxy    = Sfcprop_gfsjedi%zsnsoxy

end subroutine jedi_to_gfs

! --------------------------------------------------------------------------------------------------

end module fv3jedi_sfcprop_type_mod
