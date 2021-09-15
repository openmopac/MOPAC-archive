# Microsoft Developer Studio Project File - Name="Mopac2009" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=Mopac2009 - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Mopac2009.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Mopac2009.mak" CFG="Mopac2009 - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Mopac2009 - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Mopac2009 - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""$/Develop/CACHE/Standalone/MOPAC_CE", DVQCAAAA"
# PROP Scc_LocalPath "."
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Mopac2009 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Release_for_CAChe"
# PROP Intermediate_Dir ".\Release_for_CAChe"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /include:"Release/"
# ADD F90 /assume:nosource_include /compile_only /debug:none /include:"..\Modules\Release" /include:"..\Interfaces\Release" /math_library:fast /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /D "NDEBUG" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WIN32" /D "_X86_" /D CACHE_MY_NAME=MOPAC2000ComputeEngine /Fo".\Debug_for_CAChe/" /Fd".\Debug_for_CAChe/" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /i "..\cul" /i "..\Include" /d "NDEBUG" /d "_WINDOWS" /d HE_VERS=3.11 /d CACHE_MY_NAME=MOPAC2000ComputeEngine
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib wsock32.lib libcmt.lib gdi32.lib advapi32.lib dfconsol.lib dfauto.lib dfcom.lib dflogm.lib dformt.lib dfport.lib /nologo /subsystem:windows /pdb:none /machine:I386 /nodefaultlib /out:"c:\program files\fujitsu\CAChe\bin\MOPAC\MOPAC2kComputeEngine.exe" /libpath:"..\LibRelease"

!ELSEIF  "$(CFG)" == "Mopac2009 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\Debug_for_CAChe"
# PROP Intermediate_Dir ".\Debug_for_CAChe"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /include:"Debug/"
# ADD F90 /assume:nosource_include /compile_only /debug:full /include:"..\Modules\Debug" /include:"..\Interfaces\Debug" /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /G4 /MT /W2 /Zi /Od /I "..\include" /I ".\include" /I ".\src" /D "DEBUG" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WIN32" /D "_X86_" /D CACHE_MY_NAME=MOPAC2000ComputeEngine /FD /D /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /i "..\cul" /i "..\Include" /d "_DEBUG" /d HE_VERS=3.11 /d CACHE_MY_NAME=MOPAC2000ComputeEngine
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib wsock32.lib libcmt.lib gdi32.lib advapi32.lib dfauto.lib dfcom.lib dflogm.lib dfordll.lib dformd.lib dformt.lib dfport.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib /out:"c:\program files\fujitsu\CAChe\bin\MOPAC\MOPAC2kComputeEngine.exe" /pdbtype:sept /libpath:"E:\Research_top_copy\MOPAC_CE\library_for_CAChe\Debug_for_CAChe\library_for_CAChe.lib" /libpath:"..\LibDebug"
# SUBTRACT LINK32 /pdb:none /incremental:no

!ENDIF 

# Begin Target

# Name "Mopac2009 - Win32 Release"
# Name "Mopac2009 - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "*.c,*.cpp"
# Begin Source File

SOURCE=.\src\connev.cpp
# End Source File
# Begin Source File

SOURCE=.\src\connms.cpp
# End Source File
# Begin Source File

SOURCE=.\src\man.cpp
# End Source File
# Begin Source File

SOURCE=.\src\mopac2009.c
# End Source File
# Begin Source File

SOURCE=.\src\mopac2009wm.cpp
# End Source File
# Begin Source File

SOURCE=.\src\mot.cpp
# End Source File
# Begin Source File

SOURCE=.\src\motcr.cpp
# End Source File
# Begin Source File

SOURCE=.\src\motmn.cpp
# End Source File
# Begin Source File

SOURCE=.\src\motwm.cpp
# End Source File
# Begin Source File

SOURCE=.\src\motwms.cpp
# End Source File
# Begin Source File

SOURCE=.\src\ute_Exit.c
# End Source File
# Begin Source File

SOURCE=.\src\uteclta.c
# End Source File
# Begin Source File

SOURCE=.\src\utemsw.cpp
# End Source File
# Begin Source File

SOURCE=.\src\utetime.c
# End Source File
# End Group
# Begin Group "Include Files"

# PROP Default_Filter "*.h"
# Begin Source File

SOURCE=.\src\conn.h
# End Source File
# Begin Source File

SOURCE=.\src\connev.h
# End Source File
# Begin Source File

SOURCE=.\src\connms.h
# End Source File
# Begin Source File

SOURCE=.\src\man.h
# End Source File
# Begin Source File

SOURCE=.\src\mot.h
# End Source File
# Begin Source File

SOURCE=.\src\Resource.h
# End Source File
# Begin Source File

SOURCE=.\src\slist.h
# End Source File
# Begin Source File

SOURCE=.\src\ute.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "*.rc,*.ico"
# Begin Source File

SOURCE=.\src\MOPAC2009.rc
# End Source File
# Begin Source File

SOURCE=.\src\mot.ico
# End Source File
# End Group
# Begin Group "MOPAC2000 files"

# PROP Default_Filter "*.F90"
# Begin Source File

SOURCE=.\src\anavib.F90
DEP_F90_ANAVI=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\to_screen_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\geout.F90
DEP_F90_GEOUT=\
	"..\Interfaces\Release\chrge_I.mod"\
	"..\Interfaces\Release\wrttxt_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\mopac.F90
DEP_F90_MOPAC=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\mopend.F90
DEP_F90_MOPEN=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\second.F90
DEP_F90_SECON=\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\write_trajectory.F90
DEP_F90_WRITE=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# End Group
# Begin Group "Fortran Include Files"

# PROP Default_Filter "*.h"
# Begin Source File

SOURCE=.\include\aababc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\aabacd_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\aabbcd_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\addfck_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\addhb_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\addhcr_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\addkwd_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\addnuc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\adjvec_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\aijm_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\aintgs_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\alphaf_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\am1d_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\am1lig_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\am1par_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\analyt_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\anavib_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ansude_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\arom2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\arom_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\asum_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\atomrs_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\aval_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\axis_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\babbbc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\babbcd_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\bangle_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\bdenin_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\bdenup_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\beopor_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\betaf_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\betal1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\betall_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\betcom_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\bfn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\bintgs_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\bldsym_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\bmakuf_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\bondn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\bonds_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\bondsz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\bprint_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\break_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\brlzon_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\buildf_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\buildn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\cadima_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\calpar_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\canon_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\capcor_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\cartab_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ccprod_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ccrep_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\cdiag_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\charg_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\charmo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\charst_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\charvi_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\check_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\check_license_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\chi_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\chkion_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\chklew_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\chkmem_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\chrge_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\chrgez_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\chrgn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ciint_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ciosci_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\close_file_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\cnvg_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\cnvgz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\coe_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\collid_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\collis_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\collit_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\commop_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\commoz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\compct_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\compfg_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\copy1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\copy2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\copym_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\coscan_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\coscav_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\coscl1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\coscl2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\cosini_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\coul_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\count_nstep_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\csum_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dang_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\darea1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\daread_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dasum_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\datin_brdcst_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\datin_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dawrit_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dawrt1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\daxpy_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dcarn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dcarnz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dcart_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dcopy_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dcrypt_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ddot_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ddpo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\decode_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\delmol_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\delnew_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\delri_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\delsta_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\delta_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\denrot_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\denroz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\densf_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\densit_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\densiz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\deri0_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\deri1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\deri21_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\deri22_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\deri23_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\deri2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\derin_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\deritr_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\deriv_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dern1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dern2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dernvn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dernvo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\derp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ders_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dex2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dfield_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dfielz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dfienz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dfock2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dfpsan_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dfpsav_bcast_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dfpsav_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dgedi_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dgefa_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dgemm_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dgesv_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dhc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dhcore_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\diag_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\diagg1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\diagg2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\diagg_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\diagi_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\diat2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\diat_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\diegrd_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\digit_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dihed_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dijkl1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dijkl2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dimens_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dintrp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dipind_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dipole_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dipoln_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dipolz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dist2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dmecip_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dnrm2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dock_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dofs_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dopen_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dopro_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dot1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dot_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dpro_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\drc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\drcout_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\drepp2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\drn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\drotat_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dscal_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dsqr_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dsum_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dswap_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dtran2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dtransform_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\dvfill_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ea08c_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ea09c_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ec08c_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ef_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\efsav_bcast_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\efsav_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\efstr_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\eigen_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\eigenn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\eimp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\einvit_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\eiscor_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\elau_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\elenuc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\elesn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\elesp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\empiri_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\en_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\encoding.h
# End Source File
# Begin Source File

SOURCE=.\include\enpart_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\epsab_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\epseta_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\epslon_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\eqlrat_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\errion_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\error_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\esn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\esp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\espfit_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\estpi1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\etrbk3_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\etred3_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\evvrsp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\exchng_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\extend_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\extvdw_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fbx_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fcnpp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ffhpol_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ffreq1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ffreq2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fhpatn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fillc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\filmat_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\findn1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\finish_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fl16ch_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\flepn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\flepo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\flushm_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fm06as_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fm06bs_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fmat_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fmo_size_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\focd2z_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fock1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fock1z_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fock2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fock2n_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fock2z_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fockd2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\force_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\forcn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fordd_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\formd_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\formxy_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\forsav_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\frame_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\frameb_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\freda_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\freqcy_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fresto_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fsub_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fz2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\fz2n_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\gather_int_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\gather_real_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\genun_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\genvec_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\geochk_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\geochn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\geoun_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\geout_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\geoutg_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\geoutn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\get2c_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\get3c_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\get_sysinfo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\geta1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\getcc1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\getdat_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\getgeg_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\getgeo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\gethes_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\getpdb_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\getsym_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\gettxt_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\getval_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\gmetrn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\gmetry_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\gover_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\greek_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\greenf_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\grid_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\grids_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\gstore_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\h1elec_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\h1elez_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\haddon_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\hbonds_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\hcore_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\hcorn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\hcorz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\header_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\helect_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\helecz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\hesini_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\hespow_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\hmuf_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\hplusf_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\hxvec_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\hybrid_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\idamax_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ijkl_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\inid_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\inighd_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\insymc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\int8ch_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\interp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\intfc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\invfn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ionout_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ird_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\isencd_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\isitsc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\iten_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\itenz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\iter_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\iterz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\jab_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\jcarin_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\kab_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ldima_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ldimn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\lewis_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ligand_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\linmin_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\local2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\local_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\localz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\locanz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\locmin_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\lyse_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\makeuf_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\makopr_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\maksym_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\makvec_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\makven_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mamult_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mat33_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\maton1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\matou1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\matoun_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\matout_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mbonds_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\me08a_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\me08b_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\meci.h
# End Source File
# Begin Source File

SOURCE=.\include\meci_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mecid_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mecih_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mecip_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mecn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mem_check_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mepchg_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mepmap_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\meprot_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\merge_wmpwob_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mfinel_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mingeo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\minloc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\minv_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mkbmat_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mlig_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mlmo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\modchg_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\modgra_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\moiety_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\moint_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\moldan_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\moldat_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\molsym_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\molval_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mopend_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\moz_mem_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mpcbds_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mpcpop_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mpcsyb_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mtxm_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mtxmc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mullik_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mullin_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mulliz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mult33_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mult_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mxm_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\mxmt_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\myword_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\naican_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\naicap_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\naicas_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\names_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\newdel_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\newflg_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\newhes_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ngamtg_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ngefis_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ngidri_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ngoke_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\nllsq_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\nonbet_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\nonope_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\nonor_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\notlft_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\nuchar_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\nxtmer_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\openda_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\optbr_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\orient_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\osinv_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\outer1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\outer2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\overlp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ovlp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\packp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\parsan_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\parsav_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\partxy_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\pathk_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\paths_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\pdboun_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\pdgrid_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\pedra_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\perm_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\picopt_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\pinoun_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\pinout_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\plato_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\pmep_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\pmepco_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\poij_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\point_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\polan_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\polanz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\polar_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\polarz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\potcal_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\powsan_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\powsav_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\powsn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\powsq_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\printp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\prjfc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\proje_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\prtbet_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\prtdrc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\prtgam_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\prtgra_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\prthco_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\prthes_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\prtinv_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\prtlmn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\prtlmo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\prtpar_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\prttim_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\pulay_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\purdf1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\qnalg_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\qnaln_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\qnsave_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\qnstep_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\quadr_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\radial_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rcycmv_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\react1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\react2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\reada_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\readg_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\readmo_bcast_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\readmo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\redatm_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\redlig_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\redncd_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\redpar_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\redref_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\refer_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\renum_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\reorth_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\repp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\reppd2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\reppd_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\resen_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\reseq_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\reset_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\resolv_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rfield_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rfieln_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rijkl_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ring5_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rmopac_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rotat_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rotatd_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rotate_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rotlmn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rotlmo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rotmat_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rotmol_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rotzz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\routine_timer_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rpol1_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rprt_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rsc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\rsp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\savgrd_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\savpar_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\scfcri_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\schmib_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\schmit_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\scprm_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\search_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\search_path_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\second_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\selmos_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\senddata_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\set_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\setalp_init_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\setalp_read_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\setalp_write_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\setcup_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\setup3_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\setupg_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\setupi_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\setupk_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\setupr_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\sisms_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\snapth_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\solbon_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\solbox_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\solrot_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\sort_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\spcore_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\spline_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\ss_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\state_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\suma2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\supdot_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\superd_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\surclo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\surfa_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\surfac_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\surfat_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\swap_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\switch_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\symdec_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\symh_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\symoir_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\symopr_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\symp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\symr_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\symt_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\symtnn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\symtrn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\symtry.h
# End Source File
# Begin Source File

SOURCE=.\include\symtry_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\symtrz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\tf_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\thermo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\tidn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\tidy_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\time_out_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\timer_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\timout_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\tmpi_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\tmpmr_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\tmpzr_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\tranf_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\transf_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\transi_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\trimat_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\trsub_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\trudgu_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\trugdu_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\trugud_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\trunk_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\tx_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\txtype_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\unpack_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\upcase_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\update_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\updhes_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\updhin_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\valuen_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\values_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\vecprn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\vecprt_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\vecprz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\vecpzz_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\volume_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\w2man_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wallc_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\winmop_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\worder_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wrdkey_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\writmn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\writmo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wrtchk_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wrtcon_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wrtkey_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wrtorb2_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wrtorb_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wrtout_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wrttxt_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wrtwmn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wrtwmp_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wrtwob_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wrtwor_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wstorn_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\wwstep_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\xxx_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\xyzcry_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\xyzgeo_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\xyzint_interface.h
# End Source File
# Begin Source File

SOURCE=.\include\zerom_interface.h
# End Source File
# End Group
# End Target
# End Project
