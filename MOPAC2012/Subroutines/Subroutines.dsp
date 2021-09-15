# Microsoft Developer Studio Project File - Name="Subroutines" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=Subroutines - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Subroutines.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Subroutines.mak" CFG="Subroutines - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Subroutines - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "Subroutines - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "Subroutines - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"..\Modules\Release" /include:"..\Interfaces\Release" /nologo /traceback /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "Subroutines - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /include:"..\Modules\Debug" /include:"..\Interfaces\Debug" /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "Subroutines - Win32 Release"
# Name "Subroutines - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\src_subroutines\aababc.F90
DEP_F90_AABAB=\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\aabacd.F90
DEP_F90_AABAC=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\aabbcd.F90
DEP_F90_AABBC=\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\alpb_and_xfac_am1.F90
DEP_F90_ALPB_=\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\alpb_and_xfac_mndo.F90
DEP_F90_ALPB_A=\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\alpb_and_xfac_mndod.F90
DEP_F90_ALPB_AN=\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\alpb_and_xfac_pm3.F90
DEP_F90_ALPB_AND=\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\analyt.F90
DEP_F90_ANALY=\
	"..\Interfaces\Release\delri_I.mod"\
	"..\Interfaces\Release\ders_I.mod"\
	"..\Interfaces\Release\rotat_I.mod"\
	"..\Modules\Release\analyt_C.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\overlaps_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\anavib.F90
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

SOURCE=..\src_subroutines\axis.F90
DEP_F90_AXIS_=\
	"..\Interfaces\Release\rsp_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\to_screen_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\babbbc.F90
DEP_F90_BABBB=\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\babbcd.F90
DEP_F90_BABBC=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\bangle.F90
DEP_F90_BANGL=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\bfn.F90
DEP_F90_BFN_F=\
	"..\Modules\Release\overlaps_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\blas.F90
DEP_F90_BLAS_=\
	"..\Interfaces\Release\ddot_I.mod"\
	"..\Interfaces\Release\dgemm_I.mod"\
	"..\Interfaces\Release\dgemv_I.mod"\
	"..\Interfaces\Release\dger_I.mod"\
	"..\Interfaces\Release\dgetf2_I.mod"\
	"..\Interfaces\Release\dgetrs_I.mod"\
	"..\Interfaces\Release\dlaswp_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\dscal_I.mod"\
	"..\Interfaces\Release\dswap_I.mod"\
	"..\Interfaces\Release\dtrmv_I.mod"\
	"..\Interfaces\Release\dtrsm_I.mod"\
	"..\Interfaces\Release\idamax_I.mod"\
	"..\Interfaces\Release\ieeeck_I.mod"\
	"..\Interfaces\Release\lsame_I.mod"\
	"..\Interfaces\Release\xerbla_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\bldsym.F90
DEP_F90_BLDSY=\
	"..\Interfaces\Release\mult33_I.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\bonds.F90
DEP_F90_BONDS=\
	"..\Interfaces\Release\dopen_I.mod"\
	"..\Interfaces\Release\vecprt_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\calpar.F90
DEP_F90_CALPA=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\capcor.F90
DEP_F90_CAPCO=\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\charmo.F90
DEP_F90_CHARM=\
	"..\Interfaces\Release\dtrans_I.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\charst.F90
DEP_F90_CHARS=\
	"..\Interfaces\Release\dtrans_I.mod"\
	"..\Interfaces\Release\matout_I.mod"\
	"..\Interfaces\Release\minv_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\charvi.F90
DEP_F90_CHARV=\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\chrge.F90
DEP_F90_CHRGE=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\ciosci.F90
DEP_F90_CIOSC=\
	"..\Interfaces\Release\matout_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\cnvg.F90
DEP_F90_CNVG_=\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\coe.F90
DEP_F90_COE_F=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\compfg.F90
DEP_F90_COMPF=\
	"..\Interfaces\Release\deriv_I.mod"\
	"..\Interfaces\Release\dihed_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\gmetry_I.mod"\
	"..\Interfaces\Release\hcore_I.mod"\
	"..\Interfaces\Release\iter_I.mod"\
	"..\Interfaces\Release\mecip_I.mod"\
	"..\Interfaces\Release\prtpar_I.mod"\
	"..\Interfaces\Release\symtry_I.mod"\
	"..\Interfaces\Release\timer_I.mod"\
	"..\Interfaces\Release\volume_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\linear_cosmo.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\molmec_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\copyc6.F90
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\cosmo.F90
DEP_F90_COSMO=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dang.F90
DEP_F90_DANG_=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\datin.F90
DEP_F90_DATIN=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\upcase_I.mod"\
	"..\Interfaces\Release\update_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dcart.F90
DEP_F90_DCART=\
	"..\Interfaces\Release\dhc_I.mod"\
	"..\Interfaces\Release\dihed_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\molmec_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\denrot.F90
DEP_F90_DENRO=\
	"..\Interfaces\Release\coe_I.mod"\
	"..\Interfaces\Release\gmetry_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\densit.F90
DEP_F90_DENSI=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\deri0.F90
DEP_F90_DERI0=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\deri1.F90
DEP_F90_DERI1=\
	"..\Interfaces\Release\dcopy_I.mod"\
	"..\Interfaces\Release\dfock2_I.mod"\
	"..\Interfaces\Release\dhcore_I.mod"\
	"..\Interfaces\Release\dijkl1_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\helect_I.mod"\
	"..\Interfaces\Release\mecid_I.mod"\
	"..\Interfaces\Release\mecih_I.mod"\
	"..\Interfaces\Release\mtxm_I.mod"\
	"..\Interfaces\Release\mxm_I.mod"\
	"..\Interfaces\Release\supdot_I.mod"\
	"..\Interfaces\Release\timer_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\derivs_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\deri2.F90
DEP_F90_DERI2=\
	"..\Interfaces\Release\dcopy_I.mod"\
	"..\Interfaces\Release\deri22_I.mod"\
	"..\Interfaces\Release\deri23_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\mecid_I.mod"\
	"..\Interfaces\Release\mecih_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\mtxm_I.mod"\
	"..\Interfaces\Release\mxm_I.mod"\
	"..\Interfaces\Release\mxmt_I.mod"\
	"..\Interfaces\Release\osinv_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\supdot_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\derivs_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\deri21.F90
DEP_F90_DERI21=\
	"..\Interfaces\Release\mtxmc_I.mod"\
	"..\Interfaces\Release\mxm_I.mod"\
	"..\Interfaces\Release\rsp_I.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\deri22.F90
DEP_F90_DERI22=\
	"..\Interfaces\Release\ddot_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\fock2_I.mod"\
	"..\Interfaces\Release\mtxm_I.mod"\
	"..\Interfaces\Release\mxm_I.mod"\
	"..\Interfaces\Release\mxmt_I.mod"\
	"..\Interfaces\Release\supdot_I.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\deri23.F90
DEP_F90_DERI23=\
	"..\Interfaces\Release\dcopy_I.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\deritr.F90
DEP_F90_DERIT=\
	"..\Interfaces\Release\gmetry_I.mod"\
	"..\Interfaces\Release\hcore_I.mod"\
	"..\Interfaces\Release\iter_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\symtry_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\deriv.F90
DEP_F90_DERIV=\
	"..\Interfaces\Release\dcart_I.mod"\
	"..\Interfaces\Release\dernvo_I.mod"\
	"..\Interfaces\Release\dfield_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Interfaces\Release\gmetry_I.mod"\
	"..\Interfaces\Release\jcarin_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\mxm_I.mod"\
	"..\Interfaces\Release\symtry_I.mod"\
	"..\Interfaces\Release\upcase_I.mod"\
	"..\Interfaces\Release\volume_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\derivs_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dernvo.F90
DEP_F90_DERNV=\
	"..\Interfaces\Release\deri0_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dfield.F90
DEP_F90_DFIEL=\
	"..\Interfaces\Release\chrge_I.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dfock2.F90
DEP_F90_DFOCK=\
	"..\Interfaces\Release\jab_I.mod"\
	"..\Interfaces\Release\kab_I.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dfpsav.F90
DEP_F90_DFPSA=\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\ef_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dftd3.F90
DEP_F90_DFTD3=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dftd3_bits.F90
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dhc.F90
DEP_F90_DHC_F=\
	"..\Interfaces\Release\helect_I.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dhcore.F90
DEP_F90_DHCOR=\
	"..\Interfaces\Release\h1elec_I.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\diag.F90
DEP_F90_DIAG_=\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\diagi.F90
DEP_F90_DIAGI=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\diat.F90
DEP_F90_DIAT_=\
	"..\Interfaces\Release\bfn_I.mod"\
	"..\Interfaces\Release\coe_I.mod"\
	"..\Interfaces\Release\diat2_I.mod"\
	"..\Interfaces\Release\set_I.mod"\
	"..\Interfaces\Release\ss_I.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\overlaps_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\digit.F90
DEP_F90_DIGIT=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dihed.F90
DEP_F90_DIHED=\
	"..\Interfaces\Release\dang_I.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dijkl1.F90
DEP_F90_DIJKL=\
	"..\Interfaces\Release\formxy_I.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dijkl2.F90
DEP_F90_DIJKL2=\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dimens.F90
DEP_F90_DIMEN=\
	"..\Interfaces\Release\symopr_I.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dipole.F90
DEP_F90_DIPOL=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\to_screen_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dist2.F90
DEP_F90_DIST2=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dofs.F90
DEP_F90_DOFS_=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dot.F90
DEP_F90_DOT_F=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\drc.F90
DEP_F90_DRC_F=\
	"..\Interfaces\Release\compfg_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\prtdrc_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\drcout.F90
DEP_F90_DRCOU=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\write_trajectory_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dtran2.F90
DEP_F90_DTRAN=\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\dtrans.F90
DEP_F90_DTRANS=\
	"..\Interfaces\Release\dtran2_I.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\ef.F90
DEP_F90_EF_F9=\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\ef_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\enpart.F90
DEP_F90_ENPAR=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\rotate_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\esp.F90
DEP_F90_ESP_F=\
	"..\Interfaces\Release\collid_I.mod"\
	"..\Interfaces\Release\dex2_I.mod"\
	"..\Interfaces\Release\dist2_I.mod"\
	"..\Interfaces\Release\espfit_I.mod"\
	"..\Interfaces\Release\fsub_I.mod"\
	"..\Interfaces\Release\genun_I.mod"\
	"..\Interfaces\Release\gmetry_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\mult_I.mod"\
	"..\Interfaces\Release\naicas_I.mod"\
	"..\Interfaces\Release\ovlp_I.mod"\
	"..\Interfaces\Release\pdgrid_I.mod"\
	"..\Interfaces\Release\potcal_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\rsp_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\setup3_I.mod"\
	"..\Interfaces\Release\surfac_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\esp_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\overlaps_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\esp_utilities.F90
DEP_F90_ESP_U=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\exchng.F90
DEP_F90_EXCHN=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\finish.F90
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\flepo.F90
DEP_F90_FLEPO=\
	"..\Interfaces\Release\compfg_I.mod"\
	"..\Interfaces\Release\dcopy_I.mod"\
	"..\Interfaces\Release\dfpsav_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Interfaces\Release\linmin_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\prttim_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\supdot_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\fmat.F90
DEP_F90_FMAT_=\
	"..\Interfaces\Release\chrge_I.mod"\
	"..\Interfaces\Release\compfg_I.mod"\
	"..\Interfaces\Release\dipole_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\sympop_I.mod"\
	"..\Interfaces\Release\symr_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\fock1.F90
DEP_F90_FOCK1=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\fock2.F90
DEP_F90_FOCK2=\
	"..\Interfaces\Release\jab_I.mod"\
	"..\Interfaces\Release\kab_I.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\force.F90
DEP_F90_FORCE=\
	"..\Interfaces\Release\axis_I.mod"\
	"..\Interfaces\Release\compfg_I.mod"\
	"..\Interfaces\Release\dipole_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\drc_I.mod"\
	"..\Interfaces\Release\frame_I.mod"\
	"..\Interfaces\Release\gmetry_I.mod"\
	"..\Interfaces\Release\intfc_I.mod"\
	"..\Interfaces\Release\matou1_I.mod"\
	"..\Interfaces\Release\matout_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\rsp_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\symtrz_I.mod"\
	"..\Interfaces\Release\thermo_I.mod"\
	"..\Interfaces\Release\vecprt_I.mod"\
	"..\Interfaces\Release\write_trajectory_I.mod"\
	"..\Interfaces\Release\xyzint_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\to_screen_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\formxy.F90
DEP_F90_FORMX=\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\forsav.F90
DEP_F90_FORSA=\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\frame.F90
DEP_F90_FRAME=\
	"..\Interfaces\Release\axis_I.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\freqcy.F90
DEP_F90_FREQC=\
	"..\Interfaces\Release\frame_I.mod"\
	"..\Interfaces\Release\rsp_I.mod"\
	"..\Interfaces\Release\symt_I.mod"\
	"..\Interfaces\Release\symtrz_I.mod"\
	"..\Interfaces\Release\vecprt_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\to_screen_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\genun.F90
DEP_F90_GENUN=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\geout.F90
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

SOURCE=..\src_subroutines\geoutg.F90
DEP_F90_GEOUTG=\
	"..\Interfaces\Release\xxx_I.mod"\
	"..\Interfaces\Release\xyzint_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\getdat.F90
DEP_F90_GETDA=\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\GetDateStamp.F90
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\getgeg.F90
DEP_F90_GETGE=\
	"..\Interfaces\Release\getval_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\getgeo.F90
DEP_F90_GETGEO=\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Interfaces\Release\gmetry_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\nuchar_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\upcase_I.mod"\
	"..\Interfaces\Release\xyzint_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\getsym.F90
DEP_F90_GETSY=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\gettxt.F90
DEP_F90_GETTX=\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\upcase_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\getval.F90
DEP_F90_GETVA=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\gmetry.F90
DEP_F90_GMETR=\
	"..\Interfaces\Release\bangle_I.mod"\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\grid.F90
DEP_F90_GRID_=\
	"..\Interfaces\Release\dfpsav_I.mod"\
	"..\Interfaces\Release\ef_I.mod"\
	"..\Interfaces\Release\flepo_I.mod"\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\h1elec.F90
DEP_F90_H1ELE=\
	"..\Interfaces\Release\diat_I.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\H_bond_correction.F90
DEP_F90_H_BON=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\hcore.F90
DEP_F90_HCORE=\
	"..\Interfaces\Release\addhcr_I.mod"\
	"..\Interfaces\Release\addnuc_I.mod"\
	"..\Interfaces\Release\h1elec_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\rotate_I.mod"\
	"..\Interfaces\Release\solrot_I.mod"\
	"..\Interfaces\Release\vecprt_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\overlaps_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\helect.F90
DEP_F90_HELEC=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\ijkl.F90
DEP_F90_IJKL_=\
	"..\Interfaces\Release\ciint_I.mod"\
	"..\Interfaces\Release\partxy_I.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\init_filenames.F90
DEP_F90_INIT_=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\interp.F90
DEP_F90_INTER=\
	"..\Interfaces\Release\rsp_I.mod"\
	"..\Interfaces\Release\schmib_I.mod"\
	"..\Interfaces\Release\schmit_I.mod"\
	"..\Interfaces\Release\spline_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\intfc.F90
DEP_F90_INTFC=\
	"..\Interfaces\Release\jcarin_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\to_screen_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\iter.F90
DEP_F90_ITER_=\
	"..\Interfaces\Release\capcor_I.mod"\
	"..\Interfaces\Release\chrge_I.mod"\
	"..\Interfaces\Release\cnvg_I.mod"\
	"..\Interfaces\Release\diag_I.mod"\
	"..\Interfaces\Release\fock2_I.mod"\
	"..\Interfaces\Release\helect_I.mod"\
	"..\Interfaces\Release\interp_I.mod"\
	"..\Interfaces\Release\matout_I.mod"\
	"..\Interfaces\Release\meci_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\pulay_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\rsp_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\swap_I.mod"\
	"..\Interfaces\Release\timer_I.mod"\
	"..\Interfaces\Release\vecprt_I.mod"\
	"..\Interfaces\Release\writmo_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\iter_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\jab.F90
DEP_F90_JAB_F=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\jcarin.F90
DEP_F90_JCARI=\
	"..\Interfaces\Release\gmetry_I.mod"\
	"..\Interfaces\Release\symtry_I.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\kab.F90
DEP_F90_KAB_F=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\linmin.F90
DEP_F90_LINMI=\
	"..\Interfaces\Release\compfg_I.mod"\
	"..\Interfaces\Release\exchng_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\local.F90
DEP_F90_LOCAL=\
	"..\Interfaces\Release\matout_I.mod"\
	"..\Interfaces\Release\resolv_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\locmin.F90
DEP_F90_LOCMI=\
	"..\Interfaces\Release\compfg_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\exchng_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\maksym.F90
DEP_F90_MAKSY=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mamult.F90
DEP_F90_MAMUL=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mat33.F90
DEP_F90_MAT33=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\matou1.F90
DEP_F90_MATOU=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\to_screen_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\matout.F90
DEP_F90_MATOUT=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\meci.F90
DEP_F90_MECI_=\
	"..\Interfaces\Release\ciosci_I.mod"\
	"..\Interfaces\Release\diagi_I.mod"\
	"..\Interfaces\Release\dmecip_I.mod"\
	"..\Interfaces\Release\matou1_I.mod"\
	"..\Interfaces\Release\matout_I.mod"\
	"..\Interfaces\Release\mecih_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\perm_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\rsp_I.mod"\
	"..\Interfaces\Release\symtrz_I.mod"\
	"..\Interfaces\Release\upcase_I.mod"\
	"..\Interfaces\Release\vecprt_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\mndod_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mecid.F90
DEP_F90_MECID=\
	"..\Interfaces\Release\diagi_I.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mecih.F90
DEP_F90_MECIH=\
	"..\Interfaces\Release\aababc_I.mod"\
	"..\Interfaces\Release\aabacd_I.mod"\
	"..\Interfaces\Release\aabbcd_I.mod"\
	"..\Interfaces\Release\babbbc_I.mod"\
	"..\Interfaces\Release\babbcd_I.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mecip.F90
DEP_F90_MECIP=\
	"..\Interfaces\Release\mxm_I.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\minv.F90
DEP_F90_MINV_=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mndod.F90
DEP_F90_MNDOD=\
	"..\Interfaces\Release\aijm_I.mod"\
	"..\Interfaces\Release\charg_I.mod"\
	"..\Interfaces\Release\ddpo_I.mod"\
	"..\Interfaces\Release\eiscor_I.mod"\
	"..\Interfaces\Release\inighd_I.mod"\
	"..\Interfaces\Release\poij_I.mod"\
	"..\Interfaces\Release\printp_I.mod"\
	"..\Interfaces\Release\reppd2_I.mod"\
	"..\Interfaces\Release\reppd_I.mod"\
	"..\Interfaces\Release\rijkl_I.mod"\
	"..\Interfaces\Release\rotmat_I.mod"\
	"..\Interfaces\Release\rsc_I.mod"\
	"..\Interfaces\Release\scprm_I.mod"\
	"..\Interfaces\Release\spcore_I.mod"\
	"..\Interfaces\Release\tx_I.mod"\
	"..\Interfaces\Release\w2mat_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\mndod_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\moldat.F90
DEP_F90_MOLDA=\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\gmetry_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\refer_I.mod"\
	"..\Interfaces\Release\symtrz_I.mod"\
	"..\Interfaces\Release\vecprt_I.mod"\
	"..\Interfaces\Release\volume_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\ef_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\molmec_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\molsymy.F90
DEP_F90_MOLSY=\
	"..\Interfaces\Release\bangle_I.mod"\
	"..\Interfaces\Release\bldsym_I.mod"\
	"..\Interfaces\Release\cartab_I.mod"\
	"..\Interfaces\Release\chi_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\mult33_I.mod"\
	"..\Interfaces\Release\orient_I.mod"\
	"..\Interfaces\Release\plato_I.mod"\
	"..\Interfaces\Release\rotmol_I.mod"\
	"..\Interfaces\Release\rsp_I.mod"\
	"..\Interfaces\Release\symdec_I.mod"\
	"..\Interfaces\Release\symopr_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\molval.F90
DEP_F90_MOLVA=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mopend.F90
DEP_F90_MOPEN=\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mpcsyb.f90
DEP_F90_MPCSY=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mtxm.F90
DEP_F90_MTXM_=\
	"..\Interfaces\Release\dgemm_I.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mtxmc.F90
DEP_F90_MTXMC=\
	"..\Interfaces\Release\mxm_I.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mullik.F90
DEP_F90_MULLI=\
	"..\Interfaces\Release\mult_I.mod"\
	"..\Interfaces\Release\rsp_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mult.F90
DEP_F90_MULT_=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mult33.F90
DEP_F90_MULT3=\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mxm.F90
DEP_F90_MXM_F=\
	"..\Interfaces\Release\dgemm_I.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\mxmt.F90
DEP_F90_MXMT_=\
	"..\Interfaces\Release\dgemm_I.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\myword.F90
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\new_esp.F90
DEP_F90_NEW_E=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\esp_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\overlaps_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\nllsq.F90
DEP_F90_NLLSQ=\
	"..\Interfaces\Release\compfg_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Interfaces\Release\locmin_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\parsav_I.mod"\
	"..\Interfaces\Release\prttim_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\nuchar.F90
DEP_F90_NUCHA=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\osinv.F90
DEP_F90_OSINV=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\parsav.F90
DEP_F90_PARSA=\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\partxy.F90
DEP_F90_PARTX=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\Password.f90
DEP_F90_PASSW=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\pathk.F90
DEP_F90_PATHK=\
	"..\Interfaces\Release\dfpsav_I.mod"\
	"..\Interfaces\Release\ef_I.mod"\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\wrttxt_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\paths.F90
DEP_F90_PATHS=\
	"..\Interfaces\Release\dfpsav_I.mod"\
	"..\Interfaces\Release\ef_I.mod"\
	"..\Interfaces\Release\flepo_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\writmo_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\ef_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\perm.F90
DEP_F90_PERM_=\
	"..\Modules\Release\meci_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\pmep.F90
DEP_F90_PMEP_=\
	"..\Interfaces\Release\collis_I.mod"\
	"..\Interfaces\Release\drepp2_I.mod"\
	"..\Interfaces\Release\drotat_I.mod"\
	"..\Interfaces\Release\genvec_I.mod"\
	"..\Interfaces\Release\gmetry_I.mod"\
	"..\Interfaces\Release\grids_I.mod"\
	"..\Interfaces\Release\mepchg_I.mod"\
	"..\Interfaces\Release\mepmap_I.mod"\
	"..\Interfaces\Release\meprot_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\osinv_I.mod"\
	"..\Interfaces\Release\packp_I.mod"\
	"..\Interfaces\Release\pmepco_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\surfa_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\rotate_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\polar.F90
DEP_F90_POLAR=\
	"..\Interfaces\Release\alphaf_I.mod"\
	"..\Interfaces\Release\aval_I.mod"\
	"..\Interfaces\Release\axis_I.mod"\
	"..\Interfaces\Release\bdenin_I.mod"\
	"..\Interfaces\Release\bdenup_I.mod"\
	"..\Interfaces\Release\beopor_I.mod"\
	"..\Interfaces\Release\betaf_I.mod"\
	"..\Interfaces\Release\betal1_I.mod"\
	"..\Interfaces\Release\betall_I.mod"\
	"..\Interfaces\Release\betcom_I.mod"\
	"..\Interfaces\Release\bmakuf_I.mod"\
	"..\Interfaces\Release\compfg_I.mod"\
	"..\Interfaces\Release\copym_I.mod"\
	"..\Interfaces\Release\darea1_I.mod"\
	"..\Interfaces\Release\daread_I.mod"\
	"..\Interfaces\Release\dawrit_I.mod"\
	"..\Interfaces\Release\dawrt1_I.mod"\
	"..\Interfaces\Release\densf_I.mod"\
	"..\Interfaces\Release\epsab_I.mod"\
	"..\Interfaces\Release\ffreq1_I.mod"\
	"..\Interfaces\Release\ffreq2_I.mod"\
	"..\Interfaces\Release\fhpatn_I.mod"\
	"..\Interfaces\Release\gmetry_I.mod"\
	"..\Interfaces\Release\hmuf_I.mod"\
	"..\Interfaces\Release\hplusf_I.mod"\
	"..\Interfaces\Release\makeuf_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\ngamtg_I.mod"\
	"..\Interfaces\Release\ngefis_I.mod"\
	"..\Interfaces\Release\ngidri_I.mod"\
	"..\Interfaces\Release\ngoke_I.mod"\
	"..\Interfaces\Release\nonbet_I.mod"\
	"..\Interfaces\Release\nonope_I.mod"\
	"..\Interfaces\Release\nonor_I.mod"\
	"..\Interfaces\Release\openda_I.mod"\
	"..\Interfaces\Release\pol_vol_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\tf_I.mod"\
	"..\Interfaces\Release\transf_I.mod"\
	"..\Interfaces\Release\trsub_I.mod"\
	"..\Interfaces\Release\trudgu_I.mod"\
	"..\Interfaces\Release\trugdu_I.mod"\
	"..\Interfaces\Release\trugud_I.mod"\
	"..\Interfaces\Release\wrdkey_I.mod"\
	"..\Interfaces\Release\zerom_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\polar_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\powsq.F90
DEP_F90_POWSQ=\
	"..\Interfaces\Release\compfg_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Interfaces\Release\prttim_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\rsp_I.mod"\
	"..\Interfaces\Release\search_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\vecprt_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\ef_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\prtdrc.F90
DEP_F90_PRTDR=\
	"..\Interfaces\Release\chrge_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\drcout_I.mod"\
	"..\Interfaces\Release\quadr_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\xyzint_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\drc_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\prtpka.F90
DEP_F90_PRTPK=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\linear_cosmo.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\prttim.F90
DEP_F90_PRTTI=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\pulay.F90
DEP_F90_PULAY=\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\mamult_I.mod"\
	"..\Interfaces\Release\osinv_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\quadr.F90
DEP_F90_QUADR=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\react1.F90
DEP_F90_REACT=\
	"..\Interfaces\Release\compfg_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Interfaces\Release\getgeo_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\writmo_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\reada.F90
DEP_F90_READA=\
	"..\Interfaces\Release\digit_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\readmo.F90
DEP_F90_READM=\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Interfaces\Release\getgeg_I.mod"\
	"..\Interfaces\Release\getgeo_I.mod"\
	"..\Interfaces\Release\getsym_I.mod"\
	"..\Interfaces\Release\gettxt_I.mod"\
	"..\Interfaces\Release\gmetry_I.mod"\
	"..\Interfaces\Release\maksym_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\nuchar_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\symtry_I.mod"\
	"..\Interfaces\Release\wrtkey_I.mod"\
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

SOURCE=..\src_subroutines\refer.F90
DEP_F90_REFER=\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\journal_references_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\parameters_for_AM1_Sparkles_C.mod"\
	"..\Modules\Release\parameters_for_PM3_Sparkles_C.mod"\
	"..\Modules\Release\parameters_for_PM6_Sparkles_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\resolv.F90
DEP_F90_RESOL=\
	"..\Interfaces\Release\rsp_I.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\rotate.F90
DEP_F90_ROTAT=\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\rotmol.F90
DEP_F90_ROTMO=\
	"..\Interfaces\Release\symopr_I.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\rsp.F90
DEP_F90_RSP_F=\
	"..\Interfaces\Release\dasum_I.mod"\
	"..\Interfaces\Release\daxpy_I.mod"\
	"..\Interfaces\Release\ddot_I.mod"\
	"..\Interfaces\Release\dnrm2_I.mod"\
	"..\Interfaces\Release\dscal_I.mod"\
	"..\Interfaces\Release\einvit_I.mod"\
	"..\Interfaces\Release\elau_I.mod"\
	"..\Interfaces\Release\epslon_I.mod"\
	"..\Interfaces\Release\eqlrat_I.mod"\
	"..\Interfaces\Release\estpi1_I.mod"\
	"..\Interfaces\Release\etrbk3_I.mod"\
	"..\Interfaces\Release\etred3_I.mod"\
	"..\Interfaces\Release\evvrsp_I.mod"\
	"..\Interfaces\Release\freda_I.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\run_mopac.F90
DEP_F90_RUN_M=\
	"..\Interfaces\Release\calpar_I.mod"\
	"..\Interfaces\Release\compfg_I.mod"\
	"..\Interfaces\Release\datin_I.mod"\
	"..\Interfaces\Release\drc_I.mod"\
	"..\Interfaces\Release\ef_I.mod"\
	"..\Interfaces\Release\fbx_I.mod"\
	"..\Interfaces\Release\flepo_I.mod"\
	"..\Interfaces\Release\force_I.mod"\
	"..\Interfaces\Release\fordd_I.mod"\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Interfaces\Release\geoutg_I.mod"\
	"..\Interfaces\Release\grid_I.mod"\
	"..\Interfaces\Release\nllsq_I.mod"\
	"..\Interfaces\Release\pathk_I.mod"\
	"..\Interfaces\Release\paths_I.mod"\
	"..\Interfaces\Release\pmep_I.mod"\
	"..\Interfaces\Release\polar_I.mod"\
	"..\Interfaces\Release\powsq_I.mod"\
	"..\Interfaces\Release\react1_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\upcase_I.mod"\
	"..\Interfaces\Release\writmo_I.mod"\
	"..\Interfaces\Release\wrttxt_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	

!IF  "$(CFG)" == "Subroutines - Win32 Release"

# ADD F90 /debug:minimal /optimize:4

!ELSEIF  "$(CFG)" == "Subroutines - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\src_subroutines\schmib.F90
DEP_F90_SCHMI=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\schmit.F90
DEP_F90_SCHMIT=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\second.F90
DEP_F90_SECON=\
	"..\Interfaces\Release\geout_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\set.F90
DEP_F90_SET_F=\
	"..\Interfaces\Release\aintgs_I.mod"\
	"..\Interfaces\Release\bintgs_I.mod"\
	"..\Modules\Release\overlaps_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\set_up_dentate.F90
DEP_F90_SET_U=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\setup_mopac_arrays.F90
DEP_F90_SETUP=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\derivs_C.mod"\
	"..\Modules\Release\drc_C.mod"\
	"..\Modules\Release\ef_C.mod"\
	"..\Modules\Release\esp_C.mod"\
	"..\Modules\Release\iter_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\to_screen_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\setupg.F90
DEP_F90_SETUPG=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\overlaps_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\solrot.F90
DEP_F90_SOLRO=\
	"..\Interfaces\Release\rotate_I.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\sort.F90
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\sp_two_electron.f90
DEP_F90_SP_TW=\
	"..\Interfaces\Release\rsc_I.mod"\
	"..\Modules\Release\mndod_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\static_polarizability.f90
DEP_F90_STATI=\
	"..\Interfaces\Release\axis_I.mod"\
	"..\Interfaces\Release\chrge_I.mod"\
	"..\Interfaces\Release\compfg_I.mod"\
	"..\Interfaces\Release\gmetry_I.mod"\
	"..\Interfaces\Release\matout_I.mod"\
	"..\Interfaces\Release\pol_vol_I.mod"\
	"..\Interfaces\Release\rsp_I.mod"\
	"..\Interfaces\Release\vecprt_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\supdot.F90
DEP_F90_SUPDO=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\superd.F90
DEP_F90_SUPER=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\swap.F90
DEP_F90_SWAP_=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\iter_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\switch.F90
DEP_F90_SWITC=\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\conref_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\parameters_for_AM1_C.mod"\
	"..\Modules\Release\parameters_for_AM1_Sparkles_C.mod"\
	"..\Modules\Release\parameters_for_mndo_C.mod"\
	"..\Modules\Release\parameters_for_mndod_C.mod"\
	"..\Modules\Release\parameters_for_PM3_C.mod"\
	"..\Modules\Release\parameters_for_PM3_Sparkles_C.mod"\
	"..\Modules\Release\parameters_for_PM6_C.mod"\
	"..\Modules\Release\parameters_for_PM6_Sparkles_C.mod"\
	"..\Modules\Release\parameters_for_PM7_C.mod"\
	"..\Modules\Release\parameters_for_RM1_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\symh.F90
DEP_F90_SYMH_=\
	"..\Interfaces\Release\mat33_I.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\symoir.F90
DEP_F90_SYMOI=\
	"..\Interfaces\Release\charmo_I.mod"\
	"..\Interfaces\Release\charst_I.mod"\
	"..\Interfaces\Release\charvi_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\symopr.F90
DEP_F90_SYMOP=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\sympop.F90
DEP_F90_SYMPO=\
	"..\Interfaces\Release\symh_I.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\symr.F90
DEP_F90_SYMR_=\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Interfaces\Release\symp_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\symt.F90
DEP_F90_SYMT_=\
	"..\Interfaces\Release\mat33_I.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\symtry.F90
DEP_F90_SYMTR=\
	"..\Interfaces\Release\haddon_I.mod"\
	"..\Interfaces\Release\mopend_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\symtrz.F90
DEP_F90_SYMTRZ=\
	"..\Interfaces\Release\makopr_I.mod"\
	"..\Interfaces\Release\molsym_I.mod"\
	"..\Interfaces\Release\mult33_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\thermo.F90
DEP_F90_THERM=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\timer.F90
DEP_F90_TIMER=\
	"..\Interfaces\Release\second_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\timout.F90
DEP_F90_TIMOU=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\upcase.F90
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\update.F90
DEP_F90_UPDAT=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\vecprt.F90
DEP_F90_VECPR=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\volume.F90
DEP_F90_VOLUM=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\write_trajectory.F90
DEP_F90_WRITE=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\writmo.F90
DEP_F90_WRITM=\
	"..\Interfaces\Release\dipole_I.mod"\
	"..\Interfaces\Release\geoutg_I.mod"\
	"..\Interfaces\Release\meci_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\volume_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\conref_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\linear_cosmo.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\wrtkey.F90
DEP_F90_WRTKE=\
	"..\Interfaces\Release\myword_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\conref_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\wrttxt.F90
DEP_F90_WRTTX=\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\xxx.F90
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\xyzcry.F90
DEP_F90_XYZCR=\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\xyzint.F90
DEP_F90_XYZIN=\
	"..\Interfaces\Release\bangle_I.mod"\
	"..\Interfaces\Release\dihed_I.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\vast_kind_param.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# End Target
# End Project
