# Microsoft Developer Studio Project File - Name="XP_CVF_MOPAC2012" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=XP_CVF_MOPAC2012 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "XP_CVF_MOPAC2012.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "XP_CVF_MOPAC2012.mak" CFG="XP_CVF_MOPAC2012 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "XP_CVF_MOPAC2012 - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "XP_CVF_MOPAC2012 - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "XP_CVF_MOPAC2012 - Win32 Release"

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
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "XP_CVF_MOPAC2012 - Win32 Debug"

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
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /define:"aa" /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ  /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ  /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "XP_CVF_MOPAC2012 - Win32 Release"
# Name "XP_CVF_MOPAC2012 - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\MOPAC_source_code\aababc.F90
NODEP_F90_AABAB=\
	".\Debug\meci_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\aababc_I.F90
NODEP_F90_AABABC=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\aabacd.F90
NODEP_F90_AABAC=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\aabacd_I.F90
NODEP_F90_AABACD=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\aabbcd.F90
NODEP_F90_AABBC=\
	".\Debug\meci_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\aabbcd_I.F90
NODEP_F90_AABBCD=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\add_hydrogen_atoms.F90
NODEP_F90_ADD_H=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\mod_atomradii.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\add_more_interactions.F90
NODEP_F90_ADD_M=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\iter_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\addfck_I.F90
NODEP_F90_ADDFC=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\addhb.F90
NODEP_F90_ADDHB=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\addhcr_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\addnuc_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\adjvec.F90
NODEP_F90_ADJVE=\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\afmm_mod.F90
NODEP_F90_AFMM_=\
	".\Debug\chanel_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\aijm_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\aintgs_I.F90
NODEP_F90_AINTG=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\alpb_and_xfac_am1.F90
NODEP_F90_ALPB_=\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\alpb_and_xfac_mndo.F90
NODEP_F90_ALPB_A=\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\alpb_and_xfac_mndod.F90
NODEP_F90_ALPB_AN=\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\alpb_and_xfac_pm3.F90
NODEP_F90_ALPB_AND=\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\alphaf_I.F90
NODEP_F90_ALPHA=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\analyt.F90
NODEP_F90_ANALY=\
	".\Debug\analyt_C.mod"\
	".\Debug\chanel_C.mod"\
	".\Debug\delri_I.mod"\
	".\Debug\ders_I.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\overlaps_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\rotat_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\analyt_C.F90
NODEP_F90_ANALYT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\anavib.F90
NODEP_F90_ANAVI=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\to_screen_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\asum_I.F90
NODEP_F90_ASUM_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\atomrs.F90
NODEP_F90_ATOMR=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\aval_I.F90
NODEP_F90_AVAL_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\axis.F90
NODEP_F90_AXIS_=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\to_screen_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\axis_I.F90
NODEP_F90_AXIS_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\babbbc.F90
NODEP_F90_BABBB=\
	".\Debug\meci_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\babbbc_I.F90
NODEP_F90_BABBBC=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\babbcd.F90
NODEP_F90_BABBC=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\babbcd_I.F90
NODEP_F90_BABBCD=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\bangle.F90
NODEP_F90_BANGL=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\bangle_I.F90
NODEP_F90_BANGLE=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\bdenin_I.F90
NODEP_F90_BDENI=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\bdenup_I.F90
NODEP_F90_BDENU=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\beopor_I.F90
NODEP_F90_BEOPO=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\betaf_I.F90
NODEP_F90_BETAF=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\betal1_I.F90
NODEP_F90_BETAL=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\betall_I.F90
NODEP_F90_BETALL=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\betcom_I.F90
NODEP_F90_BETCO=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\bfn.F90
NODEP_F90_BFN_F=\
	".\Debug\overlaps_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\bfn_I.F90
NODEP_F90_BFN_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\big_swap.F90
NODEP_F90_BIG_S=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\ef_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\bintgs_I.F90
NODEP_F90_BINTG=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\blas_I.F90
NODEP_F90_BLAS_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\bldsym.F90
NODEP_F90_BLDSY=\
	".\Debug\mult33_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\bldsym_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\bmakuf_I.F90
NODEP_F90_BMAKU=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\bonds.F90
NODEP_F90_BONDS=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\dopen_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\vecprt_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\bonds_for_MOZYME.F90
NODEP_F90_BONDS_=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\bonds_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\break_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\btoc_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\buildf.F90
NODEP_F90_BUILD=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\calpar.F90
NODEP_F90_CALPA=\
	".\Debug\chanel_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\calpar_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\capcor.F90
NODEP_F90_CAPCO=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\capcor_I.F90
NODEP_F90_CAPCOR=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\cartab_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ccprod_I.F90
NODEP_F90_CCPRO=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\cdiag_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\chanel_C.F90
NODEP_F90_CHANE=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\charg_I.F90
NODEP_F90_CHARG=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\charmo.F90
NODEP_F90_CHARM=\
	".\Debug\molkst_C.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\charmo_I.F90
NODEP_F90_CHARMO=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\charst.F90
NODEP_F90_CHARS=\
	".\Debug\chanel_C.mod"\
	".\Debug\matout_I.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\minv_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\charst_I.F90
NODEP_F90_CHARST=\
	".\Debug\meci_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\charvi.F90
NODEP_F90_CHARV=\
	".\Debug\molkst_C.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\charvi_I.F90
NODEP_F90_CHARVI=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\check.F90
NODEP_F90_CHECK=\
	".\Debug\chanel_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\chi_I.F90
NODEP_F90_CHI_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\chkion.F90
NODEP_F90_CHKIO=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\chklew.F90
NODEP_F90_CHKLE=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\chrge.F90
NODEP_F90_CHRGE=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\chrge_for_MOZYME.F90
NODEP_F90_CHRGE_=\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\chrge_I.F90
NODEP_F90_CHRGE_I=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ciint_I.F90
NODEP_F90_CIINT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ciosci.F90
NODEP_F90_CIOSC=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\matout_I.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ciosci_I.F90
NODEP_F90_CIOSCI=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\cnvg.F90
NODEP_F90_CNVG_=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\cnvg_I.F90
NODEP_F90_CNVG_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\cnvgz.F90
NODEP_F90_CNVGZ=\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\coe.F90
NODEP_F90_COE_F=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\coe_I.F90
NODEP_F90_COE_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\collid_I.F90
NODEP_F90_COLLI=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\collis_I.F90
NODEP_F90_COLLIS=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\Common_arrays_C.F90
NODEP_F90_COMMO=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\compct.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\compfg.F90
NODEP_F90_COMPF=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\deriv_I.mod"\
	".\Debug\dihed_I.mod"\
	".\Debug\dot_I.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\gmetry_I.mod"\
	".\Debug\hcore_I.mod"\
	".\Debug\iter_I.mod"\
	".\Debug\linear_cosmo.mod"\
	".\Debug\mecip_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\molmec_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\prtpar_I.mod"\
	".\Debug\symtry_I.mod"\
	".\Debug\timer_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\volume_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\compfg_I.F90
NODEP_F90_COMPFG=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\compfn_I.F90
NODEP_F90_COMPFN=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\conref_C.F90
NODEP_F90_CONRE=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\consts_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\convert_storage.F90
NODEP_F90_CONVE=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\copyc6.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\copym_I.F90
NODEP_F90_COPYM=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\cosmo.F90
NODEP_F90_COSMO=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\cosmo_C.F90
NODEP_F90_COSMO_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\cqden_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\csum_I.F90
NODEP_F90_CSUM_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dang.F90
NODEP_F90_DANG_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dang_I.F90
NODEP_F90_DANG_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\darea1_I.F90
NODEP_F90_DAREA=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\daread_I.F90
NODEP_F90_DAREAD=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dasum_I.F90
NODEP_F90_DASUM=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\datin.F90
NODEP_F90_DATIN=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\upcase_I.mod"\
	".\Debug\update_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\datin_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dawrit_I.F90
NODEP_F90_DAWRI=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dawrt1_I.F90
NODEP_F90_DAWRT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\daxpy_I.F90
NODEP_F90_DAXPY=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dcart.F90
NODEP_F90_DCART=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\dhc_I.mod"\
	".\Debug\dihed_I.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\molmec_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dcart_I.F90
NODEP_F90_DCART_=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ddot_I.F90
NODEP_F90_DDOT_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ddpo_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\delmol_I.F90
NODEP_F90_DELMO=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\delri_I.F90
NODEP_F90_DELRI=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\delsta.F90
NODEP_F90_DELST=\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\delta_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\denrot.F90
NODEP_F90_DENRO=\
	".\Debug\chanel_C.mod"\
	".\Debug\coe_I.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\gmetry_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\denrot_for_MOZYME.F90
NODEP_F90_DENROT=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\denrot_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\densf_I.F90
NODEP_F90_DENSF=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\densit.F90
NODEP_F90_DENSI=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\density_cuda_i.F90
NODEP_F90_DENSIT=\
	".\Debug\iso_c_binding.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\density_for_GPU.F90
NODEP_F90_DENSITY=\
	".\Debug\call_gemm_cublas.mod"\
	".\Debug\call_syrk_cublas.mod"\
	".\Debug\density_cuda_i.mod"\
	".\Debug\iso_c_binding.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\density_for_MOZYME.F90
NODEP_F90_DENSITY_=\
	".\Debug\chanel_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deri0.F90
NODEP_F90_DERI0=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deri0_I.F90
NODEP_F90_DERI0_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deri1.F90
NODEP_F90_DERI1=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\derivs_C.mod"\
	".\Debug\dfock2_I.mod"\
	".\Debug\dhcore_I.mod"\
	".\Debug\dijkl1_I.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\helect_I.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\mecid_I.mod"\
	".\Debug\mecih_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mtxm_I.mod"\
	".\Debug\mxm_I.mod"\
	".\Debug\supdot_I.mod"\
	".\Debug\timer_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deri1_I.F90
NODEP_F90_DERI1_=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deri2.F90
NODEP_F90_DERI2=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\deri22_I.mod"\
	".\Debug\deri23_I.mod"\
	".\Debug\derivs_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\mecid_I.mod"\
	".\Debug\mecih_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\mtxm_I.mod"\
	".\Debug\mxm_I.mod"\
	".\Debug\mxmt_I.mod"\
	".\Debug\osinv_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\supdot_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deri21.F90
NODEP_F90_DERI21=\
	".\Debug\mtxmc_I.mod"\
	".\Debug\mxm_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deri21_I.F90
NODEP_F90_DERI21_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deri22.F90
NODEP_F90_DERI22=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\fock2_I.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mtxm_I.mod"\
	".\Debug\mxm_I.mod"\
	".\Debug\mxmt_I.mod"\
	".\Debug\supdot_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deri22_I.F90
NODEP_F90_DERI22_=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deri23.F90
NODEP_F90_DERI23=\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deri23_I.F90
NODEP_F90_DERI23_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deri2_I.F90
NODEP_F90_DERI2_=\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deritr.F90
NODEP_F90_DERIT=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\gmetry_I.mod"\
	".\Debug\hcore_I.mod"\
	".\Debug\iter_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\symtry_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deritr_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deriv.F90
NODEP_F90_DERIV=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\dcart_I.mod"\
	".\Debug\derivs_C.mod"\
	".\Debug\dernvo_I.mod"\
	".\Debug\dfield_I.mod"\
	".\Debug\dot_I.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\gmetry_I.mod"\
	".\Debug\jcarin_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\mxm_I.mod"\
	".\Debug\mxv_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\symtry_I.mod"\
	".\Debug\upcase_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\volume_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\deriv_I.F90
NODEP_F90_DERIV_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dernvo.F90
NODEP_F90_DERNV=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\deri0_I.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dernvo_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ders_I.F90
NODEP_F90_DERS_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dex2_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dfield.F90
NODEP_F90_DFIEL=\
	".\Debug\chrge_I.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dfield_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dfock2.F90
NODEP_F90_DFOCK=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\jab_I.mod"\
	".\Debug\kab_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dfock2_I.F90
NODEP_F90_DFOCK2=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dfport.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dfpsav.F90
NODEP_F90_DFPSA=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\ef_C.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dfpsav_I.F90
NODEP_F90_DFPSAV=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dftd3.F90
NODEP_F90_DFTD3=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dftd3_bits.F90
NODEP_F90_DFTD3_=\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dgefa_I.F90
NODEP_F90_DGEFA=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dgetri_I.F90
NODEP_F90_DGETR=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dhc.F90
NODEP_F90_DHC_F=\
	".\Debug\helect_I.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dhc_I.F90
NODEP_F90_DHC_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dhcore.F90
NODEP_F90_DHCOR=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\h1elec_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dhcore_I.F90
NODEP_F90_DHCORE=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\diag.F90
NODEP_F90_DIAG_=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\diag_for_GPU.F90
NODEP_F90_DIAG_F=\
	".\Debug\call_gemm_cublas.mod"\
	".\Debug\call_rot_cuda.mod"\
	".\Debug\iso_c_binding.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\diag_I.F90
NODEP_F90_DIAG_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\diagg.F90
NODEP_F90_DIAGG=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\diagg1.F90
NODEP_F90_DIAGG1=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\diagg2.F90
NODEP_F90_DIAGG2=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\diagi.F90
NODEP_F90_DIAGI=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\diagi_I.F90
NODEP_F90_DIAGI_=\
	".\Debug\meci_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\diat.F90
NODEP_F90_DIAT_=\
	".\Debug\bfn_I.mod"\
	".\Debug\coe_I.mod"\
	".\Debug\diat2_I.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\overlaps_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\set_I.mod"\
	".\Debug\ss_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\diat2_I.F90
NODEP_F90_DIAT2=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\diat_I.F90
NODEP_F90_DIAT_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\diegrd_I.F90
NODEP_F90_DIEGR=\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dielen_I.F90
NODEP_F90_DIELE=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\digit.F90
NODEP_F90_DIGIT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\digit_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dihed.F90
NODEP_F90_DIHED=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\dang_I.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dihed_I.F90
NODEP_F90_DIHED_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dijkl1.F90
NODEP_F90_DIJKL=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\formxy_I.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dijkl1_I.F90
NODEP_F90_DIJKL1=\
	".\Debug\meci_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dijkl2.F90
NODEP_F90_DIJKL2=\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dijkl2_I.F90
NODEP_F90_DIJKL2_=\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dijkld_I.F90
NODEP_F90_DIJKLD=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dimens.F90
NODEP_F90_DIMEN=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\symopr_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dimens_I.F90
NODEP_F90_DIMENS=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dipole.F90
NODEP_F90_DIPOL=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\to_screen_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dipole_for_MOZYME.F90
NODEP_F90_DIPOLE=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dipole_I.F90
NODEP_F90_DIPOLE_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dist2.F90
NODEP_F90_DIST2=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dist2_I.F90
NODEP_F90_DIST2_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dmecip_I.F90
NODEP_F90_DMECI=\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dnrm2_I.F90
NODEP_F90_DNRM2=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dofs.F90
NODEP_F90_DOFS_=\
	".\Debug\chanel_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dofs_I.F90
NODEP_F90_DOFS_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dopen_I.F90
NODEP_F90_DOPEN=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dot.F90
NODEP_F90_DOT_F=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dot_I.F90
NODEP_F90_DOT_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\drc.F90
NODEP_F90_DRC_F=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\compfg_I.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\prtdrc_I.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\drc_I.F90
NODEP_F90_DRC_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\drcout.F90
NODEP_F90_DRCOU=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\write_trajectory_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\drcout_I.F90
NODEP_F90_DRCOUT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\drepp2_I.F90
NODEP_F90_DREPP=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\drotat_I.F90
NODEP_F90_DROTA=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dsum_I.F90
NODEP_F90_DSUM_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dtran2.F90
NODEP_F90_DTRAN=\
	".\Debug\funcon_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dtran2_I.F90
NODEP_F90_DTRAN2=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dtrans.F90
NODEP_F90_DTRANS=\
	".\Debug\dtran2_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dtrmm_I.F90
NODEP_F90_DTRMM=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dtrmv_I.F90
NODEP_F90_DTRMV=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dtrti2_I.F90
NODEP_F90_DTRTI=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\dvfill_I.F90
NODEP_F90_DVFIL=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ef.F90
NODEP_F90_EF_F9=\
	".\Debug\call_gemm_cublas.mod"\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\ef_C.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ef_C.F90
NODEP_F90_EF_C_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ef_I.F90
NODEP_F90_EF_I_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\eigen.F90
NODEP_F90_EIGEN=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\reada_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\eigenvectors_LAPACK.F90
NODEP_F90_EIGENV=\
	".\Debug\chanel_C.mod"\
	".\Debug\initMagma.mod"\
	".\Debug\magma.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\eimp.F90
NODEP_F90_EIMP_=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\einvit_I.F90
NODEP_F90_EINVI=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\eiscor_I.F90
NODEP_F90_EISCO=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\elau_I.F90
NODEP_F90_ELAU_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\elemts_C.F90
NODEP_F90_ELEMT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\elenuc_I.F90
NODEP_F90_ELENU=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\elesn_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\en_I.F90
NODEP_F90_EN_I_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\enpart.F90
NODEP_F90_ENPAR=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\rotate_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\enpart_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\epsab_I.F90
NODEP_F90_EPSAB=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\epseta.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\epseta_I.F90
NODEP_F90_EPSET=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\epslon_I.F90
NODEP_F90_EPSLO=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\eqlrat_I.F90
NODEP_F90_EQLRA=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\esn_I.F90
NODEP_F90_ESN_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\esp.F90
NODEP_F90_ESP_F=\
	".\Debug\chanel_C.mod"\
	".\Debug\collid_I.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\dex2_I.mod"\
	".\Debug\dist2_I.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\esp_C.mod"\
	".\Debug\espfit_I.mod"\
	".\Debug\fsub_I.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\genun_I.mod"\
	".\Debug\gmetry_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\mult_I.mod"\
	".\Debug\naicas_I.mod"\
	".\Debug\overlaps_C.mod"\
	".\Debug\ovlp_I.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\pdgrid_I.mod"\
	".\Debug\potcal_I.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\setup3_I.mod"\
	".\Debug\surfac_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\esp1_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\esp_C.F90
NODEP_F90_ESP_C=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\esp_utilities.F90
NODEP_F90_ESP_U=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\espfit_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\estpi1_I.F90
NODEP_F90_ESTPI=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\etime_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\etrbk3_I.F90
NODEP_F90_ETRBK=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\etred3_I.F90
NODEP_F90_ETRED=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\euler_C.F90
NODEP_F90_EULER=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\evvrsp_I.F90
NODEP_F90_EVVRS=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\exchng.F90
NODEP_F90_EXCHN=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\exchng_I.F90
NODEP_F90_EXCHNG=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\fbx_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\fcnpp_I.F90
NODEP_F90_FCNPP=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ffreq1_I.F90
NODEP_F90_FFREQ=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ffreq2_I.F90
NODEP_F90_FFREQ2=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\fhpatn_I.F90
NODEP_F90_FHPAT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\fillij.F90
NODEP_F90_FILLI=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\overlaps_C.mod"\
	".\Debug\reada_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\findn1.F90
NODEP_F90_FINDN=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\finish.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\flepo.F90
NODEP_F90_FLEPO=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\compfg_I.mod"\
	".\Debug\dfpsav_I.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\linmin_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\prttim_I.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\supdot_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\flepo_I.F90
NODEP_F90_FLEPO_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\fmat.F90
NODEP_F90_FMAT_=\
	".\Debug\chanel_C.mod"\
	".\Debug\chrge_I.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\compfg_I.mod"\
	".\Debug\dipole_I.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\sympop_I.mod"\
	".\Debug\symr_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\fock1.F90
NODEP_F90_FOCK1=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\fock1_for_MOZYME.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\fock1_I.F90
NODEP_F90_FOCK1_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\fock2.F90
NODEP_F90_FOCK2=\
	".\Debug\cosmo_C.mod"\
	".\Debug\jab_I.mod"\
	".\Debug\kab_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\fock2_I.F90
NODEP_F90_FOCK2_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\fock2z.F90
NODEP_F90_FOCK2Z=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\linear_cosmo.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\fockd2_I.F90
NODEP_F90_FOCKD=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\force.F90
NODEP_F90_FORCE=\
	".\Debug\axis_I.mod"\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\compfg_I.mod"\
	".\Debug\dipole_I.mod"\
	".\Debug\drc_I.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\frame_I.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\gmetry_I.mod"\
	".\Debug\intfc_I.mod"\
	".\Debug\matou1_I.mod"\
	".\Debug\matout_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\symtrz_I.mod"\
	".\Debug\thermo_I.mod"\
	".\Debug\to_screen_C.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\vecprt_I.mod"\
	".\Debug\write_trajectory_I.mod"\
	".\Debug\xyzint_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\force_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\fordd_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\formxy.F90
NODEP_F90_FORMX=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\formxy_I.F90
NODEP_F90_FORMXY=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\forsav.F90
NODEP_F90_FORSA=\
	".\Debug\chanel_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\frame.F90
NODEP_F90_FRAME=\
	".\Debug\axis_I.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\frame_I.F90
NODEP_F90_FRAME_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\freda_I.F90
NODEP_F90_FREDA=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\freqcy.F90
NODEP_F90_FREQC=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\frame_I.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\symt_I.mod"\
	".\Debug\symtrz_I.mod"\
	".\Debug\to_screen_C.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\vecprt_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\fsub_I.F90
NODEP_F90_FSUB_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\funcon_C.F90
NODEP_F90_FUNCO=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\genun.F90
NODEP_F90_GENUN=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\genun_I.F90
NODEP_F90_GENUN_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\genvec_I.F90
NODEP_F90_GENVE=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\geo_ref.F90
NODEP_F90_GEO_R=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\geochk.F90
NODEP_F90_GEOCH=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\mod_atomradii.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\geout.F90
NODEP_F90_GEOUT=\
	".\Debug\chanel_C.mod"\
	".\Debug\chrge_I.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\wrttxt_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\geout_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\geoutg.F90
NODEP_F90_GEOUTG=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\xxx_I.mod"\
	".\Debug\xyzint_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\geoutg_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\getdat.F90
NODEP_F90_GETDA=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\GetDateStamp.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\getgeg.F90
NODEP_F90_GETGE=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\getval_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\getgeg_I.F90
NODEP_F90_GETGEG=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\getgeo.F90
NODEP_F90_GETGEO=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\gmetry_I.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\nuchar_I.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\upcase_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\xyzint_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\getgeo_I.F90
NODEP_F90_GETGEO_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\getpdb.F90
NODEP_F90_GETPD=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\getsym.F90
NODEP_F90_GETSY=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\getsym_I.F90
NODEP_F90_GETSYM=\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\gettxt.F90
NODEP_F90_GETTX=\
	".\Debug\chanel_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\upcase_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\gettxt_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\getval.F90
NODEP_F90_GETVA=\
	".\Debug\reada_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\getval_I.F90
NODEP_F90_GETVAL=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\gmetry.F90
NODEP_F90_GMETR=\
	".\Debug\bangle_I.mod"\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\gmetry_I.F90
NODEP_F90_GMETRY=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\gover_I.F90
NODEP_F90_GOVER=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\greek.F90
NODEP_F90_GREEK=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\grid.F90
NODEP_F90_GRID_=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\dfpsav_I.mod"\
	".\Debug\ef_I.mod"\
	".\Debug\flepo_I.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\grid_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\grids_I.F90
NODEP_F90_GRIDS=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\gstore_I.F90
NODEP_F90_GSTOR=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\h1elec.F90
NODEP_F90_H1ELE=\
	".\Debug\diat_I.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\h1elec_I.F90
NODEP_F90_H1ELEC=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\H_bond_correction.F90
NODEP_F90_H_BON=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\H_bonds4.F90
NODEP_F90_H_BOND=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\haddon_I.F90
NODEP_F90_HADDO=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\hbonds.F90
NODEP_F90_HBOND=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\hcore.F90
NODEP_F90_HCORE=\
	".\Debug\addhcr_I.mod"\
	".\Debug\addnuc_I.mod"\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\h1elec_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\overlaps_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\rotate_I.mod"\
	".\Debug\solrot_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\vecprt_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\hcore_for_MOZYME.F90
NODEP_F90_HCORE_=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\linear_cosmo.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\overlaps_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\hcore_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\hcored_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\helect.F90
NODEP_F90_HELEC=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\helect_I.F90
NODEP_F90_HELECT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\helecz.F90
NODEP_F90_HELECZ=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\hmuf_I.F90
NODEP_F90_HMUF_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\hplusf_I.F90
NODEP_F90_HPLUS=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\hybrid.F90
NODEP_F90_HYBRI=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ijbo.F90
NODEP_F90_IJBO_=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\overlaps_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ijkl.F90
NODEP_F90_IJKL_=\
	".\Debug\ciint_I.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\partxy_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\inid_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\inighd_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\init_filenames.F90
NODEP_F90_INIT_=\
	".\Debug\chanel_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\initsn_I.F90
NODEP_F90_INITS=\
	".\Debug\cosmo_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\initsv_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\insymc_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\interp.F90
NODEP_F90_INTER=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\schmib_I.mod"\
	".\Debug\schmit_I.mod"\
	".\Debug\spline_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\interp_I.F90
NODEP_F90_INTERP=\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\intfc.F90
NODEP_F90_INTFC=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\jcarin_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\to_screen_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\intfc_I.F90
NODEP_F90_INTFC_=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ionout.F90
NODEP_F90_IONOU=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ird_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\isitsc.F90
NODEP_F90_ISITS=\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\iten_I.F90
NODEP_F90_ITEN_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\iter.F90
NODEP_F90_ITER_=\
	".\Debug\capcor_I.mod"\
	".\Debug\chanel_C.mod"\
	".\Debug\chrge_I.mod"\
	".\Debug\cnvg_I.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\density_cuda_i.mod"\
	".\Debug\fock2_I.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\helect_I.mod"\
	".\Debug\interp_I.mod"\
	".\Debug\iter_C.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\matout_I.mod"\
	".\Debug\meci_I.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\pulay_I.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\swap_I.mod"\
	".\Debug\timer_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\vecprt_I.mod"\
	".\Debug\writmo_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\iter_C.F90
NODEP_F90_ITER_C=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\iter_for_MOZYME.F90
NODEP_F90_ITER_F=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\iter_C.mod"\
	".\Debug\linear_cosmo.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\reada_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\iter_I.F90
NODEP_F90_ITER_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\jab.F90
NODEP_F90_JAB_F=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\jab_for_MOZYME.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\jab_I.F90
NODEP_F90_JAB_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\jcarin.F90
NODEP_F90_JCARI=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\gmetry_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\symtry_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\jcarin_I.F90
NODEP_F90_JCARIN=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\jdate.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\journal_references_C.F90
NODEP_F90_JOURN=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\kab.F90
NODEP_F90_KAB_F=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\kab_for_MOZYME.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\kab_I.F90
NODEP_F90_KAB_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\lbfgs.F90
NODEP_F90_LBFGS=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\ef_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\lewis.F90
NODEP_F90_LEWIS=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\mod_atomradii.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ligand.F90
NODEP_F90_LIGAN=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\linear_cosmo.F90
NODEP_F90_LINEA=\
	".\Debug\afmm_mod.mod"\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\overlaps_C.mod"\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\linmin.F90
NODEP_F90_LINMI=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\compfg_I.mod"\
	".\Debug\exchng_I.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\linmin_I.F90
NODEP_F90_LINMIN=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\linpack.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\local.F90
NODEP_F90_LOCAL=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\matout_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\resolv_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\local2.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\local_for_MOZYME.F90
NODEP_F90_LOCAL_=\
	".\Debug\chanel_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\locmin.F90
NODEP_F90_LOCMI=\
	".\Debug\chanel_C.mod"\
	".\Debug\compfg_I.mod"\
	".\Debug\exchng_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\locmin_I.F90
NODEP_F90_LOCMIN=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\lsame_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\lyse.F90
NODEP_F90_LYSE_=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\makeuf_I.F90
NODEP_F90_MAKEU=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\makopr_I.F90
NODEP_F90_MAKOP=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\maksym.F90
NODEP_F90_MAKSY=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\maksym_I.F90
NODEP_F90_MAKSYM=\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\makvec.F90
NODEP_F90_MAKVE=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mamult.F90
NODEP_F90_MAMUL=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mamult_cuda_i.F90
NODEP_F90_MAMULT=\
	".\Debug\iso_c_binding.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mamult_I.F90
NODEP_F90_MAMULT_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\maps_C.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mat33.F90
NODEP_F90_MAT33=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mat33_I.F90
NODEP_F90_MAT33_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\matou1.F90
NODEP_F90_MATOU=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\to_screen_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\matou1_I.F90
NODEP_F90_MATOU1=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\matout.F90
NODEP_F90_MATOUT=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\matout_I.F90
NODEP_F90_MATOUT_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mbonds.F90
NODEP_F90_MBOND=\
	".\Debug\chanel_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\me08a_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\meci.F90
NODEP_F90_MECI_=\
	".\Debug\chanel_C.mod"\
	".\Debug\ciosci_I.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\diagi_I.mod"\
	".\Debug\dmecip_I.mod"\
	".\Debug\matou1_I.mod"\
	".\Debug\matout_I.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\mecih_I.mod"\
	".\Debug\mndod_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\perm_I.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\symtrz_I.mod"\
	".\Debug\upcase_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\vecprt_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\meci_C.F90
NODEP_F90_MECI_C=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\meci_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mecid.F90
NODEP_F90_MECID=\
	".\Debug\diagi_I.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mecid_I.F90
NODEP_F90_MECID_=\
	".\Debug\meci_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mecih.F90
NODEP_F90_MECIH=\
	".\Debug\aababc_I.mod"\
	".\Debug\aabacd_I.mod"\
	".\Debug\aabbcd_I.mod"\
	".\Debug\babbbc_I.mod"\
	".\Debug\babbcd_I.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mecih_I.F90
NODEP_F90_MECIH_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mecip.F90
NODEP_F90_MECIP=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mxm_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mecip_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mepchg_I.F90
NODEP_F90_MEPCH=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mepmap_I.F90
NODEP_F90_MEPMA=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\meprot_I.F90
NODEP_F90_MEPRO=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\minv.F90
NODEP_F90_MINV_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\minv_I.F90
NODEP_F90_MINV_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mkl_bits.F90
NODEP_F90_MKL_B=\
	".\Debug\chanel_C.mod"\
	".\Debug\ddot_I.mod"\
	".\Debug\dger_I.mod"\
	".\Debug\dgetf2_I.mod"\
	".\Debug\dgetrs_I.mod"\
	".\Debug\dlaswp_I.mod"\
	".\Debug\dot_I.mod"\
	".\Debug\dscal_I.mod"\
	".\Debug\dtrmv_I.mod"\
	".\Debug\dtrsm_I.mod"\
	".\Debug\idamax_I.mod"\
	".\Debug\ieeeck_I.mod"\
	".\Debug\lsame_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\xerbla_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mlmo.F90
NODEP_F90_MLMO_=\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mndod.F90
NODEP_F90_MNDOD=\
	".\Debug\aijm_I.mod"\
	".\Debug\chanel_C.mod"\
	".\Debug\charg_I.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\ddpo_I.mod"\
	".\Debug\eiscor_I.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\inighd_I.mod"\
	".\Debug\mndod_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\poij_I.mod"\
	".\Debug\printp_I.mod"\
	".\Debug\reppd2_I.mod"\
	".\Debug\reppd_I.mod"\
	".\Debug\rijkl_I.mod"\
	".\Debug\rotmat_I.mod"\
	".\Debug\scprm_I.mod"\
	".\Debug\spcore_I.mod"\
	".\Debug\tx_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\w2mat_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mndod_C.F90
NODEP_F90_MNDOD_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mod_atomradii.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mod_calls_cublas.F90
NODEP_F90_MOD_C=\
	".\Debug\iso_c_binding.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mod_gpu_info.F90
NODEP_F90_MOD_G=\
	".\Debug\iso_c_binding.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mod_iso_c_binding.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mod_vars_cuda.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\modchg.F90
NODEP_F90_MODCH=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\modgra.F90
NODEP_F90_MODGR=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\moldat.F90
NODEP_F90_MOLDA=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\ef_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\gmetry_I.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\molmec_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\refer_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\symtrz_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\vecprt_I.mod"\
	".\Debug\volume_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\molkst_C.F90
NODEP_F90_MOLKS=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\molmec_C.F90
NODEP_F90_MOLME=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\molsym_I.F90
NODEP_F90_MOLSY=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\molsymy.F90
NODEP_F90_MOLSYM=\
	".\Debug\bangle_I.mod"\
	".\Debug\bldsym_I.mod"\
	".\Debug\cartab_I.mod"\
	".\Debug\chanel_C.mod"\
	".\Debug\chi_I.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\mult33_I.mod"\
	".\Debug\orient_I.mod"\
	".\Debug\plato_I.mod"\
	".\Debug\rotmol_I.mod"\
	".\Debug\symdec_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\symopr_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\molval.F90
NODEP_F90_MOLVA=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\molval_I.F90
NODEP_F90_MOLVAL=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mopac.F90
NODEP_F90_MOPAC=\
	".\Debug\chanel_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mopend.F90
NODEP_F90_MOPEN=\
	".\Debug\chanel_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mopend_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\MOZYME_C.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mpcbds_I.F90
NODEP_F90_MPCBD=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mpcpop_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mpcsyb.F90
NODEP_F90_MPCSY=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mtxm.F90
NODEP_F90_MTXM_=\
	".\Debug\call_gemm_cublas.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mtxm_I.F90
NODEP_F90_MTXM_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mtxmc.F90
NODEP_F90_MTXMC=\
	".\Debug\mxm_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mtxmc_I.F90
NODEP_F90_MTXMC_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mullik.F90
NODEP_F90_MULLI=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mult_I.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mullik_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mult.F90
NODEP_F90_MULT_=\
	".\Debug\call_gemm_cublas.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mult33.F90
NODEP_F90_MULT3=\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mult33_I.F90
NODEP_F90_MULT33=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mult_I.F90
NODEP_F90_MULT_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mult_symm_AB.F90
NODEP_F90_MULT_S=\
	".\Debug\call_gemm_cublas.mod"\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\iso_c_binding.mod"\
	".\Debug\mamult_cuda_i.mod"\
	".\Debug\mamult_I.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mult_symm_AB_I.F90
NODEP_F90_MULT_SY=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mxm.F90
NODEP_F90_MXM_F=\
	".\Debug\call_gemm_cublas.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mxm_I.F90
NODEP_F90_MXM_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mxmt.F90
NODEP_F90_MXMT_=\
	".\Debug\call_gemm_cublas.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mxmt_I.F90
NODEP_F90_MXMT_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mxv.F90
NODEP_F90_MXV_F=\
	".\Debug\call_gemv_cublas.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\mxv_I.F90
NODEP_F90_MXV_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\myword.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\myword_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\naican_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\naicas_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\names.F90
NODEP_F90_NAMES=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\new_esp.F90
NODEP_F90_NEW_E=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\esp_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\overlaps_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\newflg.F90
NODEP_F90_NEWFL=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ngamtg_I.F90
NODEP_F90_NGAMT=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ngefis_I.F90
NODEP_F90_NGEFI=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ngidri_I.F90
NODEP_F90_NGIDR=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ngoke_I.F90
NODEP_F90_NGOKE=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\nllsn_I.F90
NODEP_F90_NLLSN=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\nllsq.F90
NODEP_F90_NLLSQ=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\compfg_I.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\locmin_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\parsav_I.mod"\
	".\Debug\prttim_I.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\nllsq_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\nonbet_I.F90
NODEP_F90_NONBE=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\nonope_I.F90
NODEP_F90_NONOP=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\nonor_I.F90
NODEP_F90_NONOR=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\nuchar.F90
NODEP_F90_NUCHA=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\nuchar_I.F90
NODEP_F90_NUCHAR=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\nxtmer.F90
NODEP_F90_NXTME=\
	".\Debug\common_arrays_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\openda_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\orient_I.F90
NODEP_F90_ORIEN=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\osinv.F90
NODEP_F90_OSINV=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\osinv_I.F90
NODEP_F90_OSINV_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\outer1.F90
NODEP_F90_OUTER=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\outer2.F90
NODEP_F90_OUTER2=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\overlaps_C.F90
NODEP_F90_OVERL=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ovlp_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\packp_I.F90
NODEP_F90_PACKP=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parameters_C.F90
NODEP_F90_PARAM=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parameters_for_AM1_C.F90
NODEP_F90_PARAME=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parameters_for_AM1_Sparkles_C.F90
NODEP_F90_PARAMET=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parameters_for_mndo_C.F90
NODEP_F90_PARAMETE=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parameters_for_mndod_C.F90
NODEP_F90_PARAMETER=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parameters_for_PM3_C.F90
NODEP_F90_PARAMETERS=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parameters_for_PM3_Sparkles_C.F90
NODEP_F90_PARAMETERS_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parameters_for_PM6_C.F90
NODEP_F90_PARAMETERS_F=\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parameters_for_PM6_Sparkles_C.F90
NODEP_F90_PARAMETERS_FO=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parameters_for_PM7_C.F90
NODEP_F90_PARAMETERS_FOR=\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parameters_for_PM7_Sparkles_C.F90
NODEP_F90_PARAMETERS_FOR_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parameters_for_PM7_TS_C.F90
NODEP_F90_PARAMETERS_FOR_P=\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parameters_for_RM1_C.F90
NODEP_F90_PARAMETERS_FOR_R=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parameters_for_RM1_Sparkles_C.F90
NODEP_F90_PARAMETERS_FOR_RM=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parsav.F90
NODEP_F90_PARSA=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\parsav_I.F90
NODEP_F90_PARSAV=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\partxy.F90
NODEP_F90_PARTX=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\partxy_I.F90
NODEP_F90_PARTXY=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\Password.F90
NODEP_F90_PASSW=\
	".\Debug\molkst_C.mod"\
	".\Debug\reada_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\pathk.F90
NODEP_F90_PATHK=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\dfpsav_I.mod"\
	".\Debug\ef_I.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\wrttxt_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\pathk_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\paths.F90
NODEP_F90_PATHS=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\dfpsav_I.mod"\
	".\Debug\ef_C.mod"\
	".\Debug\ef_I.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\flepo_I.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\writmo_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\paths_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\pdbout.F90
NODEP_F90_PDBOU=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\pdgrid_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\perm.F90
NODEP_F90_PERM_=\
	".\Debug\meci_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\perm_I.F90
NODEP_F90_PERM_I=\
	".\Debug\meci_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\picopt.F90
NODEP_F90_PICOP=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\symmetry_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\pinout.F90
NODEP_F90_PINOU=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\plato_I.F90
NODEP_F90_PLATO=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\pmep.F90
NODEP_F90_PMEP_=\
	".\Debug\chanel_C.mod"\
	".\Debug\collis_I.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\drepp2_I.mod"\
	".\Debug\drotat_I.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\genvec_I.mod"\
	".\Debug\gmetry_I.mod"\
	".\Debug\grids_I.mod"\
	".\Debug\mepchg_I.mod"\
	".\Debug\mepmap_I.mod"\
	".\Debug\meprot_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\osinv_I.mod"\
	".\Debug\packp_I.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\pmepco_I.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\rotate_C.mod"\
	".\Debug\second_I.mod"\
	".\Debug\surfa_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\pmep1_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\pmep_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\pmepco_I.F90
NODEP_F90_PMEPC=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\poij_I.F90
NODEP_F90_POIJ_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\pol_vol_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\polar.F90
NODEP_F90_POLAR=\
	".\Debug\alphaf_I.mod"\
	".\Debug\aval_I.mod"\
	".\Debug\axis_I.mod"\
	".\Debug\bdenin_I.mod"\
	".\Debug\bdenup_I.mod"\
	".\Debug\beopor_I.mod"\
	".\Debug\betaf_I.mod"\
	".\Debug\betal1_I.mod"\
	".\Debug\betall_I.mod"\
	".\Debug\betcom_I.mod"\
	".\Debug\bmakuf_I.mod"\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\compfg_I.mod"\
	".\Debug\copym_I.mod"\
	".\Debug\darea1_I.mod"\
	".\Debug\daread_I.mod"\
	".\Debug\dawrit_I.mod"\
	".\Debug\dawrt1_I.mod"\
	".\Debug\densf_I.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\epsab_I.mod"\
	".\Debug\ffreq1_I.mod"\
	".\Debug\ffreq2_I.mod"\
	".\Debug\fhpatn_I.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\gmetry_I.mod"\
	".\Debug\hmuf_I.mod"\
	".\Debug\hplusf_I.mod"\
	".\Debug\makeuf_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\ngamtg_I.mod"\
	".\Debug\ngefis_I.mod"\
	".\Debug\ngidri_I.mod"\
	".\Debug\ngoke_I.mod"\
	".\Debug\nonbet_I.mod"\
	".\Debug\nonope_I.mod"\
	".\Debug\nonor_I.mod"\
	".\Debug\openda_I.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\pol_vol_I.mod"\
	".\Debug\polar_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\tf_I.mod"\
	".\Debug\transf_I.mod"\
	".\Debug\trsub_I.mod"\
	".\Debug\trudgu_I.mod"\
	".\Debug\trugdu_I.mod"\
	".\Debug\trugud_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\wrdkey_I.mod"\
	".\Debug\zerom_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\polar_C.F90
NODEP_F90_POLAR_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\polar_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\potcal_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\powsav_I.F90
NODEP_F90_POWSA=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\powsq.F90
NODEP_F90_POWSQ=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\compfg_I.mod"\
	".\Debug\ef_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\prttim_I.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\search_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\vecprt_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\powsq_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\printp_I.F90
NODEP_F90_PRINT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\prtdrc.F90
NODEP_F90_PRTDR=\
	".\Debug\chanel_C.mod"\
	".\Debug\chrge_I.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\dot_I.mod"\
	".\Debug\drc_C.mod"\
	".\Debug\drcout_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\quadr_I.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\xyzint_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\prtdrc_I.F90
NODEP_F90_PRTDRC=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\prtgra.F90
NODEP_F90_PRTGR=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\prthco_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\prthes_I.F90
NODEP_F90_PRTHE=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\prtlmo.F90
NODEP_F90_PRTLM=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\prtpar_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\prtpka.F90
NODEP_F90_PRTPK=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\linear_cosmo.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\prttim.F90
NODEP_F90_PRTTI=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\prttim_I.F90
NODEP_F90_PRTTIM=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\pulay.F90
NODEP_F90_PULAY=\
	".\Debug\chanel_C.mod"\
	".\Debug\mamult_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\osinv_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\pulay_for_gpu.F90
NODEP_F90_PULAY_=\
	".\Debug\chanel_C.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mult_symm_ab_I.mod"\
	".\Debug\osinv_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\pulay_for_gpu_I.F90
NODEP_F90_PULAY_F=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\pulay_I.F90
NODEP_F90_PULAY_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\quadr.F90
NODEP_F90_QUADR=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\quadr_I.F90
NODEP_F90_QUADR_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\QuickWin_bits.F90
NODEP_F90_QUICK=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\react1.F90
NODEP_F90_REACT=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\compfg_I.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\getgeo_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\writmo_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\react1_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\reada.F90
NODEP_F90_READA=\
	".\Debug\digit_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\reada_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\readmo.F90
NODEP_F90_READM=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\conref_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\getgeg_I.mod"\
	".\Debug\getgeo_I.mod"\
	".\Debug\getsym_I.mod"\
	".\Debug\gettxt_I.mod"\
	".\Debug\gmetry_I.mod"\
	".\Debug\maksym_I.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\nuchar_I.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\symtry_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\wrtkey_I.mod"\
	".\Debug\wrttxt_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\readmo_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\redatm_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\refer.F90
NODEP_F90_REFER=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\journal_references_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\parameters_for_AM1_Sparkles_C.mod"\
	".\Debug\parameters_for_PM3_Sparkles_C.mod"\
	".\Debug\parameters_for_PM6_Sparkles_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\refer_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\refkey_C.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\reorth.F90
NODEP_F90_REORT=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\repp_I.F90
NODEP_F90_REPP_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\reppd2_I.F90
NODEP_F90_REPPD=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\reppd_I.F90
NODEP_F90_REPPD_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\reseq.F90
NODEP_F90_RESEQ=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\resolv.F90
NODEP_F90_RESOL=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\resolv_I.F90
NODEP_F90_RESOLV=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\rijkl_I.F90
NODEP_F90_RIJKL=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\rotat_I.F90
NODEP_F90_ROTAT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\rotatd_I.F90
NODEP_F90_ROTATD=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\rotate.F90
NODEP_F90_ROTATE=\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\rotate_C.F90
NODEP_F90_ROTATE_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\rotate_I.F90
NODEP_F90_ROTATE_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\rotlmo.F90
NODEP_F90_ROTLM=\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\rotmat_I.F90
NODEP_F90_ROTMA=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\rotmol.F90
NODEP_F90_ROTMO=\
	".\Debug\symopr_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\rotmol_I.F90
NODEP_F90_ROTMOL=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\rsc_I.F90
NODEP_F90_RSC_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\rsp.F90
NODEP_F90_RSP_F=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\run_mopac.F90
NODEP_F90_RUN_M=\
	".\Debug\calpar_I.mod"\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\compfg_I.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\datin_I.mod"\
	".\Debug\drc_I.mod"\
	".\Debug\ef_I.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\fbx_I.mod"\
	".\Debug\flepo_I.mod"\
	".\Debug\force_I.mod"\
	".\Debug\fordd_I.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\geoutg_I.mod"\
	".\Debug\gpu_info.mod"\
	".\Debug\grid_I.mod"\
	".\Debug\iso_c_binding.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\mod_vars_cuda.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\nllsq_I.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\parameters_for_PM6_Sparkles_C.mod"\
	".\Debug\Parameters_for_PM7_Sparkles_C.mod"\
	".\Debug\pathk_I.mod"\
	".\Debug\paths_I.mod"\
	".\Debug\pmep_I.mod"\
	".\Debug\polar_I.mod"\
	".\Debug\powsq_I.mod"\
	".\Debug\react1_I.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\settingGPUcard.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\upcase_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\writmo_I.mod"\
	".\Debug\wrttxt_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\scfcri.F90
NODEP_F90_SCFCR=\
	".\Debug\chanel_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\reada_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\schmib.F90
NODEP_F90_SCHMI=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\schmib_I.F90
NODEP_F90_SCHMIB=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\schmit.F90
NODEP_F90_SCHMIT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\schmit_I.F90
NODEP_F90_SCHMIT_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\scprm_I.F90
NODEP_F90_SCPRM=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\search_I.F90
NODEP_F90_SEARC=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\second.F90
NODEP_F90_SECON=\
	".\Debug\chanel_C.mod"\
	".\Debug\geout_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\second_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\selmos.F90
NODEP_F90_SELMO=\
	".\Debug\chanel_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\set.F90
NODEP_F90_SET_F=\
	".\Debug\aintgs_I.mod"\
	".\Debug\bintgs_I.mod"\
	".\Debug\overlaps_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\set_I.F90
NODEP_F90_SET_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\set_up_dentate.F90
NODEP_F90_SET_U=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\mod_atomradii.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\set_up_MOZYME_arrays.F90
NODEP_F90_SET_UP=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\iter_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\set_up_RAPID.F90
NODEP_F90_SET_UP_=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\setup3_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\setup_mopac_arrays.F90
NODEP_F90_SETUP=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\derivs_C.mod"\
	".\Debug\drc_C.mod"\
	".\Debug\ef_C.mod"\
	".\Debug\esp_C.mod"\
	".\Debug\iter_C.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\to_screen_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\setupg.F90
NODEP_F90_SETUPG=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\overlaps_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\setupk.F90
NODEP_F90_SETUPK=\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\solrot.F90
NODEP_F90_SOLRO=\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\rotate_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\solrot_I.F90
NODEP_F90_SOLROT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\sort.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\sort_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\sp_two_electron.F90
NODEP_F90_SP_TW=\
	".\Debug\mndod_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\spcore_I.F90
NODEP_F90_SPCOR=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\spline_I.F90
NODEP_F90_SPLIN=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\ss_I.F90
NODEP_F90_SS_I_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\static_polarizability.F90
NODEP_F90_STATI=\
	".\Debug\axis_I.mod"\
	".\Debug\chanel_C.mod"\
	".\Debug\chrge_I.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\compfg_I.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\gmetry_I.mod"\
	".\Debug\matout_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\pol_vol_I.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\vecprt_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\suma2_I.F90
NODEP_F90_SUMA2=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\supdot.F90
NODEP_F90_SUPDO=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\supdot_I.F90
NODEP_F90_SUPDOT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\superd.F90
NODEP_F90_SUPER=\
	".\Debug\chanel_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\superd_I.F90
NODEP_F90_SUPERD=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\surfa_I.F90
NODEP_F90_SURFA=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\surfac_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\swap.F90
NODEP_F90_SWAP_=\
	".\Debug\chanel_C.mod"\
	".\Debug\iter_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\swap_I.F90
NODEP_F90_SWAP_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\switch.F90
NODEP_F90_SWITC=\
	".\Debug\chanel_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\parameters_for_AM1_C.mod"\
	".\Debug\parameters_for_AM1_Sparkles_C.mod"\
	".\Debug\parameters_for_mndo_C.mod"\
	".\Debug\parameters_for_mndod_C.mod"\
	".\Debug\parameters_for_PM3_C.mod"\
	".\Debug\parameters_for_PM3_Sparkles_C.mod"\
	".\Debug\parameters_for_PM6_C.mod"\
	".\Debug\parameters_for_PM6_Sparkles_C.mod"\
	".\Debug\parameters_for_PM7_C.mod"\
	".\Debug\Parameters_for_PM7_Sparkles_C.mod"\
	".\Debug\parameters_for_PM7_TS_C.mod"\
	".\Debug\parameters_for_RM1_C.mod"\
	".\Debug\Parameters_for_RM1_Sparkles_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symdec_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symh.F90
NODEP_F90_SYMH_=\
	".\Debug\mat33_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symh_I.F90
NODEP_F90_SYMH_I=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symmetry_C.F90
NODEP_F90_SYMME=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symoir.F90
NODEP_F90_SYMOI=\
	".\Debug\chanel_C.mod"\
	".\Debug\charmo_I.mod"\
	".\Debug\charst_I.mod"\
	".\Debug\charvi_I.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symoir_I.F90
NODEP_F90_SYMOIR=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symopr.F90
NODEP_F90_SYMOP=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symopr_I.F90
NODEP_F90_SYMOPR=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symp_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\sympop.F90
NODEP_F90_SYMPO=\
	".\Debug\symh_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\sympop_I.F90
NODEP_F90_SYMPOP=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symr.F90
NODEP_F90_SYMR_=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\symp_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symr_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symt.F90
NODEP_F90_SYMT_=\
	".\Debug\mat33_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symt_I.F90
NODEP_F90_SYMT_I=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symtry.F90
NODEP_F90_SYMTR=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\haddon_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\mopend_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symtry_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symtrz.F90
NODEP_F90_SYMTRZ=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\makopr_I.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\molsym_I.mod"\
	".\Debug\mult33_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\symtrz_I.F90
NODEP_F90_SYMTRZ_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\tf_I.F90
NODEP_F90_TF_I_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\thermo.F90
NODEP_F90_THERM=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\thermo_I.F90
NODEP_F90_THERMO=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\tidy.F90
NODEP_F90_TIDY_=\
	".\Debug\chanel_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\time_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\timer.F90
NODEP_F90_TIMER=\
	".\Debug\chanel_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\second_I.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\timer_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\timout.F90
NODEP_F90_TIMOU=\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\to_screen.F90
NODEP_F90_TO_SC=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\drc_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\polar_C.mod"\
	".\Debug\reada_I.mod"\
	".\Debug\second_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\to_screen_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\to_screen_C.F90
NODEP_F90_TO_SCR=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\transf_I.F90
NODEP_F90_TRANS=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\trsub_I.F90
NODEP_F90_TRSUB=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\trudgu_I.F90
NODEP_F90_TRUDG=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\trugdu_I.F90
NODEP_F90_TRUGD=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\trugud_I.F90
NODEP_F90_TRUGU=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\tx_I.F90
NODEP_F90_TX_I_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\txtype.F90
NODEP_F90_TXTYP=\
	".\Debug\common_arrays_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\upcase.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\upcase_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\update.F90
NODEP_F90_UPDAT=\
	".\Debug\chanel_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\update_I.F90
NODEP_F90_UPDATE=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\values.F90
NODEP_F90_VALUE=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\vastkind.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\vecprt.F90
NODEP_F90_VECPR=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\vecprt_for_MOZYME.F90
NODEP_F90_VECPRT=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\vecprt_I.F90
NODEP_F90_VECPRT_=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\volume.F90
NODEP_F90_VOLUM=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\volume_I.F90
NODEP_F90_VOLUME=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\w2mat_I.F90
NODEP_F90_W2MAT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\worder_I.F90
NODEP_F90_WORDE=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\wrdkey_I.F90
NODEP_F90_WRDKE=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\write_trajectory.F90
NODEP_F90_WRITE=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\write_trajectory_I.F90
NODEP_F90_WRITE_=\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\writmo.F90
NODEP_F90_WRITM=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\conref_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\dipole_I.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\geoutg_I.mod"\
	".\Debug\linear_cosmo.mod"\
	".\Debug\maps_C.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\meci_I.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\MOZYME_C.mod"\
	".\Debug\parameters_C.mod"\
	".\Debug\second_I.mod"\
	".\Debug\symmetry_C.mod"\
	".\Debug\vast_kind_param.mod"\
	".\Debug\volume_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\writmo_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\wrt_diffs.F90
NODEP_F90_WRT_D=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\elemts_C.mod"\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\wrtkey.F90
NODEP_F90_WRTKE=\
	".\Debug\chanel_C.mod"\
	".\Debug\common_arrays_C.mod"\
	".\Debug\conref_C.mod"\
	".\Debug\cosmo_C.mod"\
	".\Debug\meci_C.mod"\
	".\Debug\molkst_C.mod"\
	".\Debug\myword_I.mod"\
	".\Debug\reada_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\wrtkey_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\wrttxt.F90
NODEP_F90_WRTTX=\
	".\Debug\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\wrttxt_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\wstore_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\wwstep_I.F90
NODEP_F90_WWSTE=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\xerbla_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\xxx.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\xxx_I.F90
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\xyzcry.F90
NODEP_F90_XYZCR=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\xyzcry_I.F90
NODEP_F90_XYZCRY=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\xyzint.F90
NODEP_F90_XYZIN=\
	".\Debug\bangle_I.mod"\
	".\Debug\dihed_I.mod"\
	".\Debug\funcon_C.mod"\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\xyzint_I.F90
NODEP_F90_XYZINT=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC_source_code\zerom_I.F90
NODEP_F90_ZEROM=\
	".\Debug\vast_kind_param.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
