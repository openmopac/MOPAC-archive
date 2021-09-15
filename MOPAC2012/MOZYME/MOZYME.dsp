# Microsoft Developer Studio Project File - Name="MOZYME" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=MOZYME - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "MOZYME.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "MOZYME.mak" CFG="MOZYME - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "MOZYME - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "MOZYME - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "MOZYME - Win32 Release"

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
# ADD F90 /compile_only /include:"..\Modules\Release" /include:"..\Interfaces\Release" /include:"..\Subroutines\Release" /nologo /warn:nofileopt
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

!ELSEIF  "$(CFG)" == "MOZYME - Win32 Debug"

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
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /include:"..\Modules\Debug" /include:"..\Interfaces\Debug" /include:"..\Subroutines\Debug" /nologo /traceback /warn:argument_checking /warn:nofileopt
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

# Name "MOZYME - Win32 Release"
# Name "MOZYME - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\src_MOZYME\add_more_interactions.F90
DEP_F90_ADD_M=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\iter_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\addhb.F90
DEP_F90_ADDHB=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\adjvec.F90
DEP_F90_ADJVE=\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\atomrs.F90
DEP_F90_ATOMR=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\bonds_for_MOZYME.F90
DEP_F90_BONDS=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\buildf.F90
DEP_F90_BUILD=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\check.F90
DEP_F90_CHECK=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\chkion.F90
DEP_F90_CHKIO=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\chklew.F90
DEP_F90_CHKLE=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\chrge_for_MOZYME.F90
DEP_F90_CHRGE=\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\cnvgz.F90
DEP_F90_CNVGZ=\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\compct.F90
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\convert_storage.F90
DEP_F90_CONVE=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\dcart_for_MOZYME.F90
DEP_F90_DCART=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\delsta.F90
DEP_F90_DELST=\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\denrot_for_MOZYME.F90
DEP_F90_DENRO=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\density_for_MOZYME.F90
DEP_F90_DENSI=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\diagg.F90
DEP_F90_DIAGG=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\diagg1.F90
DEP_F90_DIAGG1=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\diagg2.F90
DEP_F90_DIAGG2=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\dipole_for_MOZYME.F90
DEP_F90_DIPOL=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\eigen.F90
DEP_F90_EIGEN=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\eimp.F90
DEP_F90_EIMP_=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\epseta.F90
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\fillij.F90
DEP_F90_FILLI=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\overlaps_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\findn1.F90
DEP_F90_FINDN=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\fock1_for_MOZYME.F90
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\fock2z.F90
DEP_F90_FOCK2=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Subroutines\Release\linear_cosmo.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\geochk.F90
DEP_F90_GEOCH=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\wrdkey_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\mod_atomradii.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\getpdb.F90
DEP_F90_GETPD=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\greek.F90
DEP_F90_GREEK=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\hbonds.F90
DEP_F90_HBOND=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\hcore_for_MOZYME.F90
DEP_F90_HCORE=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\overlaps_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Subroutines\Release\linear_cosmo.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\helecz.F90
DEP_F90_HELEC=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\hybrid.F90
DEP_F90_HYBRI=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\ijbo.F90
DEP_F90_IJBO_=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\overlaps_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\isitsc.F90
DEP_F90_ISITS=\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\iter_for_MOZYME.F90
DEP_F90_ITER_=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\to_screen_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\iter_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Subroutines\Release\linear_cosmo.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\jab_for_MOZYME.F90
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\kab_for_MOZYME.F90
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\lbfgs.F90
DEP_F90_LBFGS=\
	"..\Interfaces\Release\ddot_I.mod"\
	"..\Interfaces\Release\dot_I.mod"\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Interfaces\Release\to_screen_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\ef_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\lewis.F90
DEP_F90_LEWIS=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\ligand.F90
DEP_F90_LIGAN=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\local2.F90
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\local_for_MOZYME.F90
DEP_F90_LOCAL=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\lyse.F90
DEP_F90_LYSE_=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\makvec.F90
DEP_F90_MAKVE=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\mbonds.F90
DEP_F90_MBOND=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\mlmo.F90
DEP_F90_MLMO_=\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\modchg.F90
DEP_F90_MODCH=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\modgra.F90
DEP_F90_MODGR=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\names.F90
DEP_F90_NAMES=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\newflg.F90
DEP_F90_NEWFL=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\nxtmer.F90
DEP_F90_NXTME=\
	"..\Modules\Release\common_arrays_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\outer1.F90
DEP_F90_OUTER=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\outer2.F90
DEP_F90_OUTER2=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\pdbout.F90
DEP_F90_PDBOU=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\picopt.F90
DEP_F90_PICOP=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\pinout.F90
DEP_F90_PINOU=\
	"..\Interfaces\Release\to_screen_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\prtgra.F90
DEP_F90_PRTGR=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\prtlmo.F90
DEP_F90_PRTLM=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\reorth.F90
DEP_F90_REORT=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\reseq.F90
DEP_F90_RESEQ=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\rotlmo.F90
DEP_F90_ROTLM=\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\scfcri.F90
DEP_F90_SCFCR=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\selmos.F90
DEP_F90_SELMO=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\set_up_MOZYME_arrays.F90
DEP_F90_SET_U=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\iter_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\set_up_RAPID.F90
DEP_F90_SET_UP=\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\setupk.F90
DEP_F90_SETUP=\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\tidy.F90
DEP_F90_TIDY_=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\txtype.F90
DEP_F90_TXTYP=\
	"..\Modules\Release\common_arrays_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\values.F90
DEP_F90_VALUE=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_MOZYME\vecprt_for_MOZYME.F90
DEP_F90_VECPR=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# End Target
# End Project
