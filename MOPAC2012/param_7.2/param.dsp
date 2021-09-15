# Microsoft Developer Studio Project File - Name="param" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=param - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "param.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "param.mak" CFG="param - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "param - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "param - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "param - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"..\MOPAC2012\Interfaces\Release" /include:"..\mopac2012\Modules\Release" /nologo /traceback /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /base:"0x1600000" /stack:0x5b8d80 /subsystem:console /pdb:"Release/param_F90.pdb" /machine:I386 /out:"M:\utility\param_7.2.exe" /pdbtype:sept /heap:80000000,8000000
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "param - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /include:"..\mopac2012\Modules\Debug" /include:"..\MOPAC2012\Interfaces\Debug" /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /stack:0x8000000 /subsystem:console /debug /machine:I386 /out:"M:\Software\param_7.2\Debug\param7.2.exe" /pdbtype:sept
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "param - Win32 Release"
# Name "param - Win32 Debug"
# Begin Source File

SOURCE=.\src\chkpar.F90
DEP_F90_CHKPA=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_CHKPA=\
	".\Release\elemts_C.mod"\
	".\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\datinp.F90
DEP_F90_DATIN=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_DATIN=\
	".\Release\chanel_C.mod"\
	".\Release\common_arrays_C.mod"\
	".\Release\elemts_C.mod"\
	".\Release\gmetry_I.mod"\
	".\Release\meci_C.mod"\
	".\Release\molkst_C.mod"\
	".\Release\molmec_C.mod"\
	".\Release\parameters_C.mod"\
	".\Release\reada_I.mod"\
	".\Release\second_I.mod"\
	".\Release\symmetry_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\depfn.F90
DEP_F90_DEPFN=\
	".\Release\param_global_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\direct.F90
DEP_F90_DIREC=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_DIREC=\
	".\Release\chanel_C.mod"\
	".\Release\common_arrays_C.mod"\
	".\Release\cosmo_C.mod"\
	".\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\empire.F90
NODEP_F90_EMPIR=\
	".\Release\common_arrays_C.mod"\
	".\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\extract_parameter.F90
NODEP_F90_EXTRA=\
	".\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\filusp.F90
NODEP_F90_FILUS=\
	".\Release\molkst_C.mod"\
	".\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\getmol.F90
DEP_F90_GETMO=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_GETMO=\
	".\Release\common_arrays_C.mod"\
	".\Release\funcon_C.mod"\
	".\Release\molkst_C.mod"\
	".\Release\parameters_C.mod"\
	".\Release\symmetry_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\getpar.F90
DEP_F90_GETPA=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_GETPA=\
	".\Release\chanel_C.mod"\
	".\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\invfn.f90
# End Source File
# Begin Source File

SOURCE=.\src\optgeo.F90
DEP_F90_OPTGE=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_OPTGE=\
	".\Release\common_arrays_C.mod"\
	".\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\param.F90
DEP_F90_PARAM=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_PARAM=\
	".\Release\chanel_C.mod"\
	".\Release\common_arrays_C.mod"\
	".\Release\conref_C.mod"\
	".\Release\cosmo_C.mod"\
	".\Release\funcon_C.mod"\
	".\Release\journal_references_C.mod"\
	".\Release\meci_C.mod"\
	".\Release\molkst_C.mod"\
	".\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\Param_global_C.F90
# End Source File
# Begin Source File

SOURCE=.\src\pardip.F90
NODEP_F90_PARDI=\
	".\Release\common_arrays_C.mod"\
	".\Release\molkst_C.mod"\
	".\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\parfg.F90
DEP_F90_PARFG=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_PARFG=\
	".\Release\common_arrays_C.mod"\
	".\Release\cosmo_C.mod"\
	".\Release\funcon_C.mod"\
	".\Release\meci_C.mod"\
	".\Release\meci_I.mod"\
	".\Release\molkst_C.mod"\
	".\Release\parameters_C.mod"\
	".\Release\symmetry_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\pargeo.F90
DEP_F90_PARGE=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_PARGE=\
	".\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\parips.F90
DEP_F90_PARIP=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_PARIP=\
	".\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\parkey.F90
DEP_F90_PARKE=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_PARKE=\
	".\Release\reada_I.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\partab.F90
DEP_F90_PARTA=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_PARTA=\
	".\Release\chanel_C.mod"\
	".\Release\common_arrays_C.mod"\
	".\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\pparsav.f90
DEP_F90_PPARS=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_PPARS=\
	".\Release\chanel_C.mod"\
	".\Release\elemts_C.mod"\
	".\Release\molkst_C.mod"\
	".\Release\parameters_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\psort.F90
DEP_F90_PSORT=\
	".\Release\param_global_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\rapid0.F90
DEP_F90_RAPID=\
	".\Release\param_global_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\rapid1.F90
DEP_F90_RAPID1=\
	".\Release\param_global_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\rapid2.F90
DEP_F90_RAPID2=\
	".\Release\param_global_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\rapid3.F90
DEP_F90_RAPID3=\
	".\Release\param_global_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\resetp.F90
DEP_F90_RESET=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_RESET=\
	".\Release\common_arrays_C.mod"\
	".\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\savgeo.F90
DEP_F90_SAVGE=\
	".\Release\param_global_C.mod"\
	
NODEP_F90_SAVGE=\
	".\Release\chanel_C.mod"\
	".\Release\common_arrays_C.mod"\
	".\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\mopac2012\MOPAC\to_screen.F90
DEP_F90_TO_SC=\
	"..\mopac2012\Interfaces\Release\reada_I.mod"\
	"..\mopac2012\Interfaces\Release\second_I.mod"\
	"..\mopac2012\Modules\Release\chanel_C.mod"\
	"..\mopac2012\Modules\Release\common_arrays_C.mod"\
	"..\mopac2012\Modules\Release\cosmo_C.mod"\
	"..\mopac2012\Modules\Release\elemts_C.mod"\
	"..\mopac2012\Modules\Release\funcon_C.mod"\
	"..\mopac2012\Modules\Release\maps_C.mod"\
	"..\mopac2012\Modules\Release\meci_C.mod"\
	"..\mopac2012\Modules\Release\molkst_C.mod"\
	"..\mopac2012\Modules\Release\MOZYME_C.mod"\
	"..\mopac2012\Modules\Release\parameters_C.mod"\
	"..\mopac2012\Modules\Release\polar_C.mod"\
	"..\mopac2012\Modules\Release\symmetry_C.mod"\
	"..\mopac2012\Modules\Release\to_screen_C.mod"\
	
# End Source File
# End Target
# End Project
