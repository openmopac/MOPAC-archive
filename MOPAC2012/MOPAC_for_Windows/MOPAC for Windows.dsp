# Microsoft Developer Studio Project File - Name="MOPAC for Windows" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=MOPAC for Windows - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "MOPAC for Windows.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "MOPAC for Windows.mak" CFG="MOPAC for Windows - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "MOPAC for Windows - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "MOPAC for Windows - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "MOPAC for Windows - Win32 Release"

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
# ADD F90 /compile_only /define:"MOPAC_FOR_WINDOWS" /fpp /include:"..\Modules\Release" /include:"..\Interfaces\Release" /libs:qwin /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /stack:0x800000 /entry:"WinMainCRTStartup" /subsystem:windows /machine:I386 /nodefaultlib:"dfconsol.lib" /nodefaultlib:"libc.lib" /out:"M:/utility/MOPAC2012.exe"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
SOURCE="$(InputPath)"
PreLink_Cmds=M:\utility\date_stamp.cmd
# End Special Build Tool

!ELSEIF  "$(CFG)" == "MOPAC for Windows - Win32 Debug"

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
# ADD F90 /compile_only /debug:full /define:"MOPAC_FOR_WINDOWS" /fpp /include:"..\Modules\Debug" /include:"..\Interfaces\Debug" /libs:qwin /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /stack:0x800000 /entry:"WinMainCRTStartup" /subsystem:windows /debug /machine:I386 /nodefaultlib:"dfconsol.lib" /nodefaultlib:"libcd.lib" /out:"M:\Software\mopac\MOPAC_for_Windows\Debug\MOPAC_for_Windows.exe" /pdbtype:sept
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "MOPAC for Windows - Win32 Release"
# Name "MOPAC for Windows - Win32 Debug"
# Begin Source File

SOURCE=.\icon1.ico
# End Source File
# Begin Source File

SOURCE=".\MOPAC for Windows.RC"
# End Source File
# Begin Source File

SOURCE=".\MOPAC for Windows_ref.F90"
DEP_F90_MOPAC=\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\src_subroutines\Password.F90
DEP_F90_PASSW=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Modules\Release\molkst_C.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MOPAC\to_screen.f90
DEP_F90_TO_SC=\
	"..\Interfaces\Release\reada_I.mod"\
	"..\Interfaces\Release\second_I.mod"\
	"..\Modules\Release\chanel_C.mod"\
	"..\Modules\Release\common_arrays_C.mod"\
	"..\Modules\Release\cosmo_C.mod"\
	"..\Modules\Release\elemts_C.mod"\
	"..\Modules\Release\funcon_C.mod"\
	"..\Modules\Release\maps_C.mod"\
	"..\Modules\Release\meci_C.mod"\
	"..\Modules\Release\molkst_C.mod"\
	"..\Modules\Release\MOZYME_C.mod"\
	"..\Modules\Release\parameters_C.mod"\
	"..\Modules\Release\polar_C.mod"\
	"..\Modules\Release\symmetry_C.mod"\
	"..\Modules\Release\to_screen_C.mod"\
	
# End Source File
# End Target
# End Project
