# Microsoft Developer Studio Generated NMAKE File, Based on mopac.dsp
!IF "$(CFG)" == ""
CFG=mopac - Win32 Debug
!MESSAGE No configuration specified. Defaulting to mopac - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "mopac - Win32 Release" && "$(CFG)" != "mopac - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mopac.mak" CFG="mopac - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mopac - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "mopac - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "mopac - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release

!IF "$(RECURSE)" == "0" 

ALL : "..\..\..\utility\NOT_XP\MOPAC2009.exe"

!ELSE 

ALL : "MOZYME - Win32 Release" "Modules - Win32 Release" "Interfaces - Win32 Release" "Subroutines - Win32 Release" "..\..\..\utility\NOT_XP\MOPAC2009.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Subroutines - Win32 ReleaseCLEAN" "Interfaces - Win32 ReleaseCLEAN" "Modules - Win32 ReleaseCLEAN" "MOZYME - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\mopac.obj"
	-@erase "$(INTDIR)\to_screen.obj"
	-@erase "..\..\..\utility\NOT_XP\MOPAC2009.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=df.exe
F90_PROJ=/compile_only /include:"..\Modules\Release" /include:"..\Interfaces\Release" /nologo /warn:nofileopt /module:"Release/" /object:"Release/" 
F90_OBJS=.\Release/

.SUFFIXES: .fpp

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.fpp{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

CPP=cl.exe
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\mopac.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mopac.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /stack:0x800000 /subsystem:console /incremental:no /pdb:"$(OUTDIR)\MOPAC2009.pdb" /machine:I386 /out:"M:/utility/NOT_XP/MOPAC2009.exe" 
LINK32_OBJS= \
	"$(INTDIR)\mopac.obj" \
	"$(INTDIR)\to_screen.obj" \
	"..\Subroutines\Release\Subroutines.lib" \
	"..\Interfaces\Release\Interfaces.lib" \
	"..\Modules\Release\Modules.lib" \
	"..\MOZYME\Release\MOZYME.lib"

"..\..\..\utility\NOT_XP\MOPAC2009.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
   M:\utility\date_stamp.cmd
	 $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE="$(InputPath)"

!ELSEIF  "$(CFG)" == "mopac - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\mopac.exe"

!ELSE 

ALL : "MOZYME - Win32 Debug" "Modules - Win32 Debug" "Interfaces - Win32 Debug" "Subroutines - Win32 Debug" "$(OUTDIR)\mopac.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Subroutines - Win32 DebugCLEAN" "Interfaces - Win32 DebugCLEAN" "Modules - Win32 DebugCLEAN" "MOZYME - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\DF60.PDB"
	-@erase "$(INTDIR)\mopac.obj"
	-@erase "$(INTDIR)\to_screen.obj"
	-@erase "$(OUTDIR)\mopac.exe"
	-@erase "$(OUTDIR)\mopac.ilk"
	-@erase "$(OUTDIR)\mopac.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=df.exe
F90_PROJ=/check:bounds /compile_only /dbglibs /debug:full /include:"..\Modules\Debug" /include:"..\Interfaces\Debug" /nologo /traceback /warn:argument_checking /warn:nofileopt /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF60.PDB" 
F90_OBJS=.\Debug/

.SUFFIXES: .fpp

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.fpp{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

CPP=cl.exe
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\mopac.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mopac.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /stack:0x800000 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\mopac.pdb" /debug /machine:I386 /out:"$(OUTDIR)\mopac.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\mopac.obj" \
	"$(INTDIR)\to_screen.obj" \
	"..\Subroutines\Debug\Subroutines.lib" \
	"..\Interfaces\Debug\Interfaces.lib" \
	"..\Modules\Debug\Modules.lib" \
	"..\MOZYME\Debug\MOZYME.lib"

"$(OUTDIR)\mopac.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("mopac.dep")
!INCLUDE "mopac.dep"
!ELSE 
!MESSAGE Warning: cannot find "mopac.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "mopac - Win32 Release" || "$(CFG)" == "mopac - Win32 Debug"

!IF  "$(CFG)" == "mopac - Win32 Release"

"Subroutines - Win32 Release" : 
   cd "\Software\mopac2009\Subroutines"
   $(MAKE) /$(MAKEFLAGS) /F .\Subroutines.mak CFG="Subroutines - Win32 Release" 
   cd "..\MOPAC"

"Subroutines - Win32 ReleaseCLEAN" : 
   cd "\Software\mopac2009\Subroutines"
   $(MAKE) /$(MAKEFLAGS) /F .\Subroutines.mak CFG="Subroutines - Win32 Release" RECURSE=1 CLEAN 
   cd "..\MOPAC"

!ELSEIF  "$(CFG)" == "mopac - Win32 Debug"

"Subroutines - Win32 Debug" : 
   cd "\Software\mopac2009\Subroutines"
   $(MAKE) /$(MAKEFLAGS) /F .\Subroutines.mak CFG="Subroutines - Win32 Debug" 
   cd "..\MOPAC"

"Subroutines - Win32 DebugCLEAN" : 
   cd "\Software\mopac2009\Subroutines"
   $(MAKE) /$(MAKEFLAGS) /F .\Subroutines.mak CFG="Subroutines - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\MOPAC"

!ENDIF 

!IF  "$(CFG)" == "mopac - Win32 Release"

"Interfaces - Win32 Release" : 
   cd "\Software\mopac2009\Interfaces"
   $(MAKE) /$(MAKEFLAGS) /F .\Interfaces.mak CFG="Interfaces - Win32 Release" 
   cd "..\MOPAC"

"Interfaces - Win32 ReleaseCLEAN" : 
   cd "\Software\mopac2009\Interfaces"
   $(MAKE) /$(MAKEFLAGS) /F .\Interfaces.mak CFG="Interfaces - Win32 Release" RECURSE=1 CLEAN 
   cd "..\MOPAC"

!ELSEIF  "$(CFG)" == "mopac - Win32 Debug"

"Interfaces - Win32 Debug" : 
   cd "\Software\mopac2009\Interfaces"
   $(MAKE) /$(MAKEFLAGS) /F .\Interfaces.mak CFG="Interfaces - Win32 Debug" 
   cd "..\MOPAC"

"Interfaces - Win32 DebugCLEAN" : 
   cd "\Software\mopac2009\Interfaces"
   $(MAKE) /$(MAKEFLAGS) /F .\Interfaces.mak CFG="Interfaces - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\MOPAC"

!ENDIF 

!IF  "$(CFG)" == "mopac - Win32 Release"

"Modules - Win32 Release" : 
   cd "\Software\mopac2009\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Release" 
   cd "..\MOPAC"

"Modules - Win32 ReleaseCLEAN" : 
   cd "\Software\mopac2009\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Release" RECURSE=1 CLEAN 
   cd "..\MOPAC"

!ELSEIF  "$(CFG)" == "mopac - Win32 Debug"

"Modules - Win32 Debug" : 
   cd "\Software\mopac2009\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Debug" 
   cd "..\MOPAC"

"Modules - Win32 DebugCLEAN" : 
   cd "\Software\mopac2009\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\MOPAC"

!ENDIF 

!IF  "$(CFG)" == "mopac - Win32 Release"

"MOZYME - Win32 Release" : 
   cd "\Software\mopac2009\MOZYME"
   $(MAKE) /$(MAKEFLAGS) /F .\MOZYME.mak CFG="MOZYME - Win32 Release" 
   cd "..\MOPAC"

"MOZYME - Win32 ReleaseCLEAN" : 
   cd "\Software\mopac2009\MOZYME"
   $(MAKE) /$(MAKEFLAGS) /F .\MOZYME.mak CFG="MOZYME - Win32 Release" RECURSE=1 CLEAN 
   cd "..\MOPAC"

!ELSEIF  "$(CFG)" == "mopac - Win32 Debug"

"MOZYME - Win32 Debug" : 
   cd "\Software\mopac2009\MOZYME"
   $(MAKE) /$(MAKEFLAGS) /F .\MOZYME.mak CFG="MOZYME - Win32 Debug" 
   cd "..\MOPAC"

"MOZYME - Win32 DebugCLEAN" : 
   cd "\Software\mopac2009\MOZYME"
   $(MAKE) /$(MAKEFLAGS) /F .\MOZYME.mak CFG="MOZYME - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\MOPAC"

!ENDIF 

SOURCE=.\mopac.f90

"$(INTDIR)\mopac.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\to_screen.f90

"$(INTDIR)\to_screen.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

