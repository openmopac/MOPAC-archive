# Microsoft Developer Studio Generated NMAKE File, Based on param.dsp
!IF "$(CFG)" == ""
CFG=param - Win32 Debug
!MESSAGE No configuration specified. Defaulting to param - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "param - Win32 Release" && "$(CFG)" != "param - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
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
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "param - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release

!IF "$(RECURSE)" == "0" 

ALL : "..\..\utility\param_7.2.exe"

!ELSE 

ALL : "MOZYME - Win32 Release" "Modules - Win32 Release" "Subroutines - Win32 Release" "..\..\utility\param_7.2.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Subroutines - Win32 ReleaseCLEAN" "Modules - Win32 ReleaseCLEAN" "MOZYME - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\chkpar.obj"
	-@erase "$(INTDIR)\datinp.obj"
	-@erase "$(INTDIR)\depfn.obj"
	-@erase "$(INTDIR)\direct.obj"
	-@erase "$(INTDIR)\empire.obj"
	-@erase "$(INTDIR)\extract_parameter.obj"
	-@erase "$(INTDIR)\filusp.obj"
	-@erase "$(INTDIR)\getmol.obj"
	-@erase "$(INTDIR)\getpar.obj"
	-@erase "$(INTDIR)\invfn.obj"
	-@erase "$(INTDIR)\mndod.obj"
	-@erase "$(INTDIR)\optgeo.obj"
	-@erase "$(INTDIR)\output_cc_rep_fn.obj"
	-@erase "$(INTDIR)\param.obj"
	-@erase "$(INTDIR)\Param_global_C.obj"
	-@erase "$(INTDIR)\parameters_for_pm5_C.obj"
	-@erase "$(INTDIR)\pardip.obj"
	-@erase "$(INTDIR)\parfg.obj"
	-@erase "$(INTDIR)\pargeo.obj"
	-@erase "$(INTDIR)\parips.obj"
	-@erase "$(INTDIR)\parkey.obj"
	-@erase "$(INTDIR)\partab.obj"
	-@erase "$(INTDIR)\pparsav.obj"
	-@erase "$(INTDIR)\psort.obj"
	-@erase "$(INTDIR)\rapid0.obj"
	-@erase "$(INTDIR)\rapid1.obj"
	-@erase "$(INTDIR)\rapid2.obj"
	-@erase "$(INTDIR)\rapid3.obj"
	-@erase "$(INTDIR)\resetp.obj"
	-@erase "$(INTDIR)\savgeo.obj"
	-@erase "$(INTDIR)\switch.obj"
	-@erase "$(INTDIR)\to_screen.obj"
	-@erase "..\..\utility\param_7.2.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=df.exe
F90_PROJ=/compile_only /include:"..\MOPAC2007\Interfaces\Release" /include:"..\mopac2007\Modules\Release" /nologo /traceback /warn:nofileopt /module:"Release/" /object:"Release/" 
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
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\param.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\param.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /base:"0x1600000" /stack:0x5b8d80 /subsystem:console /incremental:no /pdb:"$(OUTDIR)\param_F90.pdb" /machine:I386 /out:"M:\utility\param_7.2.exe" /pdbtype:sept /heap:80000000,8000000 
LINK32_OBJS= \
	"$(INTDIR)\chkpar.obj" \
	"$(INTDIR)\datinp.obj" \
	"$(INTDIR)\depfn.obj" \
	"$(INTDIR)\direct.obj" \
	"$(INTDIR)\empire.obj" \
	"$(INTDIR)\extract_parameter.obj" \
	"$(INTDIR)\filusp.obj" \
	"$(INTDIR)\getmol.obj" \
	"$(INTDIR)\getpar.obj" \
	"$(INTDIR)\invfn.obj" \
	"$(INTDIR)\mndod.obj" \
	"$(INTDIR)\optgeo.obj" \
	"$(INTDIR)\output_cc_rep_fn.obj" \
	"$(INTDIR)\param.obj" \
	"$(INTDIR)\Param_global_C.obj" \
	"$(INTDIR)\parameters_for_pm5_C.obj" \
	"$(INTDIR)\pardip.obj" \
	"$(INTDIR)\parfg.obj" \
	"$(INTDIR)\pargeo.obj" \
	"$(INTDIR)\parips.obj" \
	"$(INTDIR)\parkey.obj" \
	"$(INTDIR)\partab.obj" \
	"$(INTDIR)\pparsav.obj" \
	"$(INTDIR)\psort.obj" \
	"$(INTDIR)\rapid0.obj" \
	"$(INTDIR)\rapid1.obj" \
	"$(INTDIR)\rapid2.obj" \
	"$(INTDIR)\rapid3.obj" \
	"$(INTDIR)\resetp.obj" \
	"$(INTDIR)\savgeo.obj" \
	"$(INTDIR)\switch.obj" \
	"$(INTDIR)\to_screen.obj" \
	"..\mopac2007\Subroutines\Release\Subroutines.lib" \
	"..\mopac2007\Modules\Release\Modules.lib" \
	"..\mopac2007\MOZYME\Release\MOZYME.lib"

"..\..\utility\param_7.2.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
   copy_datinp.cmd
	 $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE="$(InputPath)"

!ELSEIF  "$(CFG)" == "param - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\param7.2.exe"

!ELSE 

ALL : "MOZYME - Win32 Debug" "Modules - Win32 Debug" "Subroutines - Win32 Debug" "$(OUTDIR)\param7.2.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Subroutines - Win32 DebugCLEAN" "Modules - Win32 DebugCLEAN" "MOZYME - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\chkpar.obj"
	-@erase "$(INTDIR)\datinp.obj"
	-@erase "$(INTDIR)\depfn.obj"
	-@erase "$(INTDIR)\DF60.PDB"
	-@erase "$(INTDIR)\direct.obj"
	-@erase "$(INTDIR)\empire.obj"
	-@erase "$(INTDIR)\extract_parameter.obj"
	-@erase "$(INTDIR)\filusp.obj"
	-@erase "$(INTDIR)\getmol.obj"
	-@erase "$(INTDIR)\getpar.obj"
	-@erase "$(INTDIR)\invfn.obj"
	-@erase "$(INTDIR)\mndod.obj"
	-@erase "$(INTDIR)\optgeo.obj"
	-@erase "$(INTDIR)\output_cc_rep_fn.obj"
	-@erase "$(INTDIR)\param.obj"
	-@erase "$(INTDIR)\Param_global_C.obj"
	-@erase "$(INTDIR)\parameters_for_pm5_C.obj"
	-@erase "$(INTDIR)\pardip.obj"
	-@erase "$(INTDIR)\parfg.obj"
	-@erase "$(INTDIR)\pargeo.obj"
	-@erase "$(INTDIR)\parips.obj"
	-@erase "$(INTDIR)\parkey.obj"
	-@erase "$(INTDIR)\partab.obj"
	-@erase "$(INTDIR)\pparsav.obj"
	-@erase "$(INTDIR)\psort.obj"
	-@erase "$(INTDIR)\rapid0.obj"
	-@erase "$(INTDIR)\rapid1.obj"
	-@erase "$(INTDIR)\rapid2.obj"
	-@erase "$(INTDIR)\rapid3.obj"
	-@erase "$(INTDIR)\resetp.obj"
	-@erase "$(INTDIR)\savgeo.obj"
	-@erase "$(INTDIR)\switch.obj"
	-@erase "$(INTDIR)\to_screen.obj"
	-@erase "$(OUTDIR)\param7.2.exe"
	-@erase "$(OUTDIR)\param7.2.ilk"
	-@erase "$(OUTDIR)\param7.2.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=df.exe
F90_PROJ=/check:bounds /compile_only /dbglibs /debug:full /include:"..\mopac2007\Modules\Debug" /include:"..\MOPAC2007\Interfaces\Debug" /nologo /traceback /warn:argument_checking /warn:nofileopt /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF60.PDB" 
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
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\param.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\param.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib /nologo /stack:0x8000000 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\param7.2.pdb" /debug /machine:I386 /out:"$(OUTDIR)\param7.2.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\chkpar.obj" \
	"$(INTDIR)\datinp.obj" \
	"$(INTDIR)\depfn.obj" \
	"$(INTDIR)\direct.obj" \
	"$(INTDIR)\empire.obj" \
	"$(INTDIR)\extract_parameter.obj" \
	"$(INTDIR)\filusp.obj" \
	"$(INTDIR)\getmol.obj" \
	"$(INTDIR)\getpar.obj" \
	"$(INTDIR)\invfn.obj" \
	"$(INTDIR)\mndod.obj" \
	"$(INTDIR)\optgeo.obj" \
	"$(INTDIR)\output_cc_rep_fn.obj" \
	"$(INTDIR)\param.obj" \
	"$(INTDIR)\Param_global_C.obj" \
	"$(INTDIR)\parameters_for_pm5_C.obj" \
	"$(INTDIR)\pardip.obj" \
	"$(INTDIR)\parfg.obj" \
	"$(INTDIR)\pargeo.obj" \
	"$(INTDIR)\parips.obj" \
	"$(INTDIR)\parkey.obj" \
	"$(INTDIR)\partab.obj" \
	"$(INTDIR)\pparsav.obj" \
	"$(INTDIR)\psort.obj" \
	"$(INTDIR)\rapid0.obj" \
	"$(INTDIR)\rapid1.obj" \
	"$(INTDIR)\rapid2.obj" \
	"$(INTDIR)\rapid3.obj" \
	"$(INTDIR)\resetp.obj" \
	"$(INTDIR)\savgeo.obj" \
	"$(INTDIR)\switch.obj" \
	"$(INTDIR)\to_screen.obj" \
	"..\mopac2007\Subroutines\Debug\Subroutines.lib" \
	"..\mopac2007\Modules\Debug\Modules.lib" \
	"..\mopac2007\MOZYME\Debug\MOZYME.lib"

"$(OUTDIR)\param7.2.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("param.dep")
!INCLUDE "param.dep"
!ELSE 
!MESSAGE Warning: cannot find "param.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "param - Win32 Release" || "$(CFG)" == "param - Win32 Debug"

!IF  "$(CFG)" == "param - Win32 Release"

"Subroutines - Win32 Release" : 
   cd "\Software\mopac2007\Subroutines"
   $(MAKE) /$(MAKEFLAGS) /F .\Subroutines.mak CFG="Subroutines - Win32 Release" 
   cd "..\..\param_7.2"

"Subroutines - Win32 ReleaseCLEAN" : 
   cd "\Software\mopac2007\Subroutines"
   $(MAKE) /$(MAKEFLAGS) /F .\Subroutines.mak CFG="Subroutines - Win32 Release" RECURSE=1 CLEAN 
   cd "..\..\param_7.2"

!ELSEIF  "$(CFG)" == "param - Win32 Debug"

"Subroutines - Win32 Debug" : 
   cd "\Software\mopac2007\Subroutines"
   $(MAKE) /$(MAKEFLAGS) /F .\Subroutines.mak CFG="Subroutines - Win32 Debug" 
   cd "..\..\param_7.2"

"Subroutines - Win32 DebugCLEAN" : 
   cd "\Software\mopac2007\Subroutines"
   $(MAKE) /$(MAKEFLAGS) /F .\Subroutines.mak CFG="Subroutines - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\..\param_7.2"

!ENDIF 

!IF  "$(CFG)" == "param - Win32 Release"

"Modules - Win32 Release" : 
   cd "\Software\mopac2007\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Release" 
   cd "..\..\param_7.2"

"Modules - Win32 ReleaseCLEAN" : 
   cd "\Software\mopac2007\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Release" RECURSE=1 CLEAN 
   cd "..\..\param_7.2"

!ELSEIF  "$(CFG)" == "param - Win32 Debug"

"Modules - Win32 Debug" : 
   cd "\Software\mopac2007\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Debug" 
   cd "..\..\param_7.2"

"Modules - Win32 DebugCLEAN" : 
   cd "\Software\mopac2007\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\..\param_7.2"

!ENDIF 

!IF  "$(CFG)" == "param - Win32 Release"

"MOZYME - Win32 Release" : 
   cd "\Software\mopac2007\MOZYME"
   $(MAKE) /$(MAKEFLAGS) /F .\MOZYME.mak CFG="MOZYME - Win32 Release" 
   cd "..\..\param_7.2"

"MOZYME - Win32 ReleaseCLEAN" : 
   cd "\Software\mopac2007\MOZYME"
   $(MAKE) /$(MAKEFLAGS) /F .\MOZYME.mak CFG="MOZYME - Win32 Release" RECURSE=1 CLEAN 
   cd "..\..\param_7.2"

!ELSEIF  "$(CFG)" == "param - Win32 Debug"

"MOZYME - Win32 Debug" : 
   cd "\Software\mopac2007\MOZYME"
   $(MAKE) /$(MAKEFLAGS) /F .\MOZYME.mak CFG="MOZYME - Win32 Debug" 
   cd "..\..\param_7.2"

"MOZYME - Win32 DebugCLEAN" : 
   cd "\Software\mopac2007\MOZYME"
   $(MAKE) /$(MAKEFLAGS) /F .\MOZYME.mak CFG="MOZYME - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\..\param_7.2"

!ENDIF 

SOURCE=.\src\chkpar.F90

"$(INTDIR)\chkpar.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\datinp.F90

"$(INTDIR)\datinp.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\depfn.F90

"$(INTDIR)\depfn.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\direct.F90

"$(INTDIR)\direct.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\empire.F90

"$(INTDIR)\empire.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\extract_parameter.F90

"$(INTDIR)\extract_parameter.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\filusp.F90

"$(INTDIR)\filusp.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\getmol.F90

"$(INTDIR)\getmol.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\getpar.F90

"$(INTDIR)\getpar.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\invfn.f90

"$(INTDIR)\invfn.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\mndod.F90

"$(INTDIR)\mndod.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\optgeo.F90

"$(INTDIR)\optgeo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\output_cc_rep_fn.F90

"$(INTDIR)\output_cc_rep_fn.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\param.F90

"$(INTDIR)\param.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\Param_global_C.F90

"$(INTDIR)\Param_global_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\parameters_for_pm5_C.f90

"$(INTDIR)\parameters_for_pm5_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\pardip.F90

"$(INTDIR)\pardip.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\parfg.F90

"$(INTDIR)\parfg.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\pargeo.F90

"$(INTDIR)\pargeo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\parips.F90

"$(INTDIR)\parips.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\parkey.F90

"$(INTDIR)\parkey.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\partab.F90

"$(INTDIR)\partab.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\pparsav.f90

"$(INTDIR)\pparsav.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\psort.F90

"$(INTDIR)\psort.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\rapid0.F90

"$(INTDIR)\rapid0.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\rapid1.F90

"$(INTDIR)\rapid1.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\rapid2.F90

"$(INTDIR)\rapid2.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\rapid3.F90

"$(INTDIR)\rapid3.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\resetp.F90

"$(INTDIR)\resetp.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\savgeo.F90

"$(INTDIR)\savgeo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=.\src\switch.f90

"$(INTDIR)\switch.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\mopac2007\MOPAC\to_screen.F90

"$(INTDIR)\to_screen.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)



!ENDIF 

