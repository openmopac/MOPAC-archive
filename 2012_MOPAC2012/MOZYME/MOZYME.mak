# Microsoft Developer Studio Generated NMAKE File, Based on MOZYME.dsp
!IF "$(CFG)" == ""
CFG=MOZYME - Win32 Debug
!MESSAGE No configuration specified. Defaulting to MOZYME - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "MOZYME - Win32 Release" && "$(CFG)" != "MOZYME - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
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
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "MOZYME - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\MOZYME.lib"

!ELSE 

ALL : "Modules - Win32 Release" "$(OUTDIR)\MOZYME.lib"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Modules - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\add_more_interactions.obj"
	-@erase "$(INTDIR)\addhb.obj"
	-@erase "$(INTDIR)\adjvec.obj"
	-@erase "$(INTDIR)\atomrs.obj"
	-@erase "$(INTDIR)\bonds_for_MOZYME.obj"
	-@erase "$(INTDIR)\buildf.obj"
	-@erase "$(INTDIR)\check.obj"
	-@erase "$(INTDIR)\chkion.obj"
	-@erase "$(INTDIR)\chklew.obj"
	-@erase "$(INTDIR)\chrge_for_MOZYME.obj"
	-@erase "$(INTDIR)\cnvgz.obj"
	-@erase "$(INTDIR)\compct.obj"
	-@erase "$(INTDIR)\convert_storage.obj"
	-@erase "$(INTDIR)\dcart_for_MOZYME.obj"
	-@erase "$(INTDIR)\delsta.obj"
	-@erase "$(INTDIR)\denrot_for_MOZYME.obj"
	-@erase "$(INTDIR)\density_for_MOZYME.obj"
	-@erase "$(INTDIR)\diagg.obj"
	-@erase "$(INTDIR)\diagg1.obj"
	-@erase "$(INTDIR)\diagg2.obj"
	-@erase "$(INTDIR)\dipole_for_MOZYME.obj"
	-@erase "$(INTDIR)\eigen.obj"
	-@erase "$(INTDIR)\eimp.obj"
	-@erase "$(INTDIR)\epseta.obj"
	-@erase "$(INTDIR)\fillij.obj"
	-@erase "$(INTDIR)\findn1.obj"
	-@erase "$(INTDIR)\fock1_for_MOZYME.obj"
	-@erase "$(INTDIR)\fock2z.obj"
	-@erase "$(INTDIR)\geochk.obj"
	-@erase "$(INTDIR)\getpdb.obj"
	-@erase "$(INTDIR)\greek.obj"
	-@erase "$(INTDIR)\hbonds.obj"
	-@erase "$(INTDIR)\hcore_for_MOZYME.obj"
	-@erase "$(INTDIR)\helecz.obj"
	-@erase "$(INTDIR)\hybrid.obj"
	-@erase "$(INTDIR)\ijbo.obj"
	-@erase "$(INTDIR)\isitsc.obj"
	-@erase "$(INTDIR)\iter_for_MOZYME.obj"
	-@erase "$(INTDIR)\jab_for_MOZYME.obj"
	-@erase "$(INTDIR)\kab_for_MOZYME.obj"
	-@erase "$(INTDIR)\lbfgs.obj"
	-@erase "$(INTDIR)\lewis.obj"
	-@erase "$(INTDIR)\ligand.obj"
	-@erase "$(INTDIR)\local2.obj"
	-@erase "$(INTDIR)\local_for_MOZYME.obj"
	-@erase "$(INTDIR)\lyse.obj"
	-@erase "$(INTDIR)\makvec.obj"
	-@erase "$(INTDIR)\mbonds.obj"
	-@erase "$(INTDIR)\mlmo.obj"
	-@erase "$(INTDIR)\modchg.obj"
	-@erase "$(INTDIR)\modgra.obj"
	-@erase "$(INTDIR)\names.obj"
	-@erase "$(INTDIR)\newflg.obj"
	-@erase "$(INTDIR)\nxtmer.obj"
	-@erase "$(INTDIR)\outer1.obj"
	-@erase "$(INTDIR)\outer2.obj"
	-@erase "$(INTDIR)\pdbout.obj"
	-@erase "$(INTDIR)\picopt.obj"
	-@erase "$(INTDIR)\pinout.obj"
	-@erase "$(INTDIR)\prtgra.obj"
	-@erase "$(INTDIR)\prtlmo.obj"
	-@erase "$(INTDIR)\reorth.obj"
	-@erase "$(INTDIR)\reseq.obj"
	-@erase "$(INTDIR)\rotlmo.obj"
	-@erase "$(INTDIR)\scfcri.obj"
	-@erase "$(INTDIR)\selmos.obj"
	-@erase "$(INTDIR)\set_up_MOZYME_arrays.obj"
	-@erase "$(INTDIR)\set_up_RAPID.obj"
	-@erase "$(INTDIR)\setupk.obj"
	-@erase "$(INTDIR)\tidy.obj"
	-@erase "$(INTDIR)\txtype.obj"
	-@erase "$(INTDIR)\values.obj"
	-@erase "$(INTDIR)\vecprt_for_MOZYME.obj"
	-@erase "$(OUTDIR)\MOZYME.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=df.exe
F90_PROJ=/compile_only /include:"..\Modules\Release" /include:"..\Interfaces\Release" /include:"..\Subroutines\Release" /nologo /warn:nofileopt /module:"Release/" /object:"Release/" 
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
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)\MOZYME.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\MOZYME.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\MOZYME.lib" 
LIB32_OBJS= \
	"$(INTDIR)\add_more_interactions.obj" \
	"$(INTDIR)\addhb.obj" \
	"$(INTDIR)\adjvec.obj" \
	"$(INTDIR)\atomrs.obj" \
	"$(INTDIR)\bonds_for_MOZYME.obj" \
	"$(INTDIR)\buildf.obj" \
	"$(INTDIR)\check.obj" \
	"$(INTDIR)\chkion.obj" \
	"$(INTDIR)\chklew.obj" \
	"$(INTDIR)\chrge_for_MOZYME.obj" \
	"$(INTDIR)\cnvgz.obj" \
	"$(INTDIR)\compct.obj" \
	"$(INTDIR)\convert_storage.obj" \
	"$(INTDIR)\dcart_for_MOZYME.obj" \
	"$(INTDIR)\delsta.obj" \
	"$(INTDIR)\denrot_for_MOZYME.obj" \
	"$(INTDIR)\density_for_MOZYME.obj" \
	"$(INTDIR)\diagg.obj" \
	"$(INTDIR)\diagg1.obj" \
	"$(INTDIR)\diagg2.obj" \
	"$(INTDIR)\dipole_for_MOZYME.obj" \
	"$(INTDIR)\eigen.obj" \
	"$(INTDIR)\eimp.obj" \
	"$(INTDIR)\epseta.obj" \
	"$(INTDIR)\fillij.obj" \
	"$(INTDIR)\findn1.obj" \
	"$(INTDIR)\fock1_for_MOZYME.obj" \
	"$(INTDIR)\fock2z.obj" \
	"$(INTDIR)\geochk.obj" \
	"$(INTDIR)\getpdb.obj" \
	"$(INTDIR)\greek.obj" \
	"$(INTDIR)\hbonds.obj" \
	"$(INTDIR)\hcore_for_MOZYME.obj" \
	"$(INTDIR)\helecz.obj" \
	"$(INTDIR)\hybrid.obj" \
	"$(INTDIR)\ijbo.obj" \
	"$(INTDIR)\isitsc.obj" \
	"$(INTDIR)\iter_for_MOZYME.obj" \
	"$(INTDIR)\jab_for_MOZYME.obj" \
	"$(INTDIR)\kab_for_MOZYME.obj" \
	"$(INTDIR)\lbfgs.obj" \
	"$(INTDIR)\lewis.obj" \
	"$(INTDIR)\ligand.obj" \
	"$(INTDIR)\local2.obj" \
	"$(INTDIR)\local_for_MOZYME.obj" \
	"$(INTDIR)\lyse.obj" \
	"$(INTDIR)\makvec.obj" \
	"$(INTDIR)\mbonds.obj" \
	"$(INTDIR)\mlmo.obj" \
	"$(INTDIR)\modchg.obj" \
	"$(INTDIR)\modgra.obj" \
	"$(INTDIR)\names.obj" \
	"$(INTDIR)\newflg.obj" \
	"$(INTDIR)\nxtmer.obj" \
	"$(INTDIR)\outer1.obj" \
	"$(INTDIR)\outer2.obj" \
	"$(INTDIR)\pdbout.obj" \
	"$(INTDIR)\picopt.obj" \
	"$(INTDIR)\pinout.obj" \
	"$(INTDIR)\prtgra.obj" \
	"$(INTDIR)\prtlmo.obj" \
	"$(INTDIR)\reorth.obj" \
	"$(INTDIR)\reseq.obj" \
	"$(INTDIR)\rotlmo.obj" \
	"$(INTDIR)\scfcri.obj" \
	"$(INTDIR)\selmos.obj" \
	"$(INTDIR)\set_up_MOZYME_arrays.obj" \
	"$(INTDIR)\set_up_RAPID.obj" \
	"$(INTDIR)\setupk.obj" \
	"$(INTDIR)\tidy.obj" \
	"$(INTDIR)\txtype.obj" \
	"$(INTDIR)\values.obj" \
	"$(INTDIR)\vecprt_for_MOZYME.obj" \
	"..\Modules\Release\Modules.lib"

"$(OUTDIR)\MOZYME.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "MOZYME - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\MOZYME.lib"

!ELSE 

ALL : "Modules - Win32 Debug" "$(OUTDIR)\MOZYME.lib"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Modules - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\add_more_interactions.obj"
	-@erase "$(INTDIR)\addhb.obj"
	-@erase "$(INTDIR)\adjvec.obj"
	-@erase "$(INTDIR)\atomrs.obj"
	-@erase "$(INTDIR)\bonds_for_MOZYME.obj"
	-@erase "$(INTDIR)\buildf.obj"
	-@erase "$(INTDIR)\check.obj"
	-@erase "$(INTDIR)\chkion.obj"
	-@erase "$(INTDIR)\chklew.obj"
	-@erase "$(INTDIR)\chrge_for_MOZYME.obj"
	-@erase "$(INTDIR)\cnvgz.obj"
	-@erase "$(INTDIR)\compct.obj"
	-@erase "$(INTDIR)\convert_storage.obj"
	-@erase "$(INTDIR)\dcart_for_MOZYME.obj"
	-@erase "$(INTDIR)\delsta.obj"
	-@erase "$(INTDIR)\denrot_for_MOZYME.obj"
	-@erase "$(INTDIR)\density_for_MOZYME.obj"
	-@erase "$(INTDIR)\DF60.PDB"
	-@erase "$(INTDIR)\diagg.obj"
	-@erase "$(INTDIR)\diagg1.obj"
	-@erase "$(INTDIR)\diagg2.obj"
	-@erase "$(INTDIR)\dipole_for_MOZYME.obj"
	-@erase "$(INTDIR)\eigen.obj"
	-@erase "$(INTDIR)\eimp.obj"
	-@erase "$(INTDIR)\epseta.obj"
	-@erase "$(INTDIR)\fillij.obj"
	-@erase "$(INTDIR)\findn1.obj"
	-@erase "$(INTDIR)\fock1_for_MOZYME.obj"
	-@erase "$(INTDIR)\fock2z.obj"
	-@erase "$(INTDIR)\geochk.obj"
	-@erase "$(INTDIR)\getpdb.obj"
	-@erase "$(INTDIR)\greek.obj"
	-@erase "$(INTDIR)\hbonds.obj"
	-@erase "$(INTDIR)\hcore_for_MOZYME.obj"
	-@erase "$(INTDIR)\helecz.obj"
	-@erase "$(INTDIR)\hybrid.obj"
	-@erase "$(INTDIR)\ijbo.obj"
	-@erase "$(INTDIR)\isitsc.obj"
	-@erase "$(INTDIR)\iter_for_MOZYME.obj"
	-@erase "$(INTDIR)\jab_for_MOZYME.obj"
	-@erase "$(INTDIR)\kab_for_MOZYME.obj"
	-@erase "$(INTDIR)\lbfgs.obj"
	-@erase "$(INTDIR)\lewis.obj"
	-@erase "$(INTDIR)\ligand.obj"
	-@erase "$(INTDIR)\local2.obj"
	-@erase "$(INTDIR)\local_for_MOZYME.obj"
	-@erase "$(INTDIR)\lyse.obj"
	-@erase "$(INTDIR)\makvec.obj"
	-@erase "$(INTDIR)\mbonds.obj"
	-@erase "$(INTDIR)\mlmo.obj"
	-@erase "$(INTDIR)\modchg.obj"
	-@erase "$(INTDIR)\modgra.obj"
	-@erase "$(INTDIR)\names.obj"
	-@erase "$(INTDIR)\newflg.obj"
	-@erase "$(INTDIR)\nxtmer.obj"
	-@erase "$(INTDIR)\outer1.obj"
	-@erase "$(INTDIR)\outer2.obj"
	-@erase "$(INTDIR)\pdbout.obj"
	-@erase "$(INTDIR)\picopt.obj"
	-@erase "$(INTDIR)\pinout.obj"
	-@erase "$(INTDIR)\prtgra.obj"
	-@erase "$(INTDIR)\prtlmo.obj"
	-@erase "$(INTDIR)\reorth.obj"
	-@erase "$(INTDIR)\reseq.obj"
	-@erase "$(INTDIR)\rotlmo.obj"
	-@erase "$(INTDIR)\scfcri.obj"
	-@erase "$(INTDIR)\selmos.obj"
	-@erase "$(INTDIR)\set_up_MOZYME_arrays.obj"
	-@erase "$(INTDIR)\set_up_RAPID.obj"
	-@erase "$(INTDIR)\setupk.obj"
	-@erase "$(INTDIR)\tidy.obj"
	-@erase "$(INTDIR)\txtype.obj"
	-@erase "$(INTDIR)\values.obj"
	-@erase "$(INTDIR)\vecprt_for_MOZYME.obj"
	-@erase "$(OUTDIR)\MOZYME.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=df.exe
F90_PROJ=/check:bounds /compile_only /dbglibs /debug:full /include:"..\Modules\Debug" /include:"..\Interfaces\Debug" /include:"..\Subroutines\Debug" /nologo /traceback /warn:argument_checking /warn:nofileopt /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF60.PDB" 
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
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)\MOZYME.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\MOZYME.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\MOZYME.lib" 
LIB32_OBJS= \
	"$(INTDIR)\add_more_interactions.obj" \
	"$(INTDIR)\addhb.obj" \
	"$(INTDIR)\adjvec.obj" \
	"$(INTDIR)\atomrs.obj" \
	"$(INTDIR)\bonds_for_MOZYME.obj" \
	"$(INTDIR)\buildf.obj" \
	"$(INTDIR)\check.obj" \
	"$(INTDIR)\chkion.obj" \
	"$(INTDIR)\chklew.obj" \
	"$(INTDIR)\chrge_for_MOZYME.obj" \
	"$(INTDIR)\cnvgz.obj" \
	"$(INTDIR)\compct.obj" \
	"$(INTDIR)\convert_storage.obj" \
	"$(INTDIR)\dcart_for_MOZYME.obj" \
	"$(INTDIR)\delsta.obj" \
	"$(INTDIR)\denrot_for_MOZYME.obj" \
	"$(INTDIR)\density_for_MOZYME.obj" \
	"$(INTDIR)\diagg.obj" \
	"$(INTDIR)\diagg1.obj" \
	"$(INTDIR)\diagg2.obj" \
	"$(INTDIR)\dipole_for_MOZYME.obj" \
	"$(INTDIR)\eigen.obj" \
	"$(INTDIR)\eimp.obj" \
	"$(INTDIR)\epseta.obj" \
	"$(INTDIR)\fillij.obj" \
	"$(INTDIR)\findn1.obj" \
	"$(INTDIR)\fock1_for_MOZYME.obj" \
	"$(INTDIR)\fock2z.obj" \
	"$(INTDIR)\geochk.obj" \
	"$(INTDIR)\getpdb.obj" \
	"$(INTDIR)\greek.obj" \
	"$(INTDIR)\hbonds.obj" \
	"$(INTDIR)\hcore_for_MOZYME.obj" \
	"$(INTDIR)\helecz.obj" \
	"$(INTDIR)\hybrid.obj" \
	"$(INTDIR)\ijbo.obj" \
	"$(INTDIR)\isitsc.obj" \
	"$(INTDIR)\iter_for_MOZYME.obj" \
	"$(INTDIR)\jab_for_MOZYME.obj" \
	"$(INTDIR)\kab_for_MOZYME.obj" \
	"$(INTDIR)\lbfgs.obj" \
	"$(INTDIR)\lewis.obj" \
	"$(INTDIR)\ligand.obj" \
	"$(INTDIR)\local2.obj" \
	"$(INTDIR)\local_for_MOZYME.obj" \
	"$(INTDIR)\lyse.obj" \
	"$(INTDIR)\makvec.obj" \
	"$(INTDIR)\mbonds.obj" \
	"$(INTDIR)\mlmo.obj" \
	"$(INTDIR)\modchg.obj" \
	"$(INTDIR)\modgra.obj" \
	"$(INTDIR)\names.obj" \
	"$(INTDIR)\newflg.obj" \
	"$(INTDIR)\nxtmer.obj" \
	"$(INTDIR)\outer1.obj" \
	"$(INTDIR)\outer2.obj" \
	"$(INTDIR)\pdbout.obj" \
	"$(INTDIR)\picopt.obj" \
	"$(INTDIR)\pinout.obj" \
	"$(INTDIR)\prtgra.obj" \
	"$(INTDIR)\prtlmo.obj" \
	"$(INTDIR)\reorth.obj" \
	"$(INTDIR)\reseq.obj" \
	"$(INTDIR)\rotlmo.obj" \
	"$(INTDIR)\scfcri.obj" \
	"$(INTDIR)\selmos.obj" \
	"$(INTDIR)\set_up_MOZYME_arrays.obj" \
	"$(INTDIR)\set_up_RAPID.obj" \
	"$(INTDIR)\setupk.obj" \
	"$(INTDIR)\tidy.obj" \
	"$(INTDIR)\txtype.obj" \
	"$(INTDIR)\values.obj" \
	"$(INTDIR)\vecprt_for_MOZYME.obj" \
	"..\Modules\Debug\Modules.lib"

"$(OUTDIR)\MOZYME.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("MOZYME.dep")
!INCLUDE "MOZYME.dep"
!ELSE 
!MESSAGE Warning: cannot find "MOZYME.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "MOZYME - Win32 Release" || "$(CFG)" == "MOZYME - Win32 Debug"
SOURCE=..\src_MOZYME\add_more_interactions.F90

"$(INTDIR)\add_more_interactions.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\addhb.F90

"$(INTDIR)\addhb.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\adjvec.F90

"$(INTDIR)\adjvec.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\atomrs.F90

"$(INTDIR)\atomrs.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\bonds_for_MOZYME.F90

"$(INTDIR)\bonds_for_MOZYME.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\buildf.F90

"$(INTDIR)\buildf.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\check.F90

"$(INTDIR)\check.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\chkion.F90

"$(INTDIR)\chkion.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\chklew.F90

"$(INTDIR)\chklew.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\chrge_for_MOZYME.F90

"$(INTDIR)\chrge_for_MOZYME.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\cnvgz.F90

"$(INTDIR)\cnvgz.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\compct.F90

"$(INTDIR)\compct.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\convert_storage.F90

"$(INTDIR)\convert_storage.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\dcart_for_MOZYME.F90

"$(INTDIR)\dcart_for_MOZYME.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\delsta.F90

"$(INTDIR)\delsta.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\denrot_for_MOZYME.F90

"$(INTDIR)\denrot_for_MOZYME.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\density_for_MOZYME.F90

"$(INTDIR)\density_for_MOZYME.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\diagg.F90

"$(INTDIR)\diagg.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\diagg1.F90

"$(INTDIR)\diagg1.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\diagg2.F90

"$(INTDIR)\diagg2.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\dipole_for_MOZYME.F90

"$(INTDIR)\dipole_for_MOZYME.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\eigen.F90

"$(INTDIR)\eigen.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\eimp.F90

"$(INTDIR)\eimp.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\epseta.F90

"$(INTDIR)\epseta.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\fillij.F90

"$(INTDIR)\fillij.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\findn1.F90

"$(INTDIR)\findn1.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\fock1_for_MOZYME.F90

"$(INTDIR)\fock1_for_MOZYME.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\fock2z.F90

"$(INTDIR)\fock2z.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\geochk.F90

"$(INTDIR)\geochk.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\getpdb.F90

"$(INTDIR)\getpdb.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\greek.F90

"$(INTDIR)\greek.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\hbonds.F90

"$(INTDIR)\hbonds.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\hcore_for_MOZYME.F90

"$(INTDIR)\hcore_for_MOZYME.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\helecz.F90

"$(INTDIR)\helecz.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\hybrid.F90

"$(INTDIR)\hybrid.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\ijbo.F90

"$(INTDIR)\ijbo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\isitsc.F90

"$(INTDIR)\isitsc.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\iter_for_MOZYME.F90

"$(INTDIR)\iter_for_MOZYME.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\jab_for_MOZYME.F90

"$(INTDIR)\jab_for_MOZYME.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\kab_for_MOZYME.F90

"$(INTDIR)\kab_for_MOZYME.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\lbfgs.F90

"$(INTDIR)\lbfgs.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\lewis.F90

"$(INTDIR)\lewis.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\ligand.F90

"$(INTDIR)\ligand.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\local2.F90

"$(INTDIR)\local2.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\local_for_MOZYME.F90

"$(INTDIR)\local_for_MOZYME.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\lyse.F90

"$(INTDIR)\lyse.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\makvec.F90

"$(INTDIR)\makvec.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\mbonds.F90

"$(INTDIR)\mbonds.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\mlmo.F90

"$(INTDIR)\mlmo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\modchg.F90

"$(INTDIR)\modchg.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\modgra.F90

"$(INTDIR)\modgra.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\names.F90

"$(INTDIR)\names.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\newflg.F90

"$(INTDIR)\newflg.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\nxtmer.F90

"$(INTDIR)\nxtmer.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\outer1.F90

"$(INTDIR)\outer1.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\outer2.F90

"$(INTDIR)\outer2.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\pdbout.F90

"$(INTDIR)\pdbout.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\picopt.F90

"$(INTDIR)\picopt.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\pinout.F90

"$(INTDIR)\pinout.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\prtgra.F90

"$(INTDIR)\prtgra.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\prtlmo.F90

"$(INTDIR)\prtlmo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\reorth.F90

"$(INTDIR)\reorth.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\reseq.F90

"$(INTDIR)\reseq.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\rotlmo.F90

"$(INTDIR)\rotlmo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\scfcri.F90

"$(INTDIR)\scfcri.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\selmos.F90

"$(INTDIR)\selmos.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\set_up_MOZYME_arrays.F90

"$(INTDIR)\set_up_MOZYME_arrays.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\set_up_RAPID.F90

"$(INTDIR)\set_up_RAPID.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\setupk.F90

"$(INTDIR)\setupk.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\tidy.F90

"$(INTDIR)\tidy.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\txtype.F90

"$(INTDIR)\txtype.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\values.F90

"$(INTDIR)\values.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\vecprt_for_MOZYME.F90

"$(INTDIR)\vecprt_for_MOZYME.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!IF  "$(CFG)" == "MOZYME - Win32 Release"

"Modules - Win32 Release" : 
   cd "\Software\mopac2009\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Release" 
   cd "..\MOZYME"

"Modules - Win32 ReleaseCLEAN" : 
   cd "\Software\mopac2009\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Release" RECURSE=1 CLEAN 
   cd "..\MOZYME"

!ELSEIF  "$(CFG)" == "MOZYME - Win32 Debug"

"Modules - Win32 Debug" : 
   cd "\Software\mopac2009\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Debug" 
   cd "..\MOZYME"

"Modules - Win32 DebugCLEAN" : 
   cd "\Software\mopac2009\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\MOZYME"

!ENDIF 


!ENDIF 

