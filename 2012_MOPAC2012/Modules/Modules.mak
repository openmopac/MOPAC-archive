# Microsoft Developer Studio Generated NMAKE File, Based on Modules.dsp
!IF "$(CFG)" == ""
CFG=Modules - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Modules - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Modules - Win32 Release" && "$(CFG)" != "Modules - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Modules.mak" CFG="Modules - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Modules - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "Modules - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "Modules - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\Modules.lib" "$(OUTDIR)\analyt_C.mod" "$(OUTDIR)\chanel_C.mod" "$(OUTDIR)\common_arrays_C.mod" "$(OUTDIR)\conref_C.mod" "$(OUTDIR)\cosmo_C.mod" "$(OUTDIR)\derivs_C.mod" "$(OUTDIR)\drc_C.mod" "$(OUTDIR)\ef_C.mod" "$(OUTDIR)\elemts_C.mod" "$(OUTDIR)\esp_C.mod" "$(OUTDIR)\euler_C.mod" "$(OUTDIR)\funcon_C.mod" "$(OUTDIR)\iter_C.mod" "$(OUTDIR)\journal_references_C.mod" "$(OUTDIR)\maps_C.mod" "$(OUTDIR)\meci_C.mod" "$(OUTDIR)\mndod_C.mod" "$(OUTDIR)\mod_atomradii.mod" "$(OUTDIR)\molkst_C.mod" "$(OUTDIR)\molmec_C.mod" "$(OUTDIR)\MOZYME_C.mod" "$(OUTDIR)\overlaps_C.mod" "$(OUTDIR)\Parameters_for_AM1_C.mod" "$(OUTDIR)\Parameters_for_AM1_Sparkles_C.mod" "$(OUTDIR)\Parameters_for_MNDO_C.mod" "$(OUTDIR)\Parameters_for_MNDOD_C.mod" "$(OUTDIR)\Parameters_for_PM3_C.mod" "$(OUTDIR)\Parameters_for_PM3_Sparkles_C.mod" "$(OUTDIR)\Parameters_for_PM6_C.mod" "$(OUTDIR)\Parameters_for_PM6_Sparkles_C.mod" "$(OUTDIR)\Parameters_for_RM1_C.mod" "$(OUTDIR)\polar_C.mod" "$(OUTDIR)\refkey_C.mod" "$(OUTDIR)\rotate_C.mod" "$(OUTDIR)\symmetry_C.mod" "$(OUTDIR)\to_screen_C.mod"


CLEAN :
	-@erase "$(INTDIR)\analyt_C.mod"
	-@erase "$(INTDIR)\analyt_C.obj"
	-@erase "$(INTDIR)\chanel_C.mod"
	-@erase "$(INTDIR)\chanel_C.obj"
	-@erase "$(INTDIR)\common_arrays_C.mod"
	-@erase "$(INTDIR)\Common_arrays_C.obj"
	-@erase "$(INTDIR)\conref_C.mod"
	-@erase "$(INTDIR)\conref_C.obj"
	-@erase "$(INTDIR)\cosmo_C.mod"
	-@erase "$(INTDIR)\cosmo_C.obj"
	-@erase "$(INTDIR)\derivs_C.mod"
	-@erase "$(INTDIR)\drc_C.mod"
	-@erase "$(INTDIR)\ef_C.mod"
	-@erase "$(INTDIR)\ef_C.obj"
	-@erase "$(INTDIR)\elemts_C.mod"
	-@erase "$(INTDIR)\elemts_C.obj"
	-@erase "$(INTDIR)\esp_C.mod"
	-@erase "$(INTDIR)\esp_C.obj"
	-@erase "$(INTDIR)\euler_C.mod"
	-@erase "$(INTDIR)\euler_C.obj"
	-@erase "$(INTDIR)\funcon_C.mod"
	-@erase "$(INTDIR)\funcon_C.obj"
	-@erase "$(INTDIR)\iter_C.mod"
	-@erase "$(INTDIR)\iter_C.obj"
	-@erase "$(INTDIR)\journal_references_C.mod"
	-@erase "$(INTDIR)\journal_references_C.obj"
	-@erase "$(INTDIR)\maps_C.mod"
	-@erase "$(INTDIR)\maps_C.obj"
	-@erase "$(INTDIR)\meci_C.mod"
	-@erase "$(INTDIR)\meci_C.obj"
	-@erase "$(INTDIR)\mndod_C.mod"
	-@erase "$(INTDIR)\mndod_C.obj"
	-@erase "$(INTDIR)\mod_atomradii.mod"
	-@erase "$(INTDIR)\mod_atomradii.obj"
	-@erase "$(INTDIR)\molkst_C.mod"
	-@erase "$(INTDIR)\molkst_C.obj"
	-@erase "$(INTDIR)\molmec_C.mod"
	-@erase "$(INTDIR)\molmec_C.obj"
	-@erase "$(INTDIR)\MOZYME_C.mod"
	-@erase "$(INTDIR)\MOZYME_C.OBJ"
	-@erase "$(INTDIR)\overlaps_C.mod"
	-@erase "$(INTDIR)\overlaps_C.obj"
	-@erase "$(INTDIR)\parameters_C.mod"
	-@erase "$(INTDIR)\parameters_C.obj"
	-@erase "$(INTDIR)\Parameters_for_AM1_C.mod"
	-@erase "$(INTDIR)\parameters_for_AM1_C.obj"
	-@erase "$(INTDIR)\Parameters_for_AM1_Sparkles_C.mod"
	-@erase "$(INTDIR)\parameters_for_AM1_Sparkles_C.obj"
	-@erase "$(INTDIR)\Parameters_for_MNDO_C.mod"
	-@erase "$(INTDIR)\parameters_for_mndo_C.obj"
	-@erase "$(INTDIR)\Parameters_for_MNDOD_C.mod"
	-@erase "$(INTDIR)\parameters_for_mndod_C.obj"
	-@erase "$(INTDIR)\Parameters_for_PM3_C.mod"
	-@erase "$(INTDIR)\parameters_for_pm3_C.obj"
	-@erase "$(INTDIR)\Parameters_for_PM3_Sparkles_C.mod"
	-@erase "$(INTDIR)\parameters_for_pm3_Sparkles_C.obj"
	-@erase "$(INTDIR)\Parameters_for_PM6_C.mod"
	-@erase "$(INTDIR)\parameters_for_PM6_C.obj"
	-@erase "$(INTDIR)\Parameters_for_PM6_Sparkles_C.mod"
	-@erase "$(INTDIR)\parameters_for_PM6_Sparkles_C.obj"
	-@erase "$(INTDIR)\Parameters_for_RM1_C.mod"
	-@erase "$(INTDIR)\parameters_for_RM1_C.obj"
	-@erase "$(INTDIR)\polar_C.mod"
	-@erase "$(INTDIR)\polar_C.obj"
	-@erase "$(INTDIR)\refkey_C.mod"
	-@erase "$(INTDIR)\refkey_C.obj"
	-@erase "$(INTDIR)\rotate_C.mod"
	-@erase "$(INTDIR)\rotate_C.obj"
	-@erase "$(INTDIR)\symmetry_C.mod"
	-@erase "$(INTDIR)\symmetry_C.obj"
	-@erase "$(INTDIR)\to_screen_C.mod"
	-@erase "$(INTDIR)\to_screen_C.obj"
	-@erase "$(INTDIR)\vast_kind_param.mod"
	-@erase "$(INTDIR)\vastkind.obj"
	-@erase "$(OUTDIR)\Modules.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=df.exe
F90_PROJ=/compile_only /include:"..\Modules\Release" /include:"..\Interfaces\Release" /nologo /traceback /warn:nofileopt /module:"Release/" /object:"Release/" 
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
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)\Modules.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Modules.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\Modules.lib" 
LIB32_OBJS= \
	"$(INTDIR)\analyt_C.obj" \
	"$(INTDIR)\chanel_C.obj" \
	"$(INTDIR)\Common_arrays_C.obj" \
	"$(INTDIR)\conref_C.obj" \
	"$(INTDIR)\cosmo_C.obj" \
	"$(INTDIR)\ef_C.obj" \
	"$(INTDIR)\elemts_C.obj" \
	"$(INTDIR)\esp_C.obj" \
	"$(INTDIR)\euler_C.obj" \
	"$(INTDIR)\funcon_C.obj" \
	"$(INTDIR)\iter_C.obj" \
	"$(INTDIR)\journal_references_C.obj" \
	"$(INTDIR)\maps_C.obj" \
	"$(INTDIR)\meci_C.obj" \
	"$(INTDIR)\mndod_C.obj" \
	"$(INTDIR)\mod_atomradii.obj" \
	"$(INTDIR)\molkst_C.obj" \
	"$(INTDIR)\molmec_C.obj" \
	"$(INTDIR)\MOZYME_C.OBJ" \
	"$(INTDIR)\overlaps_C.obj" \
	"$(INTDIR)\parameters_C.obj" \
	"$(INTDIR)\parameters_for_AM1_C.obj" \
	"$(INTDIR)\parameters_for_AM1_Sparkles_C.obj" \
	"$(INTDIR)\parameters_for_mndo_C.obj" \
	"$(INTDIR)\parameters_for_mndod_C.obj" \
	"$(INTDIR)\parameters_for_pm3_C.obj" \
	"$(INTDIR)\parameters_for_pm3_Sparkles_C.obj" \
	"$(INTDIR)\parameters_for_PM6_C.obj" \
	"$(INTDIR)\parameters_for_PM6_Sparkles_C.obj" \
	"$(INTDIR)\parameters_for_RM1_C.obj" \
	"$(INTDIR)\polar_C.obj" \
	"$(INTDIR)\refkey_C.obj" \
	"$(INTDIR)\rotate_C.obj" \
	"$(INTDIR)\symmetry_C.obj" \
	"$(INTDIR)\to_screen_C.obj" \
	"$(INTDIR)\vastkind.obj"

"$(OUTDIR)\Modules.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\Modules.lib"


CLEAN :
	-@erase "$(INTDIR)\analyt_C.obj"
	-@erase "$(INTDIR)\chanel_C.obj"
	-@erase "$(INTDIR)\Common_arrays_C.obj"
	-@erase "$(INTDIR)\conref_C.obj"
	-@erase "$(INTDIR)\cosmo_C.obj"
	-@erase "$(INTDIR)\DF60.PDB"
	-@erase "$(INTDIR)\ef_C.obj"
	-@erase "$(INTDIR)\elemts_C.obj"
	-@erase "$(INTDIR)\esp_C.obj"
	-@erase "$(INTDIR)\euler_C.obj"
	-@erase "$(INTDIR)\funcon_C.obj"
	-@erase "$(INTDIR)\iter_C.obj"
	-@erase "$(INTDIR)\journal_references_C.obj"
	-@erase "$(INTDIR)\maps_C.obj"
	-@erase "$(INTDIR)\meci_C.obj"
	-@erase "$(INTDIR)\mndod_C.obj"
	-@erase "$(INTDIR)\mod_atomradii.obj"
	-@erase "$(INTDIR)\molkst_C.obj"
	-@erase "$(INTDIR)\molmec_C.obj"
	-@erase "$(INTDIR)\MOZYME_C.OBJ"
	-@erase "$(INTDIR)\overlaps_C.obj"
	-@erase "$(INTDIR)\parameters_C.obj"
	-@erase "$(INTDIR)\parameters_for_AM1_C.obj"
	-@erase "$(INTDIR)\parameters_for_AM1_Sparkles_C.obj"
	-@erase "$(INTDIR)\parameters_for_mndo_C.obj"
	-@erase "$(INTDIR)\parameters_for_mndod_C.obj"
	-@erase "$(INTDIR)\parameters_for_pm3_C.obj"
	-@erase "$(INTDIR)\parameters_for_pm3_Sparkles_C.obj"
	-@erase "$(INTDIR)\parameters_for_PM6_C.obj"
	-@erase "$(INTDIR)\parameters_for_PM6_Sparkles_C.obj"
	-@erase "$(INTDIR)\parameters_for_RM1_C.obj"
	-@erase "$(INTDIR)\polar_C.obj"
	-@erase "$(INTDIR)\refkey_C.obj"
	-@erase "$(INTDIR)\rotate_C.obj"
	-@erase "$(INTDIR)\symmetry_C.obj"
	-@erase "$(INTDIR)\to_screen_C.obj"
	-@erase "$(INTDIR)\vastkind.obj"
	-@erase "$(OUTDIR)\Modules.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=df.exe
F90_PROJ=/check:bounds /compile_only /dbglibs /debug:full /include:"..\Modules\Debug" /traceback /warn:argument_checking /warn:nofileopt /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF60.PDB" /include:"..\Interfaces\Debug"/nologo 
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
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)\Modules.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Modules.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\Modules.lib" 
LIB32_OBJS= \
	"$(INTDIR)\analyt_C.obj" \
	"$(INTDIR)\chanel_C.obj" \
	"$(INTDIR)\Common_arrays_C.obj" \
	"$(INTDIR)\conref_C.obj" \
	"$(INTDIR)\cosmo_C.obj" \
	"$(INTDIR)\ef_C.obj" \
	"$(INTDIR)\elemts_C.obj" \
	"$(INTDIR)\esp_C.obj" \
	"$(INTDIR)\euler_C.obj" \
	"$(INTDIR)\funcon_C.obj" \
	"$(INTDIR)\iter_C.obj" \
	"$(INTDIR)\journal_references_C.obj" \
	"$(INTDIR)\maps_C.obj" \
	"$(INTDIR)\meci_C.obj" \
	"$(INTDIR)\mndod_C.obj" \
	"$(INTDIR)\mod_atomradii.obj" \
	"$(INTDIR)\molkst_C.obj" \
	"$(INTDIR)\molmec_C.obj" \
	"$(INTDIR)\MOZYME_C.OBJ" \
	"$(INTDIR)\overlaps_C.obj" \
	"$(INTDIR)\parameters_C.obj" \
	"$(INTDIR)\parameters_for_AM1_C.obj" \
	"$(INTDIR)\parameters_for_AM1_Sparkles_C.obj" \
	"$(INTDIR)\parameters_for_mndo_C.obj" \
	"$(INTDIR)\parameters_for_mndod_C.obj" \
	"$(INTDIR)\parameters_for_pm3_C.obj" \
	"$(INTDIR)\parameters_for_pm3_Sparkles_C.obj" \
	"$(INTDIR)\parameters_for_PM6_C.obj" \
	"$(INTDIR)\parameters_for_PM6_Sparkles_C.obj" \
	"$(INTDIR)\parameters_for_RM1_C.obj" \
	"$(INTDIR)\polar_C.obj" \
	"$(INTDIR)\refkey_C.obj" \
	"$(INTDIR)\rotate_C.obj" \
	"$(INTDIR)\symmetry_C.obj" \
	"$(INTDIR)\to_screen_C.obj" \
	"$(INTDIR)\vastkind.obj"

"$(OUTDIR)\Modules.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("Modules.dep")
!INCLUDE "Modules.dep"
!ELSE 
!MESSAGE Warning: cannot find "Modules.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "Modules - Win32 Release" || "$(CFG)" == "Modules - Win32 Debug"
SOURCE=..\src_modules\analyt_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"analyt_C"


"$(INTDIR)\analyt_C.obj"	"$(INTDIR)\analyt_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\analyt_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\chanel_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"chanel_C"


"$(INTDIR)\chanel_C.obj"	"$(INTDIR)\chanel_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\chanel_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\Common_arrays_C.F90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"Common_arrays_C"


"$(INTDIR)\Common_arrays_C.obj"	"$(INTDIR)\common_arrays_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\Common_arrays_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\conref_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"conref_C"


"$(INTDIR)\conref_C.obj"	"$(INTDIR)\conref_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\conref_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\cosmo_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"cosmo_C"


"$(INTDIR)\cosmo_C.obj"	"$(INTDIR)\cosmo_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\cosmo_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\ef_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"derivs_C" \
	"drc_C" \
	"ef_C"


"$(INTDIR)\ef_C.obj"	"$(INTDIR)\derivs_C.mod"	"$(INTDIR)\drc_C.mod"	"$(INTDIR)\ef_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\ef_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\elemts_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"elemts_C"


"$(INTDIR)\elemts_C.obj"	"$(INTDIR)\elemts_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\elemts_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\esp_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"esp_C"


"$(INTDIR)\esp_C.obj"	"$(INTDIR)\esp_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\esp_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\euler_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"euler_C"


"$(INTDIR)\euler_C.obj"	"$(INTDIR)\euler_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\euler_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\funcon_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"funcon_C"


"$(INTDIR)\funcon_C.obj"	"$(INTDIR)\funcon_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\funcon_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\iter_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"iter_C"


"$(INTDIR)\iter_C.obj"	"$(INTDIR)\iter_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\iter_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\journal_references_C.F90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"journal_references_C"


"$(INTDIR)\journal_references_C.obj"	"$(INTDIR)\journal_references_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\journal_references_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\maps_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"maps_C"


"$(INTDIR)\maps_C.obj"	"$(INTDIR)\maps_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\maps_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\meci_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"meci_C"


"$(INTDIR)\meci_C.obj"	"$(INTDIR)\meci_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\meci_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\mndod_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"mndod_C"


"$(INTDIR)\mndod_C.obj"	"$(INTDIR)\mndod_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\mndod_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\mod_atomradii.F90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"mod_atomradii"


"$(INTDIR)\mod_atomradii.obj"	"$(INTDIR)\mod_atomradii.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\mod_atomradii.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\molkst_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"molkst_C"


"$(INTDIR)\molkst_C.obj"	"$(INTDIR)\molkst_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\molkst_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\molmec_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"molmec_C"


"$(INTDIR)\molmec_C.obj"	"$(INTDIR)\molmec_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\molmec_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_MOZYME\MOZYME_C.F90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"MOZYME_C"


"$(INTDIR)\MOZYME_C.OBJ"	"$(INTDIR)\MOZYME_C.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\MOZYME_C.OBJ" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\overlaps_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"overlaps_C"


"$(INTDIR)\overlaps_C.obj"	"$(INTDIR)\overlaps_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\overlaps_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\parameters_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"parameters_C"


"$(INTDIR)\parameters_C.obj"	"$(INTDIR)\parameters_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\parameters_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\parameters_for_AM1_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"Parameters_for_AM1_C"


"$(INTDIR)\parameters_for_AM1_C.obj"	"$(INTDIR)\Parameters_for_AM1_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\parameters_for_AM1_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\parameters_for_AM1_Sparkles_C.F90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"Parameters_for_AM1_Sparkles_C"


"$(INTDIR)\parameters_for_AM1_Sparkles_C.obj"	"$(INTDIR)\Parameters_for_AM1_Sparkles_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\parameters_for_AM1_Sparkles_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\parameters_for_mndo_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"Parameters_for_MNDO_C"


"$(INTDIR)\parameters_for_mndo_C.obj"	"$(INTDIR)\Parameters_for_MNDO_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\parameters_for_mndo_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\parameters_for_mndod_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"Parameters_for_MNDOD_C"


"$(INTDIR)\parameters_for_mndod_C.obj"	"$(INTDIR)\Parameters_for_MNDOD_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\parameters_for_mndod_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\parameters_for_pm3_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"Parameters_for_PM3_C"


"$(INTDIR)\parameters_for_pm3_C.obj"	"$(INTDIR)\Parameters_for_PM3_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\parameters_for_pm3_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\parameters_for_pm3_Sparkles_C.F90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"Parameters_for_PM3_Sparkles_C"


"$(INTDIR)\parameters_for_pm3_Sparkles_C.obj"	"$(INTDIR)\Parameters_for_PM3_Sparkles_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\parameters_for_pm3_Sparkles_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\parameters_for_PM6_C.F90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"Parameters_for_PM6_C"


"$(INTDIR)\parameters_for_PM6_C.obj"	"$(INTDIR)\Parameters_for_PM6_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\parameters_C.mod" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\parameters_for_PM6_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\parameters_for_PM6_Sparkles_C.F90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"Parameters_for_PM6_Sparkles_C"


"$(INTDIR)\parameters_for_PM6_Sparkles_C.obj"	"$(INTDIR)\Parameters_for_PM6_Sparkles_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\parameters_for_PM6_Sparkles_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\parameters_for_RM1_C.F90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"Parameters_for_RM1_C"


"$(INTDIR)\parameters_for_RM1_C.obj"	"$(INTDIR)\Parameters_for_RM1_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\parameters_for_RM1_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\polar_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"polar_C"


"$(INTDIR)\polar_C.obj"	"$(INTDIR)\polar_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\polar_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\refkey_C.F90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"refkey_C"


"$(INTDIR)\refkey_C.obj"	"$(INTDIR)\refkey_C.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\refkey_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\rotate_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"rotate_C"


"$(INTDIR)\rotate_C.obj"	"$(INTDIR)\rotate_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\rotate_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\symmetry_C.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"symmetry_C"


"$(INTDIR)\symmetry_C.obj"	"$(INTDIR)\symmetry_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\symmetry_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\to_screen_C.F90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"to_screen_C"


"$(INTDIR)\to_screen_C.obj"	"$(INTDIR)\to_screen_C.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\to_screen_C.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_modules\vastkind.f90

!IF  "$(CFG)" == "Modules - Win32 Release"

F90_MODOUT=\
	"vast_kind_param"


"$(INTDIR)\vastkind.obj"	"$(INTDIR)\vast_kind_param.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Modules - Win32 Debug"


"$(INTDIR)\vastkind.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

