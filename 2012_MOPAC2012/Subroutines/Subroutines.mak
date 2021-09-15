# Microsoft Developer Studio Generated NMAKE File, Based on Subroutines.dsp
!IF "$(CFG)" == ""
CFG=Subroutines - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Subroutines - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Subroutines - Win32 Release" && "$(CFG)" != "Subroutines - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
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
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "Subroutines - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Subroutines.lib" "$(OUTDIR)\saxpy_I.mod" "$(OUTDIR)\scopy_I.mod" "$(OUTDIR)\sdot_I.mod" "$(OUTDIR)\snrm2_I.mod" "$(OUTDIR)\cosmo_mini.mod"

!ELSE 

ALL : "Interfaces - Win32 Release" "$(OUTDIR)\Subroutines.lib" "$(OUTDIR)\saxpy_I.mod" "$(OUTDIR)\scopy_I.mod" "$(OUTDIR)\sdot_I.mod" "$(OUTDIR)\snrm2_I.mod" "$(OUTDIR)\cosmo_mini.mod"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Interfaces - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\aababc.obj"
	-@erase "$(INTDIR)\aabacd.obj"
	-@erase "$(INTDIR)\aabbcd.obj"
	-@erase "$(INTDIR)\afmm_mod.mod"
	-@erase "$(INTDIR)\afmm_mod.obj"
	-@erase "$(INTDIR)\alpb_and_xfac_am1.obj"
	-@erase "$(INTDIR)\alpb_and_xfac_mndo.obj"
	-@erase "$(INTDIR)\alpb_and_xfac_mndod.obj"
	-@erase "$(INTDIR)\alpb_and_xfac_pm3.obj"
	-@erase "$(INTDIR)\analyt.obj"
	-@erase "$(INTDIR)\anavib.obj"
	-@erase "$(INTDIR)\axis.obj"
	-@erase "$(INTDIR)\babbbc.obj"
	-@erase "$(INTDIR)\babbcd.obj"
	-@erase "$(INTDIR)\bangle.obj"
	-@erase "$(INTDIR)\bfn.obj"
	-@erase "$(INTDIR)\blas.obj"
	-@erase "$(INTDIR)\bldsym.obj"
	-@erase "$(INTDIR)\bonds.obj"
	-@erase "$(INTDIR)\calpar.obj"
	-@erase "$(INTDIR)\capcor.obj"
	-@erase "$(INTDIR)\charmo.obj"
	-@erase "$(INTDIR)\charst.obj"
	-@erase "$(INTDIR)\charvi.obj"
	-@erase "$(INTDIR)\chrge.obj"
	-@erase "$(INTDIR)\ciosci.obj"
	-@erase "$(INTDIR)\cnvg.obj"
	-@erase "$(INTDIR)\coe.obj"
	-@erase "$(INTDIR)\compfg.obj"
	-@erase "$(INTDIR)\cosmo.obj"
	-@erase "$(INTDIR)\cosmo_mini.mod"
	-@erase "$(INTDIR)\dang.obj"
	-@erase "$(INTDIR)\datin.obj"
	-@erase "$(INTDIR)\dcart.obj"
	-@erase "$(INTDIR)\denrot.obj"
	-@erase "$(INTDIR)\densit.obj"
	-@erase "$(INTDIR)\deri0.obj"
	-@erase "$(INTDIR)\deri1.obj"
	-@erase "$(INTDIR)\deri2.obj"
	-@erase "$(INTDIR)\deri21.obj"
	-@erase "$(INTDIR)\deri22.obj"
	-@erase "$(INTDIR)\deri23.obj"
	-@erase "$(INTDIR)\deritr.obj"
	-@erase "$(INTDIR)\deriv.obj"
	-@erase "$(INTDIR)\dernvo.obj"
	-@erase "$(INTDIR)\dfield.obj"
	-@erase "$(INTDIR)\dfock2.obj"
	-@erase "$(INTDIR)\dfpsav.obj"
	-@erase "$(INTDIR)\dhc.obj"
	-@erase "$(INTDIR)\dhcore.obj"
	-@erase "$(INTDIR)\diag.obj"
	-@erase "$(INTDIR)\diagi.obj"
	-@erase "$(INTDIR)\diat.obj"
	-@erase "$(INTDIR)\digit.obj"
	-@erase "$(INTDIR)\dihed.obj"
	-@erase "$(INTDIR)\dijkl1.obj"
	-@erase "$(INTDIR)\dijkl2.obj"
	-@erase "$(INTDIR)\dimens.obj"
	-@erase "$(INTDIR)\dipole.obj"
	-@erase "$(INTDIR)\dist2.obj"
	-@erase "$(INTDIR)\dofs.obj"
	-@erase "$(INTDIR)\dot.obj"
	-@erase "$(INTDIR)\drc.obj"
	-@erase "$(INTDIR)\drcout.obj"
	-@erase "$(INTDIR)\dtran2.obj"
	-@erase "$(INTDIR)\dtrans.obj"
	-@erase "$(INTDIR)\ef.obj"
	-@erase "$(INTDIR)\enpart.obj"
	-@erase "$(INTDIR)\esp.obj"
	-@erase "$(INTDIR)\esp_utilities.obj"
	-@erase "$(INTDIR)\exchng.obj"
	-@erase "$(INTDIR)\finish.obj"
	-@erase "$(INTDIR)\flepo.obj"
	-@erase "$(INTDIR)\fmat.obj"
	-@erase "$(INTDIR)\fock1.obj"
	-@erase "$(INTDIR)\fock2.obj"
	-@erase "$(INTDIR)\force.obj"
	-@erase "$(INTDIR)\formxy.obj"
	-@erase "$(INTDIR)\forsav.obj"
	-@erase "$(INTDIR)\frame.obj"
	-@erase "$(INTDIR)\freqcy.obj"
	-@erase "$(INTDIR)\genun.obj"
	-@erase "$(INTDIR)\geout.obj"
	-@erase "$(INTDIR)\geoutg.obj"
	-@erase "$(INTDIR)\getdat.obj"
	-@erase "$(INTDIR)\GetDateStamp.obj"
	-@erase "$(INTDIR)\getgeg.obj"
	-@erase "$(INTDIR)\getgeo.obj"
	-@erase "$(INTDIR)\getsym.obj"
	-@erase "$(INTDIR)\gettxt.obj"
	-@erase "$(INTDIR)\getval.obj"
	-@erase "$(INTDIR)\gmetry.obj"
	-@erase "$(INTDIR)\grid.obj"
	-@erase "$(INTDIR)\h1elec.obj"
	-@erase "$(INTDIR)\H_bond_correction.obj"
	-@erase "$(INTDIR)\hcore.obj"
	-@erase "$(INTDIR)\helect.obj"
	-@erase "$(INTDIR)\ijkl.obj"
	-@erase "$(INTDIR)\init_filenames.obj"
	-@erase "$(INTDIR)\interp.obj"
	-@erase "$(INTDIR)\intfc.obj"
	-@erase "$(INTDIR)\iter.obj"
	-@erase "$(INTDIR)\jab.obj"
	-@erase "$(INTDIR)\jcarin.obj"
	-@erase "$(INTDIR)\kab.obj"
	-@erase "$(INTDIR)\linear_cosmo.mod"
	-@erase "$(INTDIR)\linear_cosmo.obj"
	-@erase "$(INTDIR)\linmin.obj"
	-@erase "$(INTDIR)\local.obj"
	-@erase "$(INTDIR)\locmin.obj"
	-@erase "$(INTDIR)\maksym.obj"
	-@erase "$(INTDIR)\mamult.obj"
	-@erase "$(INTDIR)\mat33.obj"
	-@erase "$(INTDIR)\matou1.obj"
	-@erase "$(INTDIR)\matout.obj"
	-@erase "$(INTDIR)\meci.obj"
	-@erase "$(INTDIR)\mecid.obj"
	-@erase "$(INTDIR)\mecih.obj"
	-@erase "$(INTDIR)\mecip.obj"
	-@erase "$(INTDIR)\minv.obj"
	-@erase "$(INTDIR)\mndod.obj"
	-@erase "$(INTDIR)\moldat.obj"
	-@erase "$(INTDIR)\molsymy.obj"
	-@erase "$(INTDIR)\molval.obj"
	-@erase "$(INTDIR)\mopend.obj"
	-@erase "$(INTDIR)\mpcsyb.obj"
	-@erase "$(INTDIR)\mtxm.obj"
	-@erase "$(INTDIR)\mtxmc.obj"
	-@erase "$(INTDIR)\mullik.obj"
	-@erase "$(INTDIR)\mult.obj"
	-@erase "$(INTDIR)\mult33.obj"
	-@erase "$(INTDIR)\mxm.obj"
	-@erase "$(INTDIR)\mxmt.obj"
	-@erase "$(INTDIR)\myword.obj"
	-@erase "$(INTDIR)\new_esp.obj"
	-@erase "$(INTDIR)\nllsq.obj"
	-@erase "$(INTDIR)\nuchar.obj"
	-@erase "$(INTDIR)\osinv.obj"
	-@erase "$(INTDIR)\parsav.obj"
	-@erase "$(INTDIR)\partxy.obj"
	-@erase "$(INTDIR)\Password.obj"
	-@erase "$(INTDIR)\pathk.obj"
	-@erase "$(INTDIR)\paths.obj"
	-@erase "$(INTDIR)\perm.obj"
	-@erase "$(INTDIR)\pmep.obj"
	-@erase "$(INTDIR)\polar.obj"
	-@erase "$(INTDIR)\powsq.obj"
	-@erase "$(INTDIR)\prtdrc.obj"
	-@erase "$(INTDIR)\prtpka.obj"
	-@erase "$(INTDIR)\prttim.obj"
	-@erase "$(INTDIR)\pulay.obj"
	-@erase "$(INTDIR)\quadr.obj"
	-@erase "$(INTDIR)\react1.obj"
	-@erase "$(INTDIR)\reada.obj"
	-@erase "$(INTDIR)\readmo.obj"
	-@erase "$(INTDIR)\refer.obj"
	-@erase "$(INTDIR)\resolv.obj"
	-@erase "$(INTDIR)\rotate.obj"
	-@erase "$(INTDIR)\rotmol.obj"
	-@erase "$(INTDIR)\rsp.obj"
	-@erase "$(INTDIR)\run_mopac.obj"
	-@erase "$(INTDIR)\saxpy_I.mod"
	-@erase "$(INTDIR)\schmib.obj"
	-@erase "$(INTDIR)\schmit.obj"
	-@erase "$(INTDIR)\scopy_I.mod"
	-@erase "$(INTDIR)\sdot_I.mod"
	-@erase "$(INTDIR)\second.obj"
	-@erase "$(INTDIR)\set.obj"
	-@erase "$(INTDIR)\set_up_dentate.obj"
	-@erase "$(INTDIR)\setup_mopac_arrays.obj"
	-@erase "$(INTDIR)\setupg.obj"
	-@erase "$(INTDIR)\snrm2_I.mod"
	-@erase "$(INTDIR)\solrot.obj"
	-@erase "$(INTDIR)\sort.obj"
	-@erase "$(INTDIR)\sp_two_electron.obj"
	-@erase "$(INTDIR)\static_polarizability.obj"
	-@erase "$(INTDIR)\supdot.obj"
	-@erase "$(INTDIR)\superd.obj"
	-@erase "$(INTDIR)\swap.obj"
	-@erase "$(INTDIR)\switch.obj"
	-@erase "$(INTDIR)\symh.obj"
	-@erase "$(INTDIR)\symoir.obj"
	-@erase "$(INTDIR)\symopr.obj"
	-@erase "$(INTDIR)\sympop.obj"
	-@erase "$(INTDIR)\symr.obj"
	-@erase "$(INTDIR)\symt.obj"
	-@erase "$(INTDIR)\symtry.obj"
	-@erase "$(INTDIR)\symtrz.obj"
	-@erase "$(INTDIR)\thermo.obj"
	-@erase "$(INTDIR)\timer.obj"
	-@erase "$(INTDIR)\timout.obj"
	-@erase "$(INTDIR)\upcase.obj"
	-@erase "$(INTDIR)\update.obj"
	-@erase "$(INTDIR)\vecprt.obj"
	-@erase "$(INTDIR)\volume.obj"
	-@erase "$(INTDIR)\write_trajectory.obj"
	-@erase "$(INTDIR)\writmo.obj"
	-@erase "$(INTDIR)\wrtkey.obj"
	-@erase "$(INTDIR)\wrttxt.obj"
	-@erase "$(INTDIR)\xxx.obj"
	-@erase "$(INTDIR)\xyzcry.obj"
	-@erase "$(INTDIR)\xyzint.obj"
	-@erase "$(OUTDIR)\Subroutines.lib"

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
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)\Subroutines.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Subroutines.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\Subroutines.lib" 
LIB32_OBJS= \
	"$(INTDIR)\aababc.obj" \
	"$(INTDIR)\aabacd.obj" \
	"$(INTDIR)\aabbcd.obj" \
	"$(INTDIR)\afmm_mod.obj" \
	"$(INTDIR)\alpb_and_xfac_am1.obj" \
	"$(INTDIR)\alpb_and_xfac_mndo.obj" \
	"$(INTDIR)\alpb_and_xfac_mndod.obj" \
	"$(INTDIR)\alpb_and_xfac_pm3.obj" \
	"$(INTDIR)\analyt.obj" \
	"$(INTDIR)\anavib.obj" \
	"$(INTDIR)\axis.obj" \
	"$(INTDIR)\babbbc.obj" \
	"$(INTDIR)\babbcd.obj" \
	"$(INTDIR)\bangle.obj" \
	"$(INTDIR)\bfn.obj" \
	"$(INTDIR)\blas.obj" \
	"$(INTDIR)\bldsym.obj" \
	"$(INTDIR)\bonds.obj" \
	"$(INTDIR)\calpar.obj" \
	"$(INTDIR)\capcor.obj" \
	"$(INTDIR)\charmo.obj" \
	"$(INTDIR)\charst.obj" \
	"$(INTDIR)\charvi.obj" \
	"$(INTDIR)\chrge.obj" \
	"$(INTDIR)\ciosci.obj" \
	"$(INTDIR)\cnvg.obj" \
	"$(INTDIR)\coe.obj" \
	"$(INTDIR)\compfg.obj" \
	"$(INTDIR)\cosmo.obj" \
	"$(INTDIR)\dang.obj" \
	"$(INTDIR)\datin.obj" \
	"$(INTDIR)\dcart.obj" \
	"$(INTDIR)\denrot.obj" \
	"$(INTDIR)\densit.obj" \
	"$(INTDIR)\deri0.obj" \
	"$(INTDIR)\deri1.obj" \
	"$(INTDIR)\deri2.obj" \
	"$(INTDIR)\deri21.obj" \
	"$(INTDIR)\deri22.obj" \
	"$(INTDIR)\deri23.obj" \
	"$(INTDIR)\deritr.obj" \
	"$(INTDIR)\deriv.obj" \
	"$(INTDIR)\dernvo.obj" \
	"$(INTDIR)\dfield.obj" \
	"$(INTDIR)\dfock2.obj" \
	"$(INTDIR)\dfpsav.obj" \
	"$(INTDIR)\dhc.obj" \
	"$(INTDIR)\dhcore.obj" \
	"$(INTDIR)\diag.obj" \
	"$(INTDIR)\diagi.obj" \
	"$(INTDIR)\diat.obj" \
	"$(INTDIR)\digit.obj" \
	"$(INTDIR)\dihed.obj" \
	"$(INTDIR)\dijkl1.obj" \
	"$(INTDIR)\dijkl2.obj" \
	"$(INTDIR)\dimens.obj" \
	"$(INTDIR)\dipole.obj" \
	"$(INTDIR)\dist2.obj" \
	"$(INTDIR)\dofs.obj" \
	"$(INTDIR)\dot.obj" \
	"$(INTDIR)\drc.obj" \
	"$(INTDIR)\drcout.obj" \
	"$(INTDIR)\dtran2.obj" \
	"$(INTDIR)\dtrans.obj" \
	"$(INTDIR)\ef.obj" \
	"$(INTDIR)\enpart.obj" \
	"$(INTDIR)\esp.obj" \
	"$(INTDIR)\esp_utilities.obj" \
	"$(INTDIR)\exchng.obj" \
	"$(INTDIR)\finish.obj" \
	"$(INTDIR)\flepo.obj" \
	"$(INTDIR)\fmat.obj" \
	"$(INTDIR)\fock1.obj" \
	"$(INTDIR)\fock2.obj" \
	"$(INTDIR)\force.obj" \
	"$(INTDIR)\formxy.obj" \
	"$(INTDIR)\forsav.obj" \
	"$(INTDIR)\frame.obj" \
	"$(INTDIR)\freqcy.obj" \
	"$(INTDIR)\genun.obj" \
	"$(INTDIR)\geout.obj" \
	"$(INTDIR)\geoutg.obj" \
	"$(INTDIR)\getdat.obj" \
	"$(INTDIR)\GetDateStamp.obj" \
	"$(INTDIR)\getgeg.obj" \
	"$(INTDIR)\getgeo.obj" \
	"$(INTDIR)\getsym.obj" \
	"$(INTDIR)\gettxt.obj" \
	"$(INTDIR)\getval.obj" \
	"$(INTDIR)\gmetry.obj" \
	"$(INTDIR)\grid.obj" \
	"$(INTDIR)\h1elec.obj" \
	"$(INTDIR)\H_bond_correction.obj" \
	"$(INTDIR)\hcore.obj" \
	"$(INTDIR)\helect.obj" \
	"$(INTDIR)\ijkl.obj" \
	"$(INTDIR)\init_filenames.obj" \
	"$(INTDIR)\interp.obj" \
	"$(INTDIR)\intfc.obj" \
	"$(INTDIR)\iter.obj" \
	"$(INTDIR)\jab.obj" \
	"$(INTDIR)\jcarin.obj" \
	"$(INTDIR)\kab.obj" \
	"$(INTDIR)\linear_cosmo.obj" \
	"$(INTDIR)\linmin.obj" \
	"$(INTDIR)\local.obj" \
	"$(INTDIR)\locmin.obj" \
	"$(INTDIR)\maksym.obj" \
	"$(INTDIR)\mamult.obj" \
	"$(INTDIR)\mat33.obj" \
	"$(INTDIR)\matou1.obj" \
	"$(INTDIR)\matout.obj" \
	"$(INTDIR)\meci.obj" \
	"$(INTDIR)\mecid.obj" \
	"$(INTDIR)\mecih.obj" \
	"$(INTDIR)\mecip.obj" \
	"$(INTDIR)\minv.obj" \
	"$(INTDIR)\mndod.obj" \
	"$(INTDIR)\moldat.obj" \
	"$(INTDIR)\molsymy.obj" \
	"$(INTDIR)\molval.obj" \
	"$(INTDIR)\mopend.obj" \
	"$(INTDIR)\mpcsyb.obj" \
	"$(INTDIR)\mtxm.obj" \
	"$(INTDIR)\mtxmc.obj" \
	"$(INTDIR)\mullik.obj" \
	"$(INTDIR)\mult.obj" \
	"$(INTDIR)\mult33.obj" \
	"$(INTDIR)\mxm.obj" \
	"$(INTDIR)\mxmt.obj" \
	"$(INTDIR)\myword.obj" \
	"$(INTDIR)\new_esp.obj" \
	"$(INTDIR)\nllsq.obj" \
	"$(INTDIR)\nuchar.obj" \
	"$(INTDIR)\osinv.obj" \
	"$(INTDIR)\parsav.obj" \
	"$(INTDIR)\partxy.obj" \
	"$(INTDIR)\Password.obj" \
	"$(INTDIR)\pathk.obj" \
	"$(INTDIR)\paths.obj" \
	"$(INTDIR)\perm.obj" \
	"$(INTDIR)\pmep.obj" \
	"$(INTDIR)\polar.obj" \
	"$(INTDIR)\powsq.obj" \
	"$(INTDIR)\prtdrc.obj" \
	"$(INTDIR)\prtpka.obj" \
	"$(INTDIR)\prttim.obj" \
	"$(INTDIR)\pulay.obj" \
	"$(INTDIR)\quadr.obj" \
	"$(INTDIR)\react1.obj" \
	"$(INTDIR)\reada.obj" \
	"$(INTDIR)\readmo.obj" \
	"$(INTDIR)\refer.obj" \
	"$(INTDIR)\resolv.obj" \
	"$(INTDIR)\rotate.obj" \
	"$(INTDIR)\rotmol.obj" \
	"$(INTDIR)\rsp.obj" \
	"$(INTDIR)\run_mopac.obj" \
	"$(INTDIR)\schmib.obj" \
	"$(INTDIR)\schmit.obj" \
	"$(INTDIR)\second.obj" \
	"$(INTDIR)\set.obj" \
	"$(INTDIR)\set_up_dentate.obj" \
	"$(INTDIR)\setup_mopac_arrays.obj" \
	"$(INTDIR)\setupg.obj" \
	"$(INTDIR)\solrot.obj" \
	"$(INTDIR)\sort.obj" \
	"$(INTDIR)\sp_two_electron.obj" \
	"$(INTDIR)\static_polarizability.obj" \
	"$(INTDIR)\supdot.obj" \
	"$(INTDIR)\superd.obj" \
	"$(INTDIR)\swap.obj" \
	"$(INTDIR)\switch.obj" \
	"$(INTDIR)\symh.obj" \
	"$(INTDIR)\symoir.obj" \
	"$(INTDIR)\symopr.obj" \
	"$(INTDIR)\sympop.obj" \
	"$(INTDIR)\symr.obj" \
	"$(INTDIR)\symt.obj" \
	"$(INTDIR)\symtry.obj" \
	"$(INTDIR)\symtrz.obj" \
	"$(INTDIR)\thermo.obj" \
	"$(INTDIR)\timer.obj" \
	"$(INTDIR)\timout.obj" \
	"$(INTDIR)\upcase.obj" \
	"$(INTDIR)\update.obj" \
	"$(INTDIR)\vecprt.obj" \
	"$(INTDIR)\volume.obj" \
	"$(INTDIR)\write_trajectory.obj" \
	"$(INTDIR)\writmo.obj" \
	"$(INTDIR)\wrtkey.obj" \
	"$(INTDIR)\wrttxt.obj" \
	"$(INTDIR)\xxx.obj" \
	"$(INTDIR)\xyzcry.obj" \
	"$(INTDIR)\xyzint.obj" \
	"..\Interfaces\Release\Interfaces.lib"

"$(OUTDIR)\Subroutines.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Subroutines - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Subroutines.lib"

!ELSE 

ALL : "Interfaces - Win32 Debug" "$(OUTDIR)\Subroutines.lib"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Interfaces - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\aababc.obj"
	-@erase "$(INTDIR)\aabacd.obj"
	-@erase "$(INTDIR)\aabbcd.obj"
	-@erase "$(INTDIR)\afmm_mod.obj"
	-@erase "$(INTDIR)\alpb_and_xfac_am1.obj"
	-@erase "$(INTDIR)\alpb_and_xfac_mndo.obj"
	-@erase "$(INTDIR)\alpb_and_xfac_mndod.obj"
	-@erase "$(INTDIR)\alpb_and_xfac_pm3.obj"
	-@erase "$(INTDIR)\analyt.obj"
	-@erase "$(INTDIR)\anavib.obj"
	-@erase "$(INTDIR)\axis.obj"
	-@erase "$(INTDIR)\babbbc.obj"
	-@erase "$(INTDIR)\babbcd.obj"
	-@erase "$(INTDIR)\bangle.obj"
	-@erase "$(INTDIR)\bfn.obj"
	-@erase "$(INTDIR)\blas.obj"
	-@erase "$(INTDIR)\bldsym.obj"
	-@erase "$(INTDIR)\bonds.obj"
	-@erase "$(INTDIR)\calpar.obj"
	-@erase "$(INTDIR)\capcor.obj"
	-@erase "$(INTDIR)\charmo.obj"
	-@erase "$(INTDIR)\charst.obj"
	-@erase "$(INTDIR)\charvi.obj"
	-@erase "$(INTDIR)\chrge.obj"
	-@erase "$(INTDIR)\ciosci.obj"
	-@erase "$(INTDIR)\cnvg.obj"
	-@erase "$(INTDIR)\coe.obj"
	-@erase "$(INTDIR)\compfg.obj"
	-@erase "$(INTDIR)\cosmo.obj"
	-@erase "$(INTDIR)\dang.obj"
	-@erase "$(INTDIR)\datin.obj"
	-@erase "$(INTDIR)\dcart.obj"
	-@erase "$(INTDIR)\denrot.obj"
	-@erase "$(INTDIR)\densit.obj"
	-@erase "$(INTDIR)\deri0.obj"
	-@erase "$(INTDIR)\deri1.obj"
	-@erase "$(INTDIR)\deri2.obj"
	-@erase "$(INTDIR)\deri21.obj"
	-@erase "$(INTDIR)\deri22.obj"
	-@erase "$(INTDIR)\deri23.obj"
	-@erase "$(INTDIR)\deritr.obj"
	-@erase "$(INTDIR)\deriv.obj"
	-@erase "$(INTDIR)\dernvo.obj"
	-@erase "$(INTDIR)\DF60.PDB"
	-@erase "$(INTDIR)\dfield.obj"
	-@erase "$(INTDIR)\dfock2.obj"
	-@erase "$(INTDIR)\dfpsav.obj"
	-@erase "$(INTDIR)\dhc.obj"
	-@erase "$(INTDIR)\dhcore.obj"
	-@erase "$(INTDIR)\diag.obj"
	-@erase "$(INTDIR)\diagi.obj"
	-@erase "$(INTDIR)\diat.obj"
	-@erase "$(INTDIR)\digit.obj"
	-@erase "$(INTDIR)\dihed.obj"
	-@erase "$(INTDIR)\dijkl1.obj"
	-@erase "$(INTDIR)\dijkl2.obj"
	-@erase "$(INTDIR)\dimens.obj"
	-@erase "$(INTDIR)\dipole.obj"
	-@erase "$(INTDIR)\dist2.obj"
	-@erase "$(INTDIR)\dofs.obj"
	-@erase "$(INTDIR)\dot.obj"
	-@erase "$(INTDIR)\drc.obj"
	-@erase "$(INTDIR)\drcout.obj"
	-@erase "$(INTDIR)\dtran2.obj"
	-@erase "$(INTDIR)\dtrans.obj"
	-@erase "$(INTDIR)\ef.obj"
	-@erase "$(INTDIR)\enpart.obj"
	-@erase "$(INTDIR)\esp.obj"
	-@erase "$(INTDIR)\esp_utilities.obj"
	-@erase "$(INTDIR)\exchng.obj"
	-@erase "$(INTDIR)\finish.obj"
	-@erase "$(INTDIR)\flepo.obj"
	-@erase "$(INTDIR)\fmat.obj"
	-@erase "$(INTDIR)\fock1.obj"
	-@erase "$(INTDIR)\fock2.obj"
	-@erase "$(INTDIR)\force.obj"
	-@erase "$(INTDIR)\formxy.obj"
	-@erase "$(INTDIR)\forsav.obj"
	-@erase "$(INTDIR)\frame.obj"
	-@erase "$(INTDIR)\freqcy.obj"
	-@erase "$(INTDIR)\genun.obj"
	-@erase "$(INTDIR)\geout.obj"
	-@erase "$(INTDIR)\geoutg.obj"
	-@erase "$(INTDIR)\getdat.obj"
	-@erase "$(INTDIR)\GetDateStamp.obj"
	-@erase "$(INTDIR)\getgeg.obj"
	-@erase "$(INTDIR)\getgeo.obj"
	-@erase "$(INTDIR)\getsym.obj"
	-@erase "$(INTDIR)\gettxt.obj"
	-@erase "$(INTDIR)\getval.obj"
	-@erase "$(INTDIR)\gmetry.obj"
	-@erase "$(INTDIR)\grid.obj"
	-@erase "$(INTDIR)\h1elec.obj"
	-@erase "$(INTDIR)\H_bond_correction.obj"
	-@erase "$(INTDIR)\hcore.obj"
	-@erase "$(INTDIR)\helect.obj"
	-@erase "$(INTDIR)\ijkl.obj"
	-@erase "$(INTDIR)\init_filenames.obj"
	-@erase "$(INTDIR)\interp.obj"
	-@erase "$(INTDIR)\intfc.obj"
	-@erase "$(INTDIR)\iter.obj"
	-@erase "$(INTDIR)\jab.obj"
	-@erase "$(INTDIR)\jcarin.obj"
	-@erase "$(INTDIR)\kab.obj"
	-@erase "$(INTDIR)\linear_cosmo.obj"
	-@erase "$(INTDIR)\linmin.obj"
	-@erase "$(INTDIR)\local.obj"
	-@erase "$(INTDIR)\locmin.obj"
	-@erase "$(INTDIR)\maksym.obj"
	-@erase "$(INTDIR)\mamult.obj"
	-@erase "$(INTDIR)\mat33.obj"
	-@erase "$(INTDIR)\matou1.obj"
	-@erase "$(INTDIR)\matout.obj"
	-@erase "$(INTDIR)\meci.obj"
	-@erase "$(INTDIR)\mecid.obj"
	-@erase "$(INTDIR)\mecih.obj"
	-@erase "$(INTDIR)\mecip.obj"
	-@erase "$(INTDIR)\minv.obj"
	-@erase "$(INTDIR)\mndod.obj"
	-@erase "$(INTDIR)\moldat.obj"
	-@erase "$(INTDIR)\molsymy.obj"
	-@erase "$(INTDIR)\molval.obj"
	-@erase "$(INTDIR)\mopend.obj"
	-@erase "$(INTDIR)\mpcsyb.obj"
	-@erase "$(INTDIR)\mtxm.obj"
	-@erase "$(INTDIR)\mtxmc.obj"
	-@erase "$(INTDIR)\mullik.obj"
	-@erase "$(INTDIR)\mult.obj"
	-@erase "$(INTDIR)\mult33.obj"
	-@erase "$(INTDIR)\mxm.obj"
	-@erase "$(INTDIR)\mxmt.obj"
	-@erase "$(INTDIR)\myword.obj"
	-@erase "$(INTDIR)\new_esp.obj"
	-@erase "$(INTDIR)\nllsq.obj"
	-@erase "$(INTDIR)\nuchar.obj"
	-@erase "$(INTDIR)\osinv.obj"
	-@erase "$(INTDIR)\parsav.obj"
	-@erase "$(INTDIR)\partxy.obj"
	-@erase "$(INTDIR)\Password.obj"
	-@erase "$(INTDIR)\pathk.obj"
	-@erase "$(INTDIR)\paths.obj"
	-@erase "$(INTDIR)\perm.obj"
	-@erase "$(INTDIR)\pmep.obj"
	-@erase "$(INTDIR)\polar.obj"
	-@erase "$(INTDIR)\powsq.obj"
	-@erase "$(INTDIR)\prtdrc.obj"
	-@erase "$(INTDIR)\prtpka.obj"
	-@erase "$(INTDIR)\prttim.obj"
	-@erase "$(INTDIR)\pulay.obj"
	-@erase "$(INTDIR)\quadr.obj"
	-@erase "$(INTDIR)\react1.obj"
	-@erase "$(INTDIR)\reada.obj"
	-@erase "$(INTDIR)\readmo.obj"
	-@erase "$(INTDIR)\refer.obj"
	-@erase "$(INTDIR)\resolv.obj"
	-@erase "$(INTDIR)\rotate.obj"
	-@erase "$(INTDIR)\rotmol.obj"
	-@erase "$(INTDIR)\rsp.obj"
	-@erase "$(INTDIR)\run_mopac.obj"
	-@erase "$(INTDIR)\schmib.obj"
	-@erase "$(INTDIR)\schmit.obj"
	-@erase "$(INTDIR)\second.obj"
	-@erase "$(INTDIR)\set.obj"
	-@erase "$(INTDIR)\set_up_dentate.obj"
	-@erase "$(INTDIR)\setup_mopac_arrays.obj"
	-@erase "$(INTDIR)\setupg.obj"
	-@erase "$(INTDIR)\solrot.obj"
	-@erase "$(INTDIR)\sort.obj"
	-@erase "$(INTDIR)\sp_two_electron.obj"
	-@erase "$(INTDIR)\static_polarizability.obj"
	-@erase "$(INTDIR)\supdot.obj"
	-@erase "$(INTDIR)\superd.obj"
	-@erase "$(INTDIR)\swap.obj"
	-@erase "$(INTDIR)\switch.obj"
	-@erase "$(INTDIR)\symh.obj"
	-@erase "$(INTDIR)\symoir.obj"
	-@erase "$(INTDIR)\symopr.obj"
	-@erase "$(INTDIR)\sympop.obj"
	-@erase "$(INTDIR)\symr.obj"
	-@erase "$(INTDIR)\symt.obj"
	-@erase "$(INTDIR)\symtry.obj"
	-@erase "$(INTDIR)\symtrz.obj"
	-@erase "$(INTDIR)\thermo.obj"
	-@erase "$(INTDIR)\timer.obj"
	-@erase "$(INTDIR)\timout.obj"
	-@erase "$(INTDIR)\upcase.obj"
	-@erase "$(INTDIR)\update.obj"
	-@erase "$(INTDIR)\vecprt.obj"
	-@erase "$(INTDIR)\volume.obj"
	-@erase "$(INTDIR)\write_trajectory.obj"
	-@erase "$(INTDIR)\writmo.obj"
	-@erase "$(INTDIR)\wrtkey.obj"
	-@erase "$(INTDIR)\wrttxt.obj"
	-@erase "$(INTDIR)\xxx.obj"
	-@erase "$(INTDIR)\xyzcry.obj"
	-@erase "$(INTDIR)\xyzint.obj"
	-@erase "$(OUTDIR)\Subroutines.lib"

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
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)\Subroutines.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Subroutines.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\Subroutines.lib" 
LIB32_OBJS= \
	"$(INTDIR)\aababc.obj" \
	"$(INTDIR)\aabacd.obj" \
	"$(INTDIR)\aabbcd.obj" \
	"$(INTDIR)\afmm_mod.obj" \
	"$(INTDIR)\alpb_and_xfac_am1.obj" \
	"$(INTDIR)\alpb_and_xfac_mndo.obj" \
	"$(INTDIR)\alpb_and_xfac_mndod.obj" \
	"$(INTDIR)\alpb_and_xfac_pm3.obj" \
	"$(INTDIR)\analyt.obj" \
	"$(INTDIR)\anavib.obj" \
	"$(INTDIR)\axis.obj" \
	"$(INTDIR)\babbbc.obj" \
	"$(INTDIR)\babbcd.obj" \
	"$(INTDIR)\bangle.obj" \
	"$(INTDIR)\bfn.obj" \
	"$(INTDIR)\blas.obj" \
	"$(INTDIR)\bldsym.obj" \
	"$(INTDIR)\bonds.obj" \
	"$(INTDIR)\calpar.obj" \
	"$(INTDIR)\capcor.obj" \
	"$(INTDIR)\charmo.obj" \
	"$(INTDIR)\charst.obj" \
	"$(INTDIR)\charvi.obj" \
	"$(INTDIR)\chrge.obj" \
	"$(INTDIR)\ciosci.obj" \
	"$(INTDIR)\cnvg.obj" \
	"$(INTDIR)\coe.obj" \
	"$(INTDIR)\compfg.obj" \
	"$(INTDIR)\cosmo.obj" \
	"$(INTDIR)\dang.obj" \
	"$(INTDIR)\datin.obj" \
	"$(INTDIR)\dcart.obj" \
	"$(INTDIR)\denrot.obj" \
	"$(INTDIR)\densit.obj" \
	"$(INTDIR)\deri0.obj" \
	"$(INTDIR)\deri1.obj" \
	"$(INTDIR)\deri2.obj" \
	"$(INTDIR)\deri21.obj" \
	"$(INTDIR)\deri22.obj" \
	"$(INTDIR)\deri23.obj" \
	"$(INTDIR)\deritr.obj" \
	"$(INTDIR)\deriv.obj" \
	"$(INTDIR)\dernvo.obj" \
	"$(INTDIR)\dfield.obj" \
	"$(INTDIR)\dfock2.obj" \
	"$(INTDIR)\dfpsav.obj" \
	"$(INTDIR)\dhc.obj" \
	"$(INTDIR)\dhcore.obj" \
	"$(INTDIR)\diag.obj" \
	"$(INTDIR)\diagi.obj" \
	"$(INTDIR)\diat.obj" \
	"$(INTDIR)\digit.obj" \
	"$(INTDIR)\dihed.obj" \
	"$(INTDIR)\dijkl1.obj" \
	"$(INTDIR)\dijkl2.obj" \
	"$(INTDIR)\dimens.obj" \
	"$(INTDIR)\dipole.obj" \
	"$(INTDIR)\dist2.obj" \
	"$(INTDIR)\dofs.obj" \
	"$(INTDIR)\dot.obj" \
	"$(INTDIR)\drc.obj" \
	"$(INTDIR)\drcout.obj" \
	"$(INTDIR)\dtran2.obj" \
	"$(INTDIR)\dtrans.obj" \
	"$(INTDIR)\ef.obj" \
	"$(INTDIR)\enpart.obj" \
	"$(INTDIR)\esp.obj" \
	"$(INTDIR)\esp_utilities.obj" \
	"$(INTDIR)\exchng.obj" \
	"$(INTDIR)\finish.obj" \
	"$(INTDIR)\flepo.obj" \
	"$(INTDIR)\fmat.obj" \
	"$(INTDIR)\fock1.obj" \
	"$(INTDIR)\fock2.obj" \
	"$(INTDIR)\force.obj" \
	"$(INTDIR)\formxy.obj" \
	"$(INTDIR)\forsav.obj" \
	"$(INTDIR)\frame.obj" \
	"$(INTDIR)\freqcy.obj" \
	"$(INTDIR)\genun.obj" \
	"$(INTDIR)\geout.obj" \
	"$(INTDIR)\geoutg.obj" \
	"$(INTDIR)\getdat.obj" \
	"$(INTDIR)\GetDateStamp.obj" \
	"$(INTDIR)\getgeg.obj" \
	"$(INTDIR)\getgeo.obj" \
	"$(INTDIR)\getsym.obj" \
	"$(INTDIR)\gettxt.obj" \
	"$(INTDIR)\getval.obj" \
	"$(INTDIR)\gmetry.obj" \
	"$(INTDIR)\grid.obj" \
	"$(INTDIR)\h1elec.obj" \
	"$(INTDIR)\H_bond_correction.obj" \
	"$(INTDIR)\hcore.obj" \
	"$(INTDIR)\helect.obj" \
	"$(INTDIR)\ijkl.obj" \
	"$(INTDIR)\init_filenames.obj" \
	"$(INTDIR)\interp.obj" \
	"$(INTDIR)\intfc.obj" \
	"$(INTDIR)\iter.obj" \
	"$(INTDIR)\jab.obj" \
	"$(INTDIR)\jcarin.obj" \
	"$(INTDIR)\kab.obj" \
	"$(INTDIR)\linear_cosmo.obj" \
	"$(INTDIR)\linmin.obj" \
	"$(INTDIR)\local.obj" \
	"$(INTDIR)\locmin.obj" \
	"$(INTDIR)\maksym.obj" \
	"$(INTDIR)\mamult.obj" \
	"$(INTDIR)\mat33.obj" \
	"$(INTDIR)\matou1.obj" \
	"$(INTDIR)\matout.obj" \
	"$(INTDIR)\meci.obj" \
	"$(INTDIR)\mecid.obj" \
	"$(INTDIR)\mecih.obj" \
	"$(INTDIR)\mecip.obj" \
	"$(INTDIR)\minv.obj" \
	"$(INTDIR)\mndod.obj" \
	"$(INTDIR)\moldat.obj" \
	"$(INTDIR)\molsymy.obj" \
	"$(INTDIR)\molval.obj" \
	"$(INTDIR)\mopend.obj" \
	"$(INTDIR)\mpcsyb.obj" \
	"$(INTDIR)\mtxm.obj" \
	"$(INTDIR)\mtxmc.obj" \
	"$(INTDIR)\mullik.obj" \
	"$(INTDIR)\mult.obj" \
	"$(INTDIR)\mult33.obj" \
	"$(INTDIR)\mxm.obj" \
	"$(INTDIR)\mxmt.obj" \
	"$(INTDIR)\myword.obj" \
	"$(INTDIR)\new_esp.obj" \
	"$(INTDIR)\nllsq.obj" \
	"$(INTDIR)\nuchar.obj" \
	"$(INTDIR)\osinv.obj" \
	"$(INTDIR)\parsav.obj" \
	"$(INTDIR)\partxy.obj" \
	"$(INTDIR)\Password.obj" \
	"$(INTDIR)\pathk.obj" \
	"$(INTDIR)\paths.obj" \
	"$(INTDIR)\perm.obj" \
	"$(INTDIR)\pmep.obj" \
	"$(INTDIR)\polar.obj" \
	"$(INTDIR)\powsq.obj" \
	"$(INTDIR)\prtdrc.obj" \
	"$(INTDIR)\prtpka.obj" \
	"$(INTDIR)\prttim.obj" \
	"$(INTDIR)\pulay.obj" \
	"$(INTDIR)\quadr.obj" \
	"$(INTDIR)\react1.obj" \
	"$(INTDIR)\reada.obj" \
	"$(INTDIR)\readmo.obj" \
	"$(INTDIR)\refer.obj" \
	"$(INTDIR)\resolv.obj" \
	"$(INTDIR)\rotate.obj" \
	"$(INTDIR)\rotmol.obj" \
	"$(INTDIR)\rsp.obj" \
	"$(INTDIR)\run_mopac.obj" \
	"$(INTDIR)\schmib.obj" \
	"$(INTDIR)\schmit.obj" \
	"$(INTDIR)\second.obj" \
	"$(INTDIR)\set.obj" \
	"$(INTDIR)\set_up_dentate.obj" \
	"$(INTDIR)\setup_mopac_arrays.obj" \
	"$(INTDIR)\setupg.obj" \
	"$(INTDIR)\solrot.obj" \
	"$(INTDIR)\sort.obj" \
	"$(INTDIR)\sp_two_electron.obj" \
	"$(INTDIR)\static_polarizability.obj" \
	"$(INTDIR)\supdot.obj" \
	"$(INTDIR)\superd.obj" \
	"$(INTDIR)\swap.obj" \
	"$(INTDIR)\switch.obj" \
	"$(INTDIR)\symh.obj" \
	"$(INTDIR)\symoir.obj" \
	"$(INTDIR)\symopr.obj" \
	"$(INTDIR)\sympop.obj" \
	"$(INTDIR)\symr.obj" \
	"$(INTDIR)\symt.obj" \
	"$(INTDIR)\symtry.obj" \
	"$(INTDIR)\symtrz.obj" \
	"$(INTDIR)\thermo.obj" \
	"$(INTDIR)\timer.obj" \
	"$(INTDIR)\timout.obj" \
	"$(INTDIR)\upcase.obj" \
	"$(INTDIR)\update.obj" \
	"$(INTDIR)\vecprt.obj" \
	"$(INTDIR)\volume.obj" \
	"$(INTDIR)\write_trajectory.obj" \
	"$(INTDIR)\writmo.obj" \
	"$(INTDIR)\wrtkey.obj" \
	"$(INTDIR)\wrttxt.obj" \
	"$(INTDIR)\xxx.obj" \
	"$(INTDIR)\xyzcry.obj" \
	"$(INTDIR)\xyzint.obj" \
	"..\Interfaces\Debug\Interfaces.lib"

"$(OUTDIR)\Subroutines.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("Subroutines.dep")
!INCLUDE "Subroutines.dep"
!ELSE 
!MESSAGE Warning: cannot find "Subroutines.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "Subroutines - Win32 Release" || "$(CFG)" == "Subroutines - Win32 Debug"
SOURCE=..\src_subroutines\aababc.F90

"$(INTDIR)\aababc.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\aabacd.F90

"$(INTDIR)\aabacd.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\aabbcd.F90

"$(INTDIR)\aabbcd.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\afmm_mod.F90

!IF  "$(CFG)" == "Subroutines - Win32 Release"

F90_MODOUT=\
	"afmm_mod"


"$(INTDIR)\afmm_mod.obj"	"$(INTDIR)\afmm_mod.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Subroutines - Win32 Debug"


"$(INTDIR)\afmm_mod.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_subroutines\alpb_and_xfac_am1.F90

"$(INTDIR)\alpb_and_xfac_am1.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\alpb_and_xfac_mndo.F90

"$(INTDIR)\alpb_and_xfac_mndo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\alpb_and_xfac_mndod.F90

"$(INTDIR)\alpb_and_xfac_mndod.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\alpb_and_xfac_pm3.F90

"$(INTDIR)\alpb_and_xfac_pm3.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\analyt.F90

"$(INTDIR)\analyt.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\anavib.F90

"$(INTDIR)\anavib.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\axis.F90

"$(INTDIR)\axis.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\babbbc.F90

"$(INTDIR)\babbbc.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\babbcd.F90

"$(INTDIR)\babbcd.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\bangle.F90

"$(INTDIR)\bangle.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\bfn.F90

"$(INTDIR)\bfn.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\blas.F90

"$(INTDIR)\blas.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\bldsym.F90

"$(INTDIR)\bldsym.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\bonds.F90

"$(INTDIR)\bonds.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\calpar.F90

"$(INTDIR)\calpar.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\capcor.F90

"$(INTDIR)\capcor.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\charmo.F90

"$(INTDIR)\charmo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\charst.F90

"$(INTDIR)\charst.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\charvi.F90

"$(INTDIR)\charvi.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\chrge.F90

"$(INTDIR)\chrge.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\ciosci.F90

"$(INTDIR)\ciosci.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\cnvg.F90

"$(INTDIR)\cnvg.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\coe.F90

"$(INTDIR)\coe.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\compfg.F90

"$(INTDIR)\compfg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\linear_cosmo.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\cosmo.F90

"$(INTDIR)\cosmo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dang.F90

"$(INTDIR)\dang.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\datin.F90

"$(INTDIR)\datin.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dcart.F90

"$(INTDIR)\dcart.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\denrot.F90

"$(INTDIR)\denrot.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\densit.F90

"$(INTDIR)\densit.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\deri0.F90

"$(INTDIR)\deri0.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\deri1.F90

"$(INTDIR)\deri1.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\deri2.F90

"$(INTDIR)\deri2.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\deri21.F90

"$(INTDIR)\deri21.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\deri22.F90

"$(INTDIR)\deri22.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\deri23.F90

"$(INTDIR)\deri23.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\deritr.F90

"$(INTDIR)\deritr.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\deriv.F90

"$(INTDIR)\deriv.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dernvo.F90

"$(INTDIR)\dernvo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dfield.F90

"$(INTDIR)\dfield.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dfock2.F90

"$(INTDIR)\dfock2.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dfpsav.F90

"$(INTDIR)\dfpsav.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dhc.F90

"$(INTDIR)\dhc.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dhcore.F90

"$(INTDIR)\dhcore.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\diag.F90

"$(INTDIR)\diag.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\diagi.F90

"$(INTDIR)\diagi.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\diat.F90

"$(INTDIR)\diat.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\digit.F90

"$(INTDIR)\digit.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dihed.F90

"$(INTDIR)\dihed.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dijkl1.F90

"$(INTDIR)\dijkl1.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dijkl2.F90

"$(INTDIR)\dijkl2.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dimens.F90

"$(INTDIR)\dimens.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dipole.F90

"$(INTDIR)\dipole.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dist2.F90

"$(INTDIR)\dist2.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dofs.F90

"$(INTDIR)\dofs.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dot.F90

"$(INTDIR)\dot.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\drc.F90

"$(INTDIR)\drc.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\drcout.F90

"$(INTDIR)\drcout.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dtran2.F90

"$(INTDIR)\dtran2.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\dtrans.F90

"$(INTDIR)\dtrans.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\ef.F90

"$(INTDIR)\ef.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\enpart.F90

"$(INTDIR)\enpart.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\esp.F90

"$(INTDIR)\esp.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\esp_utilities.F90

!IF  "$(CFG)" == "Subroutines - Win32 Release"

F90_MODOUT=\
	"saxpy_I" \
	"scopy_I" \
	"sdot_I" \
	"snrm2_I"


"$(INTDIR)\esp_utilities.obj"	"$(INTDIR)\saxpy_I.mod"	"$(INTDIR)\scopy_I.mod"	"$(INTDIR)\sdot_I.mod"	"$(INTDIR)\snrm2_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Subroutines - Win32 Debug"


"$(INTDIR)\esp_utilities.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_subroutines\exchng.F90

"$(INTDIR)\exchng.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\finish.F90

"$(INTDIR)\finish.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\flepo.F90

"$(INTDIR)\flepo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\fmat.F90

"$(INTDIR)\fmat.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\fock1.F90

"$(INTDIR)\fock1.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\fock2.F90

"$(INTDIR)\fock2.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\force.F90

"$(INTDIR)\force.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\formxy.F90

"$(INTDIR)\formxy.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\forsav.F90

"$(INTDIR)\forsav.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\frame.F90

"$(INTDIR)\frame.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\freqcy.F90

"$(INTDIR)\freqcy.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\genun.F90

"$(INTDIR)\genun.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\geout.F90

"$(INTDIR)\geout.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\geoutg.F90

"$(INTDIR)\geoutg.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\getdat.F90

"$(INTDIR)\getdat.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\GetDateStamp.F90

"$(INTDIR)\GetDateStamp.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\getgeg.F90

"$(INTDIR)\getgeg.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\getgeo.F90

"$(INTDIR)\getgeo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\getsym.F90

"$(INTDIR)\getsym.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\gettxt.F90

"$(INTDIR)\gettxt.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\getval.F90

"$(INTDIR)\getval.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\gmetry.F90

"$(INTDIR)\gmetry.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\grid.F90

"$(INTDIR)\grid.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\h1elec.F90

"$(INTDIR)\h1elec.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\H_bond_correction.F90

"$(INTDIR)\H_bond_correction.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\hcore.F90

"$(INTDIR)\hcore.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\helect.F90

"$(INTDIR)\helect.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\ijkl.F90

"$(INTDIR)\ijkl.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\init_filenames.F90

"$(INTDIR)\init_filenames.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\interp.F90

"$(INTDIR)\interp.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\intfc.F90

"$(INTDIR)\intfc.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\iter.F90

"$(INTDIR)\iter.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\jab.F90

"$(INTDIR)\jab.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\jcarin.F90

"$(INTDIR)\jcarin.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\kab.F90

"$(INTDIR)\kab.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_MOZYME\linear_cosmo.F90

!IF  "$(CFG)" == "Subroutines - Win32 Release"

F90_MODOUT=\
	"cosmo_mini" \
	"linear_cosmo"


"$(INTDIR)\linear_cosmo.obj"	"$(INTDIR)\cosmo_mini.mod"	"$(INTDIR)\linear_cosmo.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\afmm_mod.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Subroutines - Win32 Debug"


"$(INTDIR)\linear_cosmo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_subroutines\linmin.F90

"$(INTDIR)\linmin.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\local.F90

"$(INTDIR)\local.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\locmin.F90

"$(INTDIR)\locmin.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\maksym.F90

"$(INTDIR)\maksym.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mamult.F90

"$(INTDIR)\mamult.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mat33.F90

"$(INTDIR)\mat33.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\matou1.F90

"$(INTDIR)\matou1.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\matout.F90

"$(INTDIR)\matout.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\meci.F90

"$(INTDIR)\meci.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mecid.F90

"$(INTDIR)\mecid.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mecih.F90

"$(INTDIR)\mecih.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mecip.F90

"$(INTDIR)\mecip.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\minv.F90

"$(INTDIR)\minv.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mndod.F90

"$(INTDIR)\mndod.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\moldat.F90

"$(INTDIR)\moldat.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\molsymy.F90

"$(INTDIR)\molsymy.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\molval.F90

"$(INTDIR)\molval.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mopend.F90

"$(INTDIR)\mopend.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mpcsyb.f90

"$(INTDIR)\mpcsyb.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mtxm.F90

"$(INTDIR)\mtxm.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mtxmc.F90

"$(INTDIR)\mtxmc.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mullik.F90

"$(INTDIR)\mullik.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mult.F90

"$(INTDIR)\mult.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mult33.F90

"$(INTDIR)\mult33.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mxm.F90

"$(INTDIR)\mxm.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\mxmt.F90

"$(INTDIR)\mxmt.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\myword.F90

"$(INTDIR)\myword.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\new_esp.F90

"$(INTDIR)\new_esp.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\nllsq.F90

"$(INTDIR)\nllsq.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\nuchar.F90

"$(INTDIR)\nuchar.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\osinv.F90

"$(INTDIR)\osinv.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\parsav.F90

"$(INTDIR)\parsav.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\partxy.F90

"$(INTDIR)\partxy.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\Password.f90

"$(INTDIR)\Password.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\pathk.F90

"$(INTDIR)\pathk.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\paths.F90

"$(INTDIR)\paths.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\perm.F90

"$(INTDIR)\perm.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\pmep.F90

"$(INTDIR)\pmep.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\polar.F90

"$(INTDIR)\polar.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\powsq.F90

"$(INTDIR)\powsq.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\prtdrc.F90

"$(INTDIR)\prtdrc.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\prtpka.F90

"$(INTDIR)\prtpka.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\linear_cosmo.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\prttim.F90

"$(INTDIR)\prttim.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\pulay.F90

"$(INTDIR)\pulay.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\quadr.F90

"$(INTDIR)\quadr.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\react1.F90

"$(INTDIR)\react1.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\reada.F90

"$(INTDIR)\reada.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\readmo.F90

"$(INTDIR)\readmo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\refer.F90

"$(INTDIR)\refer.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\resolv.F90

"$(INTDIR)\resolv.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\rotate.F90

"$(INTDIR)\rotate.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\rotmol.F90

"$(INTDIR)\rotmol.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\rsp.F90

"$(INTDIR)\rsp.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\run_mopac.F90

"$(INTDIR)\run_mopac.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\schmib.F90

"$(INTDIR)\schmib.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\schmit.F90

"$(INTDIR)\schmit.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\second.F90

"$(INTDIR)\second.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\set.F90

"$(INTDIR)\set.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\set_up_dentate.F90

"$(INTDIR)\set_up_dentate.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\setup_mopac_arrays.F90

"$(INTDIR)\setup_mopac_arrays.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\setupg.F90

"$(INTDIR)\setupg.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\solrot.F90

"$(INTDIR)\solrot.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\sort.F90

"$(INTDIR)\sort.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\sp_two_electron.f90

"$(INTDIR)\sp_two_electron.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\static_polarizability.f90

"$(INTDIR)\static_polarizability.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\supdot.F90

"$(INTDIR)\supdot.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\superd.F90

"$(INTDIR)\superd.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\swap.F90

"$(INTDIR)\swap.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\switch.F90

"$(INTDIR)\switch.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\symh.F90

"$(INTDIR)\symh.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\symoir.F90

"$(INTDIR)\symoir.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\symopr.F90

"$(INTDIR)\symopr.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\sympop.F90

"$(INTDIR)\sympop.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\symr.F90

"$(INTDIR)\symr.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\symt.F90

"$(INTDIR)\symt.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\symtry.F90

"$(INTDIR)\symtry.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\symtrz.F90

"$(INTDIR)\symtrz.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\thermo.F90

"$(INTDIR)\thermo.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\timer.F90

"$(INTDIR)\timer.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\timout.F90

"$(INTDIR)\timout.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\upcase.F90

"$(INTDIR)\upcase.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\update.F90

"$(INTDIR)\update.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\vecprt.F90

"$(INTDIR)\vecprt.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\volume.F90

"$(INTDIR)\volume.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\write_trajectory.F90

"$(INTDIR)\write_trajectory.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\writmo.F90

"$(INTDIR)\writmo.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\linear_cosmo.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\wrtkey.F90

"$(INTDIR)\wrtkey.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\wrttxt.F90

"$(INTDIR)\wrttxt.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\xxx.F90

"$(INTDIR)\xxx.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\xyzcry.F90

"$(INTDIR)\xyzcry.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


SOURCE=..\src_subroutines\xyzint.F90

"$(INTDIR)\xyzint.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!IF  "$(CFG)" == "Subroutines - Win32 Release"

"Interfaces - Win32 Release" : 
   cd "\Software\mopac2009\Interfaces"
   $(MAKE) /$(MAKEFLAGS) /F .\Interfaces.mak CFG="Interfaces - Win32 Release" 
   cd "..\Subroutines"

"Interfaces - Win32 ReleaseCLEAN" : 
   cd "\Software\mopac2009\Interfaces"
   $(MAKE) /$(MAKEFLAGS) /F .\Interfaces.mak CFG="Interfaces - Win32 Release" RECURSE=1 CLEAN 
   cd "..\Subroutines"

!ELSEIF  "$(CFG)" == "Subroutines - Win32 Debug"

"Interfaces - Win32 Debug" : 
   cd "\Software\mopac2009\Interfaces"
   $(MAKE) /$(MAKEFLAGS) /F .\Interfaces.mak CFG="Interfaces - Win32 Debug" 
   cd "..\Subroutines"

"Interfaces - Win32 DebugCLEAN" : 
   cd "\Software\mopac2009\Interfaces"
   $(MAKE) /$(MAKEFLAGS) /F .\Interfaces.mak CFG="Interfaces - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\Subroutines"

!ENDIF 


!ENDIF 

