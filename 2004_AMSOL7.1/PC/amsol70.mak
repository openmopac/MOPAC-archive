# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=amsol70 - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to amsol70 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "amsol70 - Win32 Release" && "$(CFG)" !=\
 "amsol70 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "amsol70.mak" CFG="amsol70 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "amsol70 - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "amsol70 - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "amsol70 - Win32 Debug"
RSC=rc.exe
F90=fl32.exe

!IF  "$(CFG)" == "amsol70 - Win32 Release"

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
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\amsol70.exe"

CLEAN : 
	-@erase ".\Release\amsol70.exe"
	-@erase ".\Release\mform.obj"
	-@erase ".\Release\amstat.obj"
	-@erase ".\Release\jcarin.obj"
	-@erase ".\Release\diprnt.obj"
	-@erase ".\Release\trust6.obj"
	-@erase ".\Release\col.obj"
	-@erase ".\Release\dfock2.obj"
	-@erase ".\Release\grid.obj"
	-@erase ".\Release\convck.obj"
	-@erase ".\Release\powell.obj"
	-@erase ".\Release\sm5hb.obj"
	-@erase ".\Release\ssifa.obj"
	-@erase ".\Release\vecred.obj"
	-@erase ".\Release\chgmp1.obj"
	-@erase ".\Release\sasard.obj"
	-@erase ".\Release\formd.obj"
	-@erase ".\Release\vecwrt.obj"
	-@erase ".\Release\ciparm.obj"
	-@erase ".\Release\scopy.obj"
	-@erase ".\Release\srfcal.obj"
	-@erase ".\Release\readif.obj"
	-@erase ".\Release\dhcore.obj"
	-@erase ".\Release\dijkl2.obj"
	-@erase ".\Release\am1.obj"
	-@erase ".\Release\paths.obj"
	-@erase ".\Release\gbscrf.obj"
	-@erase ".\Release\enpart.obj"
	-@erase ".\Release\aabbcd.obj"
	-@erase ".\Release\ebrrec.obj"
	-@erase ".\Release\writes.obj"
	-@erase ".\Release\mxp.obj"
	-@erase ".\Release\findkw.obj"
	-@erase ".\Release\tql2.obj"
	-@erase ".\Release\resinp.obj"
	-@erase ".\Release\trust1.obj"
	-@erase ".\Release\fock1.obj"
	-@erase ".\Release\ismax.obj"
	-@erase ".\Release\nllsq.obj"
	-@erase ".\Release\wrtkey.obj"
	-@erase ".\Release\hqrii.obj"
	-@erase ".\Release\pqtkl.obj"
	-@erase ".\Release\dipole.obj"
	-@erase ".\Release\loadin.obj"
	-@erase ".\Release\drot.obj"
	-@erase ".\Release\keyflg.obj"
	-@erase ".\Release\prthes.obj"
	-@erase ".\Release\rabiot.obj"
	-@erase ".\Release\deri23.obj"
	-@erase ".\Release\saxpy.obj"
	-@erase ".\Release\jincar.obj"
	-@erase ".\Release\uncano.obj"
	-@erase ".\Release\deri2.obj"
	-@erase ".\Release\linmin.obj"
	-@erase ".\Release\svopts.obj"
	-@erase ".\Release\solrot.obj"
	-@erase ".\Release\status.obj"
	-@erase ".\Release\update.obj"
	-@erase ".\Release\mxt.obj"
	-@erase ".\Release\geout.obj"
	-@erase ".\Release\reads.obj"
	-@erase ".\Release\depvar.obj"
	-@erase ".\Release\trust5.obj"
	-@erase ".\Release\bornpl.obj"
	-@erase ".\Release\dswap.obj"
	-@erase ".\Release\dhc.obj"
	-@erase ".\Release\deriv.obj"
	-@erase ".\Release\trustg.obj"
	-@erase ".\Release\smsshb.obj"
	-@erase ".\Release\ecrit.obj"
	-@erase ".\Release\spline.obj"
	-@erase ".\Release\meci.obj"
	-@erase ".\Release\ef_lib.obj"
	-@erase ".\Release\osinv.obj"
	-@erase ".\Release\denrot.obj"
	-@erase ".\Release\point3.obj"
	-@erase ".\Release\diag2.obj"
	-@erase ".\Release\perm.obj"
	-@erase ".\Release\chain.obj"
	-@erase ".\Release\supdot.obj"
	-@erase ".\Release\dijkl1.obj"
	-@erase ".\Release\dfpsav.obj"
	-@erase ".\Release\matout.obj"
	-@erase ".\Release\sm4hb.obj"
	-@erase ".\Release\coulrd.obj"
	-@erase ".\Release\readch.obj"
	-@erase ".\Release\zmprnt.obj"
	-@erase ".\Release\forsav.obj"
	-@erase ".\Release\racsmo.obj"
	-@erase ".\Release\dbrnpl.obj"
	-@erase ".\Release\ijkl.obj"
	-@erase ".\Release\tql1.obj"
	-@erase ".\Release\readvt.obj"
	-@erase ".\Release\react1.obj"
	-@erase ".\Release\mullik.obj"
	-@erase ".\Release\ef.obj"
	-@erase ".\Release\mecip.obj"
	-@erase ".\Release\frame.obj"
	-@erase ".\Release\path2.obj"
	-@erase ".\Release\aabacd.obj"
	-@erase ".\Release\h1elec.obj"
	-@erase ".\Release\ebrglq.obj"
	-@erase ".\Release\patham.obj"
	-@erase ".\Release\hfadd.obj"
	-@erase ".\Release\bfn.obj"
	-@erase ".\Release\tpchk.obj"
	-@erase ".\Release\arcch.obj"
	-@erase ".\Release\deri22.obj"
	-@erase ".\Release\ampac_unmod.obj"
	-@erase ".\Release\areah.obj"
	-@erase ".\Release\powsq.obj"
	-@erase ".\Release\kwnono.obj"
	-@erase ".\Release\deri1.obj"
	-@erase ".\Release\calpar.obj"
	-@erase ".\Release\util_unmod.obj"
	-@erase ".\Release\osm5sp.obj"
	-@erase ".\Release\getchg.obj"
	-@erase ".\Release\capcnv.obj"
	-@erase ".\Release\flepo.obj"
	-@erase ".\Release\drc.obj"
	-@erase ".\Release\fmat.obj"
	-@erase ".\Release\mamlt2.obj"
	-@erase ".\Release\trust4.obj"
	-@erase ".\Release\pythag.obj"
	-@erase ".\Release\locmin.obj"
	-@erase ".\Release\main.obj"
	-@erase ".\Release\mxmlp.obj"
	-@erase ".\Release\finds.obj"
	-@erase ".\Release\set.obj"
	-@erase ".\Release\efsav.obj"
	-@erase ".\Release\datesv.obj"
	-@erase ".\Release\stpjob.obj"
	-@erase ".\Release\simpt.obj"
	-@erase ".\Release\diagiv.obj"
	-@erase ".\Release\hcore.obj"
	-@erase ".\Release\areal.obj"
	-@erase ".\Release\stlfac.obj"
	-@erase ".\Release\anavib.obj"
	-@erase ".\Release\linsum.obj"
	-@erase ".\Release\cct1do.obj"
	-@erase ".\Release\sdot.obj"
	-@erase ".\Release\dot.obj"
	-@erase ".\Release\epslon.obj"
	-@erase ".\Release\updhes.obj"
	-@erase ".\Release\ss.obj"
	-@erase ".\Release\interp.obj"
	-@erase ".\Release\sasado.obj"
	-@erase ".\Release\cm1drv.obj"
	-@erase ".\Release\sm1ara.obj"
	-@erase ".\Release\amsel.obj"
	-@erase ".\Release\polar.obj"
	-@erase ".\Release\dotmk.obj"
	-@erase ".\Release\satbat.obj"
	-@erase ".\Release\efstr.obj"
	-@erase ".\Release\ebrtrp.obj"
	-@erase ".\Release\path1.obj"
	-@erase ".\Release\tred2.obj"
	-@erase ".\Release\cdscal.obj"
	-@erase ".\Release\compfg.obj"
	-@erase ".\Release\freqcy.obj"
	-@erase ".\Release\chgmp3.obj"
	-@erase ".\Release\deri21.obj"
	-@erase ".\Release\dsm5hb.obj"
	-@erase ".\Release\prarea.obj"
	-@erase ".\Release\mtxm.obj"
	-@erase ".\Release\haddon.obj"
	-@erase ".\Release\diat2.obj"
	-@erase ".\Release\iter.obj"
	-@erase ".\Release\calvk.obj"
	-@erase ".\Release\cisgen.obj"
	-@erase ".\Release\deri0.obj"
	-@erase ".\Release\dareal.obj"
	-@erase ".\Release\rotate.obj"
	-@erase ".\Release\force.obj"
	-@erase ".\Release\extsmy.obj"
	-@erase ".\Release\quadri.obj"
	-@erase ".\Release\selbel.obj"
	-@erase ".\Release\efcres.obj"
	-@erase ".\Release\findky.obj"
	-@erase ".\Release\overlp.obj"
	-@erase ".\Release\sterms.obj"
	-@erase ".\Release\trust3.obj"
	-@erase ".\Release\thermo.obj"
	-@erase ".\Release\smhb.obj"
	-@erase ".\Release\polenm.obj"
	-@erase ".\Release\porcpu.obj"
	-@erase ".\Release\densit.obj"
	-@erase ".\Release\cardan.obj"
	-@erase ".\Release\dpqtkl.obj"
	-@erase ".\Release\truste.obj"
	-@erase ".\Release\stat1.obj"
	-@erase ".\Release\point1.obj"
	-@erase ".\Release\diag.obj"
	-@erase ".\Release\ltrd.obj"
	-@erase ".\Release\getsym.obj"
	-@erase ".\Release\tqlrat.obj"
	-@erase ".\Release\dcart2.obj"
	-@erase ".\Release\amsol.obj"
	-@erase ".\Release\echowd.obj"
	-@erase ".\Release\gmetry.obj"
	-@erase ".\Release\xyzint.obj"
	-@erase ".\Release\header.obj"
	-@erase ".\Release\fock2d.obj"
	-@erase ".\Release\local.obj"
	-@erase ".\Release\tred1.obj"
	-@erase ".\Release\pulay.obj"
	-@erase ".\Release\gethes.obj"
	-@erase ".\Release\babbbc.obj"
	-@erase ".\Release\densts.obj"
	-@erase ".\Release\chgmp2.obj"
	-@erase ".\Release\rs.obj"
	-@erase ".\Release\ebrpd.obj"
	-@erase ".\Release\ssidi.obj"
	-@erase ".\Release\moldat.obj"
	-@erase ".\Release\vecprt.obj"
	-@erase ".\Release\axis.obj"
	-@erase ".\Release\srfcty.obj"
	-@erase ".\Release\extsmx.obj"
	-@erase ".\Release\aababc.obj"
	-@erase ".\Release\ldata.obj"
	-@erase ".\Release\intcar.obj"
	-@erase ".\Release\fock.obj"
	-@erase ".\Release\loadsp.obj"
	-@erase ".\Release\trust2.obj"
	-@erase ".\Release\zpe.obj"
	-@erase ".\Release\fock2.obj"
	-@erase ".\Release\calcbp.obj"
	-@erase ".\Release\spaout.obj"
	-@erase ".\Release\dcart.obj"
	-@erase ".\Release\kwadd.obj"
	-@erase ".\Release\bonds.obj"
	-@erase ".\Release\kwrm.obj"
	-@erase ".\Release\uhfprt.obj"
	-@erase ".\Release\smx1.obj"
	-@erase ".\Release\search.obj"
	-@erase ".\Release\sm5rhb.obj"
	-@erase ".\Release\kfinit.obj"
	-@erase ".\Release\getgeo.obj"
	-@erase ".\Release\cnvg2.obj"
	-@erase ".\Release\chrge.obj"
	-@erase ".\Release\powel1.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "Release/" /c /nologo
# ADD F90 /W0 /I "Release/" /c /nologo
# SUBTRACT F90 /Ox /FR
F90_PROJ=/W0 /I "Release/" /c /nologo /Fo"Release/" 
F90_OBJS=.\Release/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/amsol70.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/amsol70.pdb" /machine:I386 /out:"$(OUTDIR)/amsol70.exe" 
LINK32_OBJS= \
	"$(INTDIR)/mform.obj" \
	"$(INTDIR)/amstat.obj" \
	"$(INTDIR)/jcarin.obj" \
	"$(INTDIR)/diprnt.obj" \
	"$(INTDIR)/trust6.obj" \
	"$(INTDIR)/col.obj" \
	"$(INTDIR)/dfock2.obj" \
	"$(INTDIR)/grid.obj" \
	"$(INTDIR)/convck.obj" \
	"$(INTDIR)/powell.obj" \
	"$(INTDIR)/sm5hb.obj" \
	"$(INTDIR)/ssifa.obj" \
	"$(INTDIR)/vecred.obj" \
	"$(INTDIR)/chgmp1.obj" \
	"$(INTDIR)/sasard.obj" \
	"$(INTDIR)/formd.obj" \
	"$(INTDIR)/vecwrt.obj" \
	"$(INTDIR)/ciparm.obj" \
	"$(INTDIR)/scopy.obj" \
	"$(INTDIR)/srfcal.obj" \
	"$(INTDIR)/readif.obj" \
	"$(INTDIR)/dhcore.obj" \
	"$(INTDIR)/dijkl2.obj" \
	"$(INTDIR)/am1.obj" \
	"$(INTDIR)/paths.obj" \
	"$(INTDIR)/gbscrf.obj" \
	"$(INTDIR)/enpart.obj" \
	"$(INTDIR)/aabbcd.obj" \
	"$(INTDIR)/ebrrec.obj" \
	"$(INTDIR)/writes.obj" \
	"$(INTDIR)/mxp.obj" \
	"$(INTDIR)/findkw.obj" \
	"$(INTDIR)/tql2.obj" \
	"$(INTDIR)/resinp.obj" \
	"$(INTDIR)/trust1.obj" \
	"$(INTDIR)/fock1.obj" \
	"$(INTDIR)/ismax.obj" \
	"$(INTDIR)/nllsq.obj" \
	"$(INTDIR)/wrtkey.obj" \
	"$(INTDIR)/hqrii.obj" \
	"$(INTDIR)/pqtkl.obj" \
	"$(INTDIR)/dipole.obj" \
	"$(INTDIR)/loadin.obj" \
	"$(INTDIR)/drot.obj" \
	"$(INTDIR)/keyflg.obj" \
	"$(INTDIR)/prthes.obj" \
	"$(INTDIR)/rabiot.obj" \
	"$(INTDIR)/deri23.obj" \
	"$(INTDIR)/saxpy.obj" \
	"$(INTDIR)/jincar.obj" \
	"$(INTDIR)/uncano.obj" \
	"$(INTDIR)/deri2.obj" \
	"$(INTDIR)/linmin.obj" \
	"$(INTDIR)/svopts.obj" \
	"$(INTDIR)/solrot.obj" \
	"$(INTDIR)/status.obj" \
	"$(INTDIR)/update.obj" \
	"$(INTDIR)/mxt.obj" \
	"$(INTDIR)/geout.obj" \
	"$(INTDIR)/reads.obj" \
	"$(INTDIR)/depvar.obj" \
	"$(INTDIR)/trust5.obj" \
	"$(INTDIR)/bornpl.obj" \
	"$(INTDIR)/dswap.obj" \
	"$(INTDIR)/dhc.obj" \
	"$(INTDIR)/deriv.obj" \
	"$(INTDIR)/trustg.obj" \
	"$(INTDIR)/smsshb.obj" \
	"$(INTDIR)/ecrit.obj" \
	"$(INTDIR)/spline.obj" \
	"$(INTDIR)/meci.obj" \
	"$(INTDIR)/ef_lib.obj" \
	"$(INTDIR)/osinv.obj" \
	"$(INTDIR)/denrot.obj" \
	"$(INTDIR)/point3.obj" \
	"$(INTDIR)/diag2.obj" \
	"$(INTDIR)/perm.obj" \
	"$(INTDIR)/chain.obj" \
	"$(INTDIR)/supdot.obj" \
	"$(INTDIR)/dijkl1.obj" \
	"$(INTDIR)/dfpsav.obj" \
	"$(INTDIR)/matout.obj" \
	"$(INTDIR)/sm4hb.obj" \
	"$(INTDIR)/coulrd.obj" \
	"$(INTDIR)/readch.obj" \
	"$(INTDIR)/zmprnt.obj" \
	"$(INTDIR)/forsav.obj" \
	"$(INTDIR)/racsmo.obj" \
	"$(INTDIR)/dbrnpl.obj" \
	"$(INTDIR)/ijkl.obj" \
	"$(INTDIR)/tql1.obj" \
	"$(INTDIR)/readvt.obj" \
	"$(INTDIR)/react1.obj" \
	"$(INTDIR)/mullik.obj" \
	"$(INTDIR)/ef.obj" \
	"$(INTDIR)/mecip.obj" \
	"$(INTDIR)/frame.obj" \
	"$(INTDIR)/path2.obj" \
	"$(INTDIR)/aabacd.obj" \
	"$(INTDIR)/h1elec.obj" \
	"$(INTDIR)/ebrglq.obj" \
	"$(INTDIR)/patham.obj" \
	"$(INTDIR)/hfadd.obj" \
	"$(INTDIR)/bfn.obj" \
	"$(INTDIR)/tpchk.obj" \
	"$(INTDIR)/arcch.obj" \
	"$(INTDIR)/deri22.obj" \
	"$(INTDIR)/ampac_unmod.obj" \
	"$(INTDIR)/areah.obj" \
	"$(INTDIR)/powsq.obj" \
	"$(INTDIR)/kwnono.obj" \
	"$(INTDIR)/deri1.obj" \
	"$(INTDIR)/calpar.obj" \
	"$(INTDIR)/util_unmod.obj" \
	"$(INTDIR)/osm5sp.obj" \
	"$(INTDIR)/getchg.obj" \
	"$(INTDIR)/capcnv.obj" \
	"$(INTDIR)/flepo.obj" \
	"$(INTDIR)/drc.obj" \
	"$(INTDIR)/fmat.obj" \
	"$(INTDIR)/mamlt2.obj" \
	"$(INTDIR)/trust4.obj" \
	"$(INTDIR)/pythag.obj" \
	"$(INTDIR)/locmin.obj" \
	"$(INTDIR)/main.obj" \
	"$(INTDIR)/mxmlp.obj" \
	"$(INTDIR)/finds.obj" \
	"$(INTDIR)/set.obj" \
	"$(INTDIR)/efsav.obj" \
	"$(INTDIR)/datesv.obj" \
	"$(INTDIR)/stpjob.obj" \
	"$(INTDIR)/simpt.obj" \
	"$(INTDIR)/diagiv.obj" \
	"$(INTDIR)/hcore.obj" \
	"$(INTDIR)/areal.obj" \
	"$(INTDIR)/stlfac.obj" \
	"$(INTDIR)/anavib.obj" \
	"$(INTDIR)/linsum.obj" \
	"$(INTDIR)/cct1do.obj" \
	"$(INTDIR)/sdot.obj" \
	"$(INTDIR)/dot.obj" \
	"$(INTDIR)/epslon.obj" \
	"$(INTDIR)/updhes.obj" \
	"$(INTDIR)/ss.obj" \
	"$(INTDIR)/interp.obj" \
	"$(INTDIR)/sasado.obj" \
	"$(INTDIR)/cm1drv.obj" \
	"$(INTDIR)/sm1ara.obj" \
	"$(INTDIR)/amsel.obj" \
	"$(INTDIR)/polar.obj" \
	"$(INTDIR)/dotmk.obj" \
	"$(INTDIR)/satbat.obj" \
	"$(INTDIR)/efstr.obj" \
	"$(INTDIR)/ebrtrp.obj" \
	"$(INTDIR)/path1.obj" \
	"$(INTDIR)/tred2.obj" \
	"$(INTDIR)/cdscal.obj" \
	"$(INTDIR)/compfg.obj" \
	"$(INTDIR)/freqcy.obj" \
	"$(INTDIR)/chgmp3.obj" \
	"$(INTDIR)/deri21.obj" \
	"$(INTDIR)/dsm5hb.obj" \
	"$(INTDIR)/prarea.obj" \
	"$(INTDIR)/mtxm.obj" \
	"$(INTDIR)/haddon.obj" \
	"$(INTDIR)/diat2.obj" \
	"$(INTDIR)/iter.obj" \
	"$(INTDIR)/calvk.obj" \
	"$(INTDIR)/cisgen.obj" \
	"$(INTDIR)/deri0.obj" \
	"$(INTDIR)/dareal.obj" \
	"$(INTDIR)/rotate.obj" \
	"$(INTDIR)/force.obj" \
	"$(INTDIR)/extsmy.obj" \
	"$(INTDIR)/quadri.obj" \
	"$(INTDIR)/selbel.obj" \
	"$(INTDIR)/efcres.obj" \
	"$(INTDIR)/findky.obj" \
	"$(INTDIR)/overlp.obj" \
	"$(INTDIR)/sterms.obj" \
	"$(INTDIR)/trust3.obj" \
	"$(INTDIR)/thermo.obj" \
	"$(INTDIR)/smhb.obj" \
	"$(INTDIR)/polenm.obj" \
	"$(INTDIR)/porcpu.obj" \
	"$(INTDIR)/densit.obj" \
	"$(INTDIR)/cardan.obj" \
	"$(INTDIR)/dpqtkl.obj" \
	"$(INTDIR)/truste.obj" \
	"$(INTDIR)/stat1.obj" \
	"$(INTDIR)/point1.obj" \
	"$(INTDIR)/diag.obj" \
	"$(INTDIR)/ltrd.obj" \
	"$(INTDIR)/getsym.obj" \
	"$(INTDIR)/tqlrat.obj" \
	"$(INTDIR)/dcart2.obj" \
	"$(INTDIR)/amsol.obj" \
	"$(INTDIR)/echowd.obj" \
	"$(INTDIR)/gmetry.obj" \
	"$(INTDIR)/xyzint.obj" \
	"$(INTDIR)/header.obj" \
	"$(INTDIR)/fock2d.obj" \
	"$(INTDIR)/local.obj" \
	"$(INTDIR)/tred1.obj" \
	"$(INTDIR)/pulay.obj" \
	"$(INTDIR)/gethes.obj" \
	"$(INTDIR)/babbbc.obj" \
	"$(INTDIR)/densts.obj" \
	"$(INTDIR)/chgmp2.obj" \
	"$(INTDIR)/rs.obj" \
	"$(INTDIR)/ebrpd.obj" \
	"$(INTDIR)/ssidi.obj" \
	"$(INTDIR)/moldat.obj" \
	"$(INTDIR)/vecprt.obj" \
	"$(INTDIR)/axis.obj" \
	"$(INTDIR)/srfcty.obj" \
	"$(INTDIR)/extsmx.obj" \
	"$(INTDIR)/aababc.obj" \
	"$(INTDIR)/ldata.obj" \
	"$(INTDIR)/intcar.obj" \
	"$(INTDIR)/fock.obj" \
	"$(INTDIR)/loadsp.obj" \
	"$(INTDIR)/trust2.obj" \
	"$(INTDIR)/zpe.obj" \
	"$(INTDIR)/fock2.obj" \
	"$(INTDIR)/calcbp.obj" \
	"$(INTDIR)/spaout.obj" \
	"$(INTDIR)/dcart.obj" \
	"$(INTDIR)/kwadd.obj" \
	"$(INTDIR)/bonds.obj" \
	"$(INTDIR)/kwrm.obj" \
	"$(INTDIR)/uhfprt.obj" \
	"$(INTDIR)/smx1.obj" \
	"$(INTDIR)/search.obj" \
	"$(INTDIR)/sm5rhb.obj" \
	"$(INTDIR)/kfinit.obj" \
	"$(INTDIR)/getgeo.obj" \
	"$(INTDIR)/cnvg2.obj" \
	"$(INTDIR)/chrge.obj" \
	"$(INTDIR)/powel1.obj"

"$(OUTDIR)\amsol70.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

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
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\amsol70.exe"

CLEAN : 
	-@erase ".\Debug\amsol70.exe"
	-@erase ".\Debug\dotmk.obj"
	-@erase ".\Debug\smx1.obj"
	-@erase ".\Debug\efstr.obj"
	-@erase ".\Debug\trustg.obj"
	-@erase ".\Debug\bfn.obj"
	-@erase ".\Debug\spline.obj"
	-@erase ".\Debug\ef_lib.obj"
	-@erase ".\Debug\denrot.obj"
	-@erase ".\Debug\sm5hb.obj"
	-@erase ".\Debug\ssifa.obj"
	-@erase ".\Debug\point3.obj"
	-@erase ".\Debug\formd.obj"
	-@erase ".\Debug\ampac_unmod.obj"
	-@erase ".\Debug\supdot.obj"
	-@erase ".\Debug\dijkl1.obj"
	-@erase ".\Debug\scopy.obj"
	-@erase ".\Debug\dfpsav.obj"
	-@erase ".\Debug\matout.obj"
	-@erase ".\Debug\tqlrat.obj"
	-@erase ".\Debug\paths.obj"
	-@erase ".\Debug\fmat.obj"
	-@erase ".\Debug\echowd.obj"
	-@erase ".\Debug\coulrd.obj"
	-@erase ".\Debug\readch.obj"
	-@erase ".\Debug\zmprnt.obj"
	-@erase ".\Debug\forsav.obj"
	-@erase ".\Debug\racsmo.obj"
	-@erase ".\Debug\dbrnpl.obj"
	-@erase ".\Debug\xyzint.obj"
	-@erase ".\Debug\readvt.obj"
	-@erase ".\Debug\header.obj"
	-@erase ".\Debug\main.obj"
	-@erase ".\Debug\fock1.obj"
	-@erase ".\Debug\ismax.obj"
	-@erase ".\Debug\nllsq.obj"
	-@erase ".\Debug\pqtkl.obj"
	-@erase ".\Debug\set.obj"
	-@erase ".\Debug\aabacd.obj"
	-@erase ".\Debug\ebrglq.obj"
	-@erase ".\Debug\saxpy.obj"
	-@erase ".\Debug\kwnono.obj"
	-@erase ".\Debug\sdot.obj"
	-@erase ".\Debug\am1.obj"
	-@erase ".\Debug\calpar.obj"
	-@erase ".\Debug\osm5sp.obj"
	-@erase ".\Debug\tql2.obj"
	-@erase ".\Debug\getchg.obj"
	-@erase ".\Debug\deri2.obj"
	-@erase ".\Debug\mxp.obj"
	-@erase ".\Debug\capcnv.obj"
	-@erase ".\Debug\mamlt2.obj"
	-@erase ".\Debug\trust4.obj"
	-@erase ".\Debug\pythag.obj"
	-@erase ".\Debug\geout.obj"
	-@erase ".\Debug\reads.obj"
	-@erase ".\Debug\locmin.obj"
	-@erase ".\Debug\dswap.obj"
	-@erase ".\Debug\deriv.obj"
	-@erase ".\Debug\datesv.obj"
	-@erase ".\Debug\stpjob.obj"
	-@erase ".\Debug\mtxm.obj"
	-@erase ".\Debug\diagiv.obj"
	-@erase ".\Debug\iter.obj"
	-@erase ".\Debug\ecrit.obj"
	-@erase ".\Debug\osinv.obj"
	-@erase ".\Debug\ebrpd.obj"
	-@erase ".\Debug\sm5rhb.obj"
	-@erase ".\Debug\stlfac.obj"
	-@erase ".\Debug\diag2.obj"
	-@erase ".\Debug\anavib.obj"
	-@erase ".\Debug\linsum.obj"
	-@erase ".\Debug\chain.obj"
	-@erase ".\Debug\cct1do.obj"
	-@erase ".\Debug\epslon.obj"
	-@erase ".\Debug\updhes.obj"
	-@erase ".\Debug\ldata.obj"
	-@erase ".\Debug\mxt.obj"
	-@erase ".\Debug\sm4hb.obj"
	-@erase ".\Debug\interp.obj"
	-@erase ".\Debug\sasado.obj"
	-@erase ".\Debug\cm1drv.obj"
	-@erase ".\Debug\sm1ara.obj"
	-@erase ".\Debug\satbat.obj"
	-@erase ".\Debug\ebrtrp.obj"
	-@erase ".\Debug\dhc.obj"
	-@erase ".\Debug\mecip.obj"
	-@erase ".\Debug\meci.obj"
	-@erase ".\Debug\path2.obj"
	-@erase ".\Debug\cdscal.obj"
	-@erase ".\Debug\compfg.obj"
	-@erase ".\Debug\freqcy.obj"
	-@erase ".\Debug\chgmp3.obj"
	-@erase ".\Debug\hfadd.obj"
	-@erase ".\Debug\diag.obj"
	-@erase ".\Debug\dsm5hb.obj"
	-@erase ".\Debug\ltrd.obj"
	-@erase ".\Debug\prarea.obj"
	-@erase ".\Debug\arcch.obj"
	-@erase ".\Debug\areah.obj"
	-@erase ".\Debug\cisgen.obj"
	-@erase ".\Debug\powsq.obj"
	-@erase ".\Debug\dareal.obj"
	-@erase ".\Debug\rotate.obj"
	-@erase ".\Debug\extsmy.obj"
	-@erase ".\Debug\gbscrf.obj"
	-@erase ".\Debug\tql1.obj"
	-@erase ".\Debug\quadri.obj"
	-@erase ".\Debug\selbel.obj"
	-@erase ".\Debug\deri1.obj"
	-@erase ".\Debug\efcres.obj"
	-@erase ".\Debug\util_unmod.obj"
	-@erase ".\Debug\findky.obj"
	-@erase ".\Debug\flepo.obj"
	-@erase ".\Debug\overlp.obj"
	-@erase ".\Debug\trust3.obj"
	-@erase ".\Debug\thermo.obj"
	-@erase ".\Debug\polenm.obj"
	-@erase ".\Debug\porcpu.obj"
	-@erase ".\Debug\densit.obj"
	-@erase ".\Debug\dipole.obj"
	-@erase ".\Debug\cardan.obj"
	-@erase ".\Debug\dpqtkl.obj"
	-@erase ".\Debug\mxmlp.obj"
	-@erase ".\Debug\finds.obj"
	-@erase ".\Debug\truste.obj"
	-@erase ".\Debug\efsav.obj"
	-@erase ".\Debug\deri23.obj"
	-@erase ".\Debug\point1.obj"
	-@erase ".\Debug\axis.obj"
	-@erase ".\Debug\simpt.obj"
	-@erase ".\Debug\ss.obj"
	-@erase ".\Debug\getsym.obj"
	-@erase ".\Debug\hcore.obj"
	-@erase ".\Debug\areal.obj"
	-@erase ".\Debug\fock.obj"
	-@erase ".\Debug\dcart2.obj"
	-@erase ".\Debug\status.obj"
	-@erase ".\Debug\gmetry.obj"
	-@erase ".\Debug\drc.obj"
	-@erase ".\Debug\zpe.obj"
	-@erase ".\Debug\fock2d.obj"
	-@erase ".\Debug\amsel.obj"
	-@erase ".\Debug\hqrii.obj"
	-@erase ".\Debug\kwrm.obj"
	-@erase ".\Debug\gethes.obj"
	-@erase ".\Debug\babbbc.obj"
	-@erase ".\Debug\path1.obj"
	-@erase ".\Debug\densts.obj"
	-@erase ".\Debug\tred2.obj"
	-@erase ".\Debug\smsshb.obj"
	-@erase ".\Debug\chgmp2.obj"
	-@erase ".\Debug\moldat.obj"
	-@erase ".\Debug\vecprt.obj"
	-@erase ".\Debug\srfcty.obj"
	-@erase ".\Debug\extsmx.obj"
	-@erase ".\Debug\dot.obj"
	-@erase ".\Debug\diat2.obj"
	-@erase ".\Debug\aababc.obj"
	-@erase ".\Debug\calvk.obj"
	-@erase ".\Debug\deri0.obj"
	-@erase ".\Debug\force.obj"
	-@erase ".\Debug\intcar.obj"
	-@erase ".\Debug\loadsp.obj"
	-@erase ".\Debug\react1.obj"
	-@erase ".\Debug\trust2.obj"
	-@erase ".\Debug\mullik.obj"
	-@erase ".\Debug\calcbp.obj"
	-@erase ".\Debug\col.obj"
	-@erase ".\Debug\grid.obj"
	-@erase ".\Debug\spaout.obj"
	-@erase ".\Debug\h1elec.obj"
	-@erase ".\Debug\patham.obj"
	-@erase ".\Debug\uhfprt.obj"
	-@erase ".\Debug\deri22.obj"
	-@erase ".\Debug\stat1.obj"
	-@erase ".\Debug\search.obj"
	-@erase ".\Debug\kfinit.obj"
	-@erase ".\Debug\getgeo.obj"
	-@erase ".\Debug\ef.obj"
	-@erase ".\Debug\powel1.obj"
	-@erase ".\Debug\amsol.obj"
	-@erase ".\Debug\amstat.obj"
	-@erase ".\Debug\jcarin.obj"
	-@erase ".\Debug\diprnt.obj"
	-@erase ".\Debug\smhb.obj"
	-@erase ".\Debug\trust6.obj"
	-@erase ".\Debug\rs.obj"
	-@erase ".\Debug\drot.obj"
	-@erase ".\Debug\dfock2.obj"
	-@erase ".\Debug\local.obj"
	-@erase ".\Debug\convck.obj"
	-@erase ".\Debug\powell.obj"
	-@erase ".\Debug\frame.obj"
	-@erase ".\Debug\tred1.obj"
	-@erase ".\Debug\vecred.obj"
	-@erase ".\Debug\chgmp1.obj"
	-@erase ".\Debug\pulay.obj"
	-@erase ".\Debug\sasard.obj"
	-@erase ".\Debug\vecwrt.obj"
	-@erase ".\Debug\ciparm.obj"
	-@erase ".\Debug\tpchk.obj"
	-@erase ".\Debug\srfcal.obj"
	-@erase ".\Debug\readif.obj"
	-@erase ".\Debug\ssidi.obj"
	-@erase ".\Debug\dhcore.obj"
	-@erase ".\Debug\dijkl2.obj"
	-@erase ".\Debug\enpart.obj"
	-@erase ".\Debug\aabbcd.obj"
	-@erase ".\Debug\ebrrec.obj"
	-@erase ".\Debug\writes.obj"
	-@erase ".\Debug\findkw.obj"
	-@erase ".\Debug\resinp.obj"
	-@erase ".\Debug\trust1.obj"
	-@erase ".\Debug\wrtkey.obj"
	-@erase ".\Debug\fock2.obj"
	-@erase ".\Debug\loadin.obj"
	-@erase ".\Debug\dcart.obj"
	-@erase ".\Debug\kwadd.obj"
	-@erase ".\Debug\keyflg.obj"
	-@erase ".\Debug\bonds.obj"
	-@erase ".\Debug\prthes.obj"
	-@erase ".\Debug\rabiot.obj"
	-@erase ".\Debug\deri21.obj"
	-@erase ".\Debug\perm.obj"
	-@erase ".\Debug\jincar.obj"
	-@erase ".\Debug\uncano.obj"
	-@erase ".\Debug\haddon.obj"
	-@erase ".\Debug\linmin.obj"
	-@erase ".\Debug\svopts.obj"
	-@erase ".\Debug\ijkl.obj"
	-@erase ".\Debug\solrot.obj"
	-@erase ".\Debug\update.obj"
	-@erase ".\Debug\cnvg2.obj"
	-@erase ".\Debug\chrge.obj"
	-@erase ".\Debug\mform.obj"
	-@erase ".\Debug\sterms.obj"
	-@erase ".\Debug\depvar.obj"
	-@erase ".\Debug\trust5.obj"
	-@erase ".\Debug\bornpl.obj"
	-@erase ".\Debug\polar.obj"
	-@erase ".\Debug\amsol70.ilk"
	-@erase ".\Debug\amsol70.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "Debug/" /c /nologo
# ADD F90 /W0 /Zi /I "Debug/" /c /nologo
# SUBTRACT F90 /Ox /FR
F90_PROJ=/W0 /Zi /I "Debug/" /c /nologo /Fo"Debug/" /Fd"Debug/amsol70.pdb" 
F90_OBJS=.\Debug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/amsol70.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/amsol70.pdb" /debug /machine:I386 /out:"$(OUTDIR)/amsol70.exe" 
LINK32_OBJS= \
	"$(INTDIR)/dotmk.obj" \
	"$(INTDIR)/smx1.obj" \
	"$(INTDIR)/efstr.obj" \
	"$(INTDIR)/trustg.obj" \
	"$(INTDIR)/bfn.obj" \
	"$(INTDIR)/spline.obj" \
	"$(INTDIR)/ef_lib.obj" \
	"$(INTDIR)/denrot.obj" \
	"$(INTDIR)/sm5hb.obj" \
	"$(INTDIR)/ssifa.obj" \
	"$(INTDIR)/point3.obj" \
	"$(INTDIR)/formd.obj" \
	"$(INTDIR)/ampac_unmod.obj" \
	"$(INTDIR)/supdot.obj" \
	"$(INTDIR)/dijkl1.obj" \
	"$(INTDIR)/scopy.obj" \
	"$(INTDIR)/dfpsav.obj" \
	"$(INTDIR)/matout.obj" \
	"$(INTDIR)/tqlrat.obj" \
	"$(INTDIR)/paths.obj" \
	"$(INTDIR)/fmat.obj" \
	"$(INTDIR)/echowd.obj" \
	"$(INTDIR)/coulrd.obj" \
	"$(INTDIR)/readch.obj" \
	"$(INTDIR)/zmprnt.obj" \
	"$(INTDIR)/forsav.obj" \
	"$(INTDIR)/racsmo.obj" \
	"$(INTDIR)/dbrnpl.obj" \
	"$(INTDIR)/xyzint.obj" \
	"$(INTDIR)/readvt.obj" \
	"$(INTDIR)/header.obj" \
	"$(INTDIR)/main.obj" \
	"$(INTDIR)/fock1.obj" \
	"$(INTDIR)/ismax.obj" \
	"$(INTDIR)/nllsq.obj" \
	"$(INTDIR)/pqtkl.obj" \
	"$(INTDIR)/set.obj" \
	"$(INTDIR)/aabacd.obj" \
	"$(INTDIR)/ebrglq.obj" \
	"$(INTDIR)/saxpy.obj" \
	"$(INTDIR)/kwnono.obj" \
	"$(INTDIR)/sdot.obj" \
	"$(INTDIR)/am1.obj" \
	"$(INTDIR)/calpar.obj" \
	"$(INTDIR)/osm5sp.obj" \
	"$(INTDIR)/tql2.obj" \
	"$(INTDIR)/getchg.obj" \
	"$(INTDIR)/deri2.obj" \
	"$(INTDIR)/mxp.obj" \
	"$(INTDIR)/capcnv.obj" \
	"$(INTDIR)/mamlt2.obj" \
	"$(INTDIR)/trust4.obj" \
	"$(INTDIR)/pythag.obj" \
	"$(INTDIR)/geout.obj" \
	"$(INTDIR)/reads.obj" \
	"$(INTDIR)/locmin.obj" \
	"$(INTDIR)/dswap.obj" \
	"$(INTDIR)/deriv.obj" \
	"$(INTDIR)/datesv.obj" \
	"$(INTDIR)/stpjob.obj" \
	"$(INTDIR)/mtxm.obj" \
	"$(INTDIR)/diagiv.obj" \
	"$(INTDIR)/iter.obj" \
	"$(INTDIR)/ecrit.obj" \
	"$(INTDIR)/osinv.obj" \
	"$(INTDIR)/ebrpd.obj" \
	"$(INTDIR)/sm5rhb.obj" \
	"$(INTDIR)/stlfac.obj" \
	"$(INTDIR)/diag2.obj" \
	"$(INTDIR)/anavib.obj" \
	"$(INTDIR)/linsum.obj" \
	"$(INTDIR)/chain.obj" \
	"$(INTDIR)/cct1do.obj" \
	"$(INTDIR)/epslon.obj" \
	"$(INTDIR)/updhes.obj" \
	"$(INTDIR)/ldata.obj" \
	"$(INTDIR)/mxt.obj" \
	"$(INTDIR)/sm4hb.obj" \
	"$(INTDIR)/interp.obj" \
	"$(INTDIR)/sasado.obj" \
	"$(INTDIR)/cm1drv.obj" \
	"$(INTDIR)/sm1ara.obj" \
	"$(INTDIR)/satbat.obj" \
	"$(INTDIR)/ebrtrp.obj" \
	"$(INTDIR)/dhc.obj" \
	"$(INTDIR)/mecip.obj" \
	"$(INTDIR)/meci.obj" \
	"$(INTDIR)/path2.obj" \
	"$(INTDIR)/cdscal.obj" \
	"$(INTDIR)/compfg.obj" \
	"$(INTDIR)/freqcy.obj" \
	"$(INTDIR)/chgmp3.obj" \
	"$(INTDIR)/hfadd.obj" \
	"$(INTDIR)/diag.obj" \
	"$(INTDIR)/dsm5hb.obj" \
	"$(INTDIR)/ltrd.obj" \
	"$(INTDIR)/prarea.obj" \
	"$(INTDIR)/arcch.obj" \
	"$(INTDIR)/areah.obj" \
	"$(INTDIR)/cisgen.obj" \
	"$(INTDIR)/powsq.obj" \
	"$(INTDIR)/dareal.obj" \
	"$(INTDIR)/rotate.obj" \
	"$(INTDIR)/extsmy.obj" \
	"$(INTDIR)/gbscrf.obj" \
	"$(INTDIR)/tql1.obj" \
	"$(INTDIR)/quadri.obj" \
	"$(INTDIR)/selbel.obj" \
	"$(INTDIR)/deri1.obj" \
	"$(INTDIR)/efcres.obj" \
	"$(INTDIR)/util_unmod.obj" \
	"$(INTDIR)/findky.obj" \
	"$(INTDIR)/flepo.obj" \
	"$(INTDIR)/overlp.obj" \
	"$(INTDIR)/trust3.obj" \
	"$(INTDIR)/thermo.obj" \
	"$(INTDIR)/polenm.obj" \
	"$(INTDIR)/porcpu.obj" \
	"$(INTDIR)/densit.obj" \
	"$(INTDIR)/dipole.obj" \
	"$(INTDIR)/cardan.obj" \
	"$(INTDIR)/dpqtkl.obj" \
	"$(INTDIR)/mxmlp.obj" \
	"$(INTDIR)/finds.obj" \
	"$(INTDIR)/truste.obj" \
	"$(INTDIR)/efsav.obj" \
	"$(INTDIR)/deri23.obj" \
	"$(INTDIR)/point1.obj" \
	"$(INTDIR)/axis.obj" \
	"$(INTDIR)/simpt.obj" \
	"$(INTDIR)/ss.obj" \
	"$(INTDIR)/getsym.obj" \
	"$(INTDIR)/hcore.obj" \
	"$(INTDIR)/areal.obj" \
	"$(INTDIR)/fock.obj" \
	"$(INTDIR)/dcart2.obj" \
	"$(INTDIR)/status.obj" \
	"$(INTDIR)/gmetry.obj" \
	"$(INTDIR)/drc.obj" \
	"$(INTDIR)/zpe.obj" \
	"$(INTDIR)/fock2d.obj" \
	"$(INTDIR)/amsel.obj" \
	"$(INTDIR)/hqrii.obj" \
	"$(INTDIR)/kwrm.obj" \
	"$(INTDIR)/gethes.obj" \
	"$(INTDIR)/babbbc.obj" \
	"$(INTDIR)/path1.obj" \
	"$(INTDIR)/densts.obj" \
	"$(INTDIR)/tred2.obj" \
	"$(INTDIR)/smsshb.obj" \
	"$(INTDIR)/chgmp2.obj" \
	"$(INTDIR)/moldat.obj" \
	"$(INTDIR)/vecprt.obj" \
	"$(INTDIR)/srfcty.obj" \
	"$(INTDIR)/extsmx.obj" \
	"$(INTDIR)/dot.obj" \
	"$(INTDIR)/diat2.obj" \
	"$(INTDIR)/aababc.obj" \
	"$(INTDIR)/calvk.obj" \
	"$(INTDIR)/deri0.obj" \
	"$(INTDIR)/force.obj" \
	"$(INTDIR)/intcar.obj" \
	"$(INTDIR)/loadsp.obj" \
	"$(INTDIR)/react1.obj" \
	"$(INTDIR)/trust2.obj" \
	"$(INTDIR)/mullik.obj" \
	"$(INTDIR)/calcbp.obj" \
	"$(INTDIR)/col.obj" \
	"$(INTDIR)/grid.obj" \
	"$(INTDIR)/spaout.obj" \
	"$(INTDIR)/h1elec.obj" \
	"$(INTDIR)/patham.obj" \
	"$(INTDIR)/uhfprt.obj" \
	"$(INTDIR)/deri22.obj" \
	"$(INTDIR)/stat1.obj" \
	"$(INTDIR)/search.obj" \
	"$(INTDIR)/kfinit.obj" \
	"$(INTDIR)/getgeo.obj" \
	"$(INTDIR)/ef.obj" \
	"$(INTDIR)/powel1.obj" \
	"$(INTDIR)/amsol.obj" \
	"$(INTDIR)/amstat.obj" \
	"$(INTDIR)/jcarin.obj" \
	"$(INTDIR)/diprnt.obj" \
	"$(INTDIR)/smhb.obj" \
	"$(INTDIR)/trust6.obj" \
	"$(INTDIR)/rs.obj" \
	"$(INTDIR)/drot.obj" \
	"$(INTDIR)/dfock2.obj" \
	"$(INTDIR)/local.obj" \
	"$(INTDIR)/convck.obj" \
	"$(INTDIR)/powell.obj" \
	"$(INTDIR)/frame.obj" \
	"$(INTDIR)/tred1.obj" \
	"$(INTDIR)/vecred.obj" \
	"$(INTDIR)/chgmp1.obj" \
	"$(INTDIR)/pulay.obj" \
	"$(INTDIR)/sasard.obj" \
	"$(INTDIR)/vecwrt.obj" \
	"$(INTDIR)/ciparm.obj" \
	"$(INTDIR)/tpchk.obj" \
	"$(INTDIR)/srfcal.obj" \
	"$(INTDIR)/readif.obj" \
	"$(INTDIR)/ssidi.obj" \
	"$(INTDIR)/dhcore.obj" \
	"$(INTDIR)/dijkl2.obj" \
	"$(INTDIR)/enpart.obj" \
	"$(INTDIR)/aabbcd.obj" \
	"$(INTDIR)/ebrrec.obj" \
	"$(INTDIR)/writes.obj" \
	"$(INTDIR)/findkw.obj" \
	"$(INTDIR)/resinp.obj" \
	"$(INTDIR)/trust1.obj" \
	"$(INTDIR)/wrtkey.obj" \
	"$(INTDIR)/fock2.obj" \
	"$(INTDIR)/loadin.obj" \
	"$(INTDIR)/dcart.obj" \
	"$(INTDIR)/kwadd.obj" \
	"$(INTDIR)/keyflg.obj" \
	"$(INTDIR)/bonds.obj" \
	"$(INTDIR)/prthes.obj" \
	"$(INTDIR)/rabiot.obj" \
	"$(INTDIR)/deri21.obj" \
	"$(INTDIR)/perm.obj" \
	"$(INTDIR)/jincar.obj" \
	"$(INTDIR)/uncano.obj" \
	"$(INTDIR)/haddon.obj" \
	"$(INTDIR)/linmin.obj" \
	"$(INTDIR)/svopts.obj" \
	"$(INTDIR)/ijkl.obj" \
	"$(INTDIR)/solrot.obj" \
	"$(INTDIR)/update.obj" \
	"$(INTDIR)/cnvg2.obj" \
	"$(INTDIR)/chrge.obj" \
	"$(INTDIR)/mform.obj" \
	"$(INTDIR)/sterms.obj" \
	"$(INTDIR)/depvar.obj" \
	"$(INTDIR)/trust5.obj" \
	"$(INTDIR)/bornpl.obj" \
	"$(INTDIR)/polar.obj"

"$(OUTDIR)\amsol70.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

################################################################################
# Begin Target

# Name "amsol70 - Win32 Release"
# Name "amsol70 - Win32 Debug"

!IF  "$(CFG)" == "amsol70 - Win32 Release"

!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\porcpu.F

"$(INTDIR)\porcpu.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\datesv.F

"$(INTDIR)\datesv.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\zpe.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_ZPE_F=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\zpe.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_ZPE_F=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\zpe.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\zmprnt.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_ZMPRN=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\zmprnt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_ZMPRN=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\zmprnt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\xyzint.f

"$(INTDIR)\xyzint.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\wrtkey.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_WRTKE=\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\wrtkey.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_WRTKE=\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\wrtkey.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\writes.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_WRITE=\
	".\Release\SIZES.i"\
	".\Release\SIZES2.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	".\Release\PARAMS.i"\
	

"$(INTDIR)\writes.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_WRITE=\
	".\Debug\SIZES.i"\
	".\Debug\SIZES2.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	".\Debug\PARAMS.i"\
	

"$(INTDIR)\writes.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\vecwrt.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_VECWR=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\vecwrt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_VECWR=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\vecwrt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\vecred.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_VECRE=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\vecred.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_VECRE=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\vecred.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\vecprt.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_VECPR=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\vecprt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_VECPR=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\vecprt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\updhes.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_UPDHE=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\updhes.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_UPDHE=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\updhes.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\uncano.f

"$(INTDIR)\uncano.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\uhfprt.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_UHFPR=\
	".\Release\KEYS.i"\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\uhfprt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_UHFPR=\
	".\Debug\KEYS.i"\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\uhfprt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\trustg.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_TRUST=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\trustg.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_TRUST=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\trustg.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\truste.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_TRUSTE=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\truste.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_TRUSTE=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\truste.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\trust6.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_TRUST6=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\trust6.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_TRUST6=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\trust6.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\trust5.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_TRUST5=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\trust5.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_TRUST5=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\trust5.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\trust4.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_TRUST4=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\trust4.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_TRUST4=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\trust4.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\trust3.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_TRUST3=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\trust3.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_TRUST3=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\trust3.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\trust2.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_TRUST2=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\trust2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_TRUST2=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\trust2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\trust1.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_TRUST1=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\trust1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_TRUST1=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\trust1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\tred2.f

"$(INTDIR)\tred2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\tred1.f

"$(INTDIR)\tred1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\tqlrat.f

"$(INTDIR)\tqlrat.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\tql2.f

"$(INTDIR)\tql2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\tql1.f

"$(INTDIR)\tql1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\tpchk.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_TPCHK=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\tpchk.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_TPCHK=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\tpchk.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\thermo.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_THERM=\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\thermo.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_THERM=\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\thermo.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\svopts.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SVOPT=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\svopts.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SVOPT=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\svopts.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\supdot.f

"$(INTDIR)\supdot.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\stpjob.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_STPJO=\
	".\Release\FFILES.i"\
	

"$(INTDIR)\stpjob.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_STPJO=\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\stpjob.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\stlfac.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_STLFA=\
	".\Release\KEYS.i"\
	".\Release\SIZES.i"\
	

"$(INTDIR)\stlfac.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_STLFA=\
	".\Debug\KEYS.i"\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\stlfac.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\sterms.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_STERM=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\sterms.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_STERM=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\sterms.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\stat1.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_STAT1=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\stat1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_STAT1=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\stat1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\ssifa.f

"$(INTDIR)\ssifa.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\ssidi.f

"$(INTDIR)\ssidi.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\ss.f

"$(INTDIR)\ss.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\srfcty.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SRFCT=\
	".\Release\SIZES.i"\
	".\Release\SIZES2.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\srfcty.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SRFCT=\
	".\Debug\SIZES.i"\
	".\Debug\SIZES2.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\srfcty.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\srfcal.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SRFCA=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\srfcal.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SRFCA=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\srfcal.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\spline.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SPLIN=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\spline.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SPLIN=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\spline.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\spaout.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SPAOU=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\PARAMS.i"\
	

"$(INTDIR)\spaout.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SPAOU=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\PARAMS.i"\
	

"$(INTDIR)\spaout.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\solrot.f

"$(INTDIR)\solrot.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\smx1.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SMX1_=\
	".\Release\SIZES.i"\
	".\Release\SIZES2.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\smx1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SMX1_=\
	".\Debug\SIZES.i"\
	".\Debug\SIZES2.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\smx1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\smsshb.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SMSSH=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\smsshb.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SMSSH=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\smsshb.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\smhb.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SMHB_=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\smhb.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SMHB_=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\smhb.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\sm5rhb.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SM5RH=\
	".\Release\SIZES.i"\
	".\Release\PARAMS.i"\
	

"$(INTDIR)\sm5rhb.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SM5RH=\
	".\Debug\SIZES.i"\
	".\Debug\PARAMS.i"\
	

"$(INTDIR)\sm5rhb.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\sm5hb.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SM5HB=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\sm5hb.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SM5HB=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\sm5hb.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\sm4hb.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SM4HB=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\sm4hb.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SM4HB=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\sm4hb.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\sm1ara.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SM1AR=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\sm1ara.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SM1AR=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\sm1ara.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\simpt.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SIMPT=\
	".\Release\KEYS.i"\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\simpt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SIMPT=\
	".\Debug\KEYS.i"\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\simpt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\set.f

"$(INTDIR)\set.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\selbel.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SELBE=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\selbel.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SELBE=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\selbel.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\search.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SEARC=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\search.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SEARC=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\search.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\sdot.f

"$(INTDIR)\sdot.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\scopy.f

"$(INTDIR)\scopy.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\saxpy.f

"$(INTDIR)\saxpy.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\satbat.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SATBA=\
	".\Release\KEYS.i"\
	".\Release\SIZES.i"\
	

"$(INTDIR)\satbat.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SATBA=\
	".\Debug\KEYS.i"\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\satbat.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\sasard.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SASAR=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\sasard.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SASAR=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\sasard.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\sasado.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_SASAD=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\sasado.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_SASAD=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\sasado.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\rs.f

"$(INTDIR)\rs.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\rotate.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_ROTAT=\
	".\Release\KEYS.i"\
	

"$(INTDIR)\rotate.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_ROTAT=\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\rotate.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\resinp.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_RESIN=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\resinp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_RESIN=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\resinp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\reads.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_READS=\
	".\Release\SIZES.i"\
	".\Release\SIZES2.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	".\Release\PARAMS.i"\
	

"$(INTDIR)\reads.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_READS=\
	".\Debug\SIZES.i"\
	".\Debug\SIZES2.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	".\Debug\PARAMS.i"\
	

"$(INTDIR)\reads.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\readif.f

"$(INTDIR)\readif.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\readch.f

"$(INTDIR)\readch.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\react1.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_REACT=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\react1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_REACT=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\react1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\racsmo.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_RACSM=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\racsmo.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_RACSM=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\racsmo.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\quadri.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_QUADR=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\quadri.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_QUADR=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\quadri.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\pythag.f

"$(INTDIR)\pythag.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\pulay.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_PULAY=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\pulay.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_PULAY=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\pulay.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\prthes.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_PRTHE=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\prthes.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_PRTHE=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\prthes.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\prarea.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_PRARE=\
	".\Release\KEYS.i"\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	".\Release\PARAMS.i"\
	

"$(INTDIR)\prarea.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_PRARE=\
	".\Debug\KEYS.i"\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	".\Debug\PARAMS.i"\
	

"$(INTDIR)\prarea.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\pqtkl.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_PQTKL=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\pqtkl.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_PQTKL=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\pqtkl.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\powsq.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_POWSQ=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\powsq.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_POWSQ=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\powsq.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\powell.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_POWEL=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\powell.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_POWEL=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\powell.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\powel1.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_POWEL1=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\powel1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_POWEL1=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\powel1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\polenm.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_POLEN=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\polenm.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_POLEN=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\polenm.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\polar.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_POLAR=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\polar.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_POLAR=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\polar.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\point3.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_POINT=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\point3.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_POINT=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\point3.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\point1.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_POINT1=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\point1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_POINT1=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\point1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\paths.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_PATHS=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\paths.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_PATHS=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\paths.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\patham.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_PATHA=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\patham.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_PATHA=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\patham.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\path2.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_PATH2=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\path2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_PATH2=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\path2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\path1.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_PATH1=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\path1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_PATH1=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\path1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\overlp.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_OVERL=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\overlp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_OVERL=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\overlp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\osm5sp.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_OSM5S=\
	".\Release\KEYS.i"\
	".\Release\PARAMS.i"\
	

"$(INTDIR)\osm5sp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_OSM5S=\
	".\Debug\KEYS.i"\
	".\Debug\PARAMS.i"\
	

"$(INTDIR)\osm5sp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\nllsq.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_NLLSQ=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\nllsq.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_NLLSQ=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\nllsq.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\mxt.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_MXT_F=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\mxt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_MXT_F=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\mxt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\mxp.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_MXP_F=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\mxp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_MXP_F=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\mxp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\mxmlp.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_MXMLP=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\mxmlp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_MXMLP=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\mxmlp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\mullik.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_MULLI=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\mullik.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_MULLI=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\mullik.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\moldat.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_MOLDA=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\moldat.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_MOLDA=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\moldat.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\mform.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_MFORM=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\mform.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_MFORM=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\mform.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\meci.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_MECI_=\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	".\Release\SIZES.i"\
	

"$(INTDIR)\meci.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_MECI_=\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\meci.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\matout.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_MATOU=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\matout.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_MATOU=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\matout.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\mamlt2.f

"$(INTDIR)\mamlt2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\main.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_MAIN_=\
	".\Release\FFILES.i"\
	

"$(INTDIR)\main.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_MAIN_=\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\main.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\ltrd.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_LTRD_=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\ltrd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_LTRD_=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\ltrd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\locmin.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_LOCMI=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\locmin.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_LOCMI=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\locmin.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\local.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_LOCAL=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\local.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_LOCAL=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\local.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\loadsp.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_LOADS=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\loadsp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_LOADS=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\loadsp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\loadin.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_LOADI=\
	".\Release\FFILES.i"\
	".\Release\KEYS.i"\
	".\Release\PARAMS.i"\
	

"$(INTDIR)\loadin.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_LOADI=\
	".\Debug\FFILES.i"\
	".\Debug\KEYS.i"\
	".\Debug\PARAMS.i"\
	

"$(INTDIR)\loadin.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\linsum.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_LINSU=\
	".\Release\KEYS.i"\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\linsum.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_LINSU=\
	".\Debug\KEYS.i"\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\linsum.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\linmin.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_LINMI=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\linmin.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_LINMI=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\linmin.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\ldata.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_LDATA=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\ldata.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_LDATA=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\ldata.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\kwrm.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_KWRM_=\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\kwrm.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_KWRM_=\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\kwrm.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\kwnono.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_KWNON=\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\kwnono.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_KWNON=\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\kwnono.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\kwadd.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_KWADD=\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\kwadd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_KWADD=\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\kwadd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\kfinit.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_KFINI=\
	".\Release\KEYS.i"\
	

"$(INTDIR)\kfinit.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_KFINI=\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\kfinit.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\keyflg.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_KEYFL=\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	".\Release\SIZES.i"\
	

"$(INTDIR)\keyflg.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_KEYFL=\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\keyflg.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\jcarin.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_JCARI=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\jcarin.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_JCARI=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\jcarin.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\iter.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_ITER_=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\iter.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_ITER_=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\iter.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\ismax.f

"$(INTDIR)\ismax.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\interp.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_INTER=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\interp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_INTER=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\interp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\hqrii.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_HQRII=\
	".\Release\FFILES.i"\
	".\Release\SIZES.i"\
	

"$(INTDIR)\hqrii.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_HQRII=\
	".\Debug\FFILES.i"\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\hqrii.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\hfadd.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_HFADD=\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\hfadd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_HFADD=\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\hfadd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\header.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_HEADE=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\header.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_HEADE=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\header.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\hcore.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_HCORE=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\hcore.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_HCORE=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\hcore.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\h1elec.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_H1ELE=\
	".\Release\KEYS.i"\
	

"$(INTDIR)\h1elec.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_H1ELE=\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\h1elec.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\grid.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_GRID_=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\grid.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_GRID_=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\grid.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\gmetry.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_GMETR=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\gmetry.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_GMETR=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\gmetry.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\getsym.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_GETSY=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\getsym.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_GETSY=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\getsym.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\gethes.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_GETHE=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\gethes.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_GETHE=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\gethes.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\getgeo.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_GETGE=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\getgeo.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_GETGE=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\getgeo.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\getchg.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_GETCH=\
	".\Release\KEYS.i"\
	".\Release\SIZES.i"\
	

"$(INTDIR)\getchg.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_GETCH=\
	".\Debug\KEYS.i"\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\getchg.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\geout.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_GEOUT=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\geout.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_GEOUT=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\geout.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\gbscrf.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_GBSCR=\
	".\Release\KEYS.i"\
	".\Release\SIZES.i"\
	

"$(INTDIR)\gbscrf.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_GBSCR=\
	".\Debug\KEYS.i"\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\gbscrf.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\forsav.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_FORSA=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\forsav.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_FORSA=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\forsav.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\formd.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_FORMD=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\formd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_FORMD=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\formd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\force.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_FORCE=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\force.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_FORCE=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\force.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\fock2d.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_FOCK2=\
	".\Release\KEYS.i"\
	

"$(INTDIR)\fock2d.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_FOCK2=\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\fock2d.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\fock2.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_FOCK2_=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\fock2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_FOCK2_=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\fock2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\fock1.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_FOCK1=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\fock1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_FOCK1=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\fock1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\fock.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_FOCK_=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\fock.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_FOCK_=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\fock.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\fmat.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_FMAT_=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\fmat.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_FMAT_=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\fmat.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\flepo.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_FLEPO=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\flepo.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_FLEPO=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\flepo.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\finds.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_FINDS=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\finds.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_FINDS=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\finds.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\findky.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_FINDK=\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\findky.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_FINDK=\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\findky.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\findkw.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_FINDKW=\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\findkw.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_FINDKW=\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\findkw.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\extsmy.f

"$(INTDIR)\extsmy.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\extsmx.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_EXTSM=\
	".\Release\FFILES.i"\
	

"$(INTDIR)\extsmx.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_EXTSM=\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\extsmx.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\epslon.f

"$(INTDIR)\epslon.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\enpart.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_ENPAR=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\enpart.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_ENPAR=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\enpart.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\efstr.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_EFSTR=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\efstr.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_EFSTR=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\efstr.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\efsav.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_EFSAV=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\efsav.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_EFSAV=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\efsav.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\efcres.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_EFCRE=\
	".\Release\KEYS.i"\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\efcres.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_EFCRE=\
	".\Debug\KEYS.i"\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\efcres.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\ef.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_EF_Ff0=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\ef.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_EF_Ff0=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\ef.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\ecrit.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_ECRIT=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\ecrit.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_ECRIT=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\ecrit.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\echowd.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_ECHOW=\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\echowd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_ECHOW=\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\echowd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\ebrtrp.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_EBRTR=\
	".\Release\SIZES.i"\
	".\Release\SIZES2.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\ebrtrp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_EBRTR=\
	".\Debug\SIZES.i"\
	".\Debug\SIZES2.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\ebrtrp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\ebrrec.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_EBRRE=\
	".\Release\KEYS.i"\
	".\Release\SIZES.i"\
	".\Release\SIZES2.i"\
	

"$(INTDIR)\ebrrec.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_EBRRE=\
	".\Debug\KEYS.i"\
	".\Debug\SIZES.i"\
	".\Debug\SIZES2.i"\
	

"$(INTDIR)\ebrrec.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\ebrpd.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_EBRPD=\
	".\Release\SIZES.i"\
	".\Release\SIZES2.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\ebrpd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_EBRPD=\
	".\Debug\SIZES.i"\
	".\Debug\SIZES2.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\ebrpd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\ebrglq.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_EBRGL=\
	".\Release\SIZES.i"\
	".\Release\SIZES2.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\ebrglq.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_EBRGL=\
	".\Debug\SIZES.i"\
	".\Debug\SIZES2.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\ebrglq.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\dswap.f

"$(INTDIR)\dswap.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\dsm5hb.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DSM5H=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\dsm5hb.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DSM5H=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\dsm5hb.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\drot.f

"$(INTDIR)\drot.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\drc.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DRC_F=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\drc.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DRC_F=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\drc.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\dpqtkl.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DPQTK=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\dpqtkl.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DPQTK=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\dpqtkl.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\dotmk.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DOTMK=\
	".\Release\SIZES.i"\
	".\Release\SIZES2.i"\
	

"$(INTDIR)\dotmk.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DOTMK=\
	".\Debug\SIZES.i"\
	".\Debug\SIZES2.i"\
	

"$(INTDIR)\dotmk.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\diprnt.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DIPRN=\
	".\Release\KEYS.i"\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\diprnt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DIPRN=\
	".\Debug\KEYS.i"\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\diprnt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\dipole.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DIPOL=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\dipole.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DIPOL=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\dipole.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\diat2.f

"$(INTDIR)\diat2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\diagiv.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DIAGI=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\diagiv.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DIAGI=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\diagiv.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\diag2.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DIAG2=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\diag2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DIAG2=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\diag2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\diag.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DIAG_=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\diag.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DIAG_=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\diag.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\dhcore.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DHCOR=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\dhcore.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DHCOR=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\dhcore.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\dhc.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DHC_F=\
	".\Release\KEYS.i"\
	

"$(INTDIR)\dhc.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DHC_F=\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\dhc.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\dfpsav.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DFPSA=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\dfpsav.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DFPSA=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\dfpsav.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\dfock2.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DFOCK=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\dfock2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DFOCK=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\dfock2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\deriv.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DERIV=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\deriv.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DERIV=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\deriv.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\deri23.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DERI2=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\deri23.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DERI2=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\deri23.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\deri21.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DERI21=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\deri21.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DERI21=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\deri21.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\deri2.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DERI2_=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\deri2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DERI2_=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\deri2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\deri1.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DERI1=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\deri1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DERI1=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\deri1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\deri0.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DERI0=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\deri0.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DERI0=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\deri0.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\depvar.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DEPVA=\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\depvar.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DEPVA=\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\depvar.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\densts.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DENST=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\densts.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DENST=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\densts.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\densit.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DENSI=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\densit.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DENSI=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\densit.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\denrot.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DENRO=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\denrot.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DENRO=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\denrot.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\dcart2.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DCART=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\dcart2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DCART=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\dcart2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\dcart.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DCART_=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\dcart.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DCART_=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\dcart.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\dbrnpl.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DBRNP=\
	".\Release\SIZES.i"\
	".\Release\SIZES2.i"\
	".\Release\FFILES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\dbrnpl.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DBRNP=\
	".\Debug\SIZES.i"\
	".\Debug\SIZES2.i"\
	".\Debug\FFILES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\dbrnpl.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\dareal.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DAREA=\
	".\Release\SIZES.i"\
	".\Release\SIZES2.i"\
	

"$(INTDIR)\dareal.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DAREA=\
	".\Debug\SIZES.i"\
	".\Debug\SIZES2.i"\
	

"$(INTDIR)\dareal.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\coulrd.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_COULR=\
	".\Release\SIZES2.i"\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\PARAMS.i"\
	

"$(INTDIR)\coulrd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_COULR=\
	".\Debug\SIZES2.i"\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\PARAMS.i"\
	

"$(INTDIR)\coulrd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\convck.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_CONVC=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\convck.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_CONVC=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\convck.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\compfg.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_COMPF=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\compfg.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_COMPF=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\compfg.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\col.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_COL_F=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\col.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_COL_F=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\col.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\cnvg2.f

"$(INTDIR)\cnvg2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\cm1drv.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_CM1DR=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\PARAMS.i"\
	

"$(INTDIR)\cm1drv.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_CM1DR=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\PARAMS.i"\
	

"$(INTDIR)\cm1drv.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\cisgen.f

"$(INTDIR)\cisgen.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\ciparm.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_CIPAR=\
	".\Release\KEYS.i"\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\ciparm.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_CIPAR=\
	".\Debug\KEYS.i"\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\ciparm.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\chrge.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_CHRGE=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\chrge.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_CHRGE=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\chrge.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\chgmp3.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_CHGMP=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\PARAMS.i"\
	

"$(INTDIR)\chgmp3.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_CHGMP=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\PARAMS.i"\
	

"$(INTDIR)\chgmp3.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\chgmp2.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_CHGMP2=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\PARAMS.i"\
	

"$(INTDIR)\chgmp2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_CHGMP2=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\PARAMS.i"\
	

"$(INTDIR)\chgmp2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\chgmp1.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_CHGMP1=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\PARAMS.i"\
	

"$(INTDIR)\chgmp1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_CHGMP1=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\PARAMS.i"\
	

"$(INTDIR)\chgmp1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\chain.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_CHAIN=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\chain.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_CHAIN=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\chain.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\cdscal.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_CDSCA=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\PARAMS.i"\
	

"$(INTDIR)\cdscal.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_CDSCA=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\PARAMS.i"\
	

"$(INTDIR)\cdscal.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\cct1do.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_CCT1D=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\cct1do.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_CCT1D=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\cct1do.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\cardan.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_CARDA=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\cardan.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_CARDA=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\cardan.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\capcnv.f

"$(INTDIR)\capcnv.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\calvk.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_CALVK=\
	".\Release\KEYS.i"\
	".\Release\SIZES.i"\
	

"$(INTDIR)\calvk.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_CALVK=\
	".\Debug\KEYS.i"\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\calvk.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\calpar.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_CALPA=\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\calpar.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_CALPA=\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\calpar.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\calcbp.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_CALCB=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\calcbp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_CALCB=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\calcbp.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\bornpl.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_BORNP=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\bornpl.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_BORNP=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\bornpl.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\bonds.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_BONDS=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\bonds.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_BONDS=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\bonds.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\bfn.f

"$(INTDIR)\bfn.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\axis.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_AXIS_=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\axis.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_AXIS_=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\axis.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\areal.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_AREAL=\
	".\Release\SIZES.i"\
	".\Release\SIZES2.i"\
	

"$(INTDIR)\areal.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_AREAL=\
	".\Debug\SIZES.i"\
	".\Debug\SIZES2.i"\
	

"$(INTDIR)\areal.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\areah.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_AREAH=\
	".\Release\SIZES.i"\
	".\Release\SIZES2.i"\
	".\Release\KEYS.i"\
	

"$(INTDIR)\areah.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_AREAH=\
	".\Debug\SIZES.i"\
	".\Debug\SIZES2.i"\
	".\Debug\KEYS.i"\
	

"$(INTDIR)\areah.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\arcch.f

"$(INTDIR)\arcch.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\anavib.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_ANAVI=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\anavib.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_ANAVI=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\anavib.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\amsol.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_AMSOL=\
	".\Release\SIZES.i"\
	".\Release\SIZES2.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	".\Release\PARAM.i"\
	

"$(INTDIR)\amsol.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_AMSOL=\
	".\Debug\SIZES.i"\
	".\Debug\SIZES2.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	".\Debug\PARAM.i"\
	

"$(INTDIR)\amsol.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\amsel.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_AMSEL=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\amsel.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_AMSEL=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\amsel.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\am1.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_AM1_F=\
	".\Release\SIZES.i"\
	".\Release\KEYS.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\am1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_AM1_F=\
	".\Debug\SIZES.i"\
	".\Debug\KEYS.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\am1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\aabbcd.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_AABBC=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\aabbcd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_AABBC=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\aabbcd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\new\aabacd.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_AABAC=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\aabacd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_AABAC=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\aabacd.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\update.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_UPDAT=\
	".\Release\FFILES.i"\
	

"$(INTDIR)\update.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_UPDAT=\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\update.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\status.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_STATU=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\status.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_STATU=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\status.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\readvt.f

"$(INTDIR)\readvt.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\rabiot.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_RABIO=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\rabiot.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_RABIO=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\rabiot.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\perm.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_PERM_=\
	".\Release\FFILES.i"\
	

"$(INTDIR)\perm.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_PERM_=\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\perm.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\osinv.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_OSINV=\
	".\Release\FFILES.i"\
	

"$(INTDIR)\osinv.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_OSINV=\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\osinv.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\mtxm.f

"$(INTDIR)\mtxm.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\mecip.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_MECIP=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\mecip.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_MECIP=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\mecip.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\jincar.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_JINCA=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\jincar.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_JINCA=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\jincar.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\intcar.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_INTCA=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\intcar.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_INTCA=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\intcar.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\ijkl.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_IJKL_=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\ijkl.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_IJKL_=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\ijkl.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\haddon.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_HADDO=\
	".\Release\FFILES.i"\
	

"$(INTDIR)\haddon.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_HADDO=\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\haddon.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\freqcy.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_FREQC=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\freqcy.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_FREQC=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\freqcy.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\frame.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_FRAME=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\frame.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_FRAME=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\frame.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\dot.f

"$(INTDIR)\dot.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\dijkl2.f

"$(INTDIR)\dijkl2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\dijkl1.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DIJKL=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\dijkl1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DIJKL=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\dijkl1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\deri22.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_DERI22=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\deri22.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_DERI22=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\deri22.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\babbbc.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_BABBB=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\babbbc.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_BABBB=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\babbbc.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\amstat.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_AMSTA=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\amstat.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_AMSTA=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\amstat.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\port\aababc.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_AABAB=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\aababc.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_AABAB=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\aababc.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\unmod\util_unmod.f

"$(INTDIR)\util_unmod.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\unmod\ef_lib.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_EF_LI=\
	".\Release\SIZES.i"\
	".\Release\FFILES.i"\
	

"$(INTDIR)\ef_lib.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_EF_LI=\
	".\Debug\SIZES.i"\
	".\Debug\FFILES.i"\
	

"$(INTDIR)\ef_lib.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MSDEV\Projects\amsol7.0\unmod\ampac_unmod.f

!IF  "$(CFG)" == "amsol70 - Win32 Release"

NODEP_F90_AMPAC=\
	".\Release\SIZES.i"\
	

"$(INTDIR)\ampac_unmod.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "amsol70 - Win32 Debug"

NODEP_F90_AMPAC=\
	".\Debug\SIZES.i"\
	

"$(INTDIR)\ampac_unmod.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
