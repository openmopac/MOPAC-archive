

#
# Compiler flags
#

FFLAGS3 = -O
FFlags= 

#
# Path definitions
#

OBJ =  objects

#
# Compile macros
#

Compile_opt    = g95   -O            -o $@ -c 
$Compile_opt    = g95   -ftrace=full  -o $@ -c 
Compile_no_opt = g95       -o $@ -c 

#
# Objects
#

# Object files included in library

OBJS = \
$(OBJ)/aababc.o                   $(OBJ)/aababc_I.o                 $(OBJ)/aabacd.o                    \
$(OBJ)/aabacd_I.o                 $(OBJ)/aabbcd.o                   $(OBJ)/aabbcd_I.o                  \
$(OBJ)/add_more_interactions.o    $(OBJ)/addfck_I.o                 $(OBJ)/addhb.o                     \
$(OBJ)/addhcr_I.o                 $(OBJ)/addnuc_I.o                 $(OBJ)/adjvec.o                    \
$(OBJ)/aijm_I.o                   $(OBJ)/aintgs_I.o                 $(OBJ)/alpb_and_xfac_am1.o         \
$(OBJ)/alpb_and_xfac_mndo.o       $(OBJ)/alpb_and_xfac_mndod.o      $(OBJ)/alpb_and_xfac_pm3.o         \
$(OBJ)/alphaf_I.o                 $(OBJ)/analyt.o                   $(OBJ)/analyt_C.o                  \
$(OBJ)/anavib.o                   $(OBJ)/asum_I.o                   $(OBJ)/atomrs.o                    \
$(OBJ)/aval_I.o                   $(OBJ)/axis.o                     $(OBJ)/axis_I.o                    \
$(OBJ)/babbbc.o                   $(OBJ)/babbbc_I.o                 $(OBJ)/babbcd.o                    \
$(OBJ)/babbcd_I.o                 $(OBJ)/bangle.o                   $(OBJ)/bangle_I.o                  \
$(OBJ)/bdenin_I.o                 $(OBJ)/bdenup_I.o                 $(OBJ)/beopor_I.o                  \
$(OBJ)/betaf_I.o                  $(OBJ)/betal1_I.o                 $(OBJ)/betall_I.o                  \
$(OBJ)/betcom_I.o                 $(OBJ)/bfn.o                      $(OBJ)/bfn_I.o                     \
$(OBJ)/bintgs_I.o                 $(OBJ)/blas.o                     $(OBJ)/blas_I.o                    \
$(OBJ)/bldsym.o                   $(OBJ)/bldsym_I.o                 $(OBJ)/bmakuf_I.o                  \
$(OBJ)/bonds.o                    $(OBJ)/bonds_for_MOZYME.o         $(OBJ)/linear_cosmo.o              \
$(OBJ)/buildf.o                   $(OBJ)/calpar.o                   $(OBJ)/calpar_I.o                  \
$(OBJ)/canon.o                    $(OBJ)/capcor.o                   $(OBJ)/capcor_I.o                  \
$(OBJ)/cartab_I.o                 $(OBJ)/ccprod_I.o                 $(OBJ)/ccrep_I.o                   \
$(OBJ)/cdiag_I.o                  $(OBJ)/chanel_C.o                 $(OBJ)/charg_I.o                   \
$(OBJ)/charmo.o                   $(OBJ)/charmo_I.o                 $(OBJ)/charst.o                    \
$(OBJ)/charst_I.o                 $(OBJ)/charvi.o                   $(OBJ)/charvi_I.o                  \
$(OBJ)/check.o                    $(OBJ)/chi_I.o                    $(OBJ)/chkion.o                    \
$(OBJ)/chklew.o                   $(OBJ)/chrge.o                    $(OBJ)/chrge_for_MOZYME.o          \
$(OBJ)/chrge_I.o                  $(OBJ)/ciint_I.o                  $(OBJ)/ciosci.o                    \
$(OBJ)/ciosci_I.o                 $(OBJ)/cnvg.o                     $(OBJ)/cnvg_I.o                    \
$(OBJ)/cnvgz.o                    $(OBJ)/coe.o                      $(OBJ)/coe_I.o                     \
$(OBJ)/collid_I.o                 $(OBJ)/collis_I.o                 $(OBJ)/Common_arrays_C.o           \
$(OBJ)/compct.o                   $(OBJ)/compfg.o                   $(OBJ)/compfg_I.o                  \
$(OBJ)/compfn_I.o                 $(OBJ)/conref_C.o                 $(OBJ)/consts_I.o                  \
$(OBJ)/convert_storage.o          $(OBJ)/copym_I.o                  $(OBJ)/cosmo.o                     \
$(OBJ)/cosmo_C.o                  $(OBJ)/cqden_I.o                  $(OBJ)/csum_I.o                    \
$(OBJ)/dang.o                     $(OBJ)/dang_I.o                   $(OBJ)/darea1_I.o                  \
$(OBJ)/daread_I.o                 $(OBJ)/dasum_I.o                  $(OBJ)/datin.o                     \
$(OBJ)/datin_I.o                  $(OBJ)/dawrit_I.o                 $(OBJ)/dawrt1_I.o                  \
$(OBJ)/daxpy_I.o                  $(OBJ)/dcart.o                    $(OBJ)/dcart_for_MOZYME.o          \
$(OBJ)/dcart_I.o                  $(OBJ)/dcopy_I.o                  $(OBJ)/ddot_I.o                    \
$(OBJ)/ddpo_I.o                   $(OBJ)/delmol_I.o                 $(OBJ)/delri_I.o                   \
$(OBJ)/delsta.o                   $(OBJ)/delta_I.o                  $(OBJ)/denrot.o                    \
$(OBJ)/denrot_for_MOZYME.o        $(OBJ)/denrot_I.o                 $(OBJ)/densf_I.o                   \
$(OBJ)/densit.o                   $(OBJ)/densit_I.o                 $(OBJ)/density_for_MOZYME.o        \
$(OBJ)/deri0.o                    $(OBJ)/deri0_I.o                  $(OBJ)/deri1.o                     \
$(OBJ)/deri1_I.o                  $(OBJ)/deri2.o                    $(OBJ)/deri21.o                    \
$(OBJ)/deri21_I.o                 $(OBJ)/deri22.o                   $(OBJ)/deri22_I.o                  \
$(OBJ)/deri23.o                   $(OBJ)/deri23_I.o                 $(OBJ)/deri2_I.o                   \
$(OBJ)/deritr.o                   $(OBJ)/deritr_I.o                 $(OBJ)/deriv.o                     \
$(OBJ)/deriv_I.o                  $(OBJ)/dernvo.o                   $(OBJ)/dernvo_I.o                  \
$(OBJ)/ders_I.o                   $(OBJ)/dex2_I.o                   $(OBJ)/dfield.o                    \
$(OBJ)/dfield_I.o                 $(OBJ)/dfock2.o                   $(OBJ)/dfock2_I.o                  \
$(OBJ)/dfpsav.o                   $(OBJ)/dfpsav_I.o                 $(OBJ)/dgefa_I.o                   \
$(OBJ)/dgemv_I.o                  $(OBJ)/dgetri_I.o                 $(OBJ)/dhc.o                       \
$(OBJ)/dhc_I.o                    $(OBJ)/dhcore.o                   $(OBJ)/dhcore_I.o                  \
$(OBJ)/diag.o                     $(OBJ)/diag_I.o                   $(OBJ)/diagg.o                     \
$(OBJ)/diagg1.o                   $(OBJ)/diagg2.o                   $(OBJ)/diagi.o                     \
$(OBJ)/diagi_I.o                  $(OBJ)/diat.o                     $(OBJ)/diat2_I.o                   \
$(OBJ)/diat_I.o                   $(OBJ)/diegrd_I.o                 $(OBJ)/dielen_I.o                  \
$(OBJ)/digit.o                    $(OBJ)/digit_I.o                  $(OBJ)/dihed.o                     \
$(OBJ)/dihed_I.o                  $(OBJ)/dijkl1.o                   $(OBJ)/dijkl1_I.o                  \
$(OBJ)/dijkl2.o                   $(OBJ)/dijkl2_I.o                 $(OBJ)/dijkld_I.o                  \
$(OBJ)/dimens.o                   $(OBJ)/dimens_I.o                 $(OBJ)/dipole.o                    \
$(OBJ)/dipole_for_MOZYME.o        $(OBJ)/dipole_I.o                 $(OBJ)/dist2.o                     \
$(OBJ)/dist2_I.o                  $(OBJ)/dmecip_I.o                 $(OBJ)/dnrm2_I.o                   \
$(OBJ)/dofs.o                     $(OBJ)/dofs_I.o                   $(OBJ)/dopen_I.o                   \
$(OBJ)/dot.o                      $(OBJ)/dot_I.o                    $(OBJ)/drc.o                       \
$(OBJ)/drc_I.o                    $(OBJ)/drcout.o                   $(OBJ)/drcout_I.o                  \
$(OBJ)/drepp2_I.o                 $(OBJ)/drotat_I.o                 $(OBJ)/dsum_I.o                    \
$(OBJ)/dtran2.o                   $(OBJ)/dtran2_I.o                 $(OBJ)/dtrans.o                    \
$(OBJ)/dtrans_I.o                 $(OBJ)/dtrmm_I.o                  $(OBJ)/dtrmv_I.o                   \
$(OBJ)/dtrti2_I.o                 $(OBJ)/dtrtri_I.o                 $(OBJ)/dvfill_I.o                  \
$(OBJ)/ef.o                       $(OBJ)/ef_C.o                     $(OBJ)/ef_I.o                      \
$(OBJ)/eigen.o                    $(OBJ)/eimp.o                     $(OBJ)/einvit_I.o                  \
$(OBJ)/eiscor_I.o                 $(OBJ)/elau_I.o                   $(OBJ)/elemts_C.o                  \
$(OBJ)/elenuc_I.o                 $(OBJ)/elesn_I.o                  $(OBJ)/en_I.o                      \
$(OBJ)/enpart.o                   $(OBJ)/enpart_I.o                 $(OBJ)/epsab_I.o                   \
$(OBJ)/epseta.o                   $(OBJ)/epseta_I.o                 $(OBJ)/epslon_I.o                  \
$(OBJ)/eqlrat_I.o                 $(OBJ)/errion.o                   $(OBJ)/esn_I.o                     \
$(OBJ)/esp.o                      $(OBJ)/esp1_I.o                   $(OBJ)/esp_C.o                     \
$(OBJ)/esp_utilities.o            $(OBJ)/espfit_I.o                 $(OBJ)/estpi1_I.o                  \
$(OBJ)/etime_I.o                  $(OBJ)/etrbk3_I.o                 $(OBJ)/etred3_I.o                  \
$(OBJ)/euler_C.o                  $(OBJ)/evvrsp_I.o                 $(OBJ)/exchng.o                    \
$(OBJ)/exchng_I.o                 $(OBJ)/fbx_I.o                    $(OBJ)/fcnpp_I.o                   \
$(OBJ)/ffreq1_I.o                 $(OBJ)/ffreq2_I.o                 $(OBJ)/fhpatn_I.o                  \
$(OBJ)/fillij.o                   $(OBJ)/findn1.o                   $(OBJ)/finish.o                    \
$(OBJ)/flepo.o                    $(OBJ)/flepo_I.o                  $(OBJ)/fmat.o                      \
$(OBJ)/fmat_I.o                   $(OBJ)/fock1.o                    $(OBJ)/fock1_for_MOZYME.o          \
$(OBJ)/fock1_I.o                  $(OBJ)/fock2.o                    $(OBJ)/fock2_I.o                   \
$(OBJ)/fock2z.o                   $(OBJ)/fockd2_I.o                 $(OBJ)/force.o                     \
$(OBJ)/force_I.o                  $(OBJ)/fordd_I.o                  $(OBJ)/formxy.o                    \
$(OBJ)/formxy_I.o                 $(OBJ)/forsav.o                   $(OBJ)/frame.o                     \
$(OBJ)/frame_I.o                  $(OBJ)/freda_I.o                  $(OBJ)/freqcy.o                    \
$(OBJ)/fsub_I.o                   $(OBJ)/funcon_C.o                 $(OBJ)/genun.o                     \
$(OBJ)/genun_I.o                  $(OBJ)/genvec_I.o                 $(OBJ)/geochk.o                    \
$(OBJ)/geout.o                    $(OBJ)/geout_I.o                  $(OBJ)/geoutg.o                    \
$(OBJ)/geoutg_I.o                 $(OBJ)/getdat.o                   $(OBJ)/GetDateStamp.o              \
$(OBJ)/getenvqq.o                 $(OBJ)/afmm_mod.o                  \
$(OBJ)/getgeg.o                   $(OBJ)/getgeg_I.o                 $(OBJ)/getgeo.o                    \
$(OBJ)/getgeo_I.o                 $(OBJ)/getpdb.o                   $(OBJ)/getsym.o                    \
$(OBJ)/getsym_I.o                 $(OBJ)/gettxt.o                   $(OBJ)/gettxt_I.o                  \
$(OBJ)/getval.o                   $(OBJ)/getval_I.o                 $(OBJ)/gmetry.o                    \
$(OBJ)/gmetry_I.o                 $(OBJ)/gover_I.o                  $(OBJ)/greek.o                     \
$(OBJ)/grid.o                     $(OBJ)/grid_I.o                   $(OBJ)/grids_I.o                   \
$(OBJ)/gstore_I.o                 $(OBJ)/h1elec.o                   $(OBJ)/h1elec_I.o                  \
$(OBJ)/haddon_I.o                 $(OBJ)/hbonds.o                   $(OBJ)/hcore.o                     \
$(OBJ)/hcore_for_MOZYME.o         $(OBJ)/hcore_I.o                  $(OBJ)/hcored_I.o                  \
$(OBJ)/helect.o                   $(OBJ)/helect_I.o                 $(OBJ)/helecz.o                    \
$(OBJ)/hmuf_I.o                   $(OBJ)/hplusf_I.o                 $(OBJ)/hybrid.o                    \
$(OBJ)/ijbo.o                     $(OBJ)/ijkl.o                     $(OBJ)/inid_I.o                    \
$(OBJ)/inighd_I.o                 $(OBJ)/init_filenames.o           $(OBJ)/initsn_I.o                  \
$(OBJ)/initsv_I.o                 $(OBJ)/insymc_I.o                 $(OBJ)/interp.o                    \
$(OBJ)/interp_I.o                 $(OBJ)/intfc.o                    $(OBJ)/intfc_I.o                   \
$(OBJ)/ionout.o                   $(OBJ)/ird_I.o                    $(OBJ)/isitsc.o                    \
$(OBJ)/iten_I.o                   $(OBJ)/iter.o                     $(OBJ)/iter_C.o                    \
$(OBJ)/iter_for_MOZYME.o          $(OBJ)/iter_I.o                   $(OBJ)/jab.o                       \
$(OBJ)/jab_for_MOZYME.o           $(OBJ)/jab_I.o                    $(OBJ)/jcarin.o                    \
$(OBJ)/jcarin_I.o                 $(OBJ)/jdate.o                    $(OBJ)/journal_references_C.o      \
$(OBJ)/kab.o                      $(OBJ)/kab_for_MOZYME.o           $(OBJ)/kab_I.o                     \
$(OBJ)/lbfgs.o                    $(OBJ)/lewis.o                    $(OBJ)/ligand.o                    \
$(OBJ)/linmin.o                   $(OBJ)/linmin_I.o                 $(OBJ)/local.o                     \
$(OBJ)/local2.o                   $(OBJ)/local_I.o                   \
$(OBJ)/locmin.o                   $(OBJ)/locmin_I.o                 $(OBJ)/lsame_I.o                   \
$(OBJ)/lyse.o                     $(OBJ)/makeuf_I.o                 $(OBJ)/makopr_I.o                  \
$(OBJ)/maksym.o                   $(OBJ)/maksym_I.o                 $(OBJ)/makvec.o                    \
$(OBJ)/mamult.o                   $(OBJ)/mamult_I.o                 $(OBJ)/maps_C.o                    \
$(OBJ)/mat33.o                    $(OBJ)/mat33_I.o                  $(OBJ)/matou1.o                    \
$(OBJ)/matou1_I.o                 $(OBJ)/matout.o                   $(OBJ)/matout_I.o                  \
$(OBJ)/mbonds.o                   $(OBJ)/me08a_I.o                  $(OBJ)/meci.o                      \
$(OBJ)/meci_C.o                   $(OBJ)/meci_I.o                   $(OBJ)/mecid.o                     \
$(OBJ)/mecid_I.o                  $(OBJ)/mecih.o                    $(OBJ)/mecih_I.o                   \
$(OBJ)/mecip.o                    $(OBJ)/mecip_I.o                  $(OBJ)/mepchg_I.o                  \
$(OBJ)/mepmap_I.o                 $(OBJ)/meprot_I.o                 $(OBJ)/minv.o                      \
$(OBJ)/minv_I.o                   $(OBJ)/mlmo.o                     $(OBJ)/mndod.o                     \
$(OBJ)/mndod_C.o                  $(OBJ)/mod_atomradii.o            $(OBJ)/modchg.o                    \
$(OBJ)/modgra.o                   $(OBJ)/moldat.o                   $(OBJ)/molkst_C.o                  \
$(OBJ)/molmec_C.o                 $(OBJ)/molsym_I.o                 $(OBJ)/molsymy.o                   \
$(OBJ)/molval.o                   $(OBJ)/molval_I.o                 $(OBJ)/mopac.o                     \
$(OBJ)/mopend.o                   $(OBJ)/mopend_I.o                 $(OBJ)/MOZYME_C.o                  \
$(OBJ)/mpcbds_I.o                 $(OBJ)/mpcpop_I.o                 $(OBJ)/mpcsyb.o                    \
$(OBJ)/mtxm.o                     $(OBJ)/mtxm_I.o                   $(OBJ)/mtxmc.o                     \
$(OBJ)/mtxmc_I.o                  $(OBJ)/mullik.o                   $(OBJ)/mullik_for_MOZYME.o         \
$(OBJ)/mullik_I.o                 $(OBJ)/mult.o                     $(OBJ)/mult33.o                    \
$(OBJ)/mult33_I.o                 $(OBJ)/mult_I.o                   $(OBJ)/mxm.o                       \
$(OBJ)/mxm_I.o                    $(OBJ)/mxmt.o                     $(OBJ)/mxmt_I.o                    \
$(OBJ)/myword.o                   $(OBJ)/myword_I.o                 $(OBJ)/naican_I.o                  \
$(OBJ)/naicas_I.o                 $(OBJ)/names.o                    $(OBJ)/new_esp.o                   \
$(OBJ)/newflg.o                   $(OBJ)/ngamtg_I.o                 $(OBJ)/ngefis_I.o                  \
$(OBJ)/ngidri_I.o                 $(OBJ)/ngoke_I.o                  $(OBJ)/nllsn_I.o                   \
$(OBJ)/nllsq.o                    $(OBJ)/nllsq_I.o                  $(OBJ)/nonbet_I.o                  \
$(OBJ)/nonope_I.o                 $(OBJ)/nonor_I.o                  $(OBJ)/nuchar.o                    \
$(OBJ)/nuchar_I.o                 $(OBJ)/nxtmer.o                   $(OBJ)/openda_I.o                  \
$(OBJ)/orient_I.o                 $(OBJ)/osinv.o                    $(OBJ)/osinv_I.o                   \
$(OBJ)/outer1.o                   $(OBJ)/outer2.o                   $(OBJ)/overlaps_C.o                \
$(OBJ)/ovlp_I.o                   $(OBJ)/packp_I.o                  $(OBJ)/parameters_C.o              \
$(OBJ)/parameters_for_AM1_C.o     $(OBJ)/parameters_for_mndo_C.o    $(OBJ)/parameters_for_mndod_C.o    \
$(OBJ)/parameters_for_pm3_C.o     $(OBJ)/parameters_for_PM6_C.o     $(OBJ)/parameters_for_RM1_C.o      \
$(OBJ)/parsav.o                   $(OBJ)/parsav_I.o                 $(OBJ)/partxy.o                    \
$(OBJ)/partxy_I.o                 $(OBJ)/Password.o                 $(OBJ)/pathk.o                     \
$(OBJ)/pathk_I.o                  $(OBJ)/paths.o                    $(OBJ)/paths_I.o                   \
$(OBJ)/pdbout.o                   $(OBJ)/pdgrid_I.o                 $(OBJ)/perm.o                      \
$(OBJ)/perm_I.o                   $(OBJ)/picopt.o                   $(OBJ)/pinout.o                    \
$(OBJ)/plato_I.o                  $(OBJ)/pmep.o                     $(OBJ)/pmep1_I.o                   \
$(OBJ)/pmep_I.o                   $(OBJ)/pmepco_I.o                 $(OBJ)/poij_I.o                    \
$(OBJ)/pol_vol_I.o                $(OBJ)/polar.o                    $(OBJ)/polar_C.o                   \
$(OBJ)/polar_I.o                  $(OBJ)/potcal_I.o                 $(OBJ)/powsav_I.o                  \
$(OBJ)/powsq.o                    $(OBJ)/powsq_I.o                  $(OBJ)/printp_I.o                  \
$(OBJ)/prtdrc.o                   $(OBJ)/prtdrc_I.o                 $(OBJ)/prtgra.o                    \
$(OBJ)/prthco_I.o                 $(OBJ)/prthes_I.o                 $(OBJ)/prtlmo.o                    \
$(OBJ)/prtpar_I.o                 $(OBJ)/prtpka.o                   $(OBJ)/prttim.o                    \
$(OBJ)/prttim_I.o                 $(OBJ)/pulay.o                    $(OBJ)/pulay_I.o                   \
$(OBJ)/quadr.o                    $(OBJ)/quadr_I.o                  $(OBJ)/react1.o                    \
$(OBJ)/react1_I.o                 $(OBJ)/reada.o                    $(OBJ)/reada_I.o                   \
$(OBJ)/readmo.o                   $(OBJ)/readmo_I.o                 $(OBJ)/redatm_I.o                  \
$(OBJ)/refer.o                    $(OBJ)/refer_I.o                  $(OBJ)/refkey_C.o                  \
$(OBJ)/reorth.o                   $(OBJ)/repp.o                     $(OBJ)/repp_I.o                    \
$(OBJ)/reppd2_I.o                 $(OBJ)/reppd_I.o                  $(OBJ)/reseq.o                     \
$(OBJ)/resolv.o                   $(OBJ)/resolv_I.o                 $(OBJ)/rijkl_I.o                   \
$(OBJ)/rotat_I.o                  $(OBJ)/rotatd_I.o                 $(OBJ)/rotate.o                    \
$(OBJ)/rotate_C.o                 $(OBJ)/rotate_I.o                 $(OBJ)/rotlmo.o                    \
$(OBJ)/rotmat_I.o                 $(OBJ)/rotmol.o                   $(OBJ)/rotmol_I.o                  \
$(OBJ)/rotzz.o                    $(OBJ)/rsc_I.o                    $(OBJ)/rsp.o                       \
$(OBJ)/rsp_I.o                    $(OBJ)/run_mopac.o                $(OBJ)/scfcri.o                    \
$(OBJ)/schmib.o                   $(OBJ)/schmib_I.o                 $(OBJ)/schmit.o                    \
$(OBJ)/schmit_I.o                 $(OBJ)/scprm_I.o                  $(OBJ)/search_I.o                  \
$(OBJ)/second.o                   $(OBJ)/second_I.o                 $(OBJ)/selmos.o                    \
$(OBJ)/set.o                      $(OBJ)/set_I.o                    $(OBJ)/set_up_dentate.o            \
$(OBJ)/set_up_MOZYME_arrays.o     $(OBJ)/set_up_RAPID.o             $(OBJ)/setup3_I.o                  \
$(OBJ)/setup_mopac_arrays.o       $(OBJ)/setupg.o                   $(OBJ)/setupk.o                    \
$(OBJ)/solrot.o                   $(OBJ)/solrot_I.o                 $(OBJ)/sort.o                      \
$(OBJ)/sort_I.o                   $(OBJ)/sp_two_electron.o          $(OBJ)/spcore_I.o                  \
$(OBJ)/spline_I.o                 $(OBJ)/ss_I.o                     $(OBJ)/static_polarizability.o      \
$(OBJ)/suma2_I.o                  $(OBJ)/supdot.o                   $(OBJ)/supdot_I.o                  \
$(OBJ)/superd.o                   $(OBJ)/superd_I.o                 $(OBJ)/surfa_I.o                   \
$(OBJ)/surfac_I.o                 $(OBJ)/swap.o                     $(OBJ)/swap_I.o                    \
$(OBJ)/switch.o                   $(OBJ)/symdec_I.o                 $(OBJ)/symh.o                      \
$(OBJ)/symh_I.o                   $(OBJ)/symmetry_C.o               $(OBJ)/symoir.o                    \
$(OBJ)/symoir_I.o                 $(OBJ)/symopr.o                   $(OBJ)/symopr_I.o                  \
$(OBJ)/symp_I.o                   $(OBJ)/sympop.o                   $(OBJ)/sympop_I.o                  \
$(OBJ)/symr.o                     $(OBJ)/symr_I.o                   $(OBJ)/symt.o                      \
$(OBJ)/symt_I.o                   $(OBJ)/symtry.o                   $(OBJ)/symtry_I.o                  \
$(OBJ)/symtrz.o                   $(OBJ)/symtrz_I.o                 $(OBJ)/tf_I.o                      \
$(OBJ)/thermo.o                   $(OBJ)/thermo_I.o                 $(OBJ)/tidy.o                      \
$(OBJ)/time_I.o                   $(OBJ)/timer.o                    $(OBJ)/timer_I.o                   \
$(OBJ)/timout.o                   $(OBJ)/timout_I.o                 $(OBJ)/to_screen.o                 \
$(OBJ)/to_screen_C.o              $(OBJ)/to_screen_I.o              $(OBJ)/transf_I.o                  \
$(OBJ)/trsub_I.o                  $(OBJ)/trudgu_I.o                 $(OBJ)/trugdu_I.o                  \
$(OBJ)/trugud_I.o                 $(OBJ)/tx_I.o                     $(OBJ)/txtype.o                    \
$(OBJ)/upcase.o                   $(OBJ)/upcase_I.o                 $(OBJ)/update.o                    \
$(OBJ)/update_I.o                 $(OBJ)/values.o                   $(OBJ)/vastkind.o                  \
$(OBJ)/vecprt.o                   $(OBJ)/vecprt_for_MOZYME.o        $(OBJ)/vecprt_I.o                  \
$(OBJ)/volume.o                   $(OBJ)/volume_I.o                 $(OBJ)/w2mat_I.o                   \
$(OBJ)/worder_I.o                 $(OBJ)/wrdkey_I.o                 $(OBJ)/write_trajectory.o          \
$(OBJ)/write_trajectory_I.o       $(OBJ)/writmo.o                   $(OBJ)/writmo_I.o                  \
$(OBJ)/wrtkey.o                   $(OBJ)/wrtkey_I.o                 $(OBJ)/wrttxt.o                    \
$(OBJ)/wrttxt_I.o                 $(OBJ)/wstore_I.o                 $(OBJ)/wwstep_I.o                  \
$(OBJ)/xerbla_I.o                 $(OBJ)/xxx.o                      $(OBJ)/xxx_I.o                     \
$(OBJ)/xyzcry.o                   $(OBJ)/xyzcry_I.o                 $(OBJ)/xyzint.o                    \
$(OBJ)/xyzint_I.o                 $(OBJ)/zerom_I.o                 

#
# Define targets
#

#
# Make MOPAC executable for Windows-XP
#

MOPAC2007.exe: $(OBJ) $(OBJS) 
	g95 -o   MOPAC2007.exe   $(OBJS) 

#
# Make MOPAC executable for Linux     
#

MOPAC2007.out: $(OBJ) $(OBJS) 
	g95 -o   MOPAC2007.out   $(OBJS) 


# Directory for object files
$(OBJ): 
	mkdir $(OBJ)


# Clean up all
clean: 
	(rm -f ./objects_for_WINDOWS_XP/*.o ; rm -f ./*.mod )


#
# Dependencies of Fortran source files
#
$(OBJ)/aababc.o: \
  $(OBJ)/meci_C.o  \
  $(OBJ)/vastkind.o  \
  aababc.F90
	 $(Compile_opt) $(FFlags) aababc.F90
  
$(OBJ)/aababc_I.o: \
  $(OBJ)/vastkind.o  \
  aababc_I.F90
	 $(Compile_opt) $(FFlags) aababc_I.F90
  
$(OBJ)/aabacd.o: \
  $(OBJ)/vastkind.o  \
  aabacd.F90
	 $(Compile_opt) $(FFlags) aabacd.F90
  
$(OBJ)/aabacd_I.o: \
  $(OBJ)/vastkind.o  \
  aabacd_I.F90
	 $(Compile_opt) $(FFlags) aabacd_I.F90
  
$(OBJ)/aabbcd.o: \
  $(OBJ)/meci_C.o  \
  $(OBJ)/vastkind.o  \
  aabbcd.F90
	 $(Compile_opt) $(FFlags) aabbcd.F90
  
$(OBJ)/aabbcd_I.o: \
  $(OBJ)/vastkind.o  \
  aabbcd_I.F90
	 $(Compile_opt) $(FFlags) aabbcd_I.F90
  
$(OBJ)/add_more_interactions.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/iter_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  add_more_interactions.F90
	 $(Compile_opt) $(FFlags) add_more_interactions.F90
  
$(OBJ)/addfck_I.o: \
  $(OBJ)/vastkind.o  \
  addfck_I.F90
	 $(Compile_opt) $(FFlags) addfck_I.F90
  
$(OBJ)/addhb.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  addhb.F90
	 $(Compile_opt) $(FFlags) addhb.F90
  
$(OBJ)/addhcr_I.o: \
  addhcr_I.F90
	 $(Compile_opt) $(FFlags) addhcr_I.F90
  
$(OBJ)/addnuc_I.o: \
  addnuc_I.F90
	 $(Compile_opt) $(FFlags) addnuc_I.F90
  
$(OBJ)/adjvec.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  adjvec.F90
	 $(Compile_opt) $(FFlags) adjvec.F90
  
$(OBJ)/aijm_I.o: \
  aijm_I.F90
	 $(Compile_opt) $(FFlags) aijm_I.F90
  
$(OBJ)/aintgs_I.o: \
  $(OBJ)/vastkind.o  \
  aintgs_I.F90
	 $(Compile_opt) $(FFlags) aintgs_I.F90
  
$(OBJ)/alpb_and_xfac_am1.o: \
  $(OBJ)/parameters_C.o  \
  alpb_and_xfac_am1.F90
	 $(Compile_opt) $(FFlags) alpb_and_xfac_am1.F90
  
$(OBJ)/alpb_and_xfac_mndo.o: \
  $(OBJ)/parameters_C.o  \
  alpb_and_xfac_mndo.F90
	 $(Compile_opt) $(FFlags) alpb_and_xfac_mndo.F90
  
$(OBJ)/alpb_and_xfac_mndod.o: \
  $(OBJ)/parameters_C.o  \
  alpb_and_xfac_mndod.F90
	 $(Compile_opt) $(FFlags) alpb_and_xfac_mndod.F90
  
$(OBJ)/alpb_and_xfac_pm3.o: \
  $(OBJ)/parameters_C.o  \
  alpb_and_xfac_pm3.F90
	 $(Compile_opt) $(FFlags) alpb_and_xfac_pm3.F90
  
$(OBJ)/alphaf_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  alphaf_I.F90
	 $(Compile_opt) $(FFlags) alphaf_I.F90
  
$(OBJ)/analyt.o: \
  $(OBJ)/analyt_C.o  \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/delri_I.o  \
  $(OBJ)/ders_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/overlaps_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/rotat_I.o  \
  $(OBJ)/vastkind.o  \
  analyt.F90
	 $(Compile_opt) $(FFlags) analyt.F90
  
$(OBJ)/analyt_C.o: \
  $(OBJ)/vastkind.o  \
  analyt_C.F90
	 $(Compile_opt) $(FFlags) analyt_C.F90
  
$(OBJ)/anavib.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/to_screen_C.o  \
  $(OBJ)/vastkind.o  \
  anavib.F90
	 $(Compile_opt) $(FFlags) anavib.F90
  
$(OBJ)/asum_I.o: \
  $(OBJ)/vastkind.o  \
  asum_I.F90
	 $(Compile_opt) $(FFlags) asum_I.F90
  
$(OBJ)/atomrs.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  atomrs.F90
	 $(Compile_opt) $(FFlags) atomrs.F90
  
$(OBJ)/aval_I.o: \
  $(OBJ)/vastkind.o  \
  aval_I.F90
	 $(Compile_opt) $(FFlags) aval_I.F90
  
$(OBJ)/axis.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/rsp_I.o  \
  $(OBJ)/to_screen_C.o  \
  $(OBJ)/vastkind.o  \
  axis.F90
	 $(Compile_opt) $(FFlags) axis.F90
  
$(OBJ)/axis_I.o: \
  $(OBJ)/vastkind.o  \
  axis_I.F90
	 $(Compile_opt) $(FFlags) axis_I.F90
  
$(OBJ)/babbbc.o: \
  $(OBJ)/meci_C.o  \
  $(OBJ)/vastkind.o  \
  babbbc.F90
	 $(Compile_opt) $(FFlags) babbbc.F90
  
$(OBJ)/babbbc_I.o: \
  $(OBJ)/vastkind.o  \
  babbbc_I.F90
	 $(Compile_opt) $(FFlags) babbbc_I.F90
  
$(OBJ)/babbcd.o: \
  $(OBJ)/vastkind.o  \
  babbcd.F90
	 $(Compile_opt) $(FFlags) babbcd.F90
  
$(OBJ)/babbcd_I.o: \
  $(OBJ)/vastkind.o  \
  babbcd_I.F90
	 $(Compile_opt) $(FFlags) babbcd_I.F90
  
$(OBJ)/bangle.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  bangle.F90
	 $(Compile_opt) $(FFlags) bangle.F90
  
$(OBJ)/bangle_I.o: \
  $(OBJ)/vastkind.o  \
  bangle_I.F90
	 $(Compile_opt) $(FFlags) bangle_I.F90
  
$(OBJ)/bdenin_I.o: \
  $(OBJ)/vastkind.o  \
  bdenin_I.F90
	 $(Compile_opt) $(FFlags) bdenin_I.F90
  
$(OBJ)/bdenup_I.o: \
  $(OBJ)/vastkind.o  \
  bdenup_I.F90
	 $(Compile_opt) $(FFlags) bdenup_I.F90
  
$(OBJ)/beopor_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  beopor_I.F90
	 $(Compile_opt) $(FFlags) beopor_I.F90
  
$(OBJ)/betaf_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  betaf_I.F90
	 $(Compile_opt) $(FFlags) betaf_I.F90
  
$(OBJ)/betal1_I.o: \
  $(OBJ)/vastkind.o  \
  betal1_I.F90
	 $(Compile_opt) $(FFlags) betal1_I.F90
  
$(OBJ)/betall_I.o: \
  $(OBJ)/vastkind.o  \
  betall_I.F90
	 $(Compile_opt) $(FFlags) betall_I.F90
  
$(OBJ)/betcom_I.o: \
  $(OBJ)/vastkind.o  \
  betcom_I.F90
	 $(Compile_opt) $(FFlags) betcom_I.F90
  
$(OBJ)/bfn.o: \
  $(OBJ)/overlaps_C.o  \
  $(OBJ)/vastkind.o  \
  bfn.F90
	 $(Compile_opt) $(FFlags) bfn.F90
  
$(OBJ)/bfn_I.o: \
  $(OBJ)/vastkind.o  \
  bfn_I.F90
	 $(Compile_opt) $(FFlags) bfn_I.F90
  
$(OBJ)/bintgs_I.o: \
  $(OBJ)/vastkind.o  \
  bintgs_I.F90
	 $(Compile_opt) $(FFlags) bintgs_I.F90
  
$(OBJ)/blas.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/blas_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/xerbla_I.o  \
  blas.F90
	 $(Compile_opt) $(FFlags) blas.F90
  
$(OBJ)/blas_I.o: \
  $(OBJ)/vastkind.o  \
  blas_I.F90
	 $(Compile_opt) $(FFlags) blas_I.F90
  
$(OBJ)/bldsym.o: \
  $(OBJ)/mult33_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  bldsym.F90
	 $(Compile_opt) $(FFlags) bldsym.F90
  
$(OBJ)/bldsym_I.o: \
  bldsym_I.F90
	 $(Compile_opt) $(FFlags) bldsym_I.F90
  
$(OBJ)/bmakuf_I.o: \
  $(OBJ)/vastkind.o  \
  bmakuf_I.F90
	 $(Compile_opt) $(FFlags) bmakuf_I.F90
  
$(OBJ)/bonds.o: \
  $(OBJ)/vastkind.o  \
  bonds.F90
	 $(Compile_opt) $(FFlags) bonds.F90
  
$(OBJ)/bonds_for_MOZYME.o: \
  $(OBJ)/vastkind.o  \
  bonds_for_MOZYME.F90
	 $(Compile_opt) $(FFlags) bonds_for_MOZYME.F90
  
$(OBJ)/buildf.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  buildf.F90
	 $(Compile_opt) $(FFlags) buildf.F90
  
$(OBJ)/calpar.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/vastkind.o  \
  calpar.F90
	 $(Compile_opt) $(FFlags) calpar.F90
  
$(OBJ)/calpar_I.o: \
  calpar_I.F90
	 $(Compile_opt) $(FFlags) calpar_I.F90
  
$(OBJ)/canon.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  canon.F90
	 $(Compile_opt) $(FFlags) canon.F90
  
$(OBJ)/capcor.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  capcor.F90
	 $(Compile_opt) $(FFlags) capcor.F90
  
$(OBJ)/capcor_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  capcor_I.F90
	 $(Compile_opt) $(FFlags) capcor_I.F90
  
$(OBJ)/cartab_I.o: \
  cartab_I.F90
	 $(Compile_opt) $(FFlags) cartab_I.F90
  
$(OBJ)/ccprod_I.o: \
  $(OBJ)/vastkind.o  \
  ccprod_I.F90
	 $(Compile_opt) $(FFlags) ccprod_I.F90
  
$(OBJ)/ccrep_I.o: \
  $(OBJ)/vastkind.o  \
  ccrep_I.F90
	 $(Compile_opt) $(FFlags) ccrep_I.F90
  
$(OBJ)/cdiag_I.o: \
  cdiag_I.F90
	 $(Compile_opt) $(FFlags) cdiag_I.F90
  
$(OBJ)/chanel_C.o: \
  $(OBJ)/vastkind.o  \
  chanel_C.F90
	 $(Compile_opt) $(FFlags) chanel_C.F90
  
$(OBJ)/charg_I.o: \
  $(OBJ)/vastkind.o  \
  charg_I.F90
	 $(Compile_opt) $(FFlags) charg_I.F90
  
$(OBJ)/charmo.o: \
  $(OBJ)/dtrans_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  charmo.F90
	 $(Compile_opt) $(FFlags) charmo.F90
  
$(OBJ)/charmo_I.o: \
  $(OBJ)/vastkind.o  \
  charmo_I.F90
	 $(Compile_opt) $(FFlags) charmo_I.F90
  
$(OBJ)/charst.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/dtrans_I.o  \
  $(OBJ)/matout_I.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/minv_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  charst.F90
	 $(Compile_opt) $(FFlags) charst.F90
  
$(OBJ)/charst_I.o: \
  $(OBJ)/meci_C.o  \
  $(OBJ)/vastkind.o  \
  charst_I.F90
	 $(Compile_opt) $(FFlags) charst_I.F90
  
$(OBJ)/charvi.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  charvi.F90
	 $(Compile_opt) $(FFlags) charvi.F90
  
$(OBJ)/charvi_I.o: \
  $(OBJ)/vastkind.o  \
  charvi_I.F90
	 $(Compile_opt) $(FFlags) charvi_I.F90
  
$(OBJ)/check.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  check.F90
	 $(Compile_opt) $(FFlags) check.F90
  
$(OBJ)/chi_I.o: \
  $(OBJ)/vastkind.o  \
  chi_I.F90
	 $(Compile_opt) $(FFlags) chi_I.F90
  
$(OBJ)/chkion.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/parameters_C.o  \
  chkion.F90
	 $(Compile_opt) $(FFlags) chkion.F90
  
$(OBJ)/chklew.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/parameters_C.o  \
  chklew.F90
	 $(Compile_opt) $(FFlags) chklew.F90
  
$(OBJ)/chrge.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  chrge.F90
	 $(Compile_opt) $(FFlags) chrge.F90
  
$(OBJ)/chrge_for_MOZYME.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  chrge_for_MOZYME.F90
	 $(Compile_opt) $(FFlags) chrge_for_MOZYME.F90
  
$(OBJ)/chrge_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  chrge_I.F90
	 $(Compile_opt) $(FFlags) chrge_I.F90
  
$(OBJ)/ciint_I.o: \
  $(OBJ)/vastkind.o  \
  ciint_I.F90
	 $(Compile_opt) $(FFlags) ciint_I.F90
  
$(OBJ)/ciosci.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/matout_I.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/vastkind.o  \
  ciosci.F90
	 $(Compile_opt) $(FFlags) ciosci.F90
  
$(OBJ)/ciosci_I.o: \
  $(OBJ)/vastkind.o  \
  ciosci_I.F90
	 $(Compile_opt) $(FFlags) ciosci_I.F90
  
$(OBJ)/cnvg.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  cnvg.F90
	 $(Compile_opt) $(FFlags) cnvg.F90
  
$(OBJ)/cnvg_I.o: \
  $(OBJ)/vastkind.o  \
  cnvg_I.F90
	 $(Compile_opt) $(FFlags) cnvg_I.F90
  
$(OBJ)/cnvgz.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  cnvgz.F90
	 $(Compile_opt) $(FFlags) cnvgz.F90
  
$(OBJ)/coe.o: \
  $(OBJ)/vastkind.o  \
  coe.F90
	 $(Compile_opt) $(FFlags) coe.F90
  
$(OBJ)/coe_I.o: \
  $(OBJ)/vastkind.o  \
  coe_I.F90
	 $(Compile_opt) $(FFlags) coe_I.F90
  
$(OBJ)/collid_I.o: \
  $(OBJ)/vastkind.o  \
  collid_I.F90
	 $(Compile_opt) $(FFlags) collid_I.F90
  
$(OBJ)/collis_I.o: \
  $(OBJ)/vastkind.o  \
  collis_I.F90
	 $(Compile_opt) $(FFlags) collis_I.F90
  
$(OBJ)/Common_arrays_C.o: \
  $(OBJ)/vastkind.o  \
  Common_arrays_C.F90
	 $(Compile_opt) $(FFlags) Common_arrays_C.F90
  
$(OBJ)/compct.o: \
  compct.F90
	 $(Compile_opt) $(FFlags) compct.F90
  
$(OBJ)/compfg.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/deriv_I.o  \
  $(OBJ)/dihed_I.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/gmetry_I.o  \
  $(OBJ)/hcore_I.o  \
  $(OBJ)/iter_I.o  \
  $(OBJ)/mecip_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/molmec_C.o  \
  $(OBJ)/prtpar_I.o  \
  $(OBJ)/symtry_I.o  \
  $(OBJ)/timer_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/volume_I.o  \
  compfg.F90
	 $(Compile_opt) $(FFlags) compfg.F90
  
$(OBJ)/compfg_I.o: \
  $(OBJ)/vastkind.o  \
  compfg_I.F90
	 $(Compile_opt) $(FFlags) compfg_I.F90
  
$(OBJ)/compfn_I.o: \
  $(OBJ)/vastkind.o  \
  compfn_I.F90
	 $(Compile_opt) $(FFlags) compfn_I.F90
  
$(OBJ)/conref_C.o: \
  $(OBJ)/vastkind.o  \
  conref_C.F90
	 $(Compile_opt) $(FFlags) conref_C.F90
  
$(OBJ)/consts_I.o: \
  consts_I.F90
	 $(Compile_opt) $(FFlags) consts_I.F90
  
$(OBJ)/convert_storage.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  convert_storage.F90
	 $(Compile_opt) $(FFlags) convert_storage.F90
  
$(OBJ)/linear_cosmo.o: \
  linear_cosmo.F90
	 $(Compile_opt) $(FFlags) linear_cosmo.F90
  
$(OBJ)/copym_I.o: \
  $(OBJ)/vastkind.o  \
  copym_I.F90
	 $(Compile_opt) $(FFlags) copym_I.F90
  
$(OBJ)/cosmo.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/reada_I.o  \
  cosmo.F90
	 $(Compile_opt) $(FFlags) cosmo.F90
  
$(OBJ)/cosmo_C.o: \
  $(OBJ)/vastkind.o  \
  cosmo_C.F90
	 $(Compile_opt) $(FFlags) cosmo_C.F90
  
$(OBJ)/cqden_I.o: \
  cqden_I.F90
	 $(Compile_opt) $(FFlags) cqden_I.F90
  
$(OBJ)/csum_I.o: \
  $(OBJ)/vastkind.o  \
  csum_I.F90
	 $(Compile_opt) $(FFlags) csum_I.F90
  
$(OBJ)/dang.o: \
  $(OBJ)/vastkind.o  \
  dang.F90
	 $(Compile_opt) $(FFlags) dang.F90
  
$(OBJ)/dang_I.o: \
  $(OBJ)/vastkind.o  \
  dang_I.F90
	 $(Compile_opt) $(FFlags) dang_I.F90
  
$(OBJ)/darea1_I.o: \
  $(OBJ)/vastkind.o  \
  darea1_I.F90
	 $(Compile_opt) $(FFlags) darea1_I.F90
  
$(OBJ)/daread_I.o: \
  $(OBJ)/vastkind.o  \
  daread_I.F90
	 $(Compile_opt) $(FFlags) daread_I.F90
  
$(OBJ)/dasum_I.o: \
  $(OBJ)/vastkind.o  \
  dasum_I.F90
	 $(Compile_opt) $(FFlags) dasum_I.F90
  
$(OBJ)/datin.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/upcase_I.o  \
  $(OBJ)/update_I.o  \
  $(OBJ)/vastkind.o  \
  datin.F90
	 $(Compile_opt) $(FFlags) datin.F90
  
$(OBJ)/datin_I.o: \
  datin_I.F90
	 $(Compile_opt) $(FFlags) datin_I.F90
  
$(OBJ)/dawrit_I.o: \
  $(OBJ)/vastkind.o  \
  dawrit_I.F90
	 $(Compile_opt) $(FFlags) dawrit_I.F90
  
$(OBJ)/dawrt1_I.o: \
  $(OBJ)/vastkind.o  \
  dawrt1_I.F90
	 $(Compile_opt) $(FFlags) dawrt1_I.F90
  
$(OBJ)/daxpy_I.o: \
  $(OBJ)/vastkind.o  \
  daxpy_I.F90
	 $(Compile_opt) $(FFlags) daxpy_I.F90
  
$(OBJ)/dcart.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/dhc_I.o  \
  $(OBJ)/dihed_I.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/molmec_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/vastkind.o  \
  dcart.F90
	 $(Compile_opt) $(FFlags) dcart.F90
  
$(OBJ)/dcart_for_MOZYME.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  dcart_for_MOZYME.F90
	 $(Compile_opt) $(FFlags) dcart_for_MOZYME.F90
  
$(OBJ)/dcart_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  dcart_I.F90
	 $(Compile_opt) $(FFlags) dcart_I.F90
  
$(OBJ)/dcopy_I.o: \
  $(OBJ)/vastkind.o  \
  dcopy_I.F90
	 $(Compile_opt) $(FFlags) dcopy_I.F90
  
$(OBJ)/ddot_I.o: \
  $(OBJ)/vastkind.o  \
  ddot_I.F90
	 $(Compile_opt) $(FFlags) ddot_I.F90
  
$(OBJ)/ddpo_I.o: \
  ddpo_I.F90
	 $(Compile_opt) $(FFlags) ddpo_I.F90
  
$(OBJ)/delmol_I.o: \
  $(OBJ)/vastkind.o  \
  delmol_I.F90
	 $(Compile_opt) $(FFlags) delmol_I.F90
  
$(OBJ)/delri_I.o: \
  $(OBJ)/vastkind.o  \
  delri_I.F90
	 $(Compile_opt) $(FFlags) delri_I.F90
  
$(OBJ)/delsta.o: \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  delsta.F90
	 $(Compile_opt) $(FFlags) delsta.F90
  
$(OBJ)/delta_I.o: \
  delta_I.F90
	 $(Compile_opt) $(FFlags) delta_I.F90
  
$(OBJ)/denrot.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/coe_I.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/gmetry_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  denrot.F90
	 $(Compile_opt) $(FFlags) denrot.F90
  
$(OBJ)/denrot_for_MOZYME.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  denrot_for_MOZYME.F90
	 $(Compile_opt) $(FFlags) denrot_for_MOZYME.F90
  
$(OBJ)/denrot_I.o: \
  denrot_I.F90
	 $(Compile_opt) $(FFlags) denrot_I.F90
  
$(OBJ)/densf_I.o: \
  $(OBJ)/vastkind.o  \
  densf_I.F90
	 $(Compile_opt) $(FFlags) densf_I.F90
  
$(OBJ)/densit.o: \
  $(OBJ)/vastkind.o  \
  densit.F90
	 $(Compile_opt) $(FFlags) densit.F90
  
$(OBJ)/densit_I.o: \
  $(OBJ)/vastkind.o  \
  densit_I.F90
	 $(Compile_opt) $(FFlags) densit_I.F90
  
$(OBJ)/density_for_MOZYME.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  density_for_MOZYME.F90
	 $(Compile_opt) $(FFlags) density_for_MOZYME.F90
  
$(OBJ)/deri0.o: \
  $(OBJ)/vastkind.o  \
  deri0.F90
	 $(Compile_opt) $(FFlags) deri0.F90
  
$(OBJ)/deri0_I.o: \
  $(OBJ)/vastkind.o  \
  deri0_I.F90
	 $(Compile_opt) $(FFlags) deri0_I.F90
  
$(OBJ)/deri1.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/dcopy_I.o  \
  $(OBJ)/ef_C.o  \
  $(OBJ)/dfock2_I.o  \
  $(OBJ)/dhcore_I.o  \
  $(OBJ)/dijkl1_I.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/helect_I.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/mecid_I.o  \
  $(OBJ)/mecih_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mtxm_I.o  \
  $(OBJ)/mxm_I.o  \
  $(OBJ)/supdot_I.o  \
  $(OBJ)/timer_I.o  \
  $(OBJ)/vastkind.o  \
  deri1.F90
	 $(Compile_opt) $(FFlags) deri1.F90
  
$(OBJ)/deri1_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  deri1_I.F90
	 $(Compile_opt) $(FFlags) deri1_I.F90
  
$(OBJ)/deri2.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/dcopy_I.o  \
  $(OBJ)/deri22_I.o  \
  $(OBJ)/deri23_I.o  \
  $(OBJ)/ef_C.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/mecid_I.o  \
  $(OBJ)/mecih_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/mtxm_I.o  \
  $(OBJ)/mxm_I.o  \
  $(OBJ)/mxmt_I.o  \
  $(OBJ)/osinv_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/supdot_I.o  \
  $(OBJ)/vastkind.o  \
  deri2.F90
	 $(Compile_opt) $(FFlags) deri2.F90
  
$(OBJ)/deri21.o: \
  $(OBJ)/mtxmc_I.o  \
  $(OBJ)/mxm_I.o  \
  $(OBJ)/rsp_I.o  \
  $(OBJ)/vastkind.o  \
  deri21.F90
	 $(Compile_opt) $(FFlags) deri21.F90
  
$(OBJ)/deri21_I.o: \
  $(OBJ)/vastkind.o  \
  deri21_I.F90
	 $(Compile_opt) $(FFlags) deri21_I.F90
  
$(OBJ)/deri22.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/ddot_I.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/fock2_I.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mtxm_I.o  \
  $(OBJ)/mxm_I.o  \
  $(OBJ)/mxmt_I.o  \
  $(OBJ)/supdot_I.o  \
  $(OBJ)/vastkind.o  \
  deri22.F90
	 $(Compile_opt) $(FFlags) deri22.F90
  
$(OBJ)/deri22_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  deri22_I.F90
	 $(Compile_opt) $(FFlags) deri22_I.F90
  
$(OBJ)/deri23.o: \
  $(OBJ)/dcopy_I.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  deri23.F90
	 $(Compile_opt) $(FFlags) deri23.F90
  
$(OBJ)/deri23_I.o: \
  $(OBJ)/vastkind.o  \
  deri23_I.F90
	 $(Compile_opt) $(FFlags) deri23_I.F90
  
$(OBJ)/deri2_I.o: \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  deri2_I.F90
	 $(Compile_opt) $(FFlags) deri2_I.F90
  
$(OBJ)/deritr.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/gmetry_I.o  \
  $(OBJ)/hcore_I.o  \
  $(OBJ)/iter_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/symtry_I.o  \
  $(OBJ)/vastkind.o  \
  deritr.F90
	 $(Compile_opt) $(FFlags) deritr.F90
  
$(OBJ)/deritr_I.o: \
  deritr_I.F90
	 $(Compile_opt) $(FFlags) deritr_I.F90
  
$(OBJ)/deriv.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/dcart_I.o  \
  $(OBJ)/ef_C.o  \
  $(OBJ)/dernvo_I.o  \
  $(OBJ)/dfield_I.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/gmetry_I.o  \
  $(OBJ)/jcarin_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/mxm_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/symtry_I.o  \
  $(OBJ)/upcase_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/volume_I.o  \
  deriv.F90
	 $(Compile_opt) $(FFlags) deriv.F90
  
$(OBJ)/deriv_I.o: \
  $(OBJ)/vastkind.o  \
  deriv_I.F90
	 $(Compile_opt) $(FFlags) deriv_I.F90
  
$(OBJ)/dernvo.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/deri0_I.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  dernvo.F90
	 $(Compile_opt) $(FFlags) dernvo.F90
  
$(OBJ)/dernvo_I.o: \
  dernvo_I.F90
	 $(Compile_opt) $(FFlags) dernvo_I.F90
  
$(OBJ)/ders_I.o: \
  $(OBJ)/vastkind.o  \
  ders_I.F90
	 $(Compile_opt) $(FFlags) ders_I.F90
  
$(OBJ)/dex2_I.o: \
  dex2_I.F90
	 $(Compile_opt) $(FFlags) dex2_I.F90
  
$(OBJ)/dfield.o: \
  $(OBJ)/chrge_I.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/vastkind.o  \
  dfield.F90
	 $(Compile_opt) $(FFlags) dfield.F90
  
$(OBJ)/dfield_I.o: \
  dfield_I.F90
	 $(Compile_opt) $(FFlags) dfield_I.F90
  
$(OBJ)/dfock2.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/jab_I.o  \
  $(OBJ)/kab_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  dfock2.F90
	 $(Compile_opt) $(FFlags) dfock2.F90
  
$(OBJ)/dfock2_I.o: \
  $(OBJ)/vastkind.o  \
  dfock2_I.F90
	 $(Compile_opt) $(FFlags) dfock2_I.F90
  
$(OBJ)/dfpsav.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/ef_C.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/maps_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/vastkind.o  \
  dfpsav.F90
	 $(Compile_opt) $(FFlags) dfpsav.F90
  
$(OBJ)/dfpsav_I.o: \
  $(OBJ)/vastkind.o  \
  dfpsav_I.F90
	 $(Compile_opt) $(FFlags) dfpsav_I.F90
  
$(OBJ)/dgefa_I.o: \
  $(OBJ)/vastkind.o  \
  dgefa_I.F90
	 $(Compile_opt) $(FFlags) dgefa_I.F90
  
$(OBJ)/dgemv_I.o: \
  $(OBJ)/vastkind.o  \
  dgemv_I.F90
	 $(Compile_opt) $(FFlags) dgemv_I.F90
  
$(OBJ)/dgetri_I.o: \
  $(OBJ)/vastkind.o  \
  dgetri_I.F90
	 $(Compile_opt) $(FFlags) dgetri_I.F90
  
$(OBJ)/dhc.o: \
  $(OBJ)/helect_I.o  \
  $(OBJ)/molkst_C.o  \
  dhc.F90
	 $(Compile_opt) $(FFlags) dhc.F90
  
$(OBJ)/dhc_I.o: \
  $(OBJ)/vastkind.o  \
  dhc_I.F90
	 $(Compile_opt) $(FFlags) dhc_I.F90
  
$(OBJ)/dhcore.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/h1elec_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  dhcore.F90
	 $(Compile_opt) $(FFlags) dhcore.F90
  
$(OBJ)/dhcore_I.o: \
  $(OBJ)/vastkind.o  \
  dhcore_I.F90
	 $(Compile_opt) $(FFlags) dhcore_I.F90
  
$(OBJ)/diag.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  diag.F90
	 $(Compile_opt) $(FFlags) diag.F90
  
$(OBJ)/diag_I.o: \
  $(OBJ)/vastkind.o  \
  diag_I.F90
	 $(Compile_opt) $(FFlags) diag_I.F90
  
$(OBJ)/diagg.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  diagg.F90
	 $(Compile_opt) $(FFlags) diagg.F90
  
$(OBJ)/diagg1.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  diagg1.F90
	 $(Compile_opt) $(FFlags) diagg1.F90
  
$(OBJ)/diagg2.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/reada_I.o  \
  diagg2.F90
	 $(Compile_opt) $(FFlags) diagg2.F90
  
$(OBJ)/diagi.o: \
  $(OBJ)/vastkind.o  \
  diagi.F90
	 $(Compile_opt) $(FFlags) diagi.F90
  
$(OBJ)/diagi_I.o: \
  $(OBJ)/meci_C.o  \
  $(OBJ)/vastkind.o  \
  diagi_I.F90
	 $(Compile_opt) $(FFlags) diagi_I.F90
  
$(OBJ)/diat.o: \
  $(OBJ)/bfn_I.o  \
  $(OBJ)/coe_I.o  \
  $(OBJ)/diat2_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/overlaps_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/set_I.o  \
  $(OBJ)/ss_I.o  \
  $(OBJ)/vastkind.o  \
  diat.F90
	 $(Compile_opt) $(FFlags) diat.F90
  
$(OBJ)/diat2_I.o: \
  $(OBJ)/vastkind.o  \
  diat2_I.F90
	 $(Compile_opt) $(FFlags) diat2_I.F90
  
$(OBJ)/diat_I.o: \
  $(OBJ)/vastkind.o  \
  diat_I.F90
	 $(Compile_opt) $(FFlags) diat_I.F90
  
$(OBJ)/diegrd_I.o: \
  $(OBJ)/molkst_C.o  \
  diegrd_I.F90
	 $(Compile_opt) $(FFlags) diegrd_I.F90
  
$(OBJ)/dielen_I.o: \
  $(OBJ)/vastkind.o  \
  dielen_I.F90
	 $(Compile_opt) $(FFlags) dielen_I.F90
  
$(OBJ)/digit.o: \
  $(OBJ)/vastkind.o  \
  digit.F90
	 $(Compile_opt) $(FFlags) digit.F90
  
$(OBJ)/digit_I.o: \
  digit_I.F90
	 $(Compile_opt) $(FFlags) digit_I.F90
  
$(OBJ)/dihed.o: \
  $(OBJ)/dang_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/vastkind.o  \
  dihed.F90
	 $(Compile_opt) $(FFlags) dihed.F90
  
$(OBJ)/dihed_I.o: \
  $(OBJ)/vastkind.o  \
  dihed_I.F90
	 $(Compile_opt) $(FFlags) dihed_I.F90
  
$(OBJ)/dijkl1.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/formxy_I.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  dijkl1.F90
	 $(Compile_opt) $(FFlags) dijkl1.F90
  
$(OBJ)/dijkl1_I.o: \
  $(OBJ)/meci_C.o  \
  $(OBJ)/vastkind.o  \
  dijkl1_I.F90
	 $(Compile_opt) $(FFlags) dijkl1_I.F90
  
$(OBJ)/dijkl2.o: \
  $(OBJ)/dot_I.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  dijkl2.F90
	 $(Compile_opt) $(FFlags) dijkl2.F90
  
$(OBJ)/dijkl2_I.o: \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  dijkl2_I.F90
	 $(Compile_opt) $(FFlags) dijkl2_I.F90
  
$(OBJ)/dijkld_I.o: \
  $(OBJ)/vastkind.o  \
  dijkld_I.F90
	 $(Compile_opt) $(FFlags) dijkld_I.F90
  
$(OBJ)/dimens.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/symopr_I.o  \
  $(OBJ)/vastkind.o  \
  dimens.F90
	 $(Compile_opt) $(FFlags) dimens.F90
  
$(OBJ)/dimens_I.o: \
  $(OBJ)/vastkind.o  \
  dimens_I.F90
	 $(Compile_opt) $(FFlags) dimens_I.F90
  
$(OBJ)/dipole.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/to_screen_C.o  \
  $(OBJ)/vastkind.o  \
  dipole.F90
	 $(Compile_opt) $(FFlags) dipole.F90
  
$(OBJ)/dipole_for_MOZYME.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/parameters_C.o  \
  dipole_for_MOZYME.F90
	 $(Compile_opt) $(FFlags) dipole_for_MOZYME.F90
  
$(OBJ)/dipole_I.o: \
  $(OBJ)/vastkind.o  \
  dipole_I.F90
	 $(Compile_opt) $(FFlags) dipole_I.F90
  
$(OBJ)/dist2.o: \
  $(OBJ)/vastkind.o  \
  dist2.F90
	 $(Compile_opt) $(FFlags) dist2.F90
  
$(OBJ)/dist2_I.o: \
  $(OBJ)/vastkind.o  \
  dist2_I.F90
	 $(Compile_opt) $(FFlags) dist2_I.F90
  
$(OBJ)/dmecip_I.o: \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  dmecip_I.F90
	 $(Compile_opt) $(FFlags) dmecip_I.F90
  
$(OBJ)/dnrm2_I.o: \
  $(OBJ)/vastkind.o  \
  dnrm2_I.F90
	 $(Compile_opt) $(FFlags) dnrm2_I.F90
  
$(OBJ)/dofs.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/vastkind.o  \
  dofs.F90
	 $(Compile_opt) $(FFlags) dofs.F90
  
$(OBJ)/dofs_I.o: \
  $(OBJ)/vastkind.o  \
  dofs_I.F90
	 $(Compile_opt) $(FFlags) dofs_I.F90
  
$(OBJ)/dopen_I.o: \
  $(OBJ)/vastkind.o  \
  dopen_I.F90
	 $(Compile_opt) $(FFlags) dopen_I.F90
  
$(OBJ)/dot.o: \
  $(OBJ)/vastkind.o  \
  dot.F90
	 $(Compile_opt) $(FFlags) dot.F90
  
$(OBJ)/dot_I.o: \
  $(OBJ)/vastkind.o  \
  dot_I.F90
	 $(Compile_opt) $(FFlags) dot_I.F90
  
$(OBJ)/drc.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/compfg_I.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/prtdrc_I.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  drc.F90
	 $(Compile_opt) $(FFlags) drc.F90
  
$(OBJ)/drc_I.o: \
  $(OBJ)/vastkind.o  \
  drc_I.F90
	 $(Compile_opt) $(FFlags) drc_I.F90
  
$(OBJ)/drcout.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/write_trajectory_I.o  \
  drcout.F90
	 $(Compile_opt) $(FFlags) drcout.F90
  
$(OBJ)/drcout_I.o: \
  $(OBJ)/vastkind.o  \
  drcout_I.F90
	 $(Compile_opt) $(FFlags) drcout_I.F90
  
$(OBJ)/drepp2_I.o: \
  $(OBJ)/vastkind.o  \
  drepp2_I.F90
	 $(Compile_opt) $(FFlags) drepp2_I.F90
  
$(OBJ)/drotat_I.o: \
  $(OBJ)/vastkind.o  \
  drotat_I.F90
	 $(Compile_opt) $(FFlags) drotat_I.F90
  
$(OBJ)/dsum_I.o: \
  $(OBJ)/vastkind.o  \
  dsum_I.F90
	 $(Compile_opt) $(FFlags) dsum_I.F90
  
$(OBJ)/dtran2.o: \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/vastkind.o  \
  dtran2.F90
	 $(Compile_opt) $(FFlags) dtran2.F90
  
$(OBJ)/dtran2_I.o: \
  $(OBJ)/vastkind.o  \
  dtran2_I.F90
	 $(Compile_opt) $(FFlags) dtran2_I.F90
  
$(OBJ)/dtrans.o: \
  $(OBJ)/dtran2_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  dtrans.F90
	 $(Compile_opt) $(FFlags) dtrans.F90
  
$(OBJ)/dtrans_I.o: \
  $(OBJ)/vastkind.o  \
  dtrans_I.F90
	 $(Compile_opt) $(FFlags) dtrans_I.F90
  
$(OBJ)/dtrmm_I.o: \
  $(OBJ)/vastkind.o  \
  dtrmm_I.F90
	 $(Compile_opt) $(FFlags) dtrmm_I.F90
  
$(OBJ)/dtrmv_I.o: \
  $(OBJ)/vastkind.o  \
  dtrmv_I.F90
	 $(Compile_opt) $(FFlags) dtrmv_I.F90
  
$(OBJ)/dtrti2_I.o: \
  $(OBJ)/vastkind.o  \
  dtrti2_I.F90
	 $(Compile_opt) $(FFlags) dtrti2_I.F90
  
$(OBJ)/dtrtri_I.o: \
  $(OBJ)/vastkind.o  \
  dtrtri_I.F90
	 $(Compile_opt) $(FFlags) dtrtri_I.F90
  
$(OBJ)/dvfill_I.o: \
  $(OBJ)/vastkind.o  \
  dvfill_I.F90
	 $(Compile_opt) $(FFlags) dvfill_I.F90
  
$(OBJ)/ef.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/ef_C.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/maps_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  ef.F90
	 $(Compile_opt) $(FFlags) ef.F90
  
$(OBJ)/ef_C.o: \
  $(OBJ)/vastkind.o  \
  ef_C.F90
	 $(Compile_opt) $(FFlags) ef_C.F90
  
$(OBJ)/ef_I.o: \
  $(OBJ)/vastkind.o  \
  ef_I.F90
	 $(Compile_opt) $(FFlags) ef_I.F90
  
$(OBJ)/eigen.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/reada_I.o  \
  eigen.F90
	 $(Compile_opt) $(FFlags) eigen.F90
  
$(OBJ)/eimp.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  eimp.F90
	 $(Compile_opt) $(FFlags) eimp.F90
  
$(OBJ)/einvit_I.o: \
  $(OBJ)/vastkind.o  \
  einvit_I.F90
	 $(Compile_opt) $(FFlags) einvit_I.F90
  
$(OBJ)/eiscor_I.o: \
  $(OBJ)/vastkind.o  \
  eiscor_I.F90
	 $(Compile_opt) $(FFlags) eiscor_I.F90
  
$(OBJ)/elau_I.o: \
  $(OBJ)/vastkind.o  \
  elau_I.F90
	 $(Compile_opt) $(FFlags) elau_I.F90
  
$(OBJ)/elemts_C.o: \
  $(OBJ)/vastkind.o  \
  elemts_C.F90
	 $(Compile_opt) $(FFlags) elemts_C.F90
  
$(OBJ)/elenuc_I.o: \
  $(OBJ)/vastkind.o  \
  elenuc_I.F90
	 $(Compile_opt) $(FFlags) elenuc_I.F90
  
$(OBJ)/elesn_I.o: \
  elesn_I.F90
	 $(Compile_opt) $(FFlags) elesn_I.F90
  
$(OBJ)/en_I.o: \
  $(OBJ)/vastkind.o  \
  en_I.F90
	 $(Compile_opt) $(FFlags) en_I.F90
  
$(OBJ)/enpart.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/rotate_I.o  \
  $(OBJ)/vastkind.o  \
  enpart.F90
	 $(Compile_opt) $(FFlags) enpart.F90
  
$(OBJ)/enpart_I.o: \
  enpart_I.F90
	 $(Compile_opt) $(FFlags) enpart_I.F90
  
$(OBJ)/epsab_I.o: \
  $(OBJ)/vastkind.o  \
  epsab_I.F90
	 $(Compile_opt) $(FFlags) epsab_I.F90
  
$(OBJ)/epseta.o: \
  epseta.F90
	 $(Compile_opt) $(FFlags) epseta.F90
  
$(OBJ)/epseta_I.o: \
  $(OBJ)/vastkind.o  \
  epseta_I.F90
	 $(Compile_opt) $(FFlags) epseta_I.F90
  
$(OBJ)/epslon_I.o: \
  $(OBJ)/vastkind.o  \
  epslon_I.F90
	 $(Compile_opt) $(FFlags) epslon_I.F90
  
$(OBJ)/eqlrat_I.o: \
  $(OBJ)/vastkind.o  \
  eqlrat_I.F90
	 $(Compile_opt) $(FFlags) eqlrat_I.F90
  
$(OBJ)/errion.o: \
  $(OBJ)/chanel_C.o  \
  errion.F90
	 $(Compile_opt) $(FFlags) errion.F90
  
$(OBJ)/esn_I.o: \
  $(OBJ)/vastkind.o  \
  esn_I.F90
	 $(Compile_opt) $(FFlags) esn_I.F90
  
$(OBJ)/esp.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/collid_I.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/densit_I.o  \
  $(OBJ)/dex2_I.o  \
  $(OBJ)/dist2_I.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/esp_C.o  \
  $(OBJ)/espfit_I.o  \
  $(OBJ)/fsub_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/genun_I.o  \
  $(OBJ)/gmetry_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/mult_I.o  \
  $(OBJ)/naicas_I.o  \
  $(OBJ)/overlaps_C.o  \
  $(OBJ)/ovlp_I.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/pdgrid_I.o  \
  $(OBJ)/potcal_I.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/rsp_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/setup3_I.o  \
  $(OBJ)/surfac_I.o  \
  $(OBJ)/vastkind.o  \
  esp.F90
	 $(Compile_opt) $(FFlags) esp.F90
  
$(OBJ)/esp1_I.o: \
  esp1_I.F90
	 $(Compile_opt) $(FFlags) esp1_I.F90
  
$(OBJ)/esp_C.o: \
  $(OBJ)/vastkind.o  \
  esp_C.F90
	 $(Compile_opt) $(FFlags) esp_C.F90
  
$(OBJ)/esp_utilities.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  esp_utilities.F90
	 $(Compile_opt) $(FFlags) esp_utilities.F90
  
$(OBJ)/espfit_I.o: \
  espfit_I.F90
	 $(Compile_opt) $(FFlags) espfit_I.F90
  
$(OBJ)/estpi1_I.o: \
  $(OBJ)/vastkind.o  \
  estpi1_I.F90
	 $(Compile_opt) $(FFlags) estpi1_I.F90
  
$(OBJ)/etime_I.o: \
  etime_I.F90
	 $(Compile_opt) $(FFlags) etime_I.F90
  
$(OBJ)/etrbk3_I.o: \
  $(OBJ)/vastkind.o  \
  etrbk3_I.F90
	 $(Compile_opt) $(FFlags) etrbk3_I.F90
  
$(OBJ)/etred3_I.o: \
  $(OBJ)/vastkind.o  \
  etred3_I.F90
	 $(Compile_opt) $(FFlags) etred3_I.F90
  
$(OBJ)/euler_C.o: \
  $(OBJ)/vastkind.o  \
  euler_C.F90
	 $(Compile_opt) $(FFlags) euler_C.F90
  
$(OBJ)/evvrsp_I.o: \
  $(OBJ)/vastkind.o  \
  evvrsp_I.F90
	 $(Compile_opt) $(FFlags) evvrsp_I.F90
  
$(OBJ)/exchng.o: \
  $(OBJ)/vastkind.o  \
  exchng.F90
	 $(Compile_opt) $(FFlags) exchng.F90
  
$(OBJ)/exchng_I.o: \
  $(OBJ)/vastkind.o  \
  exchng_I.F90
	 $(Compile_opt) $(FFlags) exchng_I.F90
  
$(OBJ)/fbx_I.o: \
  fbx_I.F90
	 $(Compile_opt) $(FFlags) fbx_I.F90
  
$(OBJ)/fcnpp_I.o: \
  $(OBJ)/vastkind.o  \
  fcnpp_I.F90
	 $(Compile_opt) $(FFlags) fcnpp_I.F90
  
$(OBJ)/ffreq1_I.o: \
  $(OBJ)/vastkind.o  \
  ffreq1_I.F90
	 $(Compile_opt) $(FFlags) ffreq1_I.F90
  
$(OBJ)/ffreq2_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  ffreq2_I.F90
	 $(Compile_opt) $(FFlags) ffreq2_I.F90
  
$(OBJ)/fhpatn_I.o: \
  $(OBJ)/vastkind.o  \
  fhpatn_I.F90
	 $(Compile_opt) $(FFlags) fhpatn_I.F90
  
$(OBJ)/fillij.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/overlaps_C.o  \
  $(OBJ)/reada_I.o  \
  fillij.F90
	 $(Compile_opt) $(FFlags) fillij.F90
  
$(OBJ)/findn1.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  findn1.F90
	 $(Compile_opt) $(FFlags) findn1.F90
  
$(OBJ)/finish.o: \
  finish.F90
	 $(Compile_opt) $(FFlags) finish.F90
  
$(OBJ)/flepo.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/compfg_I.o  \
  $(OBJ)/dcopy_I.o  \
  $(OBJ)/dfpsav_I.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/linmin_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/prttim_I.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/supdot_I.o  \
  $(OBJ)/vastkind.o  \
  flepo.F90
	 $(Compile_opt) $(FFlags) flepo.F90
  
$(OBJ)/flepo_I.o: \
  $(OBJ)/vastkind.o  \
  flepo_I.F90
	 $(Compile_opt) $(FFlags) flepo_I.F90
  
$(OBJ)/fmat.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/chrge_I.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/compfg_I.o  \
  $(OBJ)/dipole_I.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/sympop_I.o  \
  $(OBJ)/symr_I.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  fmat.F90
	 $(Compile_opt) $(FFlags) fmat.F90
  
$(OBJ)/fmat_I.o: \
  $(OBJ)/vastkind.o  \
  fmat_I.F90
	 $(Compile_opt) $(FFlags) fmat_I.F90
  
$(OBJ)/fock1.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/vastkind.o  \
  fock1.F90
	 $(Compile_opt) $(FFlags) fock1.F90
  
$(OBJ)/fock1_for_MOZYME.o: \
  fock1_for_MOZYME.F90
	 $(Compile_opt) $(FFlags) fock1_for_MOZYME.F90
  
$(OBJ)/fock1_I.o: \
  $(OBJ)/vastkind.o  \
  fock1_I.F90
	 $(Compile_opt) $(FFlags) fock1_I.F90
  
$(OBJ)/fock2.o: \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/jab_I.o  \
  $(OBJ)/kab_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  fock2.F90
	 $(Compile_opt) $(FFlags) fock2.F90
  
$(OBJ)/fock2_I.o: \
  $(OBJ)/vastkind.o  \
  fock2_I.F90
	 $(Compile_opt) $(FFlags) fock2_I.F90
  
$(OBJ)/fock2z.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/parameters_C.o  \
  fock2z.F90
	 $(Compile_opt) $(FFlags) fock2z.F90
  
$(OBJ)/fockd2_I.o: \
  $(OBJ)/vastkind.o  \
  fockd2_I.F90
	 $(Compile_opt) $(FFlags) fockd2_I.F90
  
$(OBJ)/force.o: \
  $(OBJ)/axis_I.o  \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/compfg_I.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/drc_I.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/fmat_I.o  \
  $(OBJ)/frame_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/gmetry_I.o  \
  $(OBJ)/intfc_I.o  \
  $(OBJ)/matou1_I.o  \
  $(OBJ)/matout_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/rsp_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/symtrz_I.o  \
  $(OBJ)/thermo_I.o  \
  $(OBJ)/to_screen_C.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/vecprt_I.o  \
  $(OBJ)/write_trajectory_I.o  \
  $(OBJ)/xyzint_I.o  \
  force.F90
	 $(Compile_opt) $(FFlags) force.F90
  
$(OBJ)/force_I.o: \
  force_I.F90
	 $(Compile_opt) $(FFlags) force_I.F90
  
$(OBJ)/fordd_I.o: \
  fordd_I.F90
	 $(Compile_opt) $(FFlags) fordd_I.F90
  
$(OBJ)/formxy.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  formxy.F90
	 $(Compile_opt) $(FFlags) formxy.F90
  
$(OBJ)/formxy_I.o: \
  $(OBJ)/vastkind.o  \
  formxy_I.F90
	 $(Compile_opt) $(FFlags) formxy_I.F90
  
$(OBJ)/forsav.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/vastkind.o  \
  forsav.F90
	 $(Compile_opt) $(FFlags) forsav.F90
  
$(OBJ)/frame.o: \
  $(OBJ)/axis_I.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  frame.F90
	 $(Compile_opt) $(FFlags) frame.F90
  
$(OBJ)/frame_I.o: \
  $(OBJ)/vastkind.o  \
  frame_I.F90
	 $(Compile_opt) $(FFlags) frame_I.F90
  
$(OBJ)/freda_I.o: \
  $(OBJ)/vastkind.o  \
  freda_I.F90
	 $(Compile_opt) $(FFlags) freda_I.F90
  
$(OBJ)/freqcy.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/frame_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/rsp_I.o  \
  $(OBJ)/symt_I.o  \
  $(OBJ)/symtrz_I.o  \
  $(OBJ)/to_screen_C.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/vecprt_I.o  \
  freqcy.F90
	 $(Compile_opt) $(FFlags) freqcy.F90
  
$(OBJ)/fsub_I.o: \
  $(OBJ)/vastkind.o  \
  fsub_I.F90
	 $(Compile_opt) $(FFlags) fsub_I.F90
  
$(OBJ)/funcon_C.o: \
  $(OBJ)/vastkind.o  \
  funcon_C.F90
	 $(Compile_opt) $(FFlags) funcon_C.F90
  
$(OBJ)/genun.o: \
  $(OBJ)/vastkind.o  \
  genun.F90
	 $(Compile_opt) $(FFlags) genun.F90
  
$(OBJ)/genun_I.o: \
  $(OBJ)/vastkind.o  \
  genun_I.F90
	 $(Compile_opt) $(FFlags) genun_I.F90
  
$(OBJ)/genvec_I.o: \
  $(OBJ)/vastkind.o  \
  genvec_I.F90
	 $(Compile_opt) $(FFlags) genvec_I.F90
  
$(OBJ)/geochk.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/mod_atomradii.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/wrdkey_I.o  \
  geochk.F90
	 $(Compile_opt) $(FFlags) geochk.F90
  
$(OBJ)/geout.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/chrge_I.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/maps_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/wrttxt_I.o  \
  geout.F90
	 $(Compile_opt) $(FFlags) geout.F90
  
$(OBJ)/geout_I.o: \
  geout_I.F90
	 $(Compile_opt) $(FFlags) geout_I.F90
  
$(OBJ)/geoutg.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/xxx_I.o  \
  $(OBJ)/xyzint_I.o  \
  geoutg.F90
	 $(Compile_opt) $(FFlags) geoutg.F90
  
$(OBJ)/geoutg_I.o: \
  geoutg_I.F90
	 $(Compile_opt) $(FFlags) geoutg_I.F90
  
$(OBJ)/getdat.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/to_screen_I.o  \
  getdat.F90
	 $(Compile_opt) $(FFlags) getdat.F90
  
$(OBJ)/GetDateStamp.o: \
  GetDateStamp.F90
	 $(Compile_opt) $(FFlags) GetDateStamp.F90
  
$(OBJ)/afmm_mod.o: \
  afmm_mod.F90
	 $(Compile_opt) $(FFlags) afmm_mod.F90
  
$(OBJ)/getenvqq.o: \
  getenvqq.F90
	 $(Compile_opt) $(FFlags) getenvqq.F90
  
$(OBJ)/getgeg.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/getval_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  getgeg.F90
	 $(Compile_opt) $(FFlags) getgeg.F90
  
$(OBJ)/getgeg_I.o: \
  $(OBJ)/vastkind.o  \
  getgeg_I.F90
	 $(Compile_opt) $(FFlags) getgeg_I.F90
  
$(OBJ)/getgeo.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/gmetry_I.o  \
  $(OBJ)/maps_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/nuchar_I.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/upcase_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/xyzint_I.o  \
  getgeo.F90
	 $(Compile_opt) $(FFlags) getgeo.F90
  
$(OBJ)/getgeo_I.o: \
  $(OBJ)/vastkind.o  \
  getgeo_I.F90
	 $(Compile_opt) $(FFlags) getgeo_I.F90
  
$(OBJ)/getpdb.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/reada_I.o  \
  getpdb.F90
	 $(Compile_opt) $(FFlags) getpdb.F90
  
$(OBJ)/getsym.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  getsym.F90
	 $(Compile_opt) $(FFlags) getsym.F90
  
$(OBJ)/getsym_I.o: \
  $(OBJ)/molkst_C.o  \
  getsym_I.F90
	 $(Compile_opt) $(FFlags) getsym_I.F90
  
$(OBJ)/gettxt.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/upcase_I.o  \
  gettxt.F90
	 $(Compile_opt) $(FFlags) gettxt.F90
  
$(OBJ)/gettxt_I.o: \
  gettxt_I.F90
	 $(Compile_opt) $(FFlags) gettxt_I.F90
  
$(OBJ)/getval.o: \
  $(OBJ)/reada_I.o  \
  $(OBJ)/vastkind.o  \
  getval.F90
	 $(Compile_opt) $(FFlags) getval.F90
  
$(OBJ)/getval_I.o: \
  $(OBJ)/vastkind.o  \
  getval_I.F90
	 $(Compile_opt) $(FFlags) getval_I.F90
  
$(OBJ)/gmetry.o: \
  $(OBJ)/bangle_I.o  \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/vastkind.o  \
  gmetry.F90
	 $(Compile_opt) $(FFlags) gmetry.F90
  
$(OBJ)/gmetry_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  gmetry_I.F90
	 $(Compile_opt) $(FFlags) gmetry_I.F90
  
$(OBJ)/gover_I.o: \
  $(OBJ)/vastkind.o  \
  gover_I.F90
	 $(Compile_opt) $(FFlags) gover_I.F90
  
$(OBJ)/greek.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/MOZYME_C.o  \
  greek.F90
	 $(Compile_opt) $(FFlags) greek.F90
  
$(OBJ)/grid.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/dfpsav_I.o  \
  $(OBJ)/ef_I.o  \
  $(OBJ)/flepo_I.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/maps_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  grid.F90
	 $(Compile_opt) $(FFlags) grid.F90
  
$(OBJ)/grid_I.o: \
  grid_I.F90
	 $(Compile_opt) $(FFlags) grid_I.F90
  
$(OBJ)/grids_I.o: \
  $(OBJ)/vastkind.o  \
  grids_I.F90
	 $(Compile_opt) $(FFlags) grids_I.F90
  
$(OBJ)/gstore_I.o: \
  $(OBJ)/vastkind.o  \
  gstore_I.F90
	 $(Compile_opt) $(FFlags) gstore_I.F90
  
$(OBJ)/h1elec.o: \
  $(OBJ)/diat_I.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/vastkind.o  \
  h1elec.F90
	 $(Compile_opt) $(FFlags) h1elec.F90
  
$(OBJ)/h1elec_I.o: \
  $(OBJ)/vastkind.o  \
  h1elec_I.F90
	 $(Compile_opt) $(FFlags) h1elec_I.F90
  
$(OBJ)/haddon_I.o: \
  $(OBJ)/vastkind.o  \
  haddon_I.F90
	 $(Compile_opt) $(FFlags) haddon_I.F90
  
$(OBJ)/hbonds.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  hbonds.F90
	 $(Compile_opt) $(FFlags) hbonds.F90
  
$(OBJ)/hcore.o: \
  $(OBJ)/addhcr_I.o  \
  $(OBJ)/addnuc_I.o  \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/h1elec_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/overlaps_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/rotate_I.o  \
  $(OBJ)/solrot_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/vecprt_I.o  \
  hcore.F90
	 $(Compile_opt) $(FFlags) hcore.F90
  
$(OBJ)/hcore_for_MOZYME.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/overlaps_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/reada_I.o  \
  hcore_for_MOZYME.F90
	 $(Compile_opt) $(FFlags) hcore_for_MOZYME.F90
  
$(OBJ)/hcore_I.o: \
  hcore_I.F90
	 $(Compile_opt) $(FFlags) hcore_I.F90
  
$(OBJ)/hcored_I.o: \
  hcored_I.F90
	 $(Compile_opt) $(FFlags) hcored_I.F90
  
$(OBJ)/helect.o: \
  $(OBJ)/vastkind.o  \
  helect.F90
	 $(Compile_opt) $(FFlags) helect.F90
  
$(OBJ)/helect_I.o: \
  $(OBJ)/vastkind.o  \
  helect_I.F90
	 $(Compile_opt) $(FFlags) helect_I.F90
  
$(OBJ)/helecz.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  helecz.F90
	 $(Compile_opt) $(FFlags) helecz.F90
  
$(OBJ)/hmuf_I.o: \
  $(OBJ)/vastkind.o  \
  hmuf_I.F90
	 $(Compile_opt) $(FFlags) hmuf_I.F90
  
$(OBJ)/hplusf_I.o: \
  $(OBJ)/vastkind.o  \
  hplusf_I.F90
	 $(Compile_opt) $(FFlags) hplusf_I.F90
  
$(OBJ)/hybrid.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  hybrid.F90
	 $(Compile_opt) $(FFlags) hybrid.F90
  
$(OBJ)/ijbo.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/overlaps_C.o  \
  ijbo.F90
	 $(Compile_opt) $(FFlags) ijbo.F90
  
$(OBJ)/ijkl.o: \
  $(OBJ)/ciint_I.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/partxy_I.o  \
  $(OBJ)/vastkind.o  \
  ijkl.F90
	 $(Compile_opt) $(FFlags) ijkl.F90
  
$(OBJ)/inid_I.o: \
  inid_I.F90
	 $(Compile_opt) $(FFlags) inid_I.F90
  
$(OBJ)/inighd_I.o: \
  inighd_I.F90
	 $(Compile_opt) $(FFlags) inighd_I.F90
  
$(OBJ)/init_filenames.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/molkst_C.o  \
  init_filenames.F90
	 $(Compile_opt) $(FFlags) init_filenames.F90
  
$(OBJ)/initsn_I.o: \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/vastkind.o  \
  initsn_I.F90
	 $(Compile_opt) $(FFlags) initsn_I.F90
  
$(OBJ)/initsv_I.o: \
  initsv_I.F90
	 $(Compile_opt) $(FFlags) initsv_I.F90
  
$(OBJ)/insymc_I.o: \
  insymc_I.F90
	 $(Compile_opt) $(FFlags) insymc_I.F90
  
$(OBJ)/interp.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/rsp_I.o  \
  $(OBJ)/schmib_I.o  \
  $(OBJ)/schmit_I.o  \
  $(OBJ)/spline_I.o  \
  $(OBJ)/vastkind.o  \
  interp.F90
	 $(Compile_opt) $(FFlags) interp.F90
  
$(OBJ)/interp_I.o: \
  $(OBJ)/molkst_C.o  \
  interp_I.F90
	 $(Compile_opt) $(FFlags) interp_I.F90
  
$(OBJ)/intfc.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/jcarin_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/to_screen_C.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  intfc.F90
	 $(Compile_opt) $(FFlags) intfc.F90
  
$(OBJ)/intfc_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  intfc_I.F90
	 $(Compile_opt) $(FFlags) intfc_I.F90
  
$(OBJ)/ionout.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  ionout.F90
	 $(Compile_opt) $(FFlags) ionout.F90
  
$(OBJ)/ird_I.o: \
  ird_I.F90
	 $(Compile_opt) $(FFlags) ird_I.F90
  
$(OBJ)/isitsc.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  isitsc.F90
	 $(Compile_opt) $(FFlags) isitsc.F90
  
$(OBJ)/iten_I.o: \
  $(OBJ)/vastkind.o  \
  iten_I.F90
	 $(Compile_opt) $(FFlags) iten_I.F90
  
$(OBJ)/iter.o: \
  $(OBJ)/capcor_I.o  \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/chrge_I.o  \
  $(OBJ)/cnvg_I.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/densit_I.o  \
  $(OBJ)/diag_I.o  \
  $(OBJ)/fock2_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/helect_I.o  \
  $(OBJ)/interp_I.o  \
  $(OBJ)/iter_C.o  \
  $(OBJ)/maps_C.o  \
  $(OBJ)/matout_I.o  \
  $(OBJ)/meci_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/pulay_I.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/rsp_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/swap_I.o  \
  $(OBJ)/timer_I.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/vecprt_I.o  \
  $(OBJ)/writmo_I.o  \
  iter.F90
	 $(Compile_opt) $(FFlags) iter.F90
  
$(OBJ)/iter_C.o: \
  $(OBJ)/vastkind.o  \
  iter_C.F90
	 $(Compile_opt) $(FFlags) iter_C.F90
  
$(OBJ)/iter_for_MOZYME.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/iter_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/to_screen_I.o  \
  iter_for_MOZYME.F90
	 $(Compile_opt) $(FFlags) iter_for_MOZYME.F90
  
$(OBJ)/iter_I.o: \
  $(OBJ)/vastkind.o  \
  iter_I.F90
	 $(Compile_opt) $(FFlags) iter_I.F90
  
$(OBJ)/jab.o: \
  $(OBJ)/vastkind.o  \
  jab.F90
	 $(Compile_opt) $(FFlags) jab.F90
  
$(OBJ)/jab_for_MOZYME.o: \
  jab_for_MOZYME.F90
	 $(Compile_opt) $(FFlags) jab_for_MOZYME.F90
  
$(OBJ)/jab_I.o: \
  $(OBJ)/vastkind.o  \
  jab_I.F90
	 $(Compile_opt) $(FFlags) jab_I.F90
  
$(OBJ)/jcarin.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/gmetry_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/symtry_I.o  \
  $(OBJ)/vastkind.o  \
  jcarin.F90
	 $(Compile_opt) $(FFlags) jcarin.F90
  
$(OBJ)/jcarin_I.o: \
  $(OBJ)/vastkind.o  \
  jcarin_I.F90
	 $(Compile_opt) $(FFlags) jcarin_I.F90
  
$(OBJ)/jdate.o: \
  jdate.F90
	 $(Compile_opt) $(FFlags) jdate.F90
  
$(OBJ)/journal_references_C.o: \
  $(OBJ)/vastkind.o  \
  journal_references_C.F90
	 $(Compile_opt) $(FFlags) journal_references_C.F90
  
$(OBJ)/kab.o: \
  $(OBJ)/vastkind.o  \
  kab.F90
	 $(Compile_opt) $(FFlags) kab.F90
  
$(OBJ)/kab_for_MOZYME.o: \
  kab_for_MOZYME.F90
	 $(Compile_opt) $(FFlags) kab_for_MOZYME.F90
  
$(OBJ)/kab_I.o: \
  $(OBJ)/vastkind.o  \
  kab_I.F90
	 $(Compile_opt) $(FFlags) kab_I.F90
  
$(OBJ)/lbfgs.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/ddot_I.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/to_screen_I.o  \
  lbfgs.F90
	 $(Compile_opt) $(FFlags) lbfgs.F90
  
$(OBJ)/lewis.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/reada_I.o  \
  lewis.F90
	 $(Compile_opt) $(FFlags) lewis.F90
  
$(OBJ)/ligand.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  ligand.F90
	 $(Compile_opt) $(FFlags) ligand.F90
  
$(OBJ)/linmin.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/compfg_I.o  \
  $(OBJ)/exchng_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  linmin.F90
	 $(Compile_opt) $(FFlags) linmin.F90
  
$(OBJ)/linmin_I.o: \
  $(OBJ)/vastkind.o  \
  linmin_I.F90
	 $(Compile_opt) $(FFlags) linmin_I.F90
  
$(OBJ)/local.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/matout_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/resolv_I.o  \
  $(OBJ)/vastkind.o  \
  local.F90
	 $(Compile_opt) $(FFlags) local.F90
  
$(OBJ)/local2.o: \
  local2.F90
	 $(Compile_opt) $(FFlags) local2.F90
  
  
$(OBJ)/local_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  local_I.F90
	 $(Compile_opt) $(FFlags) local_I.F90
  
$(OBJ)/locmin.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/compfg_I.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/exchng_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  locmin.F90
	 $(Compile_opt) $(FFlags) locmin.F90
  
$(OBJ)/locmin_I.o: \
  $(OBJ)/vastkind.o  \
  locmin_I.F90
	 $(Compile_opt) $(FFlags) locmin_I.F90
  
$(OBJ)/lsame_I.o: \
  lsame_I.F90
	 $(Compile_opt) $(FFlags) lsame_I.F90
  
$(OBJ)/lyse.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  lyse.F90
	 $(Compile_opt) $(FFlags) lyse.F90
  
$(OBJ)/makeuf_I.o: \
  $(OBJ)/vastkind.o  \
  makeuf_I.F90
	 $(Compile_opt) $(FFlags) makeuf_I.F90
  
$(OBJ)/makopr_I.o: \
  $(OBJ)/vastkind.o  \
  makopr_I.F90
	 $(Compile_opt) $(FFlags) makopr_I.F90
  
$(OBJ)/maksym.o: \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  maksym.F90
	 $(Compile_opt) $(FFlags) maksym.F90
  
$(OBJ)/maksym_I.o: \
  $(OBJ)/molkst_C.o  \
  maksym_I.F90
	 $(Compile_opt) $(FFlags) maksym_I.F90
  
$(OBJ)/makvec.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/parameters_C.o  \
  makvec.F90
	 $(Compile_opt) $(FFlags) makvec.F90
  
$(OBJ)/mamult.o: \
  $(OBJ)/vastkind.o  \
  mamult.F90
	 $(Compile_opt) $(FFlags) mamult.F90
  
$(OBJ)/mamult_I.o: \
  $(OBJ)/vastkind.o  \
  mamult_I.F90
	 $(Compile_opt) $(FFlags) mamult_I.F90
  
$(OBJ)/maps_C.o: \
  $(OBJ)/vastkind.o  \
  maps_C.F90
	 $(Compile_opt) $(FFlags) maps_C.F90
  
$(OBJ)/mat33.o: \
  $(OBJ)/vastkind.o  \
  mat33.F90
	 $(Compile_opt) $(FFlags) mat33.F90
  
$(OBJ)/mat33_I.o: \
  $(OBJ)/vastkind.o  \
  mat33_I.F90
	 $(Compile_opt) $(FFlags) mat33_I.F90
  
$(OBJ)/matou1.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/to_screen_C.o  \
  $(OBJ)/vastkind.o  \
  matou1.F90
	 $(Compile_opt) $(FFlags) matou1.F90
  
$(OBJ)/matou1_I.o: \
  $(OBJ)/vastkind.o  \
  matou1_I.F90
	 $(Compile_opt) $(FFlags) matou1_I.F90
  
$(OBJ)/matout.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  matout.F90
	 $(Compile_opt) $(FFlags) matout.F90
  
$(OBJ)/matout_I.o: \
  $(OBJ)/vastkind.o  \
  matout_I.F90
	 $(Compile_opt) $(FFlags) matout_I.F90
  
$(OBJ)/mbonds.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/molkst_C.o  \
  mbonds.F90
	 $(Compile_opt) $(FFlags) mbonds.F90
  
$(OBJ)/me08a_I.o: \
  me08a_I.F90
	 $(Compile_opt) $(FFlags) me08a_I.F90
  
$(OBJ)/meci.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/ciosci_I.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/diagi_I.o  \
  $(OBJ)/dmecip_I.o  \
  $(OBJ)/matou1_I.o  \
  $(OBJ)/matout_I.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/mecih_I.o  \
  $(OBJ)/mndod_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/perm_I.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/rsp_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/symtrz_I.o  \
  $(OBJ)/upcase_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/vecprt_I.o  \
  meci.F90
	 $(Compile_opt) $(FFlags) meci.F90
  
$(OBJ)/meci_C.o: \
  $(OBJ)/vastkind.o  \
  meci_C.F90
	 $(Compile_opt) $(FFlags) meci_C.F90
  
$(OBJ)/meci_I.o: \
  meci_I.F90
	 $(Compile_opt) $(FFlags) meci_I.F90
  
$(OBJ)/mecid.o: \
  $(OBJ)/diagi_I.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/vastkind.o  \
  mecid.F90
	 $(Compile_opt) $(FFlags) mecid.F90
  
$(OBJ)/mecid_I.o: \
  $(OBJ)/meci_C.o  \
  $(OBJ)/vastkind.o  \
  mecid_I.F90
	 $(Compile_opt) $(FFlags) mecid_I.F90
  
$(OBJ)/mecih.o: \
  $(OBJ)/aababc_I.o  \
  $(OBJ)/aabacd_I.o  \
  $(OBJ)/aabbcd_I.o  \
  $(OBJ)/babbbc_I.o  \
  $(OBJ)/babbcd_I.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/vastkind.o  \
  mecih.F90
	 $(Compile_opt) $(FFlags) mecih.F90
  
$(OBJ)/mecih_I.o: \
  $(OBJ)/vastkind.o  \
  mecih_I.F90
	 $(Compile_opt) $(FFlags) mecih_I.F90
  
$(OBJ)/mecip.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mxm_I.o  \
  $(OBJ)/vastkind.o  \
  mecip.F90
	 $(Compile_opt) $(FFlags) mecip.F90
  
$(OBJ)/mecip_I.o: \
  mecip_I.F90
	 $(Compile_opt) $(FFlags) mecip_I.F90
  
$(OBJ)/mepchg_I.o: \
  $(OBJ)/vastkind.o  \
  mepchg_I.F90
	 $(Compile_opt) $(FFlags) mepchg_I.F90
  
$(OBJ)/mepmap_I.o: \
  $(OBJ)/vastkind.o  \
  mepmap_I.F90
	 $(Compile_opt) $(FFlags) mepmap_I.F90
  
$(OBJ)/meprot_I.o: \
  $(OBJ)/vastkind.o  \
  meprot_I.F90
	 $(Compile_opt) $(FFlags) meprot_I.F90
  
$(OBJ)/minv.o: \
  $(OBJ)/vastkind.o  \
  minv.F90
	 $(Compile_opt) $(FFlags) minv.F90
  
$(OBJ)/minv_I.o: \
  $(OBJ)/vastkind.o  \
  minv_I.F90
	 $(Compile_opt) $(FFlags) minv_I.F90
  
$(OBJ)/mlmo.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  mlmo.F90
	 $(Compile_opt) $(FFlags) mlmo.F90
  
$(OBJ)/mndod.o: \
  $(OBJ)/aijm_I.o  \
  $(OBJ)/ccrep_I.o  \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/charg_I.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/ddpo_I.o  \
  $(OBJ)/eiscor_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/inighd_I.o  \
  $(OBJ)/mndod_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/poij_I.o  \
  $(OBJ)/printp_I.o  \
  $(OBJ)/reppd2_I.o  \
  $(OBJ)/reppd_I.o  \
  $(OBJ)/rijkl_I.o  \
  $(OBJ)/rotmat_I.o  \
  $(OBJ)/rsc_I.o  \
  $(OBJ)/scprm_I.o  \
  $(OBJ)/spcore_I.o  \
  $(OBJ)/tx_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/w2mat_I.o  \
  mndod.F90
	 $(Compile_opt) $(FFlags) mndod.F90
  
$(OBJ)/mndod_C.o: \
  $(OBJ)/vastkind.o  \
  mndod_C.F90
	 $(Compile_opt) $(FFlags) mndod_C.F90
  
$(OBJ)/mod_atomradii.o: \
  mod_atomradii.F90
	 $(Compile_opt) $(FFlags) mod_atomradii.F90
  
$(OBJ)/modchg.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  modchg.F90
	 $(Compile_opt) $(FFlags) modchg.F90
  
$(OBJ)/modgra.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  modgra.F90
	 $(Compile_opt) $(FFlags) modgra.F90
  
$(OBJ)/moldat.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/ef_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/gmetry_I.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/molmec_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/refer_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/symtrz_I.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/vecprt_I.o  \
  $(OBJ)/volume_I.o  \
  moldat.F90
	 $(Compile_opt) $(FFlags) moldat.F90
  
$(OBJ)/molkst_C.o: \
  $(OBJ)/vastkind.o  \
  molkst_C.F90
	 $(Compile_opt) $(FFlags) molkst_C.F90
  
$(OBJ)/molmec_C.o: \
  $(OBJ)/vastkind.o  \
  molmec_C.F90
	 $(Compile_opt) $(FFlags) molmec_C.F90
  
$(OBJ)/molsym_I.o: \
  $(OBJ)/vastkind.o  \
  molsym_I.F90
	 $(Compile_opt) $(FFlags) molsym_I.F90
  
$(OBJ)/molsymy.o: \
  $(OBJ)/bangle_I.o  \
  $(OBJ)/bldsym_I.o  \
  $(OBJ)/cartab_I.o  \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/chi_I.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/mult33_I.o  \
  $(OBJ)/orient_I.o  \
  $(OBJ)/plato_I.o  \
  $(OBJ)/rotmol_I.o  \
  $(OBJ)/rsp_I.o  \
  $(OBJ)/symdec_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/symopr_I.o  \
  $(OBJ)/vastkind.o  \
  molsymy.F90
	 $(Compile_opt) $(FFlags) molsymy.F90
  
$(OBJ)/molval.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  molval.F90
	 $(Compile_opt) $(FFlags) molval.F90
  
$(OBJ)/molval_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  molval_I.F90
	 $(Compile_opt) $(FFlags) molval_I.F90
  
$(OBJ)/mopac.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/molkst_C.o  \
  mopac.F90
	 $(Compile_opt) $(FFlags) mopac.F90
  
$(OBJ)/mopend.o: \
  $(OBJ)/molkst_C.o  \
  mopend.F90
	 $(Compile_opt) $(FFlags) mopend.F90
  
$(OBJ)/mopend_I.o: \
  mopend_I.F90
	 $(Compile_opt) $(FFlags) mopend_I.F90
  
$(OBJ)/MOZYME_C.o: \
  MOZYME_C.F90
	 $(Compile_opt) $(FFlags) MOZYME_C.F90
  
$(OBJ)/mpcbds_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  mpcbds_I.F90
	 $(Compile_opt) $(FFlags) mpcbds_I.F90
  
$(OBJ)/mpcpop_I.o: \
  mpcpop_I.F90
	 $(Compile_opt) $(FFlags) mpcpop_I.F90
  
$(OBJ)/mpcsyb.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/vastkind.o  \
  mpcsyb.F90
	 $(Compile_opt) $(FFlags) mpcsyb.F90
  
$(OBJ)/mtxm.o: \
  $(OBJ)/blas_I.o  \
  $(OBJ)/vastkind.o  \
  mtxm.F90
	 $(Compile_opt) $(FFlags) mtxm.F90
  
$(OBJ)/mtxm_I.o: \
  $(OBJ)/vastkind.o  \
  mtxm_I.F90
	 $(Compile_opt) $(FFlags) mtxm_I.F90
  
$(OBJ)/mtxmc.o: \
  $(OBJ)/mxm_I.o  \
  $(OBJ)/vastkind.o  \
  mtxmc.F90
	 $(Compile_opt) $(FFlags) mtxmc.F90
  
$(OBJ)/mtxmc_I.o: \
  $(OBJ)/vastkind.o  \
  mtxmc_I.F90
	 $(Compile_opt) $(FFlags) mtxmc_I.F90
  
$(OBJ)/mullik.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/densit_I.o  \
  $(OBJ)/gmetry_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mult_I.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/rsp_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/vecprt_I.o  \
  mullik.F90
	 $(Compile_opt) $(FFlags) mullik.F90
  
$(OBJ)/mullik_for_MOZYME.o: \
  $(OBJ)/molkst_C.o  \
  mullik_for_MOZYME.F90
	 $(Compile_opt) $(FFlags) mullik_for_MOZYME.F90
  
$(OBJ)/mullik_I.o: \
  mullik_I.F90
	 $(Compile_opt) $(FFlags) mullik_I.F90
  
$(OBJ)/mult.o: \
  $(OBJ)/vastkind.o  \
  mult.F90
	 $(Compile_opt) $(FFlags) mult.F90
  
$(OBJ)/mult33.o: \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  mult33.F90
	 $(Compile_opt) $(FFlags) mult33.F90
  
$(OBJ)/mult33_I.o: \
  $(OBJ)/vastkind.o  \
  mult33_I.F90
	 $(Compile_opt) $(FFlags) mult33_I.F90
  
$(OBJ)/mult_I.o: \
  $(OBJ)/vastkind.o  \
  mult_I.F90
	 $(Compile_opt) $(FFlags) mult_I.F90
  
$(OBJ)/mxm.o: \
  $(OBJ)/blas_I.o  \
  $(OBJ)/vastkind.o  \
  mxm.F90
	 $(Compile_opt) $(FFlags) mxm.F90
  
$(OBJ)/mxm_I.o: \
  $(OBJ)/vastkind.o  \
  mxm_I.F90
	 $(Compile_opt) $(FFlags) mxm_I.F90
  
$(OBJ)/mxmt.o: \
  $(OBJ)/blas_I.o  \
  $(OBJ)/vastkind.o  \
  mxmt.F90
	 $(Compile_opt) $(FFlags) mxmt.F90
  
$(OBJ)/mxmt_I.o: \
  $(OBJ)/vastkind.o  \
  mxmt_I.F90
	 $(Compile_opt) $(FFlags) mxmt_I.F90
  
$(OBJ)/myword.o: \
  myword.F90
	 $(Compile_opt) $(FFlags) myword.F90
  
$(OBJ)/myword_I.o: \
  myword_I.F90
	 $(Compile_opt) $(FFlags) myword_I.F90
  
$(OBJ)/naican_I.o: \
  naican_I.F90
	 $(Compile_opt) $(FFlags) naican_I.F90
  
$(OBJ)/naicas_I.o: \
  naicas_I.F90
	 $(Compile_opt) $(FFlags) naicas_I.F90
  
$(OBJ)/names.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/reada_I.o  \
  names.F90
	 $(Compile_opt) $(FFlags) names.F90
  
$(OBJ)/new_esp.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/esp_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/overlaps_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/reada_I.o  \
  new_esp.F90
	 $(Compile_opt) $(FFlags) new_esp.F90
  
$(OBJ)/newflg.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  newflg.F90
	 $(Compile_opt) $(FFlags) newflg.F90
  
$(OBJ)/ngamtg_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  ngamtg_I.F90
	 $(Compile_opt) $(FFlags) ngamtg_I.F90
  
$(OBJ)/ngefis_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  ngefis_I.F90
	 $(Compile_opt) $(FFlags) ngefis_I.F90
  
$(OBJ)/ngidri_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  ngidri_I.F90
	 $(Compile_opt) $(FFlags) ngidri_I.F90
  
$(OBJ)/ngoke_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  ngoke_I.F90
	 $(Compile_opt) $(FFlags) ngoke_I.F90
  
$(OBJ)/nllsn_I.o: \
  $(OBJ)/vastkind.o  \
  nllsn_I.F90
	 $(Compile_opt) $(FFlags) nllsn_I.F90
  
$(OBJ)/nllsq.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/compfg_I.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/locmin_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/parsav_I.o  \
  $(OBJ)/prttim_I.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/vastkind.o  \
  nllsq.F90
	 $(Compile_opt) $(FFlags) nllsq.F90
  
$(OBJ)/nllsq_I.o: \
  nllsq_I.F90
	 $(Compile_opt) $(FFlags) nllsq_I.F90
  
$(OBJ)/nonbet_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  nonbet_I.F90
	 $(Compile_opt) $(FFlags) nonbet_I.F90
  
$(OBJ)/nonope_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  nonope_I.F90
	 $(Compile_opt) $(FFlags) nonope_I.F90
  
$(OBJ)/nonor_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  nonor_I.F90
	 $(Compile_opt) $(FFlags) nonor_I.F90
  
$(OBJ)/nuchar.o: \
  $(OBJ)/reada_I.o  \
  $(OBJ)/vastkind.o  \
  nuchar.F90
	 $(Compile_opt) $(FFlags) nuchar.F90
  
$(OBJ)/nuchar_I.o: \
  $(OBJ)/vastkind.o  \
  nuchar_I.F90
	 $(Compile_opt) $(FFlags) nuchar_I.F90
  
$(OBJ)/nxtmer.o: \
  $(OBJ)/common_arrays_C.o  \
  nxtmer.F90
	 $(Compile_opt) $(FFlags) nxtmer.F90
  
$(OBJ)/openda_I.o: \
  openda_I.F90
	 $(Compile_opt) $(FFlags) openda_I.F90
  
$(OBJ)/orient_I.o: \
  $(OBJ)/vastkind.o  \
  orient_I.F90
	 $(Compile_opt) $(FFlags) orient_I.F90
  
$(OBJ)/osinv.o: \
  $(OBJ)/vastkind.o  \
  osinv.F90
	 $(Compile_opt) $(FFlags) osinv.F90
  
$(OBJ)/osinv_I.o: \
  $(OBJ)/vastkind.o  \
  osinv_I.F90
	 $(Compile_opt) $(FFlags) osinv_I.F90
  
$(OBJ)/outer1.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  outer1.F90
	 $(Compile_opt) $(FFlags) outer1.F90
  
$(OBJ)/outer2.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  outer2.F90
	 $(Compile_opt) $(FFlags) outer2.F90
  
$(OBJ)/overlaps_C.o: \
  $(OBJ)/vastkind.o  \
  overlaps_C.F90
	 $(Compile_opt) $(FFlags) overlaps_C.F90
  
$(OBJ)/ovlp_I.o: \
  ovlp_I.F90
	 $(Compile_opt) $(FFlags) ovlp_I.F90
  
$(OBJ)/packp_I.o: \
  $(OBJ)/vastkind.o  \
  packp_I.F90
	 $(Compile_opt) $(FFlags) packp_I.F90
  
$(OBJ)/parameters_C.o: \
  $(OBJ)/vastkind.o  \
  parameters_C.F90
	 $(Compile_opt) $(FFlags) parameters_C.F90
  
$(OBJ)/parameters_for_AM1_C.o: \
  $(OBJ)/vastkind.o  \
  parameters_for_AM1_C.F90
	 $(Compile_opt) $(FFlags) parameters_for_AM1_C.F90
  
$(OBJ)/parameters_for_mndo_C.o: \
  $(OBJ)/vastkind.o  \
  parameters_for_mndo_C.F90
	 $(Compile_opt) $(FFlags) parameters_for_mndo_C.F90
  
$(OBJ)/parameters_for_mndod_C.o: \
  $(OBJ)/vastkind.o  \
  parameters_for_mndod_C.F90
	 $(Compile_opt) $(FFlags) parameters_for_mndod_C.F90
  
$(OBJ)/parameters_for_pm3_C.o: \
  $(OBJ)/vastkind.o  \
  parameters_for_pm3_C.F90
	 $(Compile_opt) $(FFlags) parameters_for_pm3_C.F90
  
$(OBJ)/parameters_for_PM6_C.o: \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/vastkind.o  \
  parameters_for_PM6_C.F90
	 $(Compile_opt) $(FFlags) parameters_for_PM6_C.F90
  
$(OBJ)/parameters_for_RM1_C.o: \
  $(OBJ)/vastkind.o  \
  parameters_for_RM1_C.F90
	 $(Compile_opt) $(FFlags) parameters_for_RM1_C.F90
  
$(OBJ)/parsav.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/vastkind.o  \
  parsav.F90
	 $(Compile_opt) $(FFlags) parsav.F90
  
$(OBJ)/parsav_I.o: \
  $(OBJ)/vastkind.o  \
  parsav_I.F90
	 $(Compile_opt) $(FFlags) parsav_I.F90
  
$(OBJ)/partxy.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  partxy.F90
	 $(Compile_opt) $(FFlags) partxy.F90
  
$(OBJ)/partxy_I.o: \
  $(OBJ)/vastkind.o  \
  partxy_I.F90
	 $(Compile_opt) $(FFlags) partxy_I.F90
  
$(OBJ)/Password.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/reada_I.o  \
  Password.F90
	 $(Compile_opt) $(FFlags) Password.F90
  
$(OBJ)/pathk.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/dfpsav_I.o  \
  $(OBJ)/ef_I.o  \
  $(OBJ)/flepo_I.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/maps_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/wrttxt_I.o  \
  pathk.F90
	 $(Compile_opt) $(FFlags) pathk.F90
  
$(OBJ)/pathk_I.o: \
  pathk_I.F90
	 $(Compile_opt) $(FFlags) pathk_I.F90
  
$(OBJ)/paths.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/dfpsav_I.o  \
  $(OBJ)/ef_C.o  \
  $(OBJ)/ef_I.o  \
  $(OBJ)/flepo_I.o  \
  $(OBJ)/maps_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/writmo_I.o  \
  paths.F90
	 $(Compile_opt) $(FFlags) paths.F90
  
$(OBJ)/paths_I.o: \
  paths_I.F90
	 $(Compile_opt) $(FFlags) paths_I.F90
  
$(OBJ)/pdbout.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  pdbout.F90
	 $(Compile_opt) $(FFlags) pdbout.F90
  
$(OBJ)/pdgrid_I.o: \
  pdgrid_I.F90
	 $(Compile_opt) $(FFlags) pdgrid_I.F90
  
$(OBJ)/perm.o: \
  $(OBJ)/meci_C.o  \
  perm.F90
	 $(Compile_opt) $(FFlags) perm.F90
  
$(OBJ)/perm_I.o: \
  $(OBJ)/meci_C.o  \
  perm_I.F90
	 $(Compile_opt) $(FFlags) perm_I.F90
  
$(OBJ)/picopt.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/symmetry_C.o  \
  picopt.F90
	 $(Compile_opt) $(FFlags) picopt.F90
  
$(OBJ)/pinout.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  pinout.F90
	 $(Compile_opt) $(FFlags) pinout.F90
  
$(OBJ)/plato_I.o: \
  $(OBJ)/vastkind.o  \
  plato_I.F90
	 $(Compile_opt) $(FFlags) plato_I.F90
  
$(OBJ)/pmep.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/collis_I.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/drepp2_I.o  \
  $(OBJ)/drotat_I.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/genvec_I.o  \
  $(OBJ)/gmetry_I.o  \
  $(OBJ)/grids_I.o  \
  $(OBJ)/mepchg_I.o  \
  $(OBJ)/mepmap_I.o  \
  $(OBJ)/meprot_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/osinv_I.o  \
  $(OBJ)/packp_I.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/pmepco_I.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/rotate_C.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/surfa_I.o  \
  $(OBJ)/vastkind.o  \
  pmep.F90
	 $(Compile_opt) $(FFlags) pmep.F90
  
$(OBJ)/pmep1_I.o: \
  pmep1_I.F90
	 $(Compile_opt) $(FFlags) pmep1_I.F90
  
$(OBJ)/pmep_I.o: \
  pmep_I.F90
	 $(Compile_opt) $(FFlags) pmep_I.F90
  
$(OBJ)/pmepco_I.o: \
  $(OBJ)/vastkind.o  \
  pmepco_I.F90
	 $(Compile_opt) $(FFlags) pmepco_I.F90
  
$(OBJ)/poij_I.o: \
  $(OBJ)/vastkind.o  \
  poij_I.F90
	 $(Compile_opt) $(FFlags) poij_I.F90
  
$(OBJ)/pol_vol_I.o: \
  pol_vol_I.F90
	 $(Compile_opt) $(FFlags) pol_vol_I.F90
  
$(OBJ)/polar.o: \
  $(OBJ)/alphaf_I.o  \
  $(OBJ)/aval_I.o  \
  $(OBJ)/axis_I.o  \
  $(OBJ)/bdenin_I.o  \
  $(OBJ)/bdenup_I.o  \
  $(OBJ)/beopor_I.o  \
  $(OBJ)/betaf_I.o  \
  $(OBJ)/betal1_I.o  \
  $(OBJ)/betall_I.o  \
  $(OBJ)/betcom_I.o  \
  $(OBJ)/bmakuf_I.o  \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/compfg_I.o  \
  $(OBJ)/copym_I.o  \
  $(OBJ)/darea1_I.o  \
  $(OBJ)/daread_I.o  \
  $(OBJ)/dawrit_I.o  \
  $(OBJ)/dawrt1_I.o  \
  $(OBJ)/densf_I.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/epsab_I.o  \
  $(OBJ)/ffreq1_I.o  \
  $(OBJ)/ffreq2_I.o  \
  $(OBJ)/fhpatn_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/gmetry_I.o  \
  $(OBJ)/hmuf_I.o  \
  $(OBJ)/hplusf_I.o  \
  $(OBJ)/makeuf_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/ngamtg_I.o  \
  $(OBJ)/ngefis_I.o  \
  $(OBJ)/ngidri_I.o  \
  $(OBJ)/ngoke_I.o  \
  $(OBJ)/nonbet_I.o  \
  $(OBJ)/nonope_I.o  \
  $(OBJ)/nonor_I.o  \
  $(OBJ)/openda_I.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/pol_vol_I.o  \
  $(OBJ)/polar_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/tf_I.o  \
  $(OBJ)/transf_I.o  \
  $(OBJ)/trsub_I.o  \
  $(OBJ)/trudgu_I.o  \
  $(OBJ)/trugdu_I.o  \
  $(OBJ)/trugud_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/wrdkey_I.o  \
  $(OBJ)/zerom_I.o  \
  polar.F90
	 $(Compile_opt) $(FFlags) polar.F90
  
$(OBJ)/polar_C.o: \
  $(OBJ)/vastkind.o  \
  polar_C.F90
	 $(Compile_opt) $(FFlags) polar_C.F90
  
$(OBJ)/polar_I.o: \
  polar_I.F90
	 $(Compile_opt) $(FFlags) polar_I.F90
  
$(OBJ)/potcal_I.o: \
  potcal_I.F90
	 $(Compile_opt) $(FFlags) potcal_I.F90
  
$(OBJ)/powsav_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  powsav_I.F90
	 $(Compile_opt) $(FFlags) powsav_I.F90
  
$(OBJ)/powsq.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/compfg_I.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/ef_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/maps_C.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/prttim_I.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/rsp_I.o  \
  $(OBJ)/search_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/vecprt_I.o  \
  powsq.F90
	 $(Compile_opt) $(FFlags) powsq.F90
  
$(OBJ)/powsq_I.o: \
  powsq_I.F90
	 $(Compile_opt) $(FFlags) powsq_I.F90
  
$(OBJ)/printp_I.o: \
  $(OBJ)/vastkind.o  \
  printp_I.F90
	 $(Compile_opt) $(FFlags) printp_I.F90
  
$(OBJ)/prtdrc.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/chrge_I.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/ef_C.o  \
  $(OBJ)/drcout_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/quadr_I.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/xyzint_I.o  \
  prtdrc.F90
	 $(Compile_opt) $(FFlags) prtdrc.F90
  
$(OBJ)/prtdrc_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  prtdrc_I.F90
	 $(Compile_opt) $(FFlags) prtdrc_I.F90
  
$(OBJ)/prtgra.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/reada_I.o  \
  prtgra.F90
	 $(Compile_opt) $(FFlags) prtgra.F90
  
$(OBJ)/prthco_I.o: \
  prthco_I.F90
	 $(Compile_opt) $(FFlags) prthco_I.F90
  
$(OBJ)/prthes_I.o: \
  $(OBJ)/vastkind.o  \
  prthes_I.F90
	 $(Compile_opt) $(FFlags) prthes_I.F90
  
$(OBJ)/prtlmo.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  prtlmo.F90
	 $(Compile_opt) $(FFlags) prtlmo.F90
  
$(OBJ)/prtpar_I.o: \
  prtpar_I.F90
	 $(Compile_opt) $(FFlags) prtpar_I.F90
  
$(OBJ)/prtpka.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  prtpka.F90
	 $(Compile_opt) $(FFlags) prtpka.F90
  
$(OBJ)/prttim.o: \
  $(OBJ)/vastkind.o  \
  prttim.F90
	 $(Compile_opt) $(FFlags) prttim.F90
  
$(OBJ)/prttim_I.o: \
  $(OBJ)/vastkind.o  \
  prttim_I.F90
	 $(Compile_opt) $(FFlags) prttim_I.F90
  
$(OBJ)/pulay.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/mamult_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/osinv_I.o  \
  $(OBJ)/vastkind.o  \
  pulay.F90
	 $(Compile_opt) $(FFlags) pulay.F90
  
$(OBJ)/pulay_I.o: \
  $(OBJ)/vastkind.o  \
  pulay_I.F90
	 $(Compile_opt) $(FFlags) pulay_I.F90
  
$(OBJ)/quadr.o: \
  $(OBJ)/vastkind.o  \
  quadr.F90
	 $(Compile_opt) $(FFlags) quadr.F90
  
$(OBJ)/quadr_I.o: \
  $(OBJ)/vastkind.o  \
  quadr_I.F90
	 $(Compile_opt) $(FFlags) quadr_I.F90
  
$(OBJ)/react1.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/compfg_I.o  \
  $(OBJ)/dot_I.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/getgeo_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/writmo_I.o  \
  react1.F90
	 $(Compile_opt) $(FFlags) react1.F90
  
$(OBJ)/react1_I.o: \
  react1_I.F90
	 $(Compile_opt) $(FFlags) react1_I.F90
  
$(OBJ)/reada.o: \
  $(OBJ)/digit_I.o  \
  reada.F90
	 $(Compile_opt) $(FFlags) reada.F90
  
$(OBJ)/reada_I.o: \
  reada_I.F90
	 $(Compile_opt) $(FFlags) reada_I.F90
  
$(OBJ)/readmo.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/getgeg_I.o  \
  $(OBJ)/getgeo_I.o  \
  $(OBJ)/getsym_I.o  \
  $(OBJ)/gettxt_I.o  \
  $(OBJ)/gmetry_I.o  \
  $(OBJ)/maksym_I.o  \
  $(OBJ)/maps_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/nuchar_I.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/symtry_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/wrtkey_I.o  \
  $(OBJ)/wrttxt_I.o  \
  readmo.F90
	 $(Compile_opt) $(FFlags) readmo.F90
  
$(OBJ)/readmo_I.o: \
  readmo_I.F90
	 $(Compile_opt) $(FFlags) readmo_I.F90
  
$(OBJ)/redatm_I.o: \
  redatm_I.F90
	 $(Compile_opt) $(FFlags) redatm_I.F90
  
$(OBJ)/refer.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/journal_references_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  refer.F90
	 $(Compile_opt) $(FFlags) refer.F90
  
$(OBJ)/refer_I.o: \
  refer_I.F90
	 $(Compile_opt) $(FFlags) refer_I.F90
  
$(OBJ)/refkey_C.o: \
  refkey_C.F90
	 $(Compile_opt) $(FFlags) refkey_C.F90
  
$(OBJ)/reorth.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  reorth.F90
	 $(Compile_opt) $(FFlags) reorth.F90
  
$(OBJ)/repp.o: \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/vastkind.o  \
  repp.F90
	 $(Compile_opt) $(FFlags) repp.F90
  
$(OBJ)/repp_I.o: \
  $(OBJ)/vastkind.o  \
  repp_I.F90
	 $(Compile_opt) $(FFlags) repp_I.F90
  
$(OBJ)/reppd2_I.o: \
  $(OBJ)/vastkind.o  \
  reppd2_I.F90
	 $(Compile_opt) $(FFlags) reppd2_I.F90
  
$(OBJ)/reppd_I.o: \
  $(OBJ)/vastkind.o  \
  reppd_I.F90
	 $(Compile_opt) $(FFlags) reppd_I.F90
  
$(OBJ)/reseq.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  reseq.F90
	 $(Compile_opt) $(FFlags) reseq.F90
  
$(OBJ)/resolv.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/rsp_I.o  \
  $(OBJ)/vastkind.o  \
  resolv.F90
	 $(Compile_opt) $(FFlags) resolv.F90
  
$(OBJ)/resolv_I.o: \
  $(OBJ)/vastkind.o  \
  resolv_I.F90
	 $(Compile_opt) $(FFlags) resolv_I.F90
  
$(OBJ)/rijkl_I.o: \
  $(OBJ)/vastkind.o  \
  rijkl_I.F90
	 $(Compile_opt) $(FFlags) rijkl_I.F90
  
$(OBJ)/rotat_I.o: \
  $(OBJ)/vastkind.o  \
  rotat_I.F90
	 $(Compile_opt) $(FFlags) rotat_I.F90
  
$(OBJ)/rotatd_I.o: \
  $(OBJ)/vastkind.o  \
  rotatd_I.F90
	 $(Compile_opt) $(FFlags) rotatd_I.F90
  
$(OBJ)/rotate.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/vastkind.o  \
  rotate.F90
	 $(Compile_opt) $(FFlags) rotate.F90
  
$(OBJ)/rotate_C.o: \
  $(OBJ)/vastkind.o  \
  rotate_C.F90
	 $(Compile_opt) $(FFlags) rotate_C.F90
  
$(OBJ)/rotate_I.o: \
  $(OBJ)/vastkind.o  \
  rotate_I.F90
	 $(Compile_opt) $(FFlags) rotate_I.F90
  
$(OBJ)/rotlmo.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  rotlmo.F90
	 $(Compile_opt) $(FFlags) rotlmo.F90
  
$(OBJ)/rotmat_I.o: \
  $(OBJ)/vastkind.o  \
  rotmat_I.F90
	 $(Compile_opt) $(FFlags) rotmat_I.F90
  
$(OBJ)/rotmol.o: \
  $(OBJ)/symopr_I.o  \
  $(OBJ)/vastkind.o  \
  rotmol.F90
	 $(Compile_opt) $(FFlags) rotmol.F90
  
$(OBJ)/rotmol_I.o: \
  $(OBJ)/vastkind.o  \
  rotmol_I.F90
	 $(Compile_opt) $(FFlags) rotmol_I.F90
  
$(OBJ)/rotzz.o: \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  rotzz.F90
	 $(Compile_opt) $(FFlags) rotzz.F90
  
$(OBJ)/rsc_I.o: \
  $(OBJ)/vastkind.o  \
  rsc_I.F90
	 $(Compile_opt) $(FFlags) rsc_I.F90
  
$(OBJ)/rsp.o: \
  $(OBJ)/dasum_I.o  \
  $(OBJ)/daxpy_I.o  \
  $(OBJ)/ddot_I.o  \
  $(OBJ)/dnrm2_I.o  \
  $(OBJ)/blas_I.o  \
  $(OBJ)/einvit_I.o  \
  $(OBJ)/elau_I.o  \
  $(OBJ)/epslon_I.o  \
  $(OBJ)/eqlrat_I.o  \
  $(OBJ)/estpi1_I.o  \
  $(OBJ)/etrbk3_I.o  \
  $(OBJ)/etred3_I.o  \
  $(OBJ)/evvrsp_I.o  \
  $(OBJ)/freda_I.o  \
  $(OBJ)/vastkind.o  \
  rsp.F90
	 $(Compile_opt) $(FFlags) rsp.F90
  
$(OBJ)/rsp_I.o: \
  $(OBJ)/vastkind.o  \
  rsp_I.F90
	 $(Compile_opt) $(FFlags) rsp_I.F90
  
$(OBJ)/run_mopac.o: \
  $(OBJ)/calpar_I.o  \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/compfg_I.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/datin_I.o  \
  $(OBJ)/drc_I.o  \
  $(OBJ)/ef_I.o  \
  $(OBJ)/fbx_I.o  \
  $(OBJ)/flepo_I.o  \
  $(OBJ)/force_I.o  \
  $(OBJ)/fordd_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/geoutg_I.o  \
  $(OBJ)/grid_I.o  \
  $(OBJ)/maps_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/nllsq_I.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/pathk_I.o  \
  $(OBJ)/paths_I.o  \
  $(OBJ)/pmep_I.o  \
  $(OBJ)/polar_I.o  \
  $(OBJ)/powsq_I.o  \
  $(OBJ)/react1_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/writmo_I.o  \
  $(OBJ)/wrttxt_I.o  \
  run_mopac.F90
	 $(Compile_opt) $(FFlags) run_mopac.F90
  
$(OBJ)/scfcri.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/reada_I.o  \
  scfcri.F90
	 $(Compile_opt) $(FFlags) scfcri.F90
  
$(OBJ)/schmib.o: \
  $(OBJ)/vastkind.o  \
  schmib.F90
	 $(Compile_opt) $(FFlags) schmib.F90
  
$(OBJ)/schmib_I.o: \
  $(OBJ)/vastkind.o  \
  schmib_I.F90
	 $(Compile_opt) $(FFlags) schmib_I.F90
  
$(OBJ)/schmit.o: \
  $(OBJ)/vastkind.o  \
  schmit.F90
	 $(Compile_opt) $(FFlags) schmit.F90
  
$(OBJ)/schmit_I.o: \
  $(OBJ)/vastkind.o  \
  schmit_I.F90
	 $(Compile_opt) $(FFlags) schmit_I.F90
  
$(OBJ)/scprm_I.o: \
  $(OBJ)/vastkind.o  \
  scprm_I.F90
	 $(Compile_opt) $(FFlags) scprm_I.F90
  
$(OBJ)/search_I.o: \
  $(OBJ)/vastkind.o  \
  search_I.F90
	 $(Compile_opt) $(FFlags) search_I.F90
  
$(OBJ)/second.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/geout_I.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  second.F90
	 $(Compile_opt) $(FFlags) second.F90
  
$(OBJ)/second_I.o: \
  second_I.F90
	 $(Compile_opt) $(FFlags) second_I.F90
  
$(OBJ)/selmos.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  selmos.F90
	 $(Compile_opt) $(FFlags) selmos.F90
  
$(OBJ)/set.o: \
  $(OBJ)/aintgs_I.o  \
  $(OBJ)/bintgs_I.o  \
  $(OBJ)/overlaps_C.o  \
  $(OBJ)/vastkind.o  \
  set.F90
	 $(Compile_opt) $(FFlags) set.F90
  
$(OBJ)/set_I.o: \
  $(OBJ)/vastkind.o  \
  set_I.F90
	 $(Compile_opt) $(FFlags) set_I.F90
  
$(OBJ)/set_up_dentate.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  set_up_dentate.F90
	 $(Compile_opt) $(FFlags) set_up_dentate.F90
  
$(OBJ)/set_up_MOZYME_arrays.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/iter_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  set_up_MOZYME_arrays.F90
	 $(Compile_opt) $(FFlags) set_up_MOZYME_arrays.F90
  
$(OBJ)/set_up_RAPID.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  set_up_RAPID.F90
	 $(Compile_opt) $(FFlags) set_up_RAPID.F90
  
$(OBJ)/setup3_I.o: \
  setup3_I.F90
	 $(Compile_opt) $(FFlags) setup3_I.F90
  
$(OBJ)/setup_mopac_arrays.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/ef_C.o  \
  $(OBJ)/ef_C.o  \
  $(OBJ)/esp_C.o  \
  $(OBJ)/iter_C.o  \
  $(OBJ)/maps_C.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/to_screen_C.o  \
  setup_mopac_arrays.F90
	 $(Compile_opt) $(FFlags) setup_mopac_arrays.F90
  
$(OBJ)/setupg.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/overlaps_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/vastkind.o  \
  setupg.F90
	 $(Compile_opt) $(FFlags) setupg.F90
  
$(OBJ)/setupk.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  setupk.F90
	 $(Compile_opt) $(FFlags) setupk.F90
  
$(OBJ)/solrot.o: \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/rotate_I.o  \
  $(OBJ)/vastkind.o  \
  solrot.F90
	 $(Compile_opt) $(FFlags) solrot.F90
  
$(OBJ)/solrot_I.o: \
  $(OBJ)/vastkind.o  \
  solrot_I.F90
	 $(Compile_opt) $(FFlags) solrot_I.F90
  
$(OBJ)/sort.o: \
  sort.F90
	 $(Compile_opt) $(FFlags) sort.F90
  
$(OBJ)/sort_I.o: \
  sort_I.F90
	 $(Compile_opt) $(FFlags) sort_I.F90
  
$(OBJ)/sp_two_electron.o: \
  $(OBJ)/mndod_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/rsc_I.o  \
  $(OBJ)/vastkind.o  \
  sp_two_electron.F90
	 $(Compile_opt) $(FFlags) sp_two_electron.F90
  
$(OBJ)/spcore_I.o: \
  $(OBJ)/vastkind.o  \
  spcore_I.F90
	 $(Compile_opt) $(FFlags) spcore_I.F90
  
$(OBJ)/spline_I.o: \
  $(OBJ)/vastkind.o  \
  spline_I.F90
	 $(Compile_opt) $(FFlags) spline_I.F90
  
$(OBJ)/ss_I.o: \
  $(OBJ)/vastkind.o  \
  ss_I.F90
	 $(Compile_opt) $(FFlags) ss_I.F90
  
$(OBJ)/static_polarizability.o: \
  $(OBJ)/axis_I.o  \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/chrge_I.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/compfg_I.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/gmetry_I.o  \
  $(OBJ)/matout_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/pol_vol_I.o  \
  $(OBJ)/rsp_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/vecprt_I.o  \
  static_polarizability.F90
	 $(Compile_opt) $(FFlags) static_polarizability.F90
  
$(OBJ)/suma2_I.o: \
  $(OBJ)/vastkind.o  \
  suma2_I.F90
	 $(Compile_opt) $(FFlags) suma2_I.F90
  
$(OBJ)/supdot.o: \
  $(OBJ)/vastkind.o  \
  supdot.F90
	 $(Compile_opt) $(FFlags) supdot.F90
  
$(OBJ)/supdot_I.o: \
  $(OBJ)/vastkind.o  \
  supdot_I.F90
	 $(Compile_opt) $(FFlags) supdot_I.F90
  
$(OBJ)/superd.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/vastkind.o  \
  superd.F90
	 $(Compile_opt) $(FFlags) superd.F90
  
$(OBJ)/superd_I.o: \
  $(OBJ)/vastkind.o  \
  superd_I.F90
	 $(Compile_opt) $(FFlags) superd_I.F90
  
$(OBJ)/surfa_I.o: \
  $(OBJ)/vastkind.o  \
  surfa_I.F90
	 $(Compile_opt) $(FFlags) surfa_I.F90
  
$(OBJ)/surfac_I.o: \
  surfac_I.F90
	 $(Compile_opt) $(FFlags) surfac_I.F90
  
$(OBJ)/swap.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/iter_C.o  \
  $(OBJ)/vastkind.o  \
  swap.F90
	 $(Compile_opt) $(FFlags) swap.F90
  
$(OBJ)/swap_I.o: \
  $(OBJ)/vastkind.o  \
  swap_I.F90
	 $(Compile_opt) $(FFlags) swap_I.F90
  
$(OBJ)/switch.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/conref_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/parameters_for_am1_C.o  \
  $(OBJ)/parameters_for_mndo_C.o  \
  $(OBJ)/parameters_for_mndod_C.o  \
  $(OBJ)/parameters_for_pm3_C.o  \
  $(OBJ)/parameters_for_PM6_C.o  \
  $(OBJ)/Parameters_for_RM1_C.o  \
  switch.F90
	 $(Compile_opt) $(FFlags) switch.F90
  
$(OBJ)/symdec_I.o: \
  symdec_I.F90
	 $(Compile_opt) $(FFlags) symdec_I.F90
  
$(OBJ)/symh.o: \
  $(OBJ)/mat33_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  symh.F90
	 $(Compile_opt) $(FFlags) symh.F90
  
$(OBJ)/symh_I.o: \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  symh_I.F90
	 $(Compile_opt) $(FFlags) symh_I.F90
  
$(OBJ)/symmetry_C.o: \
  $(OBJ)/vastkind.o  \
  symmetry_C.F90
	 $(Compile_opt) $(FFlags) symmetry_C.F90
  
$(OBJ)/symoir.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/charmo_I.o  \
  $(OBJ)/charst_I.o  \
  $(OBJ)/charvi_I.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  symoir.F90
	 $(Compile_opt) $(FFlags) symoir.F90
  
$(OBJ)/symoir_I.o: \
  $(OBJ)/vastkind.o  \
  symoir_I.F90
	 $(Compile_opt) $(FFlags) symoir_I.F90
  
$(OBJ)/symopr.o: \
  $(OBJ)/vastkind.o  \
  symopr.F90
	 $(Compile_opt) $(FFlags) symopr.F90
  
$(OBJ)/symopr_I.o: \
  $(OBJ)/vastkind.o  \
  symopr_I.F90
	 $(Compile_opt) $(FFlags) symopr_I.F90
  
$(OBJ)/symp_I.o: \
  symp_I.F90
	 $(Compile_opt) $(FFlags) symp_I.F90
  
$(OBJ)/sympop.o: \
  $(OBJ)/symh_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  sympop.F90
	 $(Compile_opt) $(FFlags) sympop.F90
  
$(OBJ)/sympop_I.o: \
  $(OBJ)/vastkind.o  \
  sympop_I.F90
	 $(Compile_opt) $(FFlags) sympop_I.F90
  
$(OBJ)/symr.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/symp_I.o  \
  $(OBJ)/vastkind.o  \
  symr.F90
	 $(Compile_opt) $(FFlags) symr.F90
  
$(OBJ)/symr_I.o: \
  symr_I.F90
	 $(Compile_opt) $(FFlags) symr_I.F90
  
$(OBJ)/symt.o: \
  $(OBJ)/mat33_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  symt.F90
	 $(Compile_opt) $(FFlags) symt.F90
  
$(OBJ)/symt_I.o: \
  $(OBJ)/vastkind.o  \
  symt_I.F90
	 $(Compile_opt) $(FFlags) symt_I.F90
  
$(OBJ)/symtry.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/haddon_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/mopend_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  symtry.F90
	 $(Compile_opt) $(FFlags) symtry.F90
  
$(OBJ)/symtry_I.o: \
  symtry_I.F90
	 $(Compile_opt) $(FFlags) symtry_I.F90
  
$(OBJ)/symtrz.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/makopr_I.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/molsym_I.o  \
  $(OBJ)/mult33_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/vastkind.o  \
  symtrz.F90
	 $(Compile_opt) $(FFlags) symtrz.F90
  
$(OBJ)/symtrz_I.o: \
  $(OBJ)/vastkind.o  \
  symtrz_I.F90
	 $(Compile_opt) $(FFlags) symtrz_I.F90
  
$(OBJ)/tf_I.o: \
  $(OBJ)/vastkind.o  \
  tf_I.F90
	 $(Compile_opt) $(FFlags) tf_I.F90
  
$(OBJ)/thermo.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/vastkind.o  \
  thermo.F90
	 $(Compile_opt) $(FFlags) thermo.F90
  
$(OBJ)/thermo_I.o: \
  $(OBJ)/vastkind.o  \
  thermo_I.F90
	 $(Compile_opt) $(FFlags) thermo_I.F90
  
$(OBJ)/tidy.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  tidy.F90
	 $(Compile_opt) $(FFlags) tidy.F90
  
$(OBJ)/time_I.o: \
  time_I.F90
	 $(Compile_opt) $(FFlags) time_I.F90
  
$(OBJ)/timer.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/vastkind.o  \
  timer.F90
	 $(Compile_opt) $(FFlags) timer.F90
  
$(OBJ)/timer_I.o: \
  timer_I.F90
	 $(Compile_opt) $(FFlags) timer_I.F90
  
$(OBJ)/timout.o: \
  $(OBJ)/vastkind.o  \
  timout.F90
	 $(Compile_opt) $(FFlags) timout.F90
  
$(OBJ)/timout_I.o: \
  $(OBJ)/vastkind.o  \
  timout_I.F90
	 $(Compile_opt) $(FFlags) timout_I.F90
  
$(OBJ)/to_screen.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/to_screen_C.o  \
  to_screen.F90
	 $(Compile_opt) $(FFlags) to_screen.F90
  
$(OBJ)/to_screen_C.o: \
  $(OBJ)/vastkind.o  \
  to_screen_C.F90
	 $(Compile_opt) $(FFlags) to_screen_C.F90
  
$(OBJ)/to_screen_I.o: \
  to_screen_I.F90
	 $(Compile_opt) $(FFlags) to_screen_I.F90
  
$(OBJ)/transf_I.o: \
  $(OBJ)/vastkind.o  \
  transf_I.F90
	 $(Compile_opt) $(FFlags) transf_I.F90
  
$(OBJ)/trsub_I.o: \
  $(OBJ)/vastkind.o  \
  trsub_I.F90
	 $(Compile_opt) $(FFlags) trsub_I.F90
  
$(OBJ)/trudgu_I.o: \
  $(OBJ)/vastkind.o  \
  trudgu_I.F90
	 $(Compile_opt) $(FFlags) trudgu_I.F90
  
$(OBJ)/trugdu_I.o: \
  $(OBJ)/vastkind.o  \
  trugdu_I.F90
	 $(Compile_opt) $(FFlags) trugdu_I.F90
  
$(OBJ)/trugud_I.o: \
  $(OBJ)/vastkind.o  \
  trugud_I.F90
	 $(Compile_opt) $(FFlags) trugud_I.F90
  
$(OBJ)/tx_I.o: \
  $(OBJ)/vastkind.o  \
  tx_I.F90
	 $(Compile_opt) $(FFlags) tx_I.F90
  
$(OBJ)/txtype.o: \
  $(OBJ)/common_arrays_C.o  \
  txtype.F90
	 $(Compile_opt) $(FFlags) txtype.F90
  
$(OBJ)/upcase.o: \
  upcase.F90
	 $(Compile_opt) $(FFlags) upcase.F90
  
$(OBJ)/upcase_I.o: \
  upcase_I.F90
	 $(Compile_opt) $(FFlags) upcase_I.F90
  
$(OBJ)/update.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/vastkind.o  \
  update.F90
	 $(Compile_opt) $(FFlags) update.F90
  
$(OBJ)/update_I.o: \
  $(OBJ)/vastkind.o  \
  update_I.F90
	 $(Compile_opt) $(FFlags) update_I.F90
  
$(OBJ)/values.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  values.F90
	 $(Compile_opt) $(FFlags) values.F90
  
$(OBJ)/vastkind.o: \
  vastkind.F90
	 $(Compile_opt) $(FFlags) vastkind.F90
  
$(OBJ)/vecprt.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/vastkind.o  \
  vecprt.F90
	 $(Compile_opt) $(FFlags) vecprt.F90
  
$(OBJ)/vecprt_for_MOZYME.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  vecprt_for_MOZYME.F90
	 $(Compile_opt) $(FFlags) vecprt_for_MOZYME.F90
  
$(OBJ)/vecprt_I.o: \
  $(OBJ)/vastkind.o  \
  vecprt_I.F90
	 $(Compile_opt) $(FFlags) vecprt_I.F90
  
$(OBJ)/volume.o: \
  $(OBJ)/vastkind.o  \
  volume.F90
	 $(Compile_opt) $(FFlags) volume.F90
  
$(OBJ)/volume_I.o: \
  $(OBJ)/vastkind.o  \
  volume_I.F90
	 $(Compile_opt) $(FFlags) volume_I.F90
  
$(OBJ)/w2mat_I.o: \
  $(OBJ)/vastkind.o  \
  w2mat_I.F90
	 $(Compile_opt) $(FFlags) w2mat_I.F90
  
$(OBJ)/worder_I.o: \
  $(OBJ)/vastkind.o  \
  worder_I.F90
	 $(Compile_opt) $(FFlags) worder_I.F90
  
$(OBJ)/wrdkey_I.o: \
  $(OBJ)/vastkind.o  \
  wrdkey_I.F90
	 $(Compile_opt) $(FFlags) wrdkey_I.F90
  
$(OBJ)/write_trajectory.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  write_trajectory.F90
	 $(Compile_opt) $(FFlags) write_trajectory.F90
  
$(OBJ)/write_trajectory_I.o: \
  $(OBJ)/molkst_C.o  \
  write_trajectory_I.F90
	 $(Compile_opt) $(FFlags) write_trajectory_I.F90
  
$(OBJ)/writmo.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/common_arrays_C.o  \
  $(OBJ)/conref_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/dipole_I.o  \
  $(OBJ)/elemts_C.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/geoutg_I.o  \
  $(OBJ)/maps_C.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/meci_I.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/MOZYME_C.o  \
  $(OBJ)/parameters_C.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/second_I.o  \
  $(OBJ)/symmetry_C.o  \
  $(OBJ)/to_screen_I.o  \
  $(OBJ)/vastkind.o  \
  $(OBJ)/volume_I.o  \
  writmo.F90
	 $(Compile_opt) $(FFlags) writmo.F90
  
$(OBJ)/writmo_I.o: \
  writmo_I.F90
	 $(Compile_opt) $(FFlags) writmo_I.F90
  
$(OBJ)/wrtkey.o: \
  $(OBJ)/chanel_C.o  \
  $(OBJ)/conref_C.o  \
  $(OBJ)/cosmo_C.o  \
  $(OBJ)/meci_C.o  \
  $(OBJ)/molkst_C.o  \
  $(OBJ)/myword_I.o  \
  $(OBJ)/reada_I.o  \
  $(OBJ)/to_screen_I.o  \
  wrtkey.F90
	 $(Compile_opt) $(FFlags) wrtkey.F90
  
$(OBJ)/wrtkey_I.o: \
  wrtkey_I.F90
	 $(Compile_opt) $(FFlags) wrtkey_I.F90
  
$(OBJ)/wrttxt.o: \
  $(OBJ)/molkst_C.o  \
  wrttxt.F90
	 $(Compile_opt) $(FFlags) wrttxt.F90
  
$(OBJ)/wrttxt_I.o: \
  wrttxt_I.F90
	 $(Compile_opt) $(FFlags) wrttxt_I.F90
  
$(OBJ)/wstore_I.o: \
  wstore_I.F90
	 $(Compile_opt) $(FFlags) wstore_I.F90
  
$(OBJ)/wwstep_I.o: \
  $(OBJ)/vastkind.o  \
  wwstep_I.F90
	 $(Compile_opt) $(FFlags) wwstep_I.F90
  
$(OBJ)/xerbla_I.o: \
  xerbla_I.F90
	 $(Compile_opt) $(FFlags) xerbla_I.F90
  
$(OBJ)/xxx.o: \
  xxx.F90
	 $(Compile_opt) $(FFlags) xxx.F90
  
$(OBJ)/xxx_I.o: \
  xxx_I.F90
	 $(Compile_opt) $(FFlags) xxx_I.F90
  
$(OBJ)/xyzcry.o: \
  $(OBJ)/vastkind.o  \
  xyzcry.F90
	 $(Compile_opt) $(FFlags) xyzcry.F90
  
$(OBJ)/xyzcry_I.o: \
  $(OBJ)/vastkind.o  \
  xyzcry_I.F90
	 $(Compile_opt) $(FFlags) xyzcry_I.F90
  
$(OBJ)/xyzint.o: \
  $(OBJ)/bangle_I.o  \
  $(OBJ)/dihed_I.o  \
  $(OBJ)/funcon_C.o  \
  $(OBJ)/vastkind.o  \
  xyzint.F90
	 $(Compile_opt) $(FFlags) xyzint.F90
  
$(OBJ)/xyzint_I.o: \
  $(OBJ)/vastkind.o  \
  xyzint_I.F90
	 $(Compile_opt) $(FFlags) xyzint_I.F90
  
$(OBJ)/zerom_I.o: \
  $(OBJ)/vastkind.o  \
  zerom_I.F90
	 $(Compile_opt) $(FFlags) zerom_I.F90


#
# Dependencies of C source files
#

#
# Dependencies of CXX source files
#


