# Microsoft Developer Studio Generated NMAKE File, Based on Interfaces.dsp
!IF "$(CFG)" == ""
CFG=Interfaces - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Interfaces - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Interfaces - Win32 Release" && "$(CFG)" != "Interfaces - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Interfaces.mak" CFG="Interfaces - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Interfaces - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "Interfaces - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "Interfaces - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Interfaces.lib" "$(OUTDIR)\aababc_I.mod" "$(OUTDIR)\aabacd_I.mod" "$(OUTDIR)\aabbcd_I.mod" "$(OUTDIR)\addfck_I.mod" "$(OUTDIR)\addhcr_I.mod" "$(OUTDIR)\addnuc_I.mod" "$(OUTDIR)\aijm_I.mod" "$(OUTDIR)\aintgs_I.mod" "$(OUTDIR)\alphaf_I.mod" "$(OUTDIR)\asum_I.mod" "$(OUTDIR)\aval_I.mod" "$(OUTDIR)\axis_I.mod" "$(OUTDIR)\babbbc_I.mod" "$(OUTDIR)\babbcd_I.mod" "$(OUTDIR)\bangle_I.mod" "$(OUTDIR)\bdenin_I.mod" "$(OUTDIR)\bdenup_I.mod" "$(OUTDIR)\beopor_I.mod" "$(OUTDIR)\betaf_I.mod" "$(OUTDIR)\betal1_I.mod" "$(OUTDIR)\betall_I.mod" "$(OUTDIR)\betcom_I.mod" "$(OUTDIR)\bfn_I.mod" "$(OUTDIR)\bintgs_I.mod" "$(OUTDIR)\dgedi_I.mod" "$(OUTDIR)\dgemm_I.mod" "$(OUTDIR)\dger_I.mod" "$(OUTDIR)\dgetf2_I.mod" "$(OUTDIR)\dgetrs_I.mod" "$(OUTDIR)\dlaswp_I.mod" "$(OUTDIR)\dscal_I.mod" "$(OUTDIR)\dswap_I.mod" "$(OUTDIR)\dtrsm_I.mod" "$(OUTDIR)\idamax_I.mod" "$(OUTDIR)\ieeeck_I.mod" "$(OUTDIR)\bldsym_I.mod" "$(OUTDIR)\bmakuf_I.mod" "$(OUTDIR)\bonds_I.mod" "$(OUTDIR)\break_I.mod" "$(OUTDIR)\btoc_I.mod" "$(OUTDIR)\calpar_I.mod" "$(OUTDIR)\capcor_I.mod" "$(OUTDIR)\cartab_I.mod" "$(OUTDIR)\ccprod_I.mod" "$(OUTDIR)\ccrep_I.mod" "$(OUTDIR)\cdiag_I.mod" "$(OUTDIR)\charg_I.mod"\
 "$(OUTDIR)\charmo_I.mod" "$(OUTDIR)\charst_I.mod" "$(OUTDIR)\charvi_I.mod" "$(OUTDIR)\chi_I.mod" "$(OUTDIR)\chrge_I.mod" "$(OUTDIR)\ciint_I.mod" "$(OUTDIR)\ciosci_I.mod" "$(OUTDIR)\cnvg_I.mod" "$(OUTDIR)\coe_I.mod" "$(OUTDIR)\collid_I.mod" "$(OUTDIR)\collis_I.mod" "$(OUTDIR)\compfg_I.mod" "$(OUTDIR)\compfn_I.mod" "$(OUTDIR)\consts_I.mod" "$(OUTDIR)\copym_I.mod" "$(OUTDIR)\cqden_I.mod" "$(OUTDIR)\csum_I.mod" "$(OUTDIR)\dang_I.mod" "$(OUTDIR)\darea1_I.mod" "$(OUTDIR)\daread_I.mod" "$(OUTDIR)\dasum_I.mod" "$(OUTDIR)\datin_I.mod" "$(OUTDIR)\dawrit_I.mod" "$(OUTDIR)\dawrt1_I.mod" "$(OUTDIR)\daxpy_I.mod" "$(OUTDIR)\dcart_I.mod" "$(OUTDIR)\dcopy_I.mod" "$(OUTDIR)\ddot_I.mod" "$(OUTDIR)\ddpo_I.mod" "$(OUTDIR)\delmol_I.mod" "$(OUTDIR)\delri_I.mod" "$(OUTDIR)\delta_I.mod" "$(OUTDIR)\denrot_I.mod" "$(OUTDIR)\densf_I.mod" "$(OUTDIR)\densit_I.mod" "$(OUTDIR)\deri0_I.mod" "$(OUTDIR)\deri1_I.mod" "$(OUTDIR)\deri21_I.mod" "$(OUTDIR)\deri22_I.mod" "$(OUTDIR)\deri23_I.mod" "$(OUTDIR)\deri2_I.mod" "$(OUTDIR)\deritr_I.mod" "$(OUTDIR)\deriv_I.mod" "$(OUTDIR)\dernvo_I.mod" "$(OUTDIR)\ders_I.mod" "$(OUTDIR)\dex2_I.mod" "$(OUTDIR)\dfield_I.mod" "$(OUTDIR)\dfock2_I.mod" "$(OUTDIR)\dfpsav_I.mod"\
 "$(OUTDIR)\dgefa_I.mod" "$(OUTDIR)\dgemv_I.mod" "$(OUTDIR)\dgetri_I.mod" "$(OUTDIR)\dhc_I.mod" "$(OUTDIR)\dhcore_I.mod" "$(OUTDIR)\diag_I.mod" "$(OUTDIR)\diagi_I.mod" "$(OUTDIR)\diat2_I.mod" "$(OUTDIR)\diat_I.mod" "$(OUTDIR)\diegrd_I.mod" "$(OUTDIR)\dielen_I.mod" "$(OUTDIR)\digit_I.mod" "$(OUTDIR)\dihed_I.mod" "$(OUTDIR)\dijkl1_I.mod" "$(OUTDIR)\dijkl2_I.mod" "$(OUTDIR)\dijkld_I.mod" "$(OUTDIR)\dimens_I.mod" "$(OUTDIR)\dipole_I.mod" "$(OUTDIR)\dist2_I.mod" "$(OUTDIR)\dmecip_I.mod" "$(OUTDIR)\dnrm2_I.mod" "$(OUTDIR)\dofs_I.mod" "$(OUTDIR)\dopen_I.mod" "$(OUTDIR)\dot_I.mod" "$(OUTDIR)\drc_I.mod" "$(OUTDIR)\drcout_I.mod" "$(OUTDIR)\drepp2_I.mod" "$(OUTDIR)\drotat_I.mod" "$(OUTDIR)\dsum_I.mod" "$(OUTDIR)\dtran2_I.mod" "$(OUTDIR)\dtrans_I.mod" "$(OUTDIR)\dtrmm_I.mod" "$(OUTDIR)\dtrmv_I.mod" "$(OUTDIR)\dtrti2_I.mod" "$(OUTDIR)\ef_I.mod" "$(OUTDIR)\einvit_I.mod" "$(OUTDIR)\eiscor_I.mod" "$(OUTDIR)\elau_I.mod" "$(OUTDIR)\elenuc_I.mod" "$(OUTDIR)\elesn_I.mod" "$(OUTDIR)\en_I.mod" "$(OUTDIR)\enpart_I.mod" "$(OUTDIR)\epsab_I.mod" "$(OUTDIR)\epseta_I.mod" "$(OUTDIR)\epslon_I.mod" "$(OUTDIR)\eqlrat_I.mod" "$(OUTDIR)\esn_I.mod" "$(OUTDIR)\esp1_I.mod" "$(OUTDIR)\espfit_I.mod"\
 "$(OUTDIR)\estpi1_I.mod" "$(OUTDIR)\etime_I.mod" "$(OUTDIR)\etrbk3_I.mod" "$(OUTDIR)\etred3_I.mod" "$(OUTDIR)\evvrsp_I.mod" "$(OUTDIR)\exchng_I.mod" "$(OUTDIR)\fbx_I.mod" "$(OUTDIR)\fcnpp_I.mod" "$(OUTDIR)\ffreq1_I.mod" "$(OUTDIR)\ffreq2_I.mod" "$(OUTDIR)\fhpatn_I.mod" "$(OUTDIR)\flepo_I.mod" "$(OUTDIR)\fock1_I.mod" "$(OUTDIR)\fock2_I.mod" "$(OUTDIR)\fockd2_I.mod" "$(OUTDIR)\force_I.mod" "$(OUTDIR)\fordd_I.mod" "$(OUTDIR)\formxy_I.mod" "$(OUTDIR)\frame_I.mod" "$(OUTDIR)\freda_I.mod" "$(OUTDIR)\fsub_I.mod" "$(OUTDIR)\genun_I.mod" "$(OUTDIR)\genvec_I.mod" "$(OUTDIR)\geout_I.mod" "$(OUTDIR)\geoutg_I.mod" "$(OUTDIR)\getgeg_I.mod" "$(OUTDIR)\getgeo_I.mod" "$(OUTDIR)\getsym_I.mod" "$(OUTDIR)\gettxt_I.mod" "$(OUTDIR)\getval_I.mod" "$(OUTDIR)\gmetry_I.mod" "$(OUTDIR)\gover_I.mod" "$(OUTDIR)\grid_I.mod" "$(OUTDIR)\grids_I.mod" "$(OUTDIR)\gstore_I.mod" "$(OUTDIR)\h1elec_I.mod" "$(OUTDIR)\haddon_I.mod" "$(OUTDIR)\hcore_I.mod" "$(OUTDIR)\hcored_I.mod" "$(OUTDIR)\helect_I.mod" "$(OUTDIR)\hmuf_I.mod" "$(OUTDIR)\hplusf_I.mod" "$(OUTDIR)\inid_I.mod" "$(OUTDIR)\inighd_I.mod" "$(OUTDIR)\initsn_I.mod" "$(OUTDIR)\initsv_I.mod" "$(OUTDIR)\insymc_I.mod" "$(OUTDIR)\interp_I.mod" "$(OUTDIR)\intfc_I.mod"\
 "$(OUTDIR)\ird_I.mod" "$(OUTDIR)\iten_I.mod" "$(OUTDIR)\iter_I.mod" "$(OUTDIR)\jab_I.mod" "$(OUTDIR)\jcarin_I.mod" "$(OUTDIR)\kab_I.mod" "$(OUTDIR)\linmin_I.mod" "$(OUTDIR)\locmin_I.mod" "$(OUTDIR)\lsame_I.mod" "$(OUTDIR)\makeuf_I.mod" "$(OUTDIR)\makopr_I.mod" "$(OUTDIR)\maksym_I.mod" "$(OUTDIR)\mamult_I.mod" "$(OUTDIR)\mat33_I.mod" "$(OUTDIR)\matou1_I.mod" "$(OUTDIR)\matout_I.mod" "$(OUTDIR)\me08a_I.mod" "$(OUTDIR)\meci_I.mod" "$(OUTDIR)\mecid_I.mod" "$(OUTDIR)\mecih_I.mod" "$(OUTDIR)\mecip_I.mod" "$(OUTDIR)\mepchg_I.mod" "$(OUTDIR)\mepmap_I.mod" "$(OUTDIR)\meprot_I.mod" "$(OUTDIR)\minv_I.mod" "$(OUTDIR)\molsym_I.mod" "$(OUTDIR)\molval_I.mod" "$(OUTDIR)\mopend_I.mod" "$(OUTDIR)\mpcbds_I.mod" "$(OUTDIR)\mpcpop_I.mod" "$(OUTDIR)\mtxm_I.mod" "$(OUTDIR)\mtxmc_I.mod" "$(OUTDIR)\mullik_I.mod" "$(OUTDIR)\mult33_I.mod" "$(OUTDIR)\mult_I.mod" "$(OUTDIR)\mxm_I.mod" "$(OUTDIR)\mxmt_I.mod" "$(OUTDIR)\myword_I.mod" "$(OUTDIR)\naican_I.mod" "$(OUTDIR)\naicas_I.mod" "$(OUTDIR)\ngamtg_I.mod" "$(OUTDIR)\ngefis_I.mod" "$(OUTDIR)\ngidri_I.mod" "$(OUTDIR)\ngoke_I.mod" "$(OUTDIR)\nllsn_I.mod" "$(OUTDIR)\nllsq_I.mod" "$(OUTDIR)\nonbet_I.mod" "$(OUTDIR)\nonope_I.mod" "$(OUTDIR)\nonor_I.mod"\
 "$(OUTDIR)\nuchar_I.mod" "$(OUTDIR)\openda_I.mod" "$(OUTDIR)\orient_I.mod" "$(OUTDIR)\osinv_I.mod" "$(OUTDIR)\ovlp_I.mod" "$(OUTDIR)\packp_I.mod" "$(OUTDIR)\parsav_I.mod" "$(OUTDIR)\partxy_I.mod" "$(OUTDIR)\pathk_I.mod" "$(OUTDIR)\paths_I.mod" "$(OUTDIR)\pdgrid_I.mod" "$(OUTDIR)\perm_I.mod" "$(OUTDIR)\plato_I.mod" "$(OUTDIR)\pmep1_I.mod" "$(OUTDIR)\pmep_I.mod" "$(OUTDIR)\pmepco_I.mod" "$(OUTDIR)\poij_I.mod" "$(OUTDIR)\pol_vol_I.mod" "$(OUTDIR)\polar_I.mod" "$(OUTDIR)\potcal_I.mod" "$(OUTDIR)\powsav_I.mod" "$(OUTDIR)\powsq_I.mod" "$(OUTDIR)\printp_I.mod" "$(OUTDIR)\prtdrc_I.mod" "$(OUTDIR)\prthco_I.mod" "$(OUTDIR)\prthes_I.mod" "$(OUTDIR)\prtpar_I.mod" "$(OUTDIR)\prttim_I.mod" "$(OUTDIR)\pulay_I.mod" "$(OUTDIR)\quadr_I.mod" "$(OUTDIR)\react1_I.mod" "$(OUTDIR)\reada_I.mod" "$(OUTDIR)\readmo_I.mod" "$(OUTDIR)\redatm_I.mod" "$(OUTDIR)\refer_I.mod" "$(OUTDIR)\reppd2_I.mod" "$(OUTDIR)\reppd_I.mod" "$(OUTDIR)\resolv_I.mod" "$(OUTDIR)\rijkl_I.mod" "$(OUTDIR)\rotat_I.mod" "$(OUTDIR)\rotatd_I.mod" "$(OUTDIR)\rotate_I.mod" "$(OUTDIR)\rotmat_I.mod" "$(OUTDIR)\rotmol_I.mod" "$(OUTDIR)\rsc_I.mod" "$(OUTDIR)\rsp_I.mod" "$(OUTDIR)\schmib_I.mod" "$(OUTDIR)\schmit_I.mod" "$(OUTDIR)\scprm_I.mod"\
 "$(OUTDIR)\search_I.mod" "$(OUTDIR)\second_I.mod" "$(OUTDIR)\set_I.mod" "$(OUTDIR)\setup3_I.mod" "$(OUTDIR)\solrot_I.mod" "$(OUTDIR)\sort_I.mod" "$(OUTDIR)\spcore_I.mod" "$(OUTDIR)\spline_I.mod" "$(OUTDIR)\ss_I.mod" "$(OUTDIR)\suma2_I.mod" "$(OUTDIR)\supdot_I.mod" "$(OUTDIR)\superd_I.mod" "$(OUTDIR)\surfa_I.mod" "$(OUTDIR)\surfac_I.mod" "$(OUTDIR)\swap_I.mod" "$(OUTDIR)\symdec_I.mod" "$(OUTDIR)\symh_I.mod" "$(OUTDIR)\symoir_I.mod" "$(OUTDIR)\symopr_I.mod" "$(OUTDIR)\symp_I.mod" "$(OUTDIR)\sympop_I.mod" "$(OUTDIR)\symr_I.mod" "$(OUTDIR)\symt_I.mod" "$(OUTDIR)\symtry_I.mod" "$(OUTDIR)\symtrz_I.mod" "$(OUTDIR)\tf_I.mod" "$(OUTDIR)\thermo_I.mod" "$(OUTDIR)\time_I.mod" "$(OUTDIR)\timer_I.mod" "$(OUTDIR)\timout_I.mod" "$(OUTDIR)\transf_I.mod" "$(OUTDIR)\trsub_I.mod" "$(OUTDIR)\trudgu_I.mod" "$(OUTDIR)\trugdu_I.mod" "$(OUTDIR)\trugud_I.mod" "$(OUTDIR)\tx_I.mod" "$(OUTDIR)\upcase_I.mod" "$(OUTDIR)\update_I.mod" "$(OUTDIR)\vecprt_I.mod" "$(OUTDIR)\volume_I.mod" "$(OUTDIR)\w2mat_I.mod" "$(OUTDIR)\worder_I.mod" "$(OUTDIR)\wrdkey_I.mod" "$(OUTDIR)\write_trajectory_I.mod" "$(OUTDIR)\writmo_I.mod" "$(OUTDIR)\wrtkey_I.mod" "$(OUTDIR)\wrttxt_I.mod" "$(OUTDIR)\wstore_I.mod"\
 "$(OUTDIR)\wwstep_I.mod" "$(OUTDIR)\xerbla_I.mod" "$(OUTDIR)\xxx_I.mod" "$(OUTDIR)\xyzcry_I.mod" "$(OUTDIR)\xyzint_I.mod" "$(OUTDIR)\zerom_I.mod"

!ELSE 

ALL : "Modules - Win32 Release" "$(OUTDIR)\Interfaces.lib" "$(OUTDIR)\aababc_I.mod" "$(OUTDIR)\aabacd_I.mod" "$(OUTDIR)\aabbcd_I.mod" "$(OUTDIR)\addfck_I.mod" "$(OUTDIR)\addhcr_I.mod" "$(OUTDIR)\addnuc_I.mod" "$(OUTDIR)\aijm_I.mod" "$(OUTDIR)\aintgs_I.mod" "$(OUTDIR)\alphaf_I.mod" "$(OUTDIR)\asum_I.mod" "$(OUTDIR)\aval_I.mod" "$(OUTDIR)\axis_I.mod" "$(OUTDIR)\babbbc_I.mod" "$(OUTDIR)\babbcd_I.mod" "$(OUTDIR)\bangle_I.mod" "$(OUTDIR)\bdenin_I.mod" "$(OUTDIR)\bdenup_I.mod" "$(OUTDIR)\beopor_I.mod" "$(OUTDIR)\betaf_I.mod" "$(OUTDIR)\betal1_I.mod" "$(OUTDIR)\betall_I.mod" "$(OUTDIR)\betcom_I.mod" "$(OUTDIR)\bfn_I.mod" "$(OUTDIR)\bintgs_I.mod" "$(OUTDIR)\dgedi_I.mod" "$(OUTDIR)\dgemm_I.mod" "$(OUTDIR)\dger_I.mod" "$(OUTDIR)\dgetf2_I.mod" "$(OUTDIR)\dgetrs_I.mod" "$(OUTDIR)\dlaswp_I.mod" "$(OUTDIR)\dscal_I.mod" "$(OUTDIR)\dswap_I.mod" "$(OUTDIR)\dtrsm_I.mod" "$(OUTDIR)\idamax_I.mod" "$(OUTDIR)\ieeeck_I.mod" "$(OUTDIR)\bldsym_I.mod" "$(OUTDIR)\bmakuf_I.mod" "$(OUTDIR)\bonds_I.mod" "$(OUTDIR)\break_I.mod" "$(OUTDIR)\btoc_I.mod" "$(OUTDIR)\calpar_I.mod" "$(OUTDIR)\capcor_I.mod" "$(OUTDIR)\cartab_I.mod" "$(OUTDIR)\ccprod_I.mod" "$(OUTDIR)\ccrep_I.mod" "$(OUTDIR)\cdiag_I.mod"\
 "$(OUTDIR)\charg_I.mod" "$(OUTDIR)\charmo_I.mod" "$(OUTDIR)\charst_I.mod" "$(OUTDIR)\charvi_I.mod" "$(OUTDIR)\chi_I.mod" "$(OUTDIR)\chrge_I.mod" "$(OUTDIR)\ciint_I.mod" "$(OUTDIR)\ciosci_I.mod" "$(OUTDIR)\cnvg_I.mod" "$(OUTDIR)\coe_I.mod" "$(OUTDIR)\collid_I.mod" "$(OUTDIR)\collis_I.mod" "$(OUTDIR)\compfg_I.mod" "$(OUTDIR)\compfn_I.mod" "$(OUTDIR)\consts_I.mod" "$(OUTDIR)\copym_I.mod" "$(OUTDIR)\cqden_I.mod" "$(OUTDIR)\csum_I.mod" "$(OUTDIR)\dang_I.mod" "$(OUTDIR)\darea1_I.mod" "$(OUTDIR)\daread_I.mod" "$(OUTDIR)\dasum_I.mod" "$(OUTDIR)\datin_I.mod" "$(OUTDIR)\dawrit_I.mod" "$(OUTDIR)\dawrt1_I.mod" "$(OUTDIR)\daxpy_I.mod" "$(OUTDIR)\dcart_I.mod" "$(OUTDIR)\dcopy_I.mod" "$(OUTDIR)\ddot_I.mod" "$(OUTDIR)\ddpo_I.mod" "$(OUTDIR)\delmol_I.mod" "$(OUTDIR)\delri_I.mod" "$(OUTDIR)\delta_I.mod" "$(OUTDIR)\denrot_I.mod" "$(OUTDIR)\densf_I.mod" "$(OUTDIR)\densit_I.mod" "$(OUTDIR)\deri0_I.mod" "$(OUTDIR)\deri1_I.mod" "$(OUTDIR)\deri21_I.mod" "$(OUTDIR)\deri22_I.mod" "$(OUTDIR)\deri23_I.mod" "$(OUTDIR)\deri2_I.mod" "$(OUTDIR)\deritr_I.mod" "$(OUTDIR)\deriv_I.mod" "$(OUTDIR)\dernvo_I.mod" "$(OUTDIR)\ders_I.mod" "$(OUTDIR)\dex2_I.mod" "$(OUTDIR)\dfield_I.mod" "$(OUTDIR)\dfock2_I.mod"\
 "$(OUTDIR)\dfpsav_I.mod" "$(OUTDIR)\dgefa_I.mod" "$(OUTDIR)\dgemv_I.mod" "$(OUTDIR)\dgetri_I.mod" "$(OUTDIR)\dhc_I.mod" "$(OUTDIR)\dhcore_I.mod" "$(OUTDIR)\diag_I.mod" "$(OUTDIR)\diagi_I.mod" "$(OUTDIR)\diat2_I.mod" "$(OUTDIR)\diat_I.mod" "$(OUTDIR)\diegrd_I.mod" "$(OUTDIR)\dielen_I.mod" "$(OUTDIR)\digit_I.mod" "$(OUTDIR)\dihed_I.mod" "$(OUTDIR)\dijkl1_I.mod" "$(OUTDIR)\dijkl2_I.mod" "$(OUTDIR)\dijkld_I.mod" "$(OUTDIR)\dimens_I.mod" "$(OUTDIR)\dipole_I.mod" "$(OUTDIR)\dist2_I.mod" "$(OUTDIR)\dmecip_I.mod" "$(OUTDIR)\dnrm2_I.mod" "$(OUTDIR)\dofs_I.mod" "$(OUTDIR)\dopen_I.mod" "$(OUTDIR)\dot_I.mod" "$(OUTDIR)\drc_I.mod" "$(OUTDIR)\drcout_I.mod" "$(OUTDIR)\drepp2_I.mod" "$(OUTDIR)\drotat_I.mod" "$(OUTDIR)\dsum_I.mod" "$(OUTDIR)\dtran2_I.mod" "$(OUTDIR)\dtrans_I.mod" "$(OUTDIR)\dtrmm_I.mod" "$(OUTDIR)\dtrmv_I.mod" "$(OUTDIR)\dtrti2_I.mod" "$(OUTDIR)\ef_I.mod" "$(OUTDIR)\einvit_I.mod" "$(OUTDIR)\eiscor_I.mod" "$(OUTDIR)\elau_I.mod" "$(OUTDIR)\elenuc_I.mod" "$(OUTDIR)\elesn_I.mod" "$(OUTDIR)\en_I.mod" "$(OUTDIR)\enpart_I.mod" "$(OUTDIR)\epsab_I.mod" "$(OUTDIR)\epseta_I.mod" "$(OUTDIR)\epslon_I.mod" "$(OUTDIR)\eqlrat_I.mod" "$(OUTDIR)\esn_I.mod" "$(OUTDIR)\esp1_I.mod"\
 "$(OUTDIR)\espfit_I.mod" "$(OUTDIR)\estpi1_I.mod" "$(OUTDIR)\etime_I.mod" "$(OUTDIR)\etrbk3_I.mod" "$(OUTDIR)\etred3_I.mod" "$(OUTDIR)\evvrsp_I.mod" "$(OUTDIR)\exchng_I.mod" "$(OUTDIR)\fbx_I.mod" "$(OUTDIR)\fcnpp_I.mod" "$(OUTDIR)\ffreq1_I.mod" "$(OUTDIR)\ffreq2_I.mod" "$(OUTDIR)\fhpatn_I.mod" "$(OUTDIR)\flepo_I.mod" "$(OUTDIR)\fock1_I.mod" "$(OUTDIR)\fock2_I.mod" "$(OUTDIR)\fockd2_I.mod" "$(OUTDIR)\force_I.mod" "$(OUTDIR)\fordd_I.mod" "$(OUTDIR)\formxy_I.mod" "$(OUTDIR)\frame_I.mod" "$(OUTDIR)\freda_I.mod" "$(OUTDIR)\fsub_I.mod" "$(OUTDIR)\genun_I.mod" "$(OUTDIR)\genvec_I.mod" "$(OUTDIR)\geout_I.mod" "$(OUTDIR)\geoutg_I.mod" "$(OUTDIR)\getgeg_I.mod" "$(OUTDIR)\getgeo_I.mod" "$(OUTDIR)\getsym_I.mod" "$(OUTDIR)\gettxt_I.mod" "$(OUTDIR)\getval_I.mod" "$(OUTDIR)\gmetry_I.mod" "$(OUTDIR)\gover_I.mod" "$(OUTDIR)\grid_I.mod" "$(OUTDIR)\grids_I.mod" "$(OUTDIR)\gstore_I.mod" "$(OUTDIR)\h1elec_I.mod" "$(OUTDIR)\haddon_I.mod" "$(OUTDIR)\hcore_I.mod" "$(OUTDIR)\hcored_I.mod" "$(OUTDIR)\helect_I.mod" "$(OUTDIR)\hmuf_I.mod" "$(OUTDIR)\hplusf_I.mod" "$(OUTDIR)\inid_I.mod" "$(OUTDIR)\inighd_I.mod" "$(OUTDIR)\initsn_I.mod" "$(OUTDIR)\initsv_I.mod" "$(OUTDIR)\insymc_I.mod" "$(OUTDIR)\interp_I.mod"\
 "$(OUTDIR)\intfc_I.mod" "$(OUTDIR)\ird_I.mod" "$(OUTDIR)\iten_I.mod" "$(OUTDIR)\iter_I.mod" "$(OUTDIR)\jab_I.mod" "$(OUTDIR)\jcarin_I.mod" "$(OUTDIR)\kab_I.mod" "$(OUTDIR)\linmin_I.mod" "$(OUTDIR)\locmin_I.mod" "$(OUTDIR)\lsame_I.mod" "$(OUTDIR)\makeuf_I.mod" "$(OUTDIR)\makopr_I.mod" "$(OUTDIR)\maksym_I.mod" "$(OUTDIR)\mamult_I.mod" "$(OUTDIR)\mat33_I.mod" "$(OUTDIR)\matou1_I.mod" "$(OUTDIR)\matout_I.mod" "$(OUTDIR)\me08a_I.mod" "$(OUTDIR)\meci_I.mod" "$(OUTDIR)\mecid_I.mod" "$(OUTDIR)\mecih_I.mod" "$(OUTDIR)\mecip_I.mod" "$(OUTDIR)\mepchg_I.mod" "$(OUTDIR)\mepmap_I.mod" "$(OUTDIR)\meprot_I.mod" "$(OUTDIR)\minv_I.mod" "$(OUTDIR)\molsym_I.mod" "$(OUTDIR)\molval_I.mod" "$(OUTDIR)\mopend_I.mod" "$(OUTDIR)\mpcbds_I.mod" "$(OUTDIR)\mpcpop_I.mod" "$(OUTDIR)\mtxm_I.mod" "$(OUTDIR)\mtxmc_I.mod" "$(OUTDIR)\mullik_I.mod" "$(OUTDIR)\mult33_I.mod" "$(OUTDIR)\mult_I.mod" "$(OUTDIR)\mxm_I.mod" "$(OUTDIR)\mxmt_I.mod" "$(OUTDIR)\myword_I.mod" "$(OUTDIR)\naican_I.mod" "$(OUTDIR)\naicas_I.mod" "$(OUTDIR)\ngamtg_I.mod" "$(OUTDIR)\ngefis_I.mod" "$(OUTDIR)\ngidri_I.mod" "$(OUTDIR)\ngoke_I.mod" "$(OUTDIR)\nllsn_I.mod" "$(OUTDIR)\nllsq_I.mod" "$(OUTDIR)\nonbet_I.mod" "$(OUTDIR)\nonope_I.mod"\
 "$(OUTDIR)\nonor_I.mod" "$(OUTDIR)\nuchar_I.mod" "$(OUTDIR)\openda_I.mod" "$(OUTDIR)\orient_I.mod" "$(OUTDIR)\osinv_I.mod" "$(OUTDIR)\ovlp_I.mod" "$(OUTDIR)\packp_I.mod" "$(OUTDIR)\parsav_I.mod" "$(OUTDIR)\partxy_I.mod" "$(OUTDIR)\pathk_I.mod" "$(OUTDIR)\paths_I.mod" "$(OUTDIR)\pdgrid_I.mod" "$(OUTDIR)\perm_I.mod" "$(OUTDIR)\plato_I.mod" "$(OUTDIR)\pmep1_I.mod" "$(OUTDIR)\pmep_I.mod" "$(OUTDIR)\pmepco_I.mod" "$(OUTDIR)\poij_I.mod" "$(OUTDIR)\pol_vol_I.mod" "$(OUTDIR)\polar_I.mod" "$(OUTDIR)\potcal_I.mod" "$(OUTDIR)\powsav_I.mod" "$(OUTDIR)\powsq_I.mod" "$(OUTDIR)\printp_I.mod" "$(OUTDIR)\prtdrc_I.mod" "$(OUTDIR)\prthco_I.mod" "$(OUTDIR)\prthes_I.mod" "$(OUTDIR)\prtpar_I.mod" "$(OUTDIR)\prttim_I.mod" "$(OUTDIR)\pulay_I.mod" "$(OUTDIR)\quadr_I.mod" "$(OUTDIR)\react1_I.mod" "$(OUTDIR)\reada_I.mod" "$(OUTDIR)\readmo_I.mod" "$(OUTDIR)\redatm_I.mod" "$(OUTDIR)\refer_I.mod" "$(OUTDIR)\reppd2_I.mod" "$(OUTDIR)\reppd_I.mod" "$(OUTDIR)\resolv_I.mod" "$(OUTDIR)\rijkl_I.mod" "$(OUTDIR)\rotat_I.mod" "$(OUTDIR)\rotatd_I.mod" "$(OUTDIR)\rotate_I.mod" "$(OUTDIR)\rotmat_I.mod" "$(OUTDIR)\rotmol_I.mod" "$(OUTDIR)\rsc_I.mod" "$(OUTDIR)\rsp_I.mod" "$(OUTDIR)\schmib_I.mod" "$(OUTDIR)\schmit_I.mod"\
 "$(OUTDIR)\scprm_I.mod" "$(OUTDIR)\search_I.mod" "$(OUTDIR)\second_I.mod" "$(OUTDIR)\set_I.mod" "$(OUTDIR)\setup3_I.mod" "$(OUTDIR)\solrot_I.mod" "$(OUTDIR)\sort_I.mod" "$(OUTDIR)\spcore_I.mod" "$(OUTDIR)\spline_I.mod" "$(OUTDIR)\ss_I.mod" "$(OUTDIR)\suma2_I.mod" "$(OUTDIR)\supdot_I.mod" "$(OUTDIR)\superd_I.mod" "$(OUTDIR)\surfa_I.mod" "$(OUTDIR)\surfac_I.mod" "$(OUTDIR)\swap_I.mod" "$(OUTDIR)\symdec_I.mod" "$(OUTDIR)\symh_I.mod" "$(OUTDIR)\symoir_I.mod" "$(OUTDIR)\symopr_I.mod" "$(OUTDIR)\symp_I.mod" "$(OUTDIR)\sympop_I.mod" "$(OUTDIR)\symr_I.mod" "$(OUTDIR)\symt_I.mod" "$(OUTDIR)\symtry_I.mod" "$(OUTDIR)\symtrz_I.mod" "$(OUTDIR)\tf_I.mod" "$(OUTDIR)\thermo_I.mod" "$(OUTDIR)\time_I.mod" "$(OUTDIR)\timer_I.mod" "$(OUTDIR)\timout_I.mod" "$(OUTDIR)\transf_I.mod" "$(OUTDIR)\trsub_I.mod" "$(OUTDIR)\trudgu_I.mod" "$(OUTDIR)\trugdu_I.mod" "$(OUTDIR)\trugud_I.mod" "$(OUTDIR)\tx_I.mod" "$(OUTDIR)\upcase_I.mod" "$(OUTDIR)\update_I.mod" "$(OUTDIR)\vecprt_I.mod" "$(OUTDIR)\volume_I.mod" "$(OUTDIR)\w2mat_I.mod" "$(OUTDIR)\worder_I.mod" "$(OUTDIR)\wrdkey_I.mod" "$(OUTDIR)\write_trajectory_I.mod" "$(OUTDIR)\writmo_I.mod" "$(OUTDIR)\wrtkey_I.mod" "$(OUTDIR)\wrttxt_I.mod" "$(OUTDIR)\wstore_I.mod"\
 "$(OUTDIR)\wwstep_I.mod" "$(OUTDIR)\xerbla_I.mod" "$(OUTDIR)\xxx_I.mod" "$(OUTDIR)\xyzcry_I.mod" "$(OUTDIR)\xyzint_I.mod" "$(OUTDIR)\zerom_I.mod"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Modules - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\aababc_I.mod"
	-@erase "$(INTDIR)\aababc_I.obj"
	-@erase "$(INTDIR)\aabacd_I.mod"
	-@erase "$(INTDIR)\aabacd_I.obj"
	-@erase "$(INTDIR)\aabbcd_I.mod"
	-@erase "$(INTDIR)\aabbcd_I.obj"
	-@erase "$(INTDIR)\addfck_I.mod"
	-@erase "$(INTDIR)\addfck_I.obj"
	-@erase "$(INTDIR)\addhcr_I.mod"
	-@erase "$(INTDIR)\addhcr_I.obj"
	-@erase "$(INTDIR)\addnuc_I.mod"
	-@erase "$(INTDIR)\addnuc_I.obj"
	-@erase "$(INTDIR)\aijm_I.mod"
	-@erase "$(INTDIR)\aijm_I.obj"
	-@erase "$(INTDIR)\aintgs_I.mod"
	-@erase "$(INTDIR)\aintgs_I.obj"
	-@erase "$(INTDIR)\alphaf_I.mod"
	-@erase "$(INTDIR)\alphaf_I.obj"
	-@erase "$(INTDIR)\asum_I.mod"
	-@erase "$(INTDIR)\asum_I.obj"
	-@erase "$(INTDIR)\aval_I.mod"
	-@erase "$(INTDIR)\aval_I.obj"
	-@erase "$(INTDIR)\axis_I.mod"
	-@erase "$(INTDIR)\axis_I.obj"
	-@erase "$(INTDIR)\babbbc_I.mod"
	-@erase "$(INTDIR)\babbbc_I.obj"
	-@erase "$(INTDIR)\babbcd_I.mod"
	-@erase "$(INTDIR)\babbcd_I.obj"
	-@erase "$(INTDIR)\bangle_I.mod"
	-@erase "$(INTDIR)\bangle_I.obj"
	-@erase "$(INTDIR)\bdenin_I.mod"
	-@erase "$(INTDIR)\bdenin_I.obj"
	-@erase "$(INTDIR)\bdenup_I.mod"
	-@erase "$(INTDIR)\bdenup_I.obj"
	-@erase "$(INTDIR)\beopor_I.mod"
	-@erase "$(INTDIR)\beopor_I.obj"
	-@erase "$(INTDIR)\betaf_I.mod"
	-@erase "$(INTDIR)\betaf_I.obj"
	-@erase "$(INTDIR)\betal1_I.mod"
	-@erase "$(INTDIR)\betal1_I.obj"
	-@erase "$(INTDIR)\betall_I.mod"
	-@erase "$(INTDIR)\betall_I.obj"
	-@erase "$(INTDIR)\betcom_I.mod"
	-@erase "$(INTDIR)\betcom_I.obj"
	-@erase "$(INTDIR)\bfn_I.mod"
	-@erase "$(INTDIR)\bfn_I.obj"
	-@erase "$(INTDIR)\bintgs_I.mod"
	-@erase "$(INTDIR)\bintgs_I.obj"
	-@erase "$(INTDIR)\blas_I.obj"
	-@erase "$(INTDIR)\bldsym_I.mod"
	-@erase "$(INTDIR)\bldsym_I.obj"
	-@erase "$(INTDIR)\bmakuf_I.mod"
	-@erase "$(INTDIR)\bmakuf_I.obj"
	-@erase "$(INTDIR)\bonds_I.mod"
	-@erase "$(INTDIR)\bonds_I.obj"
	-@erase "$(INTDIR)\break_I.mod"
	-@erase "$(INTDIR)\break_I.obj"
	-@erase "$(INTDIR)\btoc_I.mod"
	-@erase "$(INTDIR)\btoc_I.obj"
	-@erase "$(INTDIR)\calpar_I.mod"
	-@erase "$(INTDIR)\calpar_I.obj"
	-@erase "$(INTDIR)\capcor_I.mod"
	-@erase "$(INTDIR)\capcor_I.obj"
	-@erase "$(INTDIR)\cartab_I.mod"
	-@erase "$(INTDIR)\cartab_I.obj"
	-@erase "$(INTDIR)\ccprod_I.mod"
	-@erase "$(INTDIR)\ccprod_I.obj"
	-@erase "$(INTDIR)\ccrep_I.mod"
	-@erase "$(INTDIR)\ccrep_I.obj"
	-@erase "$(INTDIR)\cdiag_I.mod"
	-@erase "$(INTDIR)\cdiag_I.obj"
	-@erase "$(INTDIR)\charg_I.mod"
	-@erase "$(INTDIR)\charg_I.obj"
	-@erase "$(INTDIR)\charmo_I.mod"
	-@erase "$(INTDIR)\charmo_I.obj"
	-@erase "$(INTDIR)\charst_I.mod"
	-@erase "$(INTDIR)\charst_I.obj"
	-@erase "$(INTDIR)\charvi_I.mod"
	-@erase "$(INTDIR)\charvi_I.obj"
	-@erase "$(INTDIR)\chi_I.mod"
	-@erase "$(INTDIR)\chi_I.obj"
	-@erase "$(INTDIR)\chrge_I.mod"
	-@erase "$(INTDIR)\chrge_I.obj"
	-@erase "$(INTDIR)\ciint_I.mod"
	-@erase "$(INTDIR)\ciint_I.obj"
	-@erase "$(INTDIR)\ciosci_I.mod"
	-@erase "$(INTDIR)\ciosci_I.obj"
	-@erase "$(INTDIR)\cnvg_I.mod"
	-@erase "$(INTDIR)\cnvg_I.obj"
	-@erase "$(INTDIR)\coe_I.mod"
	-@erase "$(INTDIR)\coe_I.obj"
	-@erase "$(INTDIR)\collid_I.mod"
	-@erase "$(INTDIR)\collid_I.obj"
	-@erase "$(INTDIR)\collis_I.mod"
	-@erase "$(INTDIR)\collis_I.obj"
	-@erase "$(INTDIR)\compfg_I.mod"
	-@erase "$(INTDIR)\compfg_I.obj"
	-@erase "$(INTDIR)\compfn_I.mod"
	-@erase "$(INTDIR)\compfn_I.obj"
	-@erase "$(INTDIR)\consts_I.mod"
	-@erase "$(INTDIR)\consts_I.obj"
	-@erase "$(INTDIR)\copym_I.mod"
	-@erase "$(INTDIR)\copym_I.obj"
	-@erase "$(INTDIR)\cqden_I.mod"
	-@erase "$(INTDIR)\cqden_I.obj"
	-@erase "$(INTDIR)\csum_I.mod"
	-@erase "$(INTDIR)\csum_I.obj"
	-@erase "$(INTDIR)\dang_I.mod"
	-@erase "$(INTDIR)\dang_I.obj"
	-@erase "$(INTDIR)\darea1_I.mod"
	-@erase "$(INTDIR)\darea1_I.obj"
	-@erase "$(INTDIR)\daread_I.mod"
	-@erase "$(INTDIR)\daread_I.obj"
	-@erase "$(INTDIR)\dasum_I.mod"
	-@erase "$(INTDIR)\dasum_I.obj"
	-@erase "$(INTDIR)\datin_I.mod"
	-@erase "$(INTDIR)\datin_I.obj"
	-@erase "$(INTDIR)\dawrit_I.mod"
	-@erase "$(INTDIR)\dawrit_I.obj"
	-@erase "$(INTDIR)\dawrt1_I.mod"
	-@erase "$(INTDIR)\dawrt1_I.obj"
	-@erase "$(INTDIR)\daxpy_I.mod"
	-@erase "$(INTDIR)\daxpy_I.obj"
	-@erase "$(INTDIR)\dcart_I.mod"
	-@erase "$(INTDIR)\dcart_I.obj"
	-@erase "$(INTDIR)\dcopy_I.mod"
	-@erase "$(INTDIR)\dcopy_I.obj"
	-@erase "$(INTDIR)\ddot_I.mod"
	-@erase "$(INTDIR)\ddot_I.obj"
	-@erase "$(INTDIR)\ddpo_I.mod"
	-@erase "$(INTDIR)\ddpo_I.obj"
	-@erase "$(INTDIR)\delmol_I.mod"
	-@erase "$(INTDIR)\delmol_I.obj"
	-@erase "$(INTDIR)\delri_I.mod"
	-@erase "$(INTDIR)\delri_I.obj"
	-@erase "$(INTDIR)\delta_I.mod"
	-@erase "$(INTDIR)\delta_I.obj"
	-@erase "$(INTDIR)\denrot_I.mod"
	-@erase "$(INTDIR)\denrot_I.obj"
	-@erase "$(INTDIR)\densf_I.mod"
	-@erase "$(INTDIR)\densf_I.obj"
	-@erase "$(INTDIR)\densit_I.mod"
	-@erase "$(INTDIR)\densit_I.obj"
	-@erase "$(INTDIR)\deri0_I.mod"
	-@erase "$(INTDIR)\deri0_I.obj"
	-@erase "$(INTDIR)\deri1_I.mod"
	-@erase "$(INTDIR)\deri1_I.obj"
	-@erase "$(INTDIR)\deri21_I.mod"
	-@erase "$(INTDIR)\deri21_I.obj"
	-@erase "$(INTDIR)\deri22_I.mod"
	-@erase "$(INTDIR)\deri22_I.obj"
	-@erase "$(INTDIR)\deri23_I.mod"
	-@erase "$(INTDIR)\deri23_I.obj"
	-@erase "$(INTDIR)\deri2_I.mod"
	-@erase "$(INTDIR)\deri2_I.obj"
	-@erase "$(INTDIR)\deritr_I.mod"
	-@erase "$(INTDIR)\deritr_I.obj"
	-@erase "$(INTDIR)\deriv_I.mod"
	-@erase "$(INTDIR)\deriv_I.obj"
	-@erase "$(INTDIR)\dernvo_I.mod"
	-@erase "$(INTDIR)\dernvo_I.obj"
	-@erase "$(INTDIR)\ders_I.mod"
	-@erase "$(INTDIR)\ders_I.obj"
	-@erase "$(INTDIR)\dex2_I.mod"
	-@erase "$(INTDIR)\dex2_I.obj"
	-@erase "$(INTDIR)\dfield_I.mod"
	-@erase "$(INTDIR)\dfield_I.obj"
	-@erase "$(INTDIR)\dfock2_I.mod"
	-@erase "$(INTDIR)\dfock2_I.obj"
	-@erase "$(INTDIR)\dfpsav_I.mod"
	-@erase "$(INTDIR)\dfpsav_I.obj"
	-@erase "$(INTDIR)\dgedi_I.mod"
	-@erase "$(INTDIR)\dgefa_I.mod"
	-@erase "$(INTDIR)\dgefa_I.obj"
	-@erase "$(INTDIR)\dgemm_I.mod"
	-@erase "$(INTDIR)\dgemv_I.mod"
	-@erase "$(INTDIR)\dgemv_I.obj"
	-@erase "$(INTDIR)\dger_I.mod"
	-@erase "$(INTDIR)\dgetf2_I.mod"
	-@erase "$(INTDIR)\dgetri_I.mod"
	-@erase "$(INTDIR)\dgetri_I.obj"
	-@erase "$(INTDIR)\dgetrs_I.mod"
	-@erase "$(INTDIR)\dhc_I.mod"
	-@erase "$(INTDIR)\dhc_I.obj"
	-@erase "$(INTDIR)\dhcore_I.mod"
	-@erase "$(INTDIR)\dhcore_I.obj"
	-@erase "$(INTDIR)\diag_I.mod"
	-@erase "$(INTDIR)\diag_I.obj"
	-@erase "$(INTDIR)\diagi_I.mod"
	-@erase "$(INTDIR)\diagi_I.obj"
	-@erase "$(INTDIR)\diat2_I.mod"
	-@erase "$(INTDIR)\diat2_I.obj"
	-@erase "$(INTDIR)\diat_I.mod"
	-@erase "$(INTDIR)\diat_I.obj"
	-@erase "$(INTDIR)\diegrd_I.mod"
	-@erase "$(INTDIR)\diegrd_I.obj"
	-@erase "$(INTDIR)\dielen_I.mod"
	-@erase "$(INTDIR)\dielen_I.obj"
	-@erase "$(INTDIR)\digit_I.mod"
	-@erase "$(INTDIR)\digit_I.obj"
	-@erase "$(INTDIR)\dihed_I.mod"
	-@erase "$(INTDIR)\dihed_I.obj"
	-@erase "$(INTDIR)\dijkl1_I.mod"
	-@erase "$(INTDIR)\dijkl1_I.obj"
	-@erase "$(INTDIR)\dijkl2_I.mod"
	-@erase "$(INTDIR)\dijkl2_I.obj"
	-@erase "$(INTDIR)\dijkld_I.mod"
	-@erase "$(INTDIR)\dijkld_I.obj"
	-@erase "$(INTDIR)\dimens_I.mod"
	-@erase "$(INTDIR)\dimens_I.obj"
	-@erase "$(INTDIR)\dipole_I.mod"
	-@erase "$(INTDIR)\dipole_I.obj"
	-@erase "$(INTDIR)\dist2_I.mod"
	-@erase "$(INTDIR)\dist2_I.obj"
	-@erase "$(INTDIR)\dlaswp_I.mod"
	-@erase "$(INTDIR)\dmecip_I.mod"
	-@erase "$(INTDIR)\dmecip_I.obj"
	-@erase "$(INTDIR)\dnrm2_I.mod"
	-@erase "$(INTDIR)\dnrm2_I.obj"
	-@erase "$(INTDIR)\dofs_I.mod"
	-@erase "$(INTDIR)\dofs_I.obj"
	-@erase "$(INTDIR)\dopen_I.mod"
	-@erase "$(INTDIR)\dopen_I.obj"
	-@erase "$(INTDIR)\dot_I.mod"
	-@erase "$(INTDIR)\dot_I.obj"
	-@erase "$(INTDIR)\drc_I.mod"
	-@erase "$(INTDIR)\drc_I.obj"
	-@erase "$(INTDIR)\drcout_I.mod"
	-@erase "$(INTDIR)\drcout_I.obj"
	-@erase "$(INTDIR)\drepp2_I.mod"
	-@erase "$(INTDIR)\drepp2_I.obj"
	-@erase "$(INTDIR)\drotat_I.mod"
	-@erase "$(INTDIR)\drotat_I.obj"
	-@erase "$(INTDIR)\dscal_I.mod"
	-@erase "$(INTDIR)\dsum_I.mod"
	-@erase "$(INTDIR)\dsum_I.obj"
	-@erase "$(INTDIR)\dswap_I.mod"
	-@erase "$(INTDIR)\dtran2_I.mod"
	-@erase "$(INTDIR)\dtran2_I.obj"
	-@erase "$(INTDIR)\dtrans_I.mod"
	-@erase "$(INTDIR)\dtrans_I.obj"
	-@erase "$(INTDIR)\dtrmm_I.mod"
	-@erase "$(INTDIR)\dtrmm_I.obj"
	-@erase "$(INTDIR)\dtrmv_I.mod"
	-@erase "$(INTDIR)\dtrmv_I.obj"
	-@erase "$(INTDIR)\dtrsm_I.mod"
	-@erase "$(INTDIR)\dtrti2_I.mod"
	-@erase "$(INTDIR)\dtrti2_I.obj"
	-@erase "$(INTDIR)\ef_I.mod"
	-@erase "$(INTDIR)\ef_I.obj"
	-@erase "$(INTDIR)\einvit_I.mod"
	-@erase "$(INTDIR)\einvit_I.obj"
	-@erase "$(INTDIR)\eiscor_I.mod"
	-@erase "$(INTDIR)\eiscor_I.obj"
	-@erase "$(INTDIR)\elau_I.mod"
	-@erase "$(INTDIR)\elau_I.obj"
	-@erase "$(INTDIR)\elenuc_I.mod"
	-@erase "$(INTDIR)\elenuc_I.obj"
	-@erase "$(INTDIR)\elesn_I.mod"
	-@erase "$(INTDIR)\elesn_I.obj"
	-@erase "$(INTDIR)\en_I.mod"
	-@erase "$(INTDIR)\en_I.obj"
	-@erase "$(INTDIR)\enpart_I.mod"
	-@erase "$(INTDIR)\enpart_I.obj"
	-@erase "$(INTDIR)\epsab_I.mod"
	-@erase "$(INTDIR)\epsab_I.obj"
	-@erase "$(INTDIR)\epseta_I.mod"
	-@erase "$(INTDIR)\epseta_I.obj"
	-@erase "$(INTDIR)\epslon_I.mod"
	-@erase "$(INTDIR)\epslon_I.obj"
	-@erase "$(INTDIR)\eqlrat_I.mod"
	-@erase "$(INTDIR)\eqlrat_I.obj"
	-@erase "$(INTDIR)\esn_I.mod"
	-@erase "$(INTDIR)\esn_I.obj"
	-@erase "$(INTDIR)\esp1_I.mod"
	-@erase "$(INTDIR)\esp1_I.obj"
	-@erase "$(INTDIR)\espfit_I.mod"
	-@erase "$(INTDIR)\espfit_I.obj"
	-@erase "$(INTDIR)\estpi1_I.mod"
	-@erase "$(INTDIR)\estpi1_I.obj"
	-@erase "$(INTDIR)\etime_I.mod"
	-@erase "$(INTDIR)\etime_I.obj"
	-@erase "$(INTDIR)\etrbk3_I.mod"
	-@erase "$(INTDIR)\etrbk3_I.obj"
	-@erase "$(INTDIR)\etred3_I.mod"
	-@erase "$(INTDIR)\etred3_I.obj"
	-@erase "$(INTDIR)\evvrsp_I.mod"
	-@erase "$(INTDIR)\evvrsp_I.obj"
	-@erase "$(INTDIR)\exchng_I.mod"
	-@erase "$(INTDIR)\exchng_I.obj"
	-@erase "$(INTDIR)\fbx_I.mod"
	-@erase "$(INTDIR)\fbx_I.obj"
	-@erase "$(INTDIR)\fcnpp_I.mod"
	-@erase "$(INTDIR)\fcnpp_I.obj"
	-@erase "$(INTDIR)\ffreq1_I.mod"
	-@erase "$(INTDIR)\ffreq1_I.obj"
	-@erase "$(INTDIR)\ffreq2_I.mod"
	-@erase "$(INTDIR)\ffreq2_I.obj"
	-@erase "$(INTDIR)\fhpatn_I.mod"
	-@erase "$(INTDIR)\fhpatn_I.obj"
	-@erase "$(INTDIR)\flepo_I.mod"
	-@erase "$(INTDIR)\flepo_I.obj"
	-@erase "$(INTDIR)\fock1_I.mod"
	-@erase "$(INTDIR)\fock1_I.obj"
	-@erase "$(INTDIR)\fock2_I.mod"
	-@erase "$(INTDIR)\fock2_I.obj"
	-@erase "$(INTDIR)\fockd2_I.mod"
	-@erase "$(INTDIR)\fockd2_I.obj"
	-@erase "$(INTDIR)\force_I.mod"
	-@erase "$(INTDIR)\force_I.obj"
	-@erase "$(INTDIR)\fordd_I.mod"
	-@erase "$(INTDIR)\fordd_I.obj"
	-@erase "$(INTDIR)\formxy_I.mod"
	-@erase "$(INTDIR)\formxy_I.obj"
	-@erase "$(INTDIR)\frame_I.mod"
	-@erase "$(INTDIR)\frame_I.obj"
	-@erase "$(INTDIR)\freda_I.mod"
	-@erase "$(INTDIR)\freda_I.obj"
	-@erase "$(INTDIR)\fsub_I.mod"
	-@erase "$(INTDIR)\fsub_I.obj"
	-@erase "$(INTDIR)\genun_I.mod"
	-@erase "$(INTDIR)\genun_I.obj"
	-@erase "$(INTDIR)\genvec_I.mod"
	-@erase "$(INTDIR)\genvec_I.obj"
	-@erase "$(INTDIR)\geout_I.mod"
	-@erase "$(INTDIR)\geout_I.obj"
	-@erase "$(INTDIR)\geoutg_I.mod"
	-@erase "$(INTDIR)\geoutg_I.obj"
	-@erase "$(INTDIR)\getgeg_I.mod"
	-@erase "$(INTDIR)\getgeg_I.obj"
	-@erase "$(INTDIR)\getgeo_I.mod"
	-@erase "$(INTDIR)\getgeo_I.obj"
	-@erase "$(INTDIR)\getsym_I.mod"
	-@erase "$(INTDIR)\getsym_I.obj"
	-@erase "$(INTDIR)\gettxt_I.mod"
	-@erase "$(INTDIR)\gettxt_I.obj"
	-@erase "$(INTDIR)\getval_I.mod"
	-@erase "$(INTDIR)\getval_I.obj"
	-@erase "$(INTDIR)\gmetry_I.mod"
	-@erase "$(INTDIR)\gmetry_I.obj"
	-@erase "$(INTDIR)\gover_I.mod"
	-@erase "$(INTDIR)\gover_I.obj"
	-@erase "$(INTDIR)\grid_I.mod"
	-@erase "$(INTDIR)\grid_I.obj"
	-@erase "$(INTDIR)\grids_I.mod"
	-@erase "$(INTDIR)\grids_I.obj"
	-@erase "$(INTDIR)\gstore_I.mod"
	-@erase "$(INTDIR)\gstore_I.obj"
	-@erase "$(INTDIR)\h1elec_I.mod"
	-@erase "$(INTDIR)\h1elec_I.obj"
	-@erase "$(INTDIR)\haddon_I.mod"
	-@erase "$(INTDIR)\haddon_I.obj"
	-@erase "$(INTDIR)\hcore_I.mod"
	-@erase "$(INTDIR)\hcore_I.obj"
	-@erase "$(INTDIR)\hcored_I.mod"
	-@erase "$(INTDIR)\hcored_I.obj"
	-@erase "$(INTDIR)\helect_I.mod"
	-@erase "$(INTDIR)\helect_I.obj"
	-@erase "$(INTDIR)\hmuf_I.mod"
	-@erase "$(INTDIR)\hmuf_I.obj"
	-@erase "$(INTDIR)\hplusf_I.mod"
	-@erase "$(INTDIR)\hplusf_I.obj"
	-@erase "$(INTDIR)\idamax_I.mod"
	-@erase "$(INTDIR)\ieeeck_I.mod"
	-@erase "$(INTDIR)\inid_I.mod"
	-@erase "$(INTDIR)\inid_I.obj"
	-@erase "$(INTDIR)\inighd_I.mod"
	-@erase "$(INTDIR)\inighd_I.obj"
	-@erase "$(INTDIR)\initsn_I.mod"
	-@erase "$(INTDIR)\initsn_I.obj"
	-@erase "$(INTDIR)\initsv_I.mod"
	-@erase "$(INTDIR)\initsv_I.obj"
	-@erase "$(INTDIR)\insymc_I.mod"
	-@erase "$(INTDIR)\insymc_I.obj"
	-@erase "$(INTDIR)\interp_I.mod"
	-@erase "$(INTDIR)\interp_I.obj"
	-@erase "$(INTDIR)\intfc_I.mod"
	-@erase "$(INTDIR)\intfc_I.obj"
	-@erase "$(INTDIR)\ird_I.mod"
	-@erase "$(INTDIR)\ird_I.obj"
	-@erase "$(INTDIR)\iten_I.mod"
	-@erase "$(INTDIR)\iten_I.obj"
	-@erase "$(INTDIR)\iter_I.mod"
	-@erase "$(INTDIR)\iter_I.obj"
	-@erase "$(INTDIR)\jab_I.mod"
	-@erase "$(INTDIR)\jab_I.obj"
	-@erase "$(INTDIR)\jcarin_I.mod"
	-@erase "$(INTDIR)\jcarin_I.obj"
	-@erase "$(INTDIR)\kab_I.mod"
	-@erase "$(INTDIR)\kab_I.obj"
	-@erase "$(INTDIR)\linmin_I.mod"
	-@erase "$(INTDIR)\linmin_I.obj"
	-@erase "$(INTDIR)\locmin_I.mod"
	-@erase "$(INTDIR)\locmin_I.obj"
	-@erase "$(INTDIR)\lsame_I.mod"
	-@erase "$(INTDIR)\lsame_I.obj"
	-@erase "$(INTDIR)\makeuf_I.mod"
	-@erase "$(INTDIR)\makeuf_I.obj"
	-@erase "$(INTDIR)\makopr_I.mod"
	-@erase "$(INTDIR)\makopr_I.obj"
	-@erase "$(INTDIR)\maksym_I.mod"
	-@erase "$(INTDIR)\maksym_I.obj"
	-@erase "$(INTDIR)\mamult_I.mod"
	-@erase "$(INTDIR)\mamult_I.obj"
	-@erase "$(INTDIR)\mat33_I.mod"
	-@erase "$(INTDIR)\mat33_I.obj"
	-@erase "$(INTDIR)\matou1_I.mod"
	-@erase "$(INTDIR)\matou1_I.obj"
	-@erase "$(INTDIR)\matout_I.mod"
	-@erase "$(INTDIR)\matout_I.obj"
	-@erase "$(INTDIR)\me08a_I.mod"
	-@erase "$(INTDIR)\me08a_I.obj"
	-@erase "$(INTDIR)\meci_I.mod"
	-@erase "$(INTDIR)\meci_I.obj"
	-@erase "$(INTDIR)\mecid_I.mod"
	-@erase "$(INTDIR)\mecid_I.obj"
	-@erase "$(INTDIR)\mecih_I.mod"
	-@erase "$(INTDIR)\mecih_I.obj"
	-@erase "$(INTDIR)\mecip_I.mod"
	-@erase "$(INTDIR)\mecip_I.obj"
	-@erase "$(INTDIR)\mepchg_I.mod"
	-@erase "$(INTDIR)\mepchg_I.obj"
	-@erase "$(INTDIR)\mepmap_I.mod"
	-@erase "$(INTDIR)\mepmap_I.obj"
	-@erase "$(INTDIR)\meprot_I.mod"
	-@erase "$(INTDIR)\meprot_I.obj"
	-@erase "$(INTDIR)\minv_I.mod"
	-@erase "$(INTDIR)\minv_I.obj"
	-@erase "$(INTDIR)\molsym_I.mod"
	-@erase "$(INTDIR)\molsym_I.obj"
	-@erase "$(INTDIR)\molval_I.mod"
	-@erase "$(INTDIR)\molval_I.obj"
	-@erase "$(INTDIR)\mopend_I.mod"
	-@erase "$(INTDIR)\mopend_I.obj"
	-@erase "$(INTDIR)\mpcbds_I.mod"
	-@erase "$(INTDIR)\mpcbds_I.obj"
	-@erase "$(INTDIR)\mpcpop_I.mod"
	-@erase "$(INTDIR)\mpcpop_I.obj"
	-@erase "$(INTDIR)\mtxm_I.mod"
	-@erase "$(INTDIR)\mtxm_I.obj"
	-@erase "$(INTDIR)\mtxmc_I.mod"
	-@erase "$(INTDIR)\mtxmc_I.obj"
	-@erase "$(INTDIR)\mullik_I.mod"
	-@erase "$(INTDIR)\mullik_I.obj"
	-@erase "$(INTDIR)\mult33_I.mod"
	-@erase "$(INTDIR)\mult33_I.obj"
	-@erase "$(INTDIR)\mult_I.mod"
	-@erase "$(INTDIR)\mult_I.obj"
	-@erase "$(INTDIR)\mxm_I.mod"
	-@erase "$(INTDIR)\mxm_I.obj"
	-@erase "$(INTDIR)\mxmt_I.mod"
	-@erase "$(INTDIR)\mxmt_I.obj"
	-@erase "$(INTDIR)\myword_I.mod"
	-@erase "$(INTDIR)\myword_I.obj"
	-@erase "$(INTDIR)\naican_I.mod"
	-@erase "$(INTDIR)\naican_I.obj"
	-@erase "$(INTDIR)\naicas_I.mod"
	-@erase "$(INTDIR)\naicas_I.obj"
	-@erase "$(INTDIR)\ngamtg_I.mod"
	-@erase "$(INTDIR)\ngamtg_I.obj"
	-@erase "$(INTDIR)\ngefis_I.mod"
	-@erase "$(INTDIR)\ngefis_I.obj"
	-@erase "$(INTDIR)\ngidri_I.mod"
	-@erase "$(INTDIR)\ngidri_I.obj"
	-@erase "$(INTDIR)\ngoke_I.mod"
	-@erase "$(INTDIR)\ngoke_I.obj"
	-@erase "$(INTDIR)\nllsn_I.mod"
	-@erase "$(INTDIR)\nllsn_I.obj"
	-@erase "$(INTDIR)\nllsq_I.mod"
	-@erase "$(INTDIR)\nllsq_I.obj"
	-@erase "$(INTDIR)\nonbet_I.mod"
	-@erase "$(INTDIR)\nonbet_I.obj"
	-@erase "$(INTDIR)\nonope_I.mod"
	-@erase "$(INTDIR)\nonope_I.obj"
	-@erase "$(INTDIR)\nonor_I.mod"
	-@erase "$(INTDIR)\nonor_I.obj"
	-@erase "$(INTDIR)\nuchar_I.mod"
	-@erase "$(INTDIR)\nuchar_I.obj"
	-@erase "$(INTDIR)\openda_I.mod"
	-@erase "$(INTDIR)\openda_I.obj"
	-@erase "$(INTDIR)\orient_I.mod"
	-@erase "$(INTDIR)\orient_I.obj"
	-@erase "$(INTDIR)\osinv_I.mod"
	-@erase "$(INTDIR)\osinv_I.obj"
	-@erase "$(INTDIR)\ovlp_I.mod"
	-@erase "$(INTDIR)\ovlp_I.obj"
	-@erase "$(INTDIR)\packp_I.mod"
	-@erase "$(INTDIR)\packp_I.obj"
	-@erase "$(INTDIR)\parsav_I.mod"
	-@erase "$(INTDIR)\parsav_I.obj"
	-@erase "$(INTDIR)\partxy_I.mod"
	-@erase "$(INTDIR)\partxy_I.obj"
	-@erase "$(INTDIR)\pathk_I.mod"
	-@erase "$(INTDIR)\pathk_I.obj"
	-@erase "$(INTDIR)\paths_I.mod"
	-@erase "$(INTDIR)\paths_I.obj"
	-@erase "$(INTDIR)\pdgrid_I.mod"
	-@erase "$(INTDIR)\pdgrid_I.obj"
	-@erase "$(INTDIR)\perm_I.mod"
	-@erase "$(INTDIR)\perm_I.obj"
	-@erase "$(INTDIR)\plato_I.mod"
	-@erase "$(INTDIR)\plato_I.obj"
	-@erase "$(INTDIR)\pmep1_I.mod"
	-@erase "$(INTDIR)\pmep1_I.obj"
	-@erase "$(INTDIR)\pmep_I.mod"
	-@erase "$(INTDIR)\pmep_I.obj"
	-@erase "$(INTDIR)\pmepco_I.mod"
	-@erase "$(INTDIR)\pmepco_I.obj"
	-@erase "$(INTDIR)\poij_I.mod"
	-@erase "$(INTDIR)\poij_I.obj"
	-@erase "$(INTDIR)\pol_vol_I.mod"
	-@erase "$(INTDIR)\pol_vol_I.obj"
	-@erase "$(INTDIR)\polar_I.mod"
	-@erase "$(INTDIR)\polar_I.obj"
	-@erase "$(INTDIR)\potcal_I.mod"
	-@erase "$(INTDIR)\potcal_I.obj"
	-@erase "$(INTDIR)\powsav_I.mod"
	-@erase "$(INTDIR)\powsav_I.obj"
	-@erase "$(INTDIR)\powsq_I.mod"
	-@erase "$(INTDIR)\powsq_I.obj"
	-@erase "$(INTDIR)\printp_I.mod"
	-@erase "$(INTDIR)\printp_I.obj"
	-@erase "$(INTDIR)\prtdrc_I.mod"
	-@erase "$(INTDIR)\prtdrc_I.obj"
	-@erase "$(INTDIR)\prthco_I.mod"
	-@erase "$(INTDIR)\prthco_I.obj"
	-@erase "$(INTDIR)\prthes_I.mod"
	-@erase "$(INTDIR)\prthes_I.obj"
	-@erase "$(INTDIR)\prtpar_I.mod"
	-@erase "$(INTDIR)\prtpar_I.obj"
	-@erase "$(INTDIR)\prttim_I.mod"
	-@erase "$(INTDIR)\prttim_I.obj"
	-@erase "$(INTDIR)\pulay_I.mod"
	-@erase "$(INTDIR)\pulay_I.obj"
	-@erase "$(INTDIR)\quadr_I.mod"
	-@erase "$(INTDIR)\quadr_I.obj"
	-@erase "$(INTDIR)\react1_I.mod"
	-@erase "$(INTDIR)\react1_I.obj"
	-@erase "$(INTDIR)\reada_I.mod"
	-@erase "$(INTDIR)\reada_I.obj"
	-@erase "$(INTDIR)\readmo_I.mod"
	-@erase "$(INTDIR)\readmo_I.obj"
	-@erase "$(INTDIR)\redatm_I.mod"
	-@erase "$(INTDIR)\redatm_I.obj"
	-@erase "$(INTDIR)\refer_I.mod"
	-@erase "$(INTDIR)\refer_I.obj"
	-@erase "$(INTDIR)\reppd2_I.mod"
	-@erase "$(INTDIR)\reppd2_I.obj"
	-@erase "$(INTDIR)\reppd_I.mod"
	-@erase "$(INTDIR)\reppd_I.obj"
	-@erase "$(INTDIR)\resolv_I.mod"
	-@erase "$(INTDIR)\resolv_I.obj"
	-@erase "$(INTDIR)\rijkl_I.mod"
	-@erase "$(INTDIR)\rijkl_I.obj"
	-@erase "$(INTDIR)\rotat_I.mod"
	-@erase "$(INTDIR)\rotat_I.obj"
	-@erase "$(INTDIR)\rotatd_I.mod"
	-@erase "$(INTDIR)\rotatd_I.obj"
	-@erase "$(INTDIR)\rotate_I.mod"
	-@erase "$(INTDIR)\rotate_I.obj"
	-@erase "$(INTDIR)\rotmat_I.mod"
	-@erase "$(INTDIR)\rotmat_I.obj"
	-@erase "$(INTDIR)\rotmol_I.mod"
	-@erase "$(INTDIR)\rotmol_I.obj"
	-@erase "$(INTDIR)\rsc_I.mod"
	-@erase "$(INTDIR)\rsc_I.obj"
	-@erase "$(INTDIR)\rsp_I.mod"
	-@erase "$(INTDIR)\rsp_I.obj"
	-@erase "$(INTDIR)\schmib_I.mod"
	-@erase "$(INTDIR)\schmib_I.obj"
	-@erase "$(INTDIR)\schmit_I.mod"
	-@erase "$(INTDIR)\schmit_I.obj"
	-@erase "$(INTDIR)\scprm_I.mod"
	-@erase "$(INTDIR)\scprm_I.obj"
	-@erase "$(INTDIR)\search_I.mod"
	-@erase "$(INTDIR)\search_I.obj"
	-@erase "$(INTDIR)\second_I.mod"
	-@erase "$(INTDIR)\second_I.obj"
	-@erase "$(INTDIR)\set_I.mod"
	-@erase "$(INTDIR)\set_I.obj"
	-@erase "$(INTDIR)\setup3_I.mod"
	-@erase "$(INTDIR)\setup3_I.obj"
	-@erase "$(INTDIR)\solrot_I.mod"
	-@erase "$(INTDIR)\solrot_I.obj"
	-@erase "$(INTDIR)\sort_I.mod"
	-@erase "$(INTDIR)\sort_I.obj"
	-@erase "$(INTDIR)\spcore_I.mod"
	-@erase "$(INTDIR)\spcore_I.obj"
	-@erase "$(INTDIR)\spline_I.mod"
	-@erase "$(INTDIR)\spline_I.obj"
	-@erase "$(INTDIR)\ss_I.mod"
	-@erase "$(INTDIR)\ss_I.obj"
	-@erase "$(INTDIR)\suma2_I.mod"
	-@erase "$(INTDIR)\suma2_I.obj"
	-@erase "$(INTDIR)\supdot_I.mod"
	-@erase "$(INTDIR)\supdot_I.obj"
	-@erase "$(INTDIR)\superd_I.mod"
	-@erase "$(INTDIR)\superd_I.obj"
	-@erase "$(INTDIR)\surfa_I.mod"
	-@erase "$(INTDIR)\surfa_I.obj"
	-@erase "$(INTDIR)\surfac_I.mod"
	-@erase "$(INTDIR)\surfac_I.obj"
	-@erase "$(INTDIR)\swap_I.mod"
	-@erase "$(INTDIR)\swap_I.obj"
	-@erase "$(INTDIR)\symdec_I.mod"
	-@erase "$(INTDIR)\symdec_I.obj"
	-@erase "$(INTDIR)\symh_I.mod"
	-@erase "$(INTDIR)\symh_I.obj"
	-@erase "$(INTDIR)\symoir_I.mod"
	-@erase "$(INTDIR)\symoir_I.obj"
	-@erase "$(INTDIR)\symopr_I.mod"
	-@erase "$(INTDIR)\symopr_I.obj"
	-@erase "$(INTDIR)\symp_I.mod"
	-@erase "$(INTDIR)\symp_I.obj"
	-@erase "$(INTDIR)\sympop_I.mod"
	-@erase "$(INTDIR)\sympop_I.obj"
	-@erase "$(INTDIR)\symr_I.mod"
	-@erase "$(INTDIR)\symr_I.obj"
	-@erase "$(INTDIR)\symt_I.mod"
	-@erase "$(INTDIR)\symt_I.obj"
	-@erase "$(INTDIR)\symtry_I.mod"
	-@erase "$(INTDIR)\symtry_I.obj"
	-@erase "$(INTDIR)\symtrz_I.mod"
	-@erase "$(INTDIR)\symtrz_I.obj"
	-@erase "$(INTDIR)\tf_I.mod"
	-@erase "$(INTDIR)\tf_I.obj"
	-@erase "$(INTDIR)\thermo_I.mod"
	-@erase "$(INTDIR)\thermo_I.obj"
	-@erase "$(INTDIR)\time_I.mod"
	-@erase "$(INTDIR)\time_I.obj"
	-@erase "$(INTDIR)\timer_I.mod"
	-@erase "$(INTDIR)\timer_I.obj"
	-@erase "$(INTDIR)\timout_I.mod"
	-@erase "$(INTDIR)\timout_I.obj"
	-@erase "$(INTDIR)\transf_I.mod"
	-@erase "$(INTDIR)\transf_I.obj"
	-@erase "$(INTDIR)\trsub_I.mod"
	-@erase "$(INTDIR)\trsub_I.obj"
	-@erase "$(INTDIR)\trudgu_I.mod"
	-@erase "$(INTDIR)\trudgu_I.obj"
	-@erase "$(INTDIR)\trugdu_I.mod"
	-@erase "$(INTDIR)\trugdu_I.obj"
	-@erase "$(INTDIR)\trugud_I.mod"
	-@erase "$(INTDIR)\trugud_I.obj"
	-@erase "$(INTDIR)\tx_I.mod"
	-@erase "$(INTDIR)\tx_I.obj"
	-@erase "$(INTDIR)\upcase_I.mod"
	-@erase "$(INTDIR)\upcase_I.obj"
	-@erase "$(INTDIR)\update_I.mod"
	-@erase "$(INTDIR)\update_I.obj"
	-@erase "$(INTDIR)\vast_kind_param.mod"
	-@erase "$(INTDIR)\vastkind.obj"
	-@erase "$(INTDIR)\vecprt_I.mod"
	-@erase "$(INTDIR)\vecprt_I.obj"
	-@erase "$(INTDIR)\volume_I.mod"
	-@erase "$(INTDIR)\volume_I.obj"
	-@erase "$(INTDIR)\w2mat_I.mod"
	-@erase "$(INTDIR)\w2mat_I.obj"
	-@erase "$(INTDIR)\worder_I.mod"
	-@erase "$(INTDIR)\worder_I.obj"
	-@erase "$(INTDIR)\wrdkey_I.mod"
	-@erase "$(INTDIR)\wrdkey_I.obj"
	-@erase "$(INTDIR)\write_trajectory_I.mod"
	-@erase "$(INTDIR)\write_trajectory_I.obj"
	-@erase "$(INTDIR)\writmo_I.mod"
	-@erase "$(INTDIR)\writmo_I.obj"
	-@erase "$(INTDIR)\wrtkey_I.mod"
	-@erase "$(INTDIR)\wrtkey_I.obj"
	-@erase "$(INTDIR)\wrttxt_I.mod"
	-@erase "$(INTDIR)\wrttxt_I.obj"
	-@erase "$(INTDIR)\wstore_I.mod"
	-@erase "$(INTDIR)\wstore_I.obj"
	-@erase "$(INTDIR)\wwstep_I.mod"
	-@erase "$(INTDIR)\wwstep_I.obj"
	-@erase "$(INTDIR)\xerbla_I.mod"
	-@erase "$(INTDIR)\xerbla_I.obj"
	-@erase "$(INTDIR)\xxx_I.mod"
	-@erase "$(INTDIR)\xxx_I.obj"
	-@erase "$(INTDIR)\xyzcry_I.mod"
	-@erase "$(INTDIR)\xyzcry_I.obj"
	-@erase "$(INTDIR)\xyzint_I.mod"
	-@erase "$(INTDIR)\xyzint_I.obj"
	-@erase "$(INTDIR)\zerom_I.mod"
	-@erase "$(INTDIR)\zerom_I.obj"
	-@erase "$(OUTDIR)\Interfaces.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=df.exe
F90_PROJ=/compile_only /include:"..\Modules\Release" /nologo /traceback /warn:nofileopt /module:"Release/" /object:"Release/" 
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
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)\Interfaces.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Interfaces.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\Interfaces.lib" 
LIB32_OBJS= \
	"$(INTDIR)\aababc_I.obj" \
	"$(INTDIR)\aabacd_I.obj" \
	"$(INTDIR)\aabbcd_I.obj" \
	"$(INTDIR)\addfck_I.obj" \
	"$(INTDIR)\addhcr_I.obj" \
	"$(INTDIR)\addnuc_I.obj" \
	"$(INTDIR)\aijm_I.obj" \
	"$(INTDIR)\aintgs_I.obj" \
	"$(INTDIR)\alphaf_I.obj" \
	"$(INTDIR)\asum_I.obj" \
	"$(INTDIR)\aval_I.obj" \
	"$(INTDIR)\axis_I.obj" \
	"$(INTDIR)\babbbc_I.obj" \
	"$(INTDIR)\babbcd_I.obj" \
	"$(INTDIR)\bangle_I.obj" \
	"$(INTDIR)\bdenin_I.obj" \
	"$(INTDIR)\bdenup_I.obj" \
	"$(INTDIR)\beopor_I.obj" \
	"$(INTDIR)\betaf_I.obj" \
	"$(INTDIR)\betal1_I.obj" \
	"$(INTDIR)\betall_I.obj" \
	"$(INTDIR)\betcom_I.obj" \
	"$(INTDIR)\bfn_I.obj" \
	"$(INTDIR)\bintgs_I.obj" \
	"$(INTDIR)\blas_I.obj" \
	"$(INTDIR)\bldsym_I.obj" \
	"$(INTDIR)\bmakuf_I.obj" \
	"$(INTDIR)\bonds_I.obj" \
	"$(INTDIR)\break_I.obj" \
	"$(INTDIR)\btoc_I.obj" \
	"$(INTDIR)\calpar_I.obj" \
	"$(INTDIR)\capcor_I.obj" \
	"$(INTDIR)\cartab_I.obj" \
	"$(INTDIR)\ccprod_I.obj" \
	"$(INTDIR)\ccrep_I.obj" \
	"$(INTDIR)\cdiag_I.obj" \
	"$(INTDIR)\charg_I.obj" \
	"$(INTDIR)\charmo_I.obj" \
	"$(INTDIR)\charst_I.obj" \
	"$(INTDIR)\charvi_I.obj" \
	"$(INTDIR)\chi_I.obj" \
	"$(INTDIR)\chrge_I.obj" \
	"$(INTDIR)\ciint_I.obj" \
	"$(INTDIR)\ciosci_I.obj" \
	"$(INTDIR)\cnvg_I.obj" \
	"$(INTDIR)\coe_I.obj" \
	"$(INTDIR)\collid_I.obj" \
	"$(INTDIR)\collis_I.obj" \
	"$(INTDIR)\compfg_I.obj" \
	"$(INTDIR)\compfn_I.obj" \
	"$(INTDIR)\consts_I.obj" \
	"$(INTDIR)\copym_I.obj" \
	"$(INTDIR)\cqden_I.obj" \
	"$(INTDIR)\csum_I.obj" \
	"$(INTDIR)\dang_I.obj" \
	"$(INTDIR)\darea1_I.obj" \
	"$(INTDIR)\daread_I.obj" \
	"$(INTDIR)\dasum_I.obj" \
	"$(INTDIR)\datin_I.obj" \
	"$(INTDIR)\dawrit_I.obj" \
	"$(INTDIR)\dawrt1_I.obj" \
	"$(INTDIR)\daxpy_I.obj" \
	"$(INTDIR)\dcart_I.obj" \
	"$(INTDIR)\dcopy_I.obj" \
	"$(INTDIR)\ddot_I.obj" \
	"$(INTDIR)\ddpo_I.obj" \
	"$(INTDIR)\delmol_I.obj" \
	"$(INTDIR)\delri_I.obj" \
	"$(INTDIR)\delta_I.obj" \
	"$(INTDIR)\denrot_I.obj" \
	"$(INTDIR)\densf_I.obj" \
	"$(INTDIR)\densit_I.obj" \
	"$(INTDIR)\deri0_I.obj" \
	"$(INTDIR)\deri1_I.obj" \
	"$(INTDIR)\deri21_I.obj" \
	"$(INTDIR)\deri22_I.obj" \
	"$(INTDIR)\deri23_I.obj" \
	"$(INTDIR)\deri2_I.obj" \
	"$(INTDIR)\deritr_I.obj" \
	"$(INTDIR)\deriv_I.obj" \
	"$(INTDIR)\dernvo_I.obj" \
	"$(INTDIR)\ders_I.obj" \
	"$(INTDIR)\dex2_I.obj" \
	"$(INTDIR)\dfield_I.obj" \
	"$(INTDIR)\dfock2_I.obj" \
	"$(INTDIR)\dfpsav_I.obj" \
	"$(INTDIR)\dgefa_I.obj" \
	"$(INTDIR)\dgemv_I.obj" \
	"$(INTDIR)\dgetri_I.obj" \
	"$(INTDIR)\dhc_I.obj" \
	"$(INTDIR)\dhcore_I.obj" \
	"$(INTDIR)\diag_I.obj" \
	"$(INTDIR)\diagi_I.obj" \
	"$(INTDIR)\diat2_I.obj" \
	"$(INTDIR)\diat_I.obj" \
	"$(INTDIR)\diegrd_I.obj" \
	"$(INTDIR)\dielen_I.obj" \
	"$(INTDIR)\digit_I.obj" \
	"$(INTDIR)\dihed_I.obj" \
	"$(INTDIR)\dijkl1_I.obj" \
	"$(INTDIR)\dijkl2_I.obj" \
	"$(INTDIR)\dijkld_I.obj" \
	"$(INTDIR)\dimens_I.obj" \
	"$(INTDIR)\dipole_I.obj" \
	"$(INTDIR)\dist2_I.obj" \
	"$(INTDIR)\dmecip_I.obj" \
	"$(INTDIR)\dnrm2_I.obj" \
	"$(INTDIR)\dofs_I.obj" \
	"$(INTDIR)\dopen_I.obj" \
	"$(INTDIR)\dot_I.obj" \
	"$(INTDIR)\drc_I.obj" \
	"$(INTDIR)\drcout_I.obj" \
	"$(INTDIR)\drepp2_I.obj" \
	"$(INTDIR)\drotat_I.obj" \
	"$(INTDIR)\dsum_I.obj" \
	"$(INTDIR)\dtran2_I.obj" \
	"$(INTDIR)\dtrans_I.obj" \
	"$(INTDIR)\dtrmm_I.obj" \
	"$(INTDIR)\dtrmv_I.obj" \
	"$(INTDIR)\dtrti2_I.obj" \
	"$(INTDIR)\ef_I.obj" \
	"$(INTDIR)\einvit_I.obj" \
	"$(INTDIR)\eiscor_I.obj" \
	"$(INTDIR)\elau_I.obj" \
	"$(INTDIR)\elenuc_I.obj" \
	"$(INTDIR)\elesn_I.obj" \
	"$(INTDIR)\en_I.obj" \
	"$(INTDIR)\enpart_I.obj" \
	"$(INTDIR)\epsab_I.obj" \
	"$(INTDIR)\epseta_I.obj" \
	"$(INTDIR)\epslon_I.obj" \
	"$(INTDIR)\eqlrat_I.obj" \
	"$(INTDIR)\esn_I.obj" \
	"$(INTDIR)\esp1_I.obj" \
	"$(INTDIR)\espfit_I.obj" \
	"$(INTDIR)\estpi1_I.obj" \
	"$(INTDIR)\etime_I.obj" \
	"$(INTDIR)\etrbk3_I.obj" \
	"$(INTDIR)\etred3_I.obj" \
	"$(INTDIR)\evvrsp_I.obj" \
	"$(INTDIR)\exchng_I.obj" \
	"$(INTDIR)\fbx_I.obj" \
	"$(INTDIR)\fcnpp_I.obj" \
	"$(INTDIR)\ffreq1_I.obj" \
	"$(INTDIR)\ffreq2_I.obj" \
	"$(INTDIR)\fhpatn_I.obj" \
	"$(INTDIR)\flepo_I.obj" \
	"$(INTDIR)\fock1_I.obj" \
	"$(INTDIR)\fock2_I.obj" \
	"$(INTDIR)\fockd2_I.obj" \
	"$(INTDIR)\force_I.obj" \
	"$(INTDIR)\fordd_I.obj" \
	"$(INTDIR)\formxy_I.obj" \
	"$(INTDIR)\frame_I.obj" \
	"$(INTDIR)\freda_I.obj" \
	"$(INTDIR)\fsub_I.obj" \
	"$(INTDIR)\genun_I.obj" \
	"$(INTDIR)\genvec_I.obj" \
	"$(INTDIR)\geout_I.obj" \
	"$(INTDIR)\geoutg_I.obj" \
	"$(INTDIR)\getgeg_I.obj" \
	"$(INTDIR)\getgeo_I.obj" \
	"$(INTDIR)\getsym_I.obj" \
	"$(INTDIR)\gettxt_I.obj" \
	"$(INTDIR)\getval_I.obj" \
	"$(INTDIR)\gmetry_I.obj" \
	"$(INTDIR)\gover_I.obj" \
	"$(INTDIR)\grid_I.obj" \
	"$(INTDIR)\grids_I.obj" \
	"$(INTDIR)\gstore_I.obj" \
	"$(INTDIR)\h1elec_I.obj" \
	"$(INTDIR)\haddon_I.obj" \
	"$(INTDIR)\hcore_I.obj" \
	"$(INTDIR)\hcored_I.obj" \
	"$(INTDIR)\helect_I.obj" \
	"$(INTDIR)\hmuf_I.obj" \
	"$(INTDIR)\hplusf_I.obj" \
	"$(INTDIR)\inid_I.obj" \
	"$(INTDIR)\inighd_I.obj" \
	"$(INTDIR)\initsn_I.obj" \
	"$(INTDIR)\initsv_I.obj" \
	"$(INTDIR)\insymc_I.obj" \
	"$(INTDIR)\interp_I.obj" \
	"$(INTDIR)\intfc_I.obj" \
	"$(INTDIR)\ird_I.obj" \
	"$(INTDIR)\iten_I.obj" \
	"$(INTDIR)\iter_I.obj" \
	"$(INTDIR)\jab_I.obj" \
	"$(INTDIR)\jcarin_I.obj" \
	"$(INTDIR)\kab_I.obj" \
	"$(INTDIR)\linmin_I.obj" \
	"$(INTDIR)\locmin_I.obj" \
	"$(INTDIR)\lsame_I.obj" \
	"$(INTDIR)\makeuf_I.obj" \
	"$(INTDIR)\makopr_I.obj" \
	"$(INTDIR)\maksym_I.obj" \
	"$(INTDIR)\mamult_I.obj" \
	"$(INTDIR)\mat33_I.obj" \
	"$(INTDIR)\matou1_I.obj" \
	"$(INTDIR)\matout_I.obj" \
	"$(INTDIR)\me08a_I.obj" \
	"$(INTDIR)\meci_I.obj" \
	"$(INTDIR)\mecid_I.obj" \
	"$(INTDIR)\mecih_I.obj" \
	"$(INTDIR)\mecip_I.obj" \
	"$(INTDIR)\mepchg_I.obj" \
	"$(INTDIR)\mepmap_I.obj" \
	"$(INTDIR)\meprot_I.obj" \
	"$(INTDIR)\minv_I.obj" \
	"$(INTDIR)\molsym_I.obj" \
	"$(INTDIR)\molval_I.obj" \
	"$(INTDIR)\mopend_I.obj" \
	"$(INTDIR)\mpcbds_I.obj" \
	"$(INTDIR)\mpcpop_I.obj" \
	"$(INTDIR)\mtxm_I.obj" \
	"$(INTDIR)\mtxmc_I.obj" \
	"$(INTDIR)\mullik_I.obj" \
	"$(INTDIR)\mult33_I.obj" \
	"$(INTDIR)\mult_I.obj" \
	"$(INTDIR)\mxm_I.obj" \
	"$(INTDIR)\mxmt_I.obj" \
	"$(INTDIR)\myword_I.obj" \
	"$(INTDIR)\naican_I.obj" \
	"$(INTDIR)\naicas_I.obj" \
	"$(INTDIR)\ngamtg_I.obj" \
	"$(INTDIR)\ngefis_I.obj" \
	"$(INTDIR)\ngidri_I.obj" \
	"$(INTDIR)\ngoke_I.obj" \
	"$(INTDIR)\nllsn_I.obj" \
	"$(INTDIR)\nllsq_I.obj" \
	"$(INTDIR)\nonbet_I.obj" \
	"$(INTDIR)\nonope_I.obj" \
	"$(INTDIR)\nonor_I.obj" \
	"$(INTDIR)\nuchar_I.obj" \
	"$(INTDIR)\openda_I.obj" \
	"$(INTDIR)\orient_I.obj" \
	"$(INTDIR)\osinv_I.obj" \
	"$(INTDIR)\ovlp_I.obj" \
	"$(INTDIR)\packp_I.obj" \
	"$(INTDIR)\parsav_I.obj" \
	"$(INTDIR)\partxy_I.obj" \
	"$(INTDIR)\pathk_I.obj" \
	"$(INTDIR)\paths_I.obj" \
	"$(INTDIR)\pdgrid_I.obj" \
	"$(INTDIR)\perm_I.obj" \
	"$(INTDIR)\plato_I.obj" \
	"$(INTDIR)\pmep1_I.obj" \
	"$(INTDIR)\pmep_I.obj" \
	"$(INTDIR)\pmepco_I.obj" \
	"$(INTDIR)\poij_I.obj" \
	"$(INTDIR)\pol_vol_I.obj" \
	"$(INTDIR)\polar_I.obj" \
	"$(INTDIR)\potcal_I.obj" \
	"$(INTDIR)\powsav_I.obj" \
	"$(INTDIR)\powsq_I.obj" \
	"$(INTDIR)\printp_I.obj" \
	"$(INTDIR)\prtdrc_I.obj" \
	"$(INTDIR)\prthco_I.obj" \
	"$(INTDIR)\prthes_I.obj" \
	"$(INTDIR)\prtpar_I.obj" \
	"$(INTDIR)\prttim_I.obj" \
	"$(INTDIR)\pulay_I.obj" \
	"$(INTDIR)\quadr_I.obj" \
	"$(INTDIR)\react1_I.obj" \
	"$(INTDIR)\reada_I.obj" \
	"$(INTDIR)\readmo_I.obj" \
	"$(INTDIR)\redatm_I.obj" \
	"$(INTDIR)\refer_I.obj" \
	"$(INTDIR)\reppd2_I.obj" \
	"$(INTDIR)\reppd_I.obj" \
	"$(INTDIR)\resolv_I.obj" \
	"$(INTDIR)\rijkl_I.obj" \
	"$(INTDIR)\rotat_I.obj" \
	"$(INTDIR)\rotatd_I.obj" \
	"$(INTDIR)\rotate_I.obj" \
	"$(INTDIR)\rotmat_I.obj" \
	"$(INTDIR)\rotmol_I.obj" \
	"$(INTDIR)\rsc_I.obj" \
	"$(INTDIR)\rsp_I.obj" \
	"$(INTDIR)\schmib_I.obj" \
	"$(INTDIR)\schmit_I.obj" \
	"$(INTDIR)\scprm_I.obj" \
	"$(INTDIR)\search_I.obj" \
	"$(INTDIR)\second_I.obj" \
	"$(INTDIR)\set_I.obj" \
	"$(INTDIR)\setup3_I.obj" \
	"$(INTDIR)\solrot_I.obj" \
	"$(INTDIR)\sort_I.obj" \
	"$(INTDIR)\spcore_I.obj" \
	"$(INTDIR)\spline_I.obj" \
	"$(INTDIR)\ss_I.obj" \
	"$(INTDIR)\suma2_I.obj" \
	"$(INTDIR)\supdot_I.obj" \
	"$(INTDIR)\superd_I.obj" \
	"$(INTDIR)\surfa_I.obj" \
	"$(INTDIR)\surfac_I.obj" \
	"$(INTDIR)\swap_I.obj" \
	"$(INTDIR)\symdec_I.obj" \
	"$(INTDIR)\symh_I.obj" \
	"$(INTDIR)\symoir_I.obj" \
	"$(INTDIR)\symopr_I.obj" \
	"$(INTDIR)\symp_I.obj" \
	"$(INTDIR)\sympop_I.obj" \
	"$(INTDIR)\symr_I.obj" \
	"$(INTDIR)\symt_I.obj" \
	"$(INTDIR)\symtry_I.obj" \
	"$(INTDIR)\symtrz_I.obj" \
	"$(INTDIR)\tf_I.obj" \
	"$(INTDIR)\thermo_I.obj" \
	"$(INTDIR)\time_I.obj" \
	"$(INTDIR)\timer_I.obj" \
	"$(INTDIR)\timout_I.obj" \
	"$(INTDIR)\transf_I.obj" \
	"$(INTDIR)\trsub_I.obj" \
	"$(INTDIR)\trudgu_I.obj" \
	"$(INTDIR)\trugdu_I.obj" \
	"$(INTDIR)\trugud_I.obj" \
	"$(INTDIR)\tx_I.obj" \
	"$(INTDIR)\upcase_I.obj" \
	"$(INTDIR)\update_I.obj" \
	"$(INTDIR)\vastkind.obj" \
	"$(INTDIR)\vecprt_I.obj" \
	"$(INTDIR)\volume_I.obj" \
	"$(INTDIR)\w2mat_I.obj" \
	"$(INTDIR)\worder_I.obj" \
	"$(INTDIR)\wrdkey_I.obj" \
	"$(INTDIR)\write_trajectory_I.obj" \
	"$(INTDIR)\writmo_I.obj" \
	"$(INTDIR)\wrtkey_I.obj" \
	"$(INTDIR)\wrttxt_I.obj" \
	"$(INTDIR)\wstore_I.obj" \
	"$(INTDIR)\wwstep_I.obj" \
	"$(INTDIR)\xerbla_I.obj" \
	"$(INTDIR)\xxx_I.obj" \
	"$(INTDIR)\xyzcry_I.obj" \
	"$(INTDIR)\xyzint_I.obj" \
	"$(INTDIR)\zerom_I.obj" \
	"..\Modules\Release\Modules.lib"

"$(OUTDIR)\Interfaces.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Interfaces.lib"

!ELSE 

ALL : "Modules - Win32 Debug" "$(OUTDIR)\Interfaces.lib"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"Modules - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\aababc_I.obj"
	-@erase "$(INTDIR)\aabacd_I.obj"
	-@erase "$(INTDIR)\aabbcd_I.obj"
	-@erase "$(INTDIR)\addfck_I.obj"
	-@erase "$(INTDIR)\addhcr_I.obj"
	-@erase "$(INTDIR)\addnuc_I.obj"
	-@erase "$(INTDIR)\aijm_I.obj"
	-@erase "$(INTDIR)\aintgs_I.obj"
	-@erase "$(INTDIR)\alphaf_I.obj"
	-@erase "$(INTDIR)\asum_I.obj"
	-@erase "$(INTDIR)\aval_I.obj"
	-@erase "$(INTDIR)\axis_I.obj"
	-@erase "$(INTDIR)\babbbc_I.obj"
	-@erase "$(INTDIR)\babbcd_I.obj"
	-@erase "$(INTDIR)\bangle_I.obj"
	-@erase "$(INTDIR)\bdenin_I.obj"
	-@erase "$(INTDIR)\bdenup_I.obj"
	-@erase "$(INTDIR)\beopor_I.obj"
	-@erase "$(INTDIR)\betaf_I.obj"
	-@erase "$(INTDIR)\betal1_I.obj"
	-@erase "$(INTDIR)\betall_I.obj"
	-@erase "$(INTDIR)\betcom_I.obj"
	-@erase "$(INTDIR)\bfn_I.obj"
	-@erase "$(INTDIR)\bintgs_I.obj"
	-@erase "$(INTDIR)\blas_I.obj"
	-@erase "$(INTDIR)\bldsym_I.obj"
	-@erase "$(INTDIR)\bmakuf_I.obj"
	-@erase "$(INTDIR)\bonds_I.obj"
	-@erase "$(INTDIR)\break_I.obj"
	-@erase "$(INTDIR)\btoc_I.obj"
	-@erase "$(INTDIR)\calpar_I.obj"
	-@erase "$(INTDIR)\capcor_I.obj"
	-@erase "$(INTDIR)\cartab_I.obj"
	-@erase "$(INTDIR)\ccprod_I.obj"
	-@erase "$(INTDIR)\ccrep_I.obj"
	-@erase "$(INTDIR)\cdiag_I.obj"
	-@erase "$(INTDIR)\charg_I.obj"
	-@erase "$(INTDIR)\charmo_I.obj"
	-@erase "$(INTDIR)\charst_I.obj"
	-@erase "$(INTDIR)\charvi_I.obj"
	-@erase "$(INTDIR)\chi_I.obj"
	-@erase "$(INTDIR)\chrge_I.obj"
	-@erase "$(INTDIR)\ciint_I.obj"
	-@erase "$(INTDIR)\ciosci_I.obj"
	-@erase "$(INTDIR)\cnvg_I.obj"
	-@erase "$(INTDIR)\coe_I.obj"
	-@erase "$(INTDIR)\collid_I.obj"
	-@erase "$(INTDIR)\collis_I.obj"
	-@erase "$(INTDIR)\compfg_I.obj"
	-@erase "$(INTDIR)\compfn_I.obj"
	-@erase "$(INTDIR)\consts_I.obj"
	-@erase "$(INTDIR)\copym_I.obj"
	-@erase "$(INTDIR)\cqden_I.obj"
	-@erase "$(INTDIR)\csum_I.obj"
	-@erase "$(INTDIR)\dang_I.obj"
	-@erase "$(INTDIR)\darea1_I.obj"
	-@erase "$(INTDIR)\daread_I.obj"
	-@erase "$(INTDIR)\dasum_I.obj"
	-@erase "$(INTDIR)\datin_I.obj"
	-@erase "$(INTDIR)\dawrit_I.obj"
	-@erase "$(INTDIR)\dawrt1_I.obj"
	-@erase "$(INTDIR)\daxpy_I.obj"
	-@erase "$(INTDIR)\dcart_I.obj"
	-@erase "$(INTDIR)\dcopy_I.obj"
	-@erase "$(INTDIR)\ddot_I.obj"
	-@erase "$(INTDIR)\ddpo_I.obj"
	-@erase "$(INTDIR)\delmol_I.obj"
	-@erase "$(INTDIR)\delri_I.obj"
	-@erase "$(INTDIR)\delta_I.obj"
	-@erase "$(INTDIR)\denrot_I.obj"
	-@erase "$(INTDIR)\densf_I.obj"
	-@erase "$(INTDIR)\densit_I.obj"
	-@erase "$(INTDIR)\deri0_I.obj"
	-@erase "$(INTDIR)\deri1_I.obj"
	-@erase "$(INTDIR)\deri21_I.obj"
	-@erase "$(INTDIR)\deri22_I.obj"
	-@erase "$(INTDIR)\deri23_I.obj"
	-@erase "$(INTDIR)\deri2_I.obj"
	-@erase "$(INTDIR)\deritr_I.obj"
	-@erase "$(INTDIR)\deriv_I.obj"
	-@erase "$(INTDIR)\dernvo_I.obj"
	-@erase "$(INTDIR)\ders_I.obj"
	-@erase "$(INTDIR)\dex2_I.obj"
	-@erase "$(INTDIR)\DF60.PDB"
	-@erase "$(INTDIR)\dfield_I.obj"
	-@erase "$(INTDIR)\dfock2_I.obj"
	-@erase "$(INTDIR)\dfpsav_I.obj"
	-@erase "$(INTDIR)\dgefa_I.obj"
	-@erase "$(INTDIR)\dgemv_I.obj"
	-@erase "$(INTDIR)\dgetri_I.obj"
	-@erase "$(INTDIR)\dhc_I.obj"
	-@erase "$(INTDIR)\dhcore_I.obj"
	-@erase "$(INTDIR)\diag_I.obj"
	-@erase "$(INTDIR)\diagi_I.obj"
	-@erase "$(INTDIR)\diat2_I.obj"
	-@erase "$(INTDIR)\diat_I.obj"
	-@erase "$(INTDIR)\diegrd_I.obj"
	-@erase "$(INTDIR)\dielen_I.obj"
	-@erase "$(INTDIR)\digit_I.obj"
	-@erase "$(INTDIR)\dihed_I.obj"
	-@erase "$(INTDIR)\dijkl1_I.obj"
	-@erase "$(INTDIR)\dijkl2_I.obj"
	-@erase "$(INTDIR)\dijkld_I.obj"
	-@erase "$(INTDIR)\dimens_I.obj"
	-@erase "$(INTDIR)\dipole_I.obj"
	-@erase "$(INTDIR)\dist2_I.obj"
	-@erase "$(INTDIR)\dmecip_I.obj"
	-@erase "$(INTDIR)\dnrm2_I.obj"
	-@erase "$(INTDIR)\dofs_I.obj"
	-@erase "$(INTDIR)\dopen_I.obj"
	-@erase "$(INTDIR)\dot_I.obj"
	-@erase "$(INTDIR)\drc_I.obj"
	-@erase "$(INTDIR)\drcout_I.obj"
	-@erase "$(INTDIR)\drepp2_I.obj"
	-@erase "$(INTDIR)\drotat_I.obj"
	-@erase "$(INTDIR)\dsum_I.obj"
	-@erase "$(INTDIR)\dtran2_I.obj"
	-@erase "$(INTDIR)\dtrans_I.obj"
	-@erase "$(INTDIR)\dtrmm_I.obj"
	-@erase "$(INTDIR)\dtrmv_I.obj"
	-@erase "$(INTDIR)\dtrti2_I.obj"
	-@erase "$(INTDIR)\ef_I.obj"
	-@erase "$(INTDIR)\einvit_I.obj"
	-@erase "$(INTDIR)\eiscor_I.obj"
	-@erase "$(INTDIR)\elau_I.obj"
	-@erase "$(INTDIR)\elenuc_I.obj"
	-@erase "$(INTDIR)\elesn_I.obj"
	-@erase "$(INTDIR)\en_I.obj"
	-@erase "$(INTDIR)\enpart_I.obj"
	-@erase "$(INTDIR)\epsab_I.obj"
	-@erase "$(INTDIR)\epseta_I.obj"
	-@erase "$(INTDIR)\epslon_I.obj"
	-@erase "$(INTDIR)\eqlrat_I.obj"
	-@erase "$(INTDIR)\esn_I.obj"
	-@erase "$(INTDIR)\esp1_I.obj"
	-@erase "$(INTDIR)\espfit_I.obj"
	-@erase "$(INTDIR)\estpi1_I.obj"
	-@erase "$(INTDIR)\etime_I.obj"
	-@erase "$(INTDIR)\etrbk3_I.obj"
	-@erase "$(INTDIR)\etred3_I.obj"
	-@erase "$(INTDIR)\evvrsp_I.obj"
	-@erase "$(INTDIR)\exchng_I.obj"
	-@erase "$(INTDIR)\fbx_I.obj"
	-@erase "$(INTDIR)\fcnpp_I.obj"
	-@erase "$(INTDIR)\ffreq1_I.obj"
	-@erase "$(INTDIR)\ffreq2_I.obj"
	-@erase "$(INTDIR)\fhpatn_I.obj"
	-@erase "$(INTDIR)\flepo_I.obj"
	-@erase "$(INTDIR)\fock1_I.obj"
	-@erase "$(INTDIR)\fock2_I.obj"
	-@erase "$(INTDIR)\fockd2_I.obj"
	-@erase "$(INTDIR)\force_I.obj"
	-@erase "$(INTDIR)\fordd_I.obj"
	-@erase "$(INTDIR)\formxy_I.obj"
	-@erase "$(INTDIR)\frame_I.obj"
	-@erase "$(INTDIR)\freda_I.obj"
	-@erase "$(INTDIR)\fsub_I.obj"
	-@erase "$(INTDIR)\genun_I.obj"
	-@erase "$(INTDIR)\genvec_I.obj"
	-@erase "$(INTDIR)\geout_I.obj"
	-@erase "$(INTDIR)\geoutg_I.obj"
	-@erase "$(INTDIR)\getgeg_I.obj"
	-@erase "$(INTDIR)\getgeo_I.obj"
	-@erase "$(INTDIR)\getsym_I.obj"
	-@erase "$(INTDIR)\gettxt_I.obj"
	-@erase "$(INTDIR)\getval_I.obj"
	-@erase "$(INTDIR)\gmetry_I.obj"
	-@erase "$(INTDIR)\gover_I.obj"
	-@erase "$(INTDIR)\grid_I.obj"
	-@erase "$(INTDIR)\grids_I.obj"
	-@erase "$(INTDIR)\gstore_I.obj"
	-@erase "$(INTDIR)\h1elec_I.obj"
	-@erase "$(INTDIR)\haddon_I.obj"
	-@erase "$(INTDIR)\hcore_I.obj"
	-@erase "$(INTDIR)\hcored_I.obj"
	-@erase "$(INTDIR)\helect_I.obj"
	-@erase "$(INTDIR)\hmuf_I.obj"
	-@erase "$(INTDIR)\hplusf_I.obj"
	-@erase "$(INTDIR)\inid_I.obj"
	-@erase "$(INTDIR)\inighd_I.obj"
	-@erase "$(INTDIR)\initsn_I.obj"
	-@erase "$(INTDIR)\initsv_I.obj"
	-@erase "$(INTDIR)\insymc_I.obj"
	-@erase "$(INTDIR)\interp_I.obj"
	-@erase "$(INTDIR)\intfc_I.obj"
	-@erase "$(INTDIR)\ird_I.obj"
	-@erase "$(INTDIR)\iten_I.obj"
	-@erase "$(INTDIR)\iter_I.obj"
	-@erase "$(INTDIR)\jab_I.obj"
	-@erase "$(INTDIR)\jcarin_I.obj"
	-@erase "$(INTDIR)\kab_I.obj"
	-@erase "$(INTDIR)\linmin_I.obj"
	-@erase "$(INTDIR)\locmin_I.obj"
	-@erase "$(INTDIR)\lsame_I.obj"
	-@erase "$(INTDIR)\makeuf_I.obj"
	-@erase "$(INTDIR)\makopr_I.obj"
	-@erase "$(INTDIR)\maksym_I.obj"
	-@erase "$(INTDIR)\mamult_I.obj"
	-@erase "$(INTDIR)\mat33_I.obj"
	-@erase "$(INTDIR)\matou1_I.obj"
	-@erase "$(INTDIR)\matout_I.obj"
	-@erase "$(INTDIR)\me08a_I.obj"
	-@erase "$(INTDIR)\meci_I.obj"
	-@erase "$(INTDIR)\mecid_I.obj"
	-@erase "$(INTDIR)\mecih_I.obj"
	-@erase "$(INTDIR)\mecip_I.obj"
	-@erase "$(INTDIR)\mepchg_I.obj"
	-@erase "$(INTDIR)\mepmap_I.obj"
	-@erase "$(INTDIR)\meprot_I.obj"
	-@erase "$(INTDIR)\minv_I.obj"
	-@erase "$(INTDIR)\molsym_I.obj"
	-@erase "$(INTDIR)\molval_I.obj"
	-@erase "$(INTDIR)\mopend_I.obj"
	-@erase "$(INTDIR)\mpcbds_I.obj"
	-@erase "$(INTDIR)\mpcpop_I.obj"
	-@erase "$(INTDIR)\mtxm_I.obj"
	-@erase "$(INTDIR)\mtxmc_I.obj"
	-@erase "$(INTDIR)\mullik_I.obj"
	-@erase "$(INTDIR)\mult33_I.obj"
	-@erase "$(INTDIR)\mult_I.obj"
	-@erase "$(INTDIR)\mxm_I.obj"
	-@erase "$(INTDIR)\mxmt_I.obj"
	-@erase "$(INTDIR)\myword_I.obj"
	-@erase "$(INTDIR)\naican_I.obj"
	-@erase "$(INTDIR)\naicas_I.obj"
	-@erase "$(INTDIR)\ngamtg_I.obj"
	-@erase "$(INTDIR)\ngefis_I.obj"
	-@erase "$(INTDIR)\ngidri_I.obj"
	-@erase "$(INTDIR)\ngoke_I.obj"
	-@erase "$(INTDIR)\nllsn_I.obj"
	-@erase "$(INTDIR)\nllsq_I.obj"
	-@erase "$(INTDIR)\nonbet_I.obj"
	-@erase "$(INTDIR)\nonope_I.obj"
	-@erase "$(INTDIR)\nonor_I.obj"
	-@erase "$(INTDIR)\nuchar_I.obj"
	-@erase "$(INTDIR)\openda_I.obj"
	-@erase "$(INTDIR)\orient_I.obj"
	-@erase "$(INTDIR)\osinv_I.obj"
	-@erase "$(INTDIR)\ovlp_I.obj"
	-@erase "$(INTDIR)\packp_I.obj"
	-@erase "$(INTDIR)\parsav_I.obj"
	-@erase "$(INTDIR)\partxy_I.obj"
	-@erase "$(INTDIR)\pathk_I.obj"
	-@erase "$(INTDIR)\paths_I.obj"
	-@erase "$(INTDIR)\pdgrid_I.obj"
	-@erase "$(INTDIR)\perm_I.obj"
	-@erase "$(INTDIR)\plato_I.obj"
	-@erase "$(INTDIR)\pmep1_I.obj"
	-@erase "$(INTDIR)\pmep_I.obj"
	-@erase "$(INTDIR)\pmepco_I.obj"
	-@erase "$(INTDIR)\poij_I.obj"
	-@erase "$(INTDIR)\pol_vol_I.obj"
	-@erase "$(INTDIR)\polar_I.obj"
	-@erase "$(INTDIR)\potcal_I.obj"
	-@erase "$(INTDIR)\powsav_I.obj"
	-@erase "$(INTDIR)\powsq_I.obj"
	-@erase "$(INTDIR)\printp_I.obj"
	-@erase "$(INTDIR)\prtdrc_I.obj"
	-@erase "$(INTDIR)\prthco_I.obj"
	-@erase "$(INTDIR)\prthes_I.obj"
	-@erase "$(INTDIR)\prtpar_I.obj"
	-@erase "$(INTDIR)\prttim_I.obj"
	-@erase "$(INTDIR)\pulay_I.obj"
	-@erase "$(INTDIR)\quadr_I.obj"
	-@erase "$(INTDIR)\react1_I.obj"
	-@erase "$(INTDIR)\reada_I.obj"
	-@erase "$(INTDIR)\readmo_I.obj"
	-@erase "$(INTDIR)\redatm_I.obj"
	-@erase "$(INTDIR)\refer_I.obj"
	-@erase "$(INTDIR)\reppd2_I.obj"
	-@erase "$(INTDIR)\reppd_I.obj"
	-@erase "$(INTDIR)\resolv_I.obj"
	-@erase "$(INTDIR)\rijkl_I.obj"
	-@erase "$(INTDIR)\rotat_I.obj"
	-@erase "$(INTDIR)\rotatd_I.obj"
	-@erase "$(INTDIR)\rotate_I.obj"
	-@erase "$(INTDIR)\rotmat_I.obj"
	-@erase "$(INTDIR)\rotmol_I.obj"
	-@erase "$(INTDIR)\rsc_I.obj"
	-@erase "$(INTDIR)\rsp_I.obj"
	-@erase "$(INTDIR)\schmib_I.obj"
	-@erase "$(INTDIR)\schmit_I.obj"
	-@erase "$(INTDIR)\scprm_I.obj"
	-@erase "$(INTDIR)\search_I.obj"
	-@erase "$(INTDIR)\second_I.obj"
	-@erase "$(INTDIR)\set_I.obj"
	-@erase "$(INTDIR)\setup3_I.obj"
	-@erase "$(INTDIR)\solrot_I.obj"
	-@erase "$(INTDIR)\sort_I.obj"
	-@erase "$(INTDIR)\spcore_I.obj"
	-@erase "$(INTDIR)\spline_I.obj"
	-@erase "$(INTDIR)\ss_I.obj"
	-@erase "$(INTDIR)\suma2_I.obj"
	-@erase "$(INTDIR)\supdot_I.obj"
	-@erase "$(INTDIR)\superd_I.obj"
	-@erase "$(INTDIR)\surfa_I.obj"
	-@erase "$(INTDIR)\surfac_I.obj"
	-@erase "$(INTDIR)\swap_I.obj"
	-@erase "$(INTDIR)\symdec_I.obj"
	-@erase "$(INTDIR)\symh_I.obj"
	-@erase "$(INTDIR)\symoir_I.obj"
	-@erase "$(INTDIR)\symopr_I.obj"
	-@erase "$(INTDIR)\symp_I.obj"
	-@erase "$(INTDIR)\sympop_I.obj"
	-@erase "$(INTDIR)\symr_I.obj"
	-@erase "$(INTDIR)\symt_I.obj"
	-@erase "$(INTDIR)\symtry_I.obj"
	-@erase "$(INTDIR)\symtrz_I.obj"
	-@erase "$(INTDIR)\tf_I.obj"
	-@erase "$(INTDIR)\thermo_I.obj"
	-@erase "$(INTDIR)\time_I.obj"
	-@erase "$(INTDIR)\timer_I.obj"
	-@erase "$(INTDIR)\timout_I.obj"
	-@erase "$(INTDIR)\transf_I.obj"
	-@erase "$(INTDIR)\trsub_I.obj"
	-@erase "$(INTDIR)\trudgu_I.obj"
	-@erase "$(INTDIR)\trugdu_I.obj"
	-@erase "$(INTDIR)\trugud_I.obj"
	-@erase "$(INTDIR)\tx_I.obj"
	-@erase "$(INTDIR)\upcase_I.obj"
	-@erase "$(INTDIR)\update_I.obj"
	-@erase "$(INTDIR)\vastkind.obj"
	-@erase "$(INTDIR)\vecprt_I.obj"
	-@erase "$(INTDIR)\volume_I.obj"
	-@erase "$(INTDIR)\w2mat_I.obj"
	-@erase "$(INTDIR)\worder_I.obj"
	-@erase "$(INTDIR)\wrdkey_I.obj"
	-@erase "$(INTDIR)\write_trajectory_I.obj"
	-@erase "$(INTDIR)\writmo_I.obj"
	-@erase "$(INTDIR)\wrtkey_I.obj"
	-@erase "$(INTDIR)\wrttxt_I.obj"
	-@erase "$(INTDIR)\wstore_I.obj"
	-@erase "$(INTDIR)\wwstep_I.obj"
	-@erase "$(INTDIR)\xerbla_I.obj"
	-@erase "$(INTDIR)\xxx_I.obj"
	-@erase "$(INTDIR)\xyzcry_I.obj"
	-@erase "$(INTDIR)\xyzint_I.obj"
	-@erase "$(INTDIR)\zerom_I.obj"
	-@erase "$(OUTDIR)\Interfaces.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=df.exe
F90_PROJ=/check:bounds /compile_only /dbglibs /debug:full /include:"..\Modules\Debug" /nologo /traceback /warn:argument_checking /warn:nofileopt /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF60.PDB" 
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
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)\Interfaces.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Interfaces.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\Interfaces.lib" 
LIB32_OBJS= \
	"$(INTDIR)\aababc_I.obj" \
	"$(INTDIR)\aabacd_I.obj" \
	"$(INTDIR)\aabbcd_I.obj" \
	"$(INTDIR)\addfck_I.obj" \
	"$(INTDIR)\addhcr_I.obj" \
	"$(INTDIR)\addnuc_I.obj" \
	"$(INTDIR)\aijm_I.obj" \
	"$(INTDIR)\aintgs_I.obj" \
	"$(INTDIR)\alphaf_I.obj" \
	"$(INTDIR)\asum_I.obj" \
	"$(INTDIR)\aval_I.obj" \
	"$(INTDIR)\axis_I.obj" \
	"$(INTDIR)\babbbc_I.obj" \
	"$(INTDIR)\babbcd_I.obj" \
	"$(INTDIR)\bangle_I.obj" \
	"$(INTDIR)\bdenin_I.obj" \
	"$(INTDIR)\bdenup_I.obj" \
	"$(INTDIR)\beopor_I.obj" \
	"$(INTDIR)\betaf_I.obj" \
	"$(INTDIR)\betal1_I.obj" \
	"$(INTDIR)\betall_I.obj" \
	"$(INTDIR)\betcom_I.obj" \
	"$(INTDIR)\bfn_I.obj" \
	"$(INTDIR)\bintgs_I.obj" \
	"$(INTDIR)\blas_I.obj" \
	"$(INTDIR)\bldsym_I.obj" \
	"$(INTDIR)\bmakuf_I.obj" \
	"$(INTDIR)\bonds_I.obj" \
	"$(INTDIR)\break_I.obj" \
	"$(INTDIR)\btoc_I.obj" \
	"$(INTDIR)\calpar_I.obj" \
	"$(INTDIR)\capcor_I.obj" \
	"$(INTDIR)\cartab_I.obj" \
	"$(INTDIR)\ccprod_I.obj" \
	"$(INTDIR)\ccrep_I.obj" \
	"$(INTDIR)\cdiag_I.obj" \
	"$(INTDIR)\charg_I.obj" \
	"$(INTDIR)\charmo_I.obj" \
	"$(INTDIR)\charst_I.obj" \
	"$(INTDIR)\charvi_I.obj" \
	"$(INTDIR)\chi_I.obj" \
	"$(INTDIR)\chrge_I.obj" \
	"$(INTDIR)\ciint_I.obj" \
	"$(INTDIR)\ciosci_I.obj" \
	"$(INTDIR)\cnvg_I.obj" \
	"$(INTDIR)\coe_I.obj" \
	"$(INTDIR)\collid_I.obj" \
	"$(INTDIR)\collis_I.obj" \
	"$(INTDIR)\compfg_I.obj" \
	"$(INTDIR)\compfn_I.obj" \
	"$(INTDIR)\consts_I.obj" \
	"$(INTDIR)\copym_I.obj" \
	"$(INTDIR)\cqden_I.obj" \
	"$(INTDIR)\csum_I.obj" \
	"$(INTDIR)\dang_I.obj" \
	"$(INTDIR)\darea1_I.obj" \
	"$(INTDIR)\daread_I.obj" \
	"$(INTDIR)\dasum_I.obj" \
	"$(INTDIR)\datin_I.obj" \
	"$(INTDIR)\dawrit_I.obj" \
	"$(INTDIR)\dawrt1_I.obj" \
	"$(INTDIR)\daxpy_I.obj" \
	"$(INTDIR)\dcart_I.obj" \
	"$(INTDIR)\dcopy_I.obj" \
	"$(INTDIR)\ddot_I.obj" \
	"$(INTDIR)\ddpo_I.obj" \
	"$(INTDIR)\delmol_I.obj" \
	"$(INTDIR)\delri_I.obj" \
	"$(INTDIR)\delta_I.obj" \
	"$(INTDIR)\denrot_I.obj" \
	"$(INTDIR)\densf_I.obj" \
	"$(INTDIR)\densit_I.obj" \
	"$(INTDIR)\deri0_I.obj" \
	"$(INTDIR)\deri1_I.obj" \
	"$(INTDIR)\deri21_I.obj" \
	"$(INTDIR)\deri22_I.obj" \
	"$(INTDIR)\deri23_I.obj" \
	"$(INTDIR)\deri2_I.obj" \
	"$(INTDIR)\deritr_I.obj" \
	"$(INTDIR)\deriv_I.obj" \
	"$(INTDIR)\dernvo_I.obj" \
	"$(INTDIR)\ders_I.obj" \
	"$(INTDIR)\dex2_I.obj" \
	"$(INTDIR)\dfield_I.obj" \
	"$(INTDIR)\dfock2_I.obj" \
	"$(INTDIR)\dfpsav_I.obj" \
	"$(INTDIR)\dgefa_I.obj" \
	"$(INTDIR)\dgemv_I.obj" \
	"$(INTDIR)\dgetri_I.obj" \
	"$(INTDIR)\dhc_I.obj" \
	"$(INTDIR)\dhcore_I.obj" \
	"$(INTDIR)\diag_I.obj" \
	"$(INTDIR)\diagi_I.obj" \
	"$(INTDIR)\diat2_I.obj" \
	"$(INTDIR)\diat_I.obj" \
	"$(INTDIR)\diegrd_I.obj" \
	"$(INTDIR)\dielen_I.obj" \
	"$(INTDIR)\digit_I.obj" \
	"$(INTDIR)\dihed_I.obj" \
	"$(INTDIR)\dijkl1_I.obj" \
	"$(INTDIR)\dijkl2_I.obj" \
	"$(INTDIR)\dijkld_I.obj" \
	"$(INTDIR)\dimens_I.obj" \
	"$(INTDIR)\dipole_I.obj" \
	"$(INTDIR)\dist2_I.obj" \
	"$(INTDIR)\dmecip_I.obj" \
	"$(INTDIR)\dnrm2_I.obj" \
	"$(INTDIR)\dofs_I.obj" \
	"$(INTDIR)\dopen_I.obj" \
	"$(INTDIR)\dot_I.obj" \
	"$(INTDIR)\drc_I.obj" \
	"$(INTDIR)\drcout_I.obj" \
	"$(INTDIR)\drepp2_I.obj" \
	"$(INTDIR)\drotat_I.obj" \
	"$(INTDIR)\dsum_I.obj" \
	"$(INTDIR)\dtran2_I.obj" \
	"$(INTDIR)\dtrans_I.obj" \
	"$(INTDIR)\dtrmm_I.obj" \
	"$(INTDIR)\dtrmv_I.obj" \
	"$(INTDIR)\dtrti2_I.obj" \
	"$(INTDIR)\ef_I.obj" \
	"$(INTDIR)\einvit_I.obj" \
	"$(INTDIR)\eiscor_I.obj" \
	"$(INTDIR)\elau_I.obj" \
	"$(INTDIR)\elenuc_I.obj" \
	"$(INTDIR)\elesn_I.obj" \
	"$(INTDIR)\en_I.obj" \
	"$(INTDIR)\enpart_I.obj" \
	"$(INTDIR)\epsab_I.obj" \
	"$(INTDIR)\epseta_I.obj" \
	"$(INTDIR)\epslon_I.obj" \
	"$(INTDIR)\eqlrat_I.obj" \
	"$(INTDIR)\esn_I.obj" \
	"$(INTDIR)\esp1_I.obj" \
	"$(INTDIR)\espfit_I.obj" \
	"$(INTDIR)\estpi1_I.obj" \
	"$(INTDIR)\etime_I.obj" \
	"$(INTDIR)\etrbk3_I.obj" \
	"$(INTDIR)\etred3_I.obj" \
	"$(INTDIR)\evvrsp_I.obj" \
	"$(INTDIR)\exchng_I.obj" \
	"$(INTDIR)\fbx_I.obj" \
	"$(INTDIR)\fcnpp_I.obj" \
	"$(INTDIR)\ffreq1_I.obj" \
	"$(INTDIR)\ffreq2_I.obj" \
	"$(INTDIR)\fhpatn_I.obj" \
	"$(INTDIR)\flepo_I.obj" \
	"$(INTDIR)\fock1_I.obj" \
	"$(INTDIR)\fock2_I.obj" \
	"$(INTDIR)\fockd2_I.obj" \
	"$(INTDIR)\force_I.obj" \
	"$(INTDIR)\fordd_I.obj" \
	"$(INTDIR)\formxy_I.obj" \
	"$(INTDIR)\frame_I.obj" \
	"$(INTDIR)\freda_I.obj" \
	"$(INTDIR)\fsub_I.obj" \
	"$(INTDIR)\genun_I.obj" \
	"$(INTDIR)\genvec_I.obj" \
	"$(INTDIR)\geout_I.obj" \
	"$(INTDIR)\geoutg_I.obj" \
	"$(INTDIR)\getgeg_I.obj" \
	"$(INTDIR)\getgeo_I.obj" \
	"$(INTDIR)\getsym_I.obj" \
	"$(INTDIR)\gettxt_I.obj" \
	"$(INTDIR)\getval_I.obj" \
	"$(INTDIR)\gmetry_I.obj" \
	"$(INTDIR)\gover_I.obj" \
	"$(INTDIR)\grid_I.obj" \
	"$(INTDIR)\grids_I.obj" \
	"$(INTDIR)\gstore_I.obj" \
	"$(INTDIR)\h1elec_I.obj" \
	"$(INTDIR)\haddon_I.obj" \
	"$(INTDIR)\hcore_I.obj" \
	"$(INTDIR)\hcored_I.obj" \
	"$(INTDIR)\helect_I.obj" \
	"$(INTDIR)\hmuf_I.obj" \
	"$(INTDIR)\hplusf_I.obj" \
	"$(INTDIR)\inid_I.obj" \
	"$(INTDIR)\inighd_I.obj" \
	"$(INTDIR)\initsn_I.obj" \
	"$(INTDIR)\initsv_I.obj" \
	"$(INTDIR)\insymc_I.obj" \
	"$(INTDIR)\interp_I.obj" \
	"$(INTDIR)\intfc_I.obj" \
	"$(INTDIR)\ird_I.obj" \
	"$(INTDIR)\iten_I.obj" \
	"$(INTDIR)\iter_I.obj" \
	"$(INTDIR)\jab_I.obj" \
	"$(INTDIR)\jcarin_I.obj" \
	"$(INTDIR)\kab_I.obj" \
	"$(INTDIR)\linmin_I.obj" \
	"$(INTDIR)\locmin_I.obj" \
	"$(INTDIR)\lsame_I.obj" \
	"$(INTDIR)\makeuf_I.obj" \
	"$(INTDIR)\makopr_I.obj" \
	"$(INTDIR)\maksym_I.obj" \
	"$(INTDIR)\mamult_I.obj" \
	"$(INTDIR)\mat33_I.obj" \
	"$(INTDIR)\matou1_I.obj" \
	"$(INTDIR)\matout_I.obj" \
	"$(INTDIR)\me08a_I.obj" \
	"$(INTDIR)\meci_I.obj" \
	"$(INTDIR)\mecid_I.obj" \
	"$(INTDIR)\mecih_I.obj" \
	"$(INTDIR)\mecip_I.obj" \
	"$(INTDIR)\mepchg_I.obj" \
	"$(INTDIR)\mepmap_I.obj" \
	"$(INTDIR)\meprot_I.obj" \
	"$(INTDIR)\minv_I.obj" \
	"$(INTDIR)\molsym_I.obj" \
	"$(INTDIR)\molval_I.obj" \
	"$(INTDIR)\mopend_I.obj" \
	"$(INTDIR)\mpcbds_I.obj" \
	"$(INTDIR)\mpcpop_I.obj" \
	"$(INTDIR)\mtxm_I.obj" \
	"$(INTDIR)\mtxmc_I.obj" \
	"$(INTDIR)\mullik_I.obj" \
	"$(INTDIR)\mult33_I.obj" \
	"$(INTDIR)\mult_I.obj" \
	"$(INTDIR)\mxm_I.obj" \
	"$(INTDIR)\mxmt_I.obj" \
	"$(INTDIR)\myword_I.obj" \
	"$(INTDIR)\naican_I.obj" \
	"$(INTDIR)\naicas_I.obj" \
	"$(INTDIR)\ngamtg_I.obj" \
	"$(INTDIR)\ngefis_I.obj" \
	"$(INTDIR)\ngidri_I.obj" \
	"$(INTDIR)\ngoke_I.obj" \
	"$(INTDIR)\nllsn_I.obj" \
	"$(INTDIR)\nllsq_I.obj" \
	"$(INTDIR)\nonbet_I.obj" \
	"$(INTDIR)\nonope_I.obj" \
	"$(INTDIR)\nonor_I.obj" \
	"$(INTDIR)\nuchar_I.obj" \
	"$(INTDIR)\openda_I.obj" \
	"$(INTDIR)\orient_I.obj" \
	"$(INTDIR)\osinv_I.obj" \
	"$(INTDIR)\ovlp_I.obj" \
	"$(INTDIR)\packp_I.obj" \
	"$(INTDIR)\parsav_I.obj" \
	"$(INTDIR)\partxy_I.obj" \
	"$(INTDIR)\pathk_I.obj" \
	"$(INTDIR)\paths_I.obj" \
	"$(INTDIR)\pdgrid_I.obj" \
	"$(INTDIR)\perm_I.obj" \
	"$(INTDIR)\plato_I.obj" \
	"$(INTDIR)\pmep1_I.obj" \
	"$(INTDIR)\pmep_I.obj" \
	"$(INTDIR)\pmepco_I.obj" \
	"$(INTDIR)\poij_I.obj" \
	"$(INTDIR)\pol_vol_I.obj" \
	"$(INTDIR)\polar_I.obj" \
	"$(INTDIR)\potcal_I.obj" \
	"$(INTDIR)\powsav_I.obj" \
	"$(INTDIR)\powsq_I.obj" \
	"$(INTDIR)\printp_I.obj" \
	"$(INTDIR)\prtdrc_I.obj" \
	"$(INTDIR)\prthco_I.obj" \
	"$(INTDIR)\prthes_I.obj" \
	"$(INTDIR)\prtpar_I.obj" \
	"$(INTDIR)\prttim_I.obj" \
	"$(INTDIR)\pulay_I.obj" \
	"$(INTDIR)\quadr_I.obj" \
	"$(INTDIR)\react1_I.obj" \
	"$(INTDIR)\reada_I.obj" \
	"$(INTDIR)\readmo_I.obj" \
	"$(INTDIR)\redatm_I.obj" \
	"$(INTDIR)\refer_I.obj" \
	"$(INTDIR)\reppd2_I.obj" \
	"$(INTDIR)\reppd_I.obj" \
	"$(INTDIR)\resolv_I.obj" \
	"$(INTDIR)\rijkl_I.obj" \
	"$(INTDIR)\rotat_I.obj" \
	"$(INTDIR)\rotatd_I.obj" \
	"$(INTDIR)\rotate_I.obj" \
	"$(INTDIR)\rotmat_I.obj" \
	"$(INTDIR)\rotmol_I.obj" \
	"$(INTDIR)\rsc_I.obj" \
	"$(INTDIR)\rsp_I.obj" \
	"$(INTDIR)\schmib_I.obj" \
	"$(INTDIR)\schmit_I.obj" \
	"$(INTDIR)\scprm_I.obj" \
	"$(INTDIR)\search_I.obj" \
	"$(INTDIR)\second_I.obj" \
	"$(INTDIR)\set_I.obj" \
	"$(INTDIR)\setup3_I.obj" \
	"$(INTDIR)\solrot_I.obj" \
	"$(INTDIR)\sort_I.obj" \
	"$(INTDIR)\spcore_I.obj" \
	"$(INTDIR)\spline_I.obj" \
	"$(INTDIR)\ss_I.obj" \
	"$(INTDIR)\suma2_I.obj" \
	"$(INTDIR)\supdot_I.obj" \
	"$(INTDIR)\superd_I.obj" \
	"$(INTDIR)\surfa_I.obj" \
	"$(INTDIR)\surfac_I.obj" \
	"$(INTDIR)\swap_I.obj" \
	"$(INTDIR)\symdec_I.obj" \
	"$(INTDIR)\symh_I.obj" \
	"$(INTDIR)\symoir_I.obj" \
	"$(INTDIR)\symopr_I.obj" \
	"$(INTDIR)\symp_I.obj" \
	"$(INTDIR)\sympop_I.obj" \
	"$(INTDIR)\symr_I.obj" \
	"$(INTDIR)\symt_I.obj" \
	"$(INTDIR)\symtry_I.obj" \
	"$(INTDIR)\symtrz_I.obj" \
	"$(INTDIR)\tf_I.obj" \
	"$(INTDIR)\thermo_I.obj" \
	"$(INTDIR)\time_I.obj" \
	"$(INTDIR)\timer_I.obj" \
	"$(INTDIR)\timout_I.obj" \
	"$(INTDIR)\transf_I.obj" \
	"$(INTDIR)\trsub_I.obj" \
	"$(INTDIR)\trudgu_I.obj" \
	"$(INTDIR)\trugdu_I.obj" \
	"$(INTDIR)\trugud_I.obj" \
	"$(INTDIR)\tx_I.obj" \
	"$(INTDIR)\upcase_I.obj" \
	"$(INTDIR)\update_I.obj" \
	"$(INTDIR)\vastkind.obj" \
	"$(INTDIR)\vecprt_I.obj" \
	"$(INTDIR)\volume_I.obj" \
	"$(INTDIR)\w2mat_I.obj" \
	"$(INTDIR)\worder_I.obj" \
	"$(INTDIR)\wrdkey_I.obj" \
	"$(INTDIR)\write_trajectory_I.obj" \
	"$(INTDIR)\writmo_I.obj" \
	"$(INTDIR)\wrtkey_I.obj" \
	"$(INTDIR)\wrttxt_I.obj" \
	"$(INTDIR)\wstore_I.obj" \
	"$(INTDIR)\wwstep_I.obj" \
	"$(INTDIR)\xerbla_I.obj" \
	"$(INTDIR)\xxx_I.obj" \
	"$(INTDIR)\xyzcry_I.obj" \
	"$(INTDIR)\xyzint_I.obj" \
	"$(INTDIR)\zerom_I.obj" \
	"..\Modules\Debug\Modules.lib"

"$(OUTDIR)\Interfaces.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("Interfaces.dep")
!INCLUDE "Interfaces.dep"
!ELSE 
!MESSAGE Warning: cannot find "Interfaces.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "Interfaces - Win32 Release" || "$(CFG)" == "Interfaces - Win32 Debug"
SOURCE=..\src_interfaces\aababc_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"aababc_I"


"$(INTDIR)\aababc_I.obj"	"$(INTDIR)\aababc_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\aababc_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\aabacd_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"aabacd_I"


"$(INTDIR)\aabacd_I.obj"	"$(INTDIR)\aabacd_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\aabacd_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\aabbcd_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"aabbcd_I"


"$(INTDIR)\aabbcd_I.obj"	"$(INTDIR)\aabbcd_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\aabbcd_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\addfck_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"addfck_I"


"$(INTDIR)\addfck_I.obj"	"$(INTDIR)\addfck_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\addfck_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\addhcr_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"addhcr_I"


"$(INTDIR)\addhcr_I.obj"	"$(INTDIR)\addhcr_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\addhcr_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\addnuc_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"addnuc_I"


"$(INTDIR)\addnuc_I.obj"	"$(INTDIR)\addnuc_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\addnuc_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\aijm_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"aijm_I"


"$(INTDIR)\aijm_I.obj"	"$(INTDIR)\aijm_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\aijm_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\aintgs_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"aintgs_I"


"$(INTDIR)\aintgs_I.obj"	"$(INTDIR)\aintgs_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\aintgs_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\alphaf_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"alphaf_I"


"$(INTDIR)\alphaf_I.obj"	"$(INTDIR)\alphaf_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\alphaf_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\asum_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"asum_I"


"$(INTDIR)\asum_I.obj"	"$(INTDIR)\asum_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\asum_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\aval_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"aval_I"


"$(INTDIR)\aval_I.obj"	"$(INTDIR)\aval_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\aval_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\axis_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"axis_I"


"$(INTDIR)\axis_I.obj"	"$(INTDIR)\axis_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\axis_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\babbbc_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"babbbc_I"


"$(INTDIR)\babbbc_I.obj"	"$(INTDIR)\babbbc_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\babbbc_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\babbcd_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"babbcd_I"


"$(INTDIR)\babbcd_I.obj"	"$(INTDIR)\babbcd_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\babbcd_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\bangle_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"bangle_I"


"$(INTDIR)\bangle_I.obj"	"$(INTDIR)\bangle_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\bangle_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\bdenin_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"bdenin_I"


"$(INTDIR)\bdenin_I.obj"	"$(INTDIR)\bdenin_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\bdenin_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\bdenup_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"bdenup_I"


"$(INTDIR)\bdenup_I.obj"	"$(INTDIR)\bdenup_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\bdenup_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\beopor_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"beopor_I"


"$(INTDIR)\beopor_I.obj"	"$(INTDIR)\beopor_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\beopor_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\betaf_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"betaf_I"


"$(INTDIR)\betaf_I.obj"	"$(INTDIR)\betaf_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\betaf_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\betal1_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"betal1_I"


"$(INTDIR)\betal1_I.obj"	"$(INTDIR)\betal1_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\betal1_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\betall_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"betall_I"


"$(INTDIR)\betall_I.obj"	"$(INTDIR)\betall_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\betall_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\betcom_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"betcom_I"


"$(INTDIR)\betcom_I.obj"	"$(INTDIR)\betcom_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\betcom_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\bfn_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"bfn_I"


"$(INTDIR)\bfn_I.obj"	"$(INTDIR)\bfn_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\bfn_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\bintgs_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"bintgs_I"


"$(INTDIR)\bintgs_I.obj"	"$(INTDIR)\bintgs_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\bintgs_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\blas_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dgedi_I" \
	"dgemm_I" \
	"dger_I" \
	"dgetf2_I" \
	"dgetrs_I" \
	"dlaswp_I" \
	"dscal_I" \
	"dswap_I" \
	"dtrsm_I" \
	"idamax_I" \
	"ieeeck_I"


"$(INTDIR)\blas_I.obj"	"$(INTDIR)\dgedi_I.mod"	"$(INTDIR)\dgemm_I.mod"	"$(INTDIR)\dger_I.mod"	"$(INTDIR)\dgetf2_I.mod"	"$(INTDIR)\dgetrs_I.mod"	"$(INTDIR)\dlaswp_I.mod"	"$(INTDIR)\dscal_I.mod"	"$(INTDIR)\dswap_I.mod"	"$(INTDIR)\dtrsm_I.mod"	"$(INTDIR)\idamax_I.mod"	"$(INTDIR)\ieeeck_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\blas_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\bldsym_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"bldsym_I"


"$(INTDIR)\bldsym_I.obj"	"$(INTDIR)\bldsym_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\bldsym_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\bmakuf_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"bmakuf_I"


"$(INTDIR)\bmakuf_I.obj"	"$(INTDIR)\bmakuf_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\bmakuf_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\bonds_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"bonds_I"


"$(INTDIR)\bonds_I.obj"	"$(INTDIR)\bonds_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\bonds_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\break_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"break_I"


"$(INTDIR)\break_I.obj"	"$(INTDIR)\break_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\break_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\btoc_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"btoc_I"


"$(INTDIR)\btoc_I.obj"	"$(INTDIR)\btoc_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\btoc_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\calpar_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"calpar_I"


"$(INTDIR)\calpar_I.obj"	"$(INTDIR)\calpar_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\calpar_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\capcor_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"capcor_I"


"$(INTDIR)\capcor_I.obj"	"$(INTDIR)\capcor_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\capcor_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\cartab_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"cartab_I"


"$(INTDIR)\cartab_I.obj"	"$(INTDIR)\cartab_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\cartab_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ccprod_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ccprod_I"


"$(INTDIR)\ccprod_I.obj"	"$(INTDIR)\ccprod_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ccprod_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ccrep_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ccrep_I"


"$(INTDIR)\ccrep_I.obj"	"$(INTDIR)\ccrep_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ccrep_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\cdiag_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"cdiag_I"


"$(INTDIR)\cdiag_I.obj"	"$(INTDIR)\cdiag_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\cdiag_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\charg_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"charg_I"


"$(INTDIR)\charg_I.obj"	"$(INTDIR)\charg_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\charg_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\charmo_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"charmo_I"


"$(INTDIR)\charmo_I.obj"	"$(INTDIR)\charmo_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\charmo_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\charst_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"charst_I"


"$(INTDIR)\charst_I.obj"	"$(INTDIR)\charst_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\charst_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\charvi_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"charvi_I"


"$(INTDIR)\charvi_I.obj"	"$(INTDIR)\charvi_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\charvi_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\chi_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"chi_I"


"$(INTDIR)\chi_I.obj"	"$(INTDIR)\chi_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\chi_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\chrge_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"chrge_I"


"$(INTDIR)\chrge_I.obj"	"$(INTDIR)\chrge_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\chrge_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ciint_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ciint_I"


"$(INTDIR)\ciint_I.obj"	"$(INTDIR)\ciint_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ciint_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ciosci_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ciosci_I"


"$(INTDIR)\ciosci_I.obj"	"$(INTDIR)\ciosci_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ciosci_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\cnvg_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"cnvg_I"


"$(INTDIR)\cnvg_I.obj"	"$(INTDIR)\cnvg_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\cnvg_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\coe_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"coe_I"


"$(INTDIR)\coe_I.obj"	"$(INTDIR)\coe_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\coe_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\collid_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"collid_I"


"$(INTDIR)\collid_I.obj"	"$(INTDIR)\collid_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\collid_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\collis_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"collis_I"


"$(INTDIR)\collis_I.obj"	"$(INTDIR)\collis_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\collis_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\compfg_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"compfg_I"


"$(INTDIR)\compfg_I.obj"	"$(INTDIR)\compfg_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\compfg_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\compfn_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"compfn_I"


"$(INTDIR)\compfn_I.obj"	"$(INTDIR)\compfn_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\compfn_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\consts_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"consts_I"


"$(INTDIR)\consts_I.obj"	"$(INTDIR)\consts_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\consts_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\copym_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"copym_I"


"$(INTDIR)\copym_I.obj"	"$(INTDIR)\copym_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\copym_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\cqden_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"cqden_I"


"$(INTDIR)\cqden_I.obj"	"$(INTDIR)\cqden_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\cqden_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\csum_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"csum_I"


"$(INTDIR)\csum_I.obj"	"$(INTDIR)\csum_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\csum_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dang_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dang_I"


"$(INTDIR)\dang_I.obj"	"$(INTDIR)\dang_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dang_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\darea1_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"darea1_I"


"$(INTDIR)\darea1_I.obj"	"$(INTDIR)\darea1_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\darea1_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\daread_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"daread_I"


"$(INTDIR)\daread_I.obj"	"$(INTDIR)\daread_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\daread_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dasum_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dasum_I"


"$(INTDIR)\dasum_I.obj"	"$(INTDIR)\dasum_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dasum_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\datin_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"datin_I"


"$(INTDIR)\datin_I.obj"	"$(INTDIR)\datin_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\datin_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dawrit_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dawrit_I"


"$(INTDIR)\dawrit_I.obj"	"$(INTDIR)\dawrit_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dawrit_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dawrt1_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dawrt1_I"


"$(INTDIR)\dawrt1_I.obj"	"$(INTDIR)\dawrt1_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dawrt1_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\daxpy_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"daxpy_I"


"$(INTDIR)\daxpy_I.obj"	"$(INTDIR)\daxpy_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\daxpy_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dcart_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dcart_I"


"$(INTDIR)\dcart_I.obj"	"$(INTDIR)\dcart_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dcart_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dcopy_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dcopy_I"


"$(INTDIR)\dcopy_I.obj"	"$(INTDIR)\dcopy_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dcopy_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ddot_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ddot_I"


"$(INTDIR)\ddot_I.obj"	"$(INTDIR)\ddot_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ddot_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ddpo_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ddpo_I"


"$(INTDIR)\ddpo_I.obj"	"$(INTDIR)\ddpo_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ddpo_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\delmol_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"delmol_I"


"$(INTDIR)\delmol_I.obj"	"$(INTDIR)\delmol_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\delmol_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\delri_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"delri_I"


"$(INTDIR)\delri_I.obj"	"$(INTDIR)\delri_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\delri_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\delta_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"delta_I"


"$(INTDIR)\delta_I.obj"	"$(INTDIR)\delta_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\delta_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\denrot_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"denrot_I"


"$(INTDIR)\denrot_I.obj"	"$(INTDIR)\denrot_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\denrot_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\densf_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"densf_I"


"$(INTDIR)\densf_I.obj"	"$(INTDIR)\densf_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\densf_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\densit_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"densit_I"


"$(INTDIR)\densit_I.obj"	"$(INTDIR)\densit_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\densit_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\deri0_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"deri0_I"


"$(INTDIR)\deri0_I.obj"	"$(INTDIR)\deri0_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\deri0_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\deri1_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"deri1_I"


"$(INTDIR)\deri1_I.obj"	"$(INTDIR)\deri1_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\deri1_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\deri21_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"deri21_I"


"$(INTDIR)\deri21_I.obj"	"$(INTDIR)\deri21_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\deri21_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\deri22_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"deri22_I"


"$(INTDIR)\deri22_I.obj"	"$(INTDIR)\deri22_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\deri22_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\deri23_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"deri23_I"


"$(INTDIR)\deri23_I.obj"	"$(INTDIR)\deri23_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\deri23_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\deri2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"deri2_I"


"$(INTDIR)\deri2_I.obj"	"$(INTDIR)\deri2_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\deri2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\deritr_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"deritr_I"


"$(INTDIR)\deritr_I.obj"	"$(INTDIR)\deritr_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\deritr_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\deriv_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"deriv_I"


"$(INTDIR)\deriv_I.obj"	"$(INTDIR)\deriv_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\deriv_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dernvo_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dernvo_I"


"$(INTDIR)\dernvo_I.obj"	"$(INTDIR)\dernvo_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dernvo_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ders_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ders_I"


"$(INTDIR)\ders_I.obj"	"$(INTDIR)\ders_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ders_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dex2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dex2_I"


"$(INTDIR)\dex2_I.obj"	"$(INTDIR)\dex2_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dex2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dfield_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dfield_I"


"$(INTDIR)\dfield_I.obj"	"$(INTDIR)\dfield_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dfield_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dfock2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dfock2_I"


"$(INTDIR)\dfock2_I.obj"	"$(INTDIR)\dfock2_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dfock2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dfpsav_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dfpsav_I"


"$(INTDIR)\dfpsav_I.obj"	"$(INTDIR)\dfpsav_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dfpsav_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dgefa_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dgefa_I"


"$(INTDIR)\dgefa_I.obj"	"$(INTDIR)\dgefa_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dgefa_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dgemv_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dgemv_I"


"$(INTDIR)\dgemv_I.obj"	"$(INTDIR)\dgemv_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dgemv_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dgetri_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dgetri_I"


"$(INTDIR)\dgetri_I.obj"	"$(INTDIR)\dgetri_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dgetri_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dhc_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dhc_I"


"$(INTDIR)\dhc_I.obj"	"$(INTDIR)\dhc_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dhc_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dhcore_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dhcore_I"


"$(INTDIR)\dhcore_I.obj"	"$(INTDIR)\dhcore_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dhcore_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\diag_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"diag_I"


"$(INTDIR)\diag_I.obj"	"$(INTDIR)\diag_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\diag_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\diagi_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"diagi_I"


"$(INTDIR)\diagi_I.obj"	"$(INTDIR)\diagi_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\diagi_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\diat2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"diat2_I"


"$(INTDIR)\diat2_I.obj"	"$(INTDIR)\diat2_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\diat2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\diat_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"diat_I"


"$(INTDIR)\diat_I.obj"	"$(INTDIR)\diat_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\diat_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\diegrd_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"diegrd_I"


"$(INTDIR)\diegrd_I.obj"	"$(INTDIR)\diegrd_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\diegrd_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dielen_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dielen_I"


"$(INTDIR)\dielen_I.obj"	"$(INTDIR)\dielen_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dielen_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\digit_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"digit_I"


"$(INTDIR)\digit_I.obj"	"$(INTDIR)\digit_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\digit_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dihed_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dihed_I"


"$(INTDIR)\dihed_I.obj"	"$(INTDIR)\dihed_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dihed_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dijkl1_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dijkl1_I"


"$(INTDIR)\dijkl1_I.obj"	"$(INTDIR)\dijkl1_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dijkl1_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dijkl2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dijkl2_I"


"$(INTDIR)\dijkl2_I.obj"	"$(INTDIR)\dijkl2_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dijkl2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dijkld_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dijkld_I"


"$(INTDIR)\dijkld_I.obj"	"$(INTDIR)\dijkld_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dijkld_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dimens_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dimens_I"


"$(INTDIR)\dimens_I.obj"	"$(INTDIR)\dimens_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dimens_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dipole_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dipole_I"


"$(INTDIR)\dipole_I.obj"	"$(INTDIR)\dipole_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dipole_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dist2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dist2_I"


"$(INTDIR)\dist2_I.obj"	"$(INTDIR)\dist2_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dist2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dmecip_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dmecip_I"


"$(INTDIR)\dmecip_I.obj"	"$(INTDIR)\dmecip_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dmecip_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dnrm2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dnrm2_I"


"$(INTDIR)\dnrm2_I.obj"	"$(INTDIR)\dnrm2_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dnrm2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dofs_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dofs_I"


"$(INTDIR)\dofs_I.obj"	"$(INTDIR)\dofs_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dofs_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dopen_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dopen_I"


"$(INTDIR)\dopen_I.obj"	"$(INTDIR)\dopen_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dopen_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dot_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dot_I"


"$(INTDIR)\dot_I.obj"	"$(INTDIR)\dot_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dot_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\drc_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"drc_I"


"$(INTDIR)\drc_I.obj"	"$(INTDIR)\drc_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\drc_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\drcout_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"drcout_I"


"$(INTDIR)\drcout_I.obj"	"$(INTDIR)\drcout_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\drcout_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\drepp2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"drepp2_I"


"$(INTDIR)\drepp2_I.obj"	"$(INTDIR)\drepp2_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\drepp2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\drotat_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"drotat_I"


"$(INTDIR)\drotat_I.obj"	"$(INTDIR)\drotat_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\drotat_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dsum_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dsum_I"


"$(INTDIR)\dsum_I.obj"	"$(INTDIR)\dsum_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dsum_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dtran2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dtran2_I"


"$(INTDIR)\dtran2_I.obj"	"$(INTDIR)\dtran2_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dtran2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dtrans_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dtrans_I"


"$(INTDIR)\dtrans_I.obj"	"$(INTDIR)\dtrans_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dtrans_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dtrmm_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dtrmm_I"


"$(INTDIR)\dtrmm_I.obj"	"$(INTDIR)\dtrmm_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dtrmm_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dtrmv_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dtrmv_I"


"$(INTDIR)\dtrmv_I.obj"	"$(INTDIR)\dtrmv_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dtrmv_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\dtrti2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"dtrti2_I"


"$(INTDIR)\dtrti2_I.obj"	"$(INTDIR)\dtrti2_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\dtrti2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ef_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ef_I"


"$(INTDIR)\ef_I.obj"	"$(INTDIR)\ef_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ef_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\einvit_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"einvit_I"


"$(INTDIR)\einvit_I.obj"	"$(INTDIR)\einvit_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\einvit_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\eiscor_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"eiscor_I"


"$(INTDIR)\eiscor_I.obj"	"$(INTDIR)\eiscor_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\eiscor_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\elau_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"elau_I"


"$(INTDIR)\elau_I.obj"	"$(INTDIR)\elau_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\elau_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\elenuc_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"elenuc_I"


"$(INTDIR)\elenuc_I.obj"	"$(INTDIR)\elenuc_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\elenuc_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\elesn_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"elesn_I"


"$(INTDIR)\elesn_I.obj"	"$(INTDIR)\elesn_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\elesn_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\en_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"en_I"


"$(INTDIR)\en_I.obj"	"$(INTDIR)\en_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\en_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\enpart_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"enpart_I"


"$(INTDIR)\enpart_I.obj"	"$(INTDIR)\enpart_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\enpart_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\epsab_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"epsab_I"


"$(INTDIR)\epsab_I.obj"	"$(INTDIR)\epsab_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\epsab_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\epseta_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"epseta_I"


"$(INTDIR)\epseta_I.obj"	"$(INTDIR)\epseta_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\epseta_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\epslon_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"epslon_I"


"$(INTDIR)\epslon_I.obj"	"$(INTDIR)\epslon_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\epslon_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\eqlrat_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"eqlrat_I"


"$(INTDIR)\eqlrat_I.obj"	"$(INTDIR)\eqlrat_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\eqlrat_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\esn_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"esn_I"


"$(INTDIR)\esn_I.obj"	"$(INTDIR)\esn_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\esn_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\esp1_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"esp1_I"


"$(INTDIR)\esp1_I.obj"	"$(INTDIR)\esp1_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\esp1_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\espfit_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"espfit_I"


"$(INTDIR)\espfit_I.obj"	"$(INTDIR)\espfit_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\espfit_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\estpi1_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"estpi1_I"


"$(INTDIR)\estpi1_I.obj"	"$(INTDIR)\estpi1_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\estpi1_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\etime_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"etime_I"


"$(INTDIR)\etime_I.obj"	"$(INTDIR)\etime_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\etime_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\etrbk3_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"etrbk3_I"


"$(INTDIR)\etrbk3_I.obj"	"$(INTDIR)\etrbk3_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\etrbk3_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\etred3_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"etred3_I"


"$(INTDIR)\etred3_I.obj"	"$(INTDIR)\etred3_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\etred3_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\evvrsp_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"evvrsp_I"


"$(INTDIR)\evvrsp_I.obj"	"$(INTDIR)\evvrsp_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\evvrsp_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\exchng_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"exchng_I"


"$(INTDIR)\exchng_I.obj"	"$(INTDIR)\exchng_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\exchng_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\fbx_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"fbx_I"


"$(INTDIR)\fbx_I.obj"	"$(INTDIR)\fbx_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\fbx_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\fcnpp_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"fcnpp_I"


"$(INTDIR)\fcnpp_I.obj"	"$(INTDIR)\fcnpp_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\fcnpp_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ffreq1_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ffreq1_I"


"$(INTDIR)\ffreq1_I.obj"	"$(INTDIR)\ffreq1_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ffreq1_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ffreq2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ffreq2_I"


"$(INTDIR)\ffreq2_I.obj"	"$(INTDIR)\ffreq2_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ffreq2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\fhpatn_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"fhpatn_I"


"$(INTDIR)\fhpatn_I.obj"	"$(INTDIR)\fhpatn_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\fhpatn_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\flepo_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"flepo_I"


"$(INTDIR)\flepo_I.obj"	"$(INTDIR)\flepo_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\flepo_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\fock1_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"fock1_I"


"$(INTDIR)\fock1_I.obj"	"$(INTDIR)\fock1_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\fock1_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\fock2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"fock2_I"


"$(INTDIR)\fock2_I.obj"	"$(INTDIR)\fock2_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\fock2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\fockd2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"fockd2_I"


"$(INTDIR)\fockd2_I.obj"	"$(INTDIR)\fockd2_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\fockd2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\force_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"force_I"


"$(INTDIR)\force_I.obj"	"$(INTDIR)\force_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\force_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\fordd_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"fordd_I"


"$(INTDIR)\fordd_I.obj"	"$(INTDIR)\fordd_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\fordd_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\formxy_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"formxy_I"


"$(INTDIR)\formxy_I.obj"	"$(INTDIR)\formxy_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\formxy_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\frame_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"frame_I"


"$(INTDIR)\frame_I.obj"	"$(INTDIR)\frame_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\frame_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\freda_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"freda_I"


"$(INTDIR)\freda_I.obj"	"$(INTDIR)\freda_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\freda_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\fsub_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"fsub_I"


"$(INTDIR)\fsub_I.obj"	"$(INTDIR)\fsub_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\fsub_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\genun_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"genun_I"


"$(INTDIR)\genun_I.obj"	"$(INTDIR)\genun_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\genun_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\genvec_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"genvec_I"


"$(INTDIR)\genvec_I.obj"	"$(INTDIR)\genvec_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\genvec_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\geout_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"geout_I"


"$(INTDIR)\geout_I.obj"	"$(INTDIR)\geout_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\geout_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\geoutg_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"geoutg_I"


"$(INTDIR)\geoutg_I.obj"	"$(INTDIR)\geoutg_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\geoutg_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\getgeg_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"getgeg_I"


"$(INTDIR)\getgeg_I.obj"	"$(INTDIR)\getgeg_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\getgeg_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\getgeo_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"getgeo_I"


"$(INTDIR)\getgeo_I.obj"	"$(INTDIR)\getgeo_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\getgeo_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\getsym_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"getsym_I"


"$(INTDIR)\getsym_I.obj"	"$(INTDIR)\getsym_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\getsym_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\gettxt_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"gettxt_I"


"$(INTDIR)\gettxt_I.obj"	"$(INTDIR)\gettxt_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\gettxt_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\getval_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"getval_I"


"$(INTDIR)\getval_I.obj"	"$(INTDIR)\getval_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\getval_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\gmetry_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"gmetry_I"


"$(INTDIR)\gmetry_I.obj"	"$(INTDIR)\gmetry_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\gmetry_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\gover_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"gover_I"


"$(INTDIR)\gover_I.obj"	"$(INTDIR)\gover_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\gover_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\grid_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"grid_I"


"$(INTDIR)\grid_I.obj"	"$(INTDIR)\grid_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\grid_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\grids_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"grids_I"


"$(INTDIR)\grids_I.obj"	"$(INTDIR)\grids_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\grids_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\gstore_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"gstore_I"


"$(INTDIR)\gstore_I.obj"	"$(INTDIR)\gstore_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\gstore_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\h1elec_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"h1elec_I"


"$(INTDIR)\h1elec_I.obj"	"$(INTDIR)\h1elec_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\h1elec_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\haddon_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"haddon_I"


"$(INTDIR)\haddon_I.obj"	"$(INTDIR)\haddon_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\haddon_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\hcore_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"hcore_I"


"$(INTDIR)\hcore_I.obj"	"$(INTDIR)\hcore_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\hcore_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\hcored_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"hcored_I"


"$(INTDIR)\hcored_I.obj"	"$(INTDIR)\hcored_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\hcored_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\helect_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"helect_I"


"$(INTDIR)\helect_I.obj"	"$(INTDIR)\helect_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\helect_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\hmuf_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"hmuf_I"


"$(INTDIR)\hmuf_I.obj"	"$(INTDIR)\hmuf_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\hmuf_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\hplusf_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"hplusf_I"


"$(INTDIR)\hplusf_I.obj"	"$(INTDIR)\hplusf_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\hplusf_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\inid_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"inid_I"


"$(INTDIR)\inid_I.obj"	"$(INTDIR)\inid_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\inid_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\inighd_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"inighd_I"


"$(INTDIR)\inighd_I.obj"	"$(INTDIR)\inighd_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\inighd_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\initsn_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"initsn_I"


"$(INTDIR)\initsn_I.obj"	"$(INTDIR)\initsn_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\initsn_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\initsv_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"initsv_I"


"$(INTDIR)\initsv_I.obj"	"$(INTDIR)\initsv_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\initsv_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\insymc_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"insymc_I"


"$(INTDIR)\insymc_I.obj"	"$(INTDIR)\insymc_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\insymc_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\interp_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"interp_I"


"$(INTDIR)\interp_I.obj"	"$(INTDIR)\interp_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\interp_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\intfc_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"intfc_I"


"$(INTDIR)\intfc_I.obj"	"$(INTDIR)\intfc_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\intfc_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ird_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ird_I"


"$(INTDIR)\ird_I.obj"	"$(INTDIR)\ird_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ird_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\iten_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"iten_I"


"$(INTDIR)\iten_I.obj"	"$(INTDIR)\iten_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\iten_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\iter_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"iter_I"


"$(INTDIR)\iter_I.obj"	"$(INTDIR)\iter_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\iter_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\jab_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"jab_I"


"$(INTDIR)\jab_I.obj"	"$(INTDIR)\jab_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\jab_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\jcarin_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"jcarin_I"


"$(INTDIR)\jcarin_I.obj"	"$(INTDIR)\jcarin_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\jcarin_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\kab_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"kab_I"


"$(INTDIR)\kab_I.obj"	"$(INTDIR)\kab_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\kab_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\linmin_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"linmin_I"


"$(INTDIR)\linmin_I.obj"	"$(INTDIR)\linmin_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\linmin_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\locmin_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"locmin_I"


"$(INTDIR)\locmin_I.obj"	"$(INTDIR)\locmin_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\locmin_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\lsame_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"lsame_I"


"$(INTDIR)\lsame_I.obj"	"$(INTDIR)\lsame_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\lsame_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\makeuf_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"makeuf_I"


"$(INTDIR)\makeuf_I.obj"	"$(INTDIR)\makeuf_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\makeuf_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\makopr_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"makopr_I"


"$(INTDIR)\makopr_I.obj"	"$(INTDIR)\makopr_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\makopr_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\maksym_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"maksym_I"


"$(INTDIR)\maksym_I.obj"	"$(INTDIR)\maksym_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\maksym_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mamult_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mamult_I"


"$(INTDIR)\mamult_I.obj"	"$(INTDIR)\mamult_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mamult_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mat33_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mat33_I"


"$(INTDIR)\mat33_I.obj"	"$(INTDIR)\mat33_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mat33_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\matou1_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"matou1_I"


"$(INTDIR)\matou1_I.obj"	"$(INTDIR)\matou1_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\matou1_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\matout_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"matout_I"


"$(INTDIR)\matout_I.obj"	"$(INTDIR)\matout_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\matout_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\me08a_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"me08a_I"


"$(INTDIR)\me08a_I.obj"	"$(INTDIR)\me08a_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\me08a_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\meci_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"meci_I"


"$(INTDIR)\meci_I.obj"	"$(INTDIR)\meci_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\meci_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mecid_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mecid_I"


"$(INTDIR)\mecid_I.obj"	"$(INTDIR)\mecid_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mecid_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mecih_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mecih_I"


"$(INTDIR)\mecih_I.obj"	"$(INTDIR)\mecih_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mecih_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mecip_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mecip_I"


"$(INTDIR)\mecip_I.obj"	"$(INTDIR)\mecip_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mecip_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mepchg_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mepchg_I"


"$(INTDIR)\mepchg_I.obj"	"$(INTDIR)\mepchg_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mepchg_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mepmap_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mepmap_I"


"$(INTDIR)\mepmap_I.obj"	"$(INTDIR)\mepmap_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mepmap_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\meprot_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"meprot_I"


"$(INTDIR)\meprot_I.obj"	"$(INTDIR)\meprot_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\meprot_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\minv_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"minv_I"


"$(INTDIR)\minv_I.obj"	"$(INTDIR)\minv_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\minv_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\molsym_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"molsym_I"


"$(INTDIR)\molsym_I.obj"	"$(INTDIR)\molsym_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\molsym_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\molval_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"molval_I"


"$(INTDIR)\molval_I.obj"	"$(INTDIR)\molval_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\molval_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mopend_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mopend_I"


"$(INTDIR)\mopend_I.obj"	"$(INTDIR)\mopend_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mopend_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mpcbds_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mpcbds_I"


"$(INTDIR)\mpcbds_I.obj"	"$(INTDIR)\mpcbds_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mpcbds_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mpcpop_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mpcpop_I"


"$(INTDIR)\mpcpop_I.obj"	"$(INTDIR)\mpcpop_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mpcpop_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mtxm_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mtxm_I"


"$(INTDIR)\mtxm_I.obj"	"$(INTDIR)\mtxm_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mtxm_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mtxmc_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mtxmc_I"


"$(INTDIR)\mtxmc_I.obj"	"$(INTDIR)\mtxmc_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mtxmc_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mullik_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mullik_I"


"$(INTDIR)\mullik_I.obj"	"$(INTDIR)\mullik_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mullik_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mult33_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mult33_I"


"$(INTDIR)\mult33_I.obj"	"$(INTDIR)\mult33_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mult33_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mult_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mult_I"


"$(INTDIR)\mult_I.obj"	"$(INTDIR)\mult_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mult_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mxm_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mxm_I"


"$(INTDIR)\mxm_I.obj"	"$(INTDIR)\mxm_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mxm_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\mxmt_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"mxmt_I"


"$(INTDIR)\mxmt_I.obj"	"$(INTDIR)\mxmt_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\mxmt_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\myword_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"myword_I"


"$(INTDIR)\myword_I.obj"	"$(INTDIR)\myword_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\myword_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\naican_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"naican_I"


"$(INTDIR)\naican_I.obj"	"$(INTDIR)\naican_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\naican_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\naicas_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"naicas_I"


"$(INTDIR)\naicas_I.obj"	"$(INTDIR)\naicas_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\naicas_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ngamtg_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ngamtg_I"


"$(INTDIR)\ngamtg_I.obj"	"$(INTDIR)\ngamtg_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ngamtg_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ngefis_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ngefis_I"


"$(INTDIR)\ngefis_I.obj"	"$(INTDIR)\ngefis_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ngefis_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ngidri_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ngidri_I"


"$(INTDIR)\ngidri_I.obj"	"$(INTDIR)\ngidri_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ngidri_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ngoke_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ngoke_I"


"$(INTDIR)\ngoke_I.obj"	"$(INTDIR)\ngoke_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ngoke_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\nllsn_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"nllsn_I"


"$(INTDIR)\nllsn_I.obj"	"$(INTDIR)\nllsn_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\nllsn_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\nllsq_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"nllsq_I"


"$(INTDIR)\nllsq_I.obj"	"$(INTDIR)\nllsq_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\nllsq_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\nonbet_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"nonbet_I"


"$(INTDIR)\nonbet_I.obj"	"$(INTDIR)\nonbet_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\nonbet_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\nonope_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"nonope_I"


"$(INTDIR)\nonope_I.obj"	"$(INTDIR)\nonope_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\nonope_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\nonor_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"nonor_I"


"$(INTDIR)\nonor_I.obj"	"$(INTDIR)\nonor_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\nonor_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\nuchar_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"nuchar_I"


"$(INTDIR)\nuchar_I.obj"	"$(INTDIR)\nuchar_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\nuchar_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\openda_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"openda_I"


"$(INTDIR)\openda_I.obj"	"$(INTDIR)\openda_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\openda_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\orient_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"orient_I"


"$(INTDIR)\orient_I.obj"	"$(INTDIR)\orient_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\orient_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\osinv_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"osinv_I"


"$(INTDIR)\osinv_I.obj"	"$(INTDIR)\osinv_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\osinv_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ovlp_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ovlp_I"


"$(INTDIR)\ovlp_I.obj"	"$(INTDIR)\ovlp_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ovlp_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\packp_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"packp_I"


"$(INTDIR)\packp_I.obj"	"$(INTDIR)\packp_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\packp_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\parsav_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"parsav_I"


"$(INTDIR)\parsav_I.obj"	"$(INTDIR)\parsav_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\parsav_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\partxy_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"partxy_I"


"$(INTDIR)\partxy_I.obj"	"$(INTDIR)\partxy_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\partxy_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\pathk_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"pathk_I"


"$(INTDIR)\pathk_I.obj"	"$(INTDIR)\pathk_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\pathk_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\paths_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"paths_I"


"$(INTDIR)\paths_I.obj"	"$(INTDIR)\paths_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\paths_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\pdgrid_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"pdgrid_I"


"$(INTDIR)\pdgrid_I.obj"	"$(INTDIR)\pdgrid_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\pdgrid_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\perm_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"perm_I"


"$(INTDIR)\perm_I.obj"	"$(INTDIR)\perm_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\perm_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\plato_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"plato_I"


"$(INTDIR)\plato_I.obj"	"$(INTDIR)\plato_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\plato_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\pmep1_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"pmep1_I"


"$(INTDIR)\pmep1_I.obj"	"$(INTDIR)\pmep1_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\pmep1_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\pmep_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"pmep_I"


"$(INTDIR)\pmep_I.obj"	"$(INTDIR)\pmep_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\pmep_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\pmepco_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"pmepco_I"


"$(INTDIR)\pmepco_I.obj"	"$(INTDIR)\pmepco_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\pmepco_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\poij_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"poij_I"


"$(INTDIR)\poij_I.obj"	"$(INTDIR)\poij_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\poij_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\pol_vol_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"pol_vol_I"


"$(INTDIR)\pol_vol_I.obj"	"$(INTDIR)\pol_vol_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\pol_vol_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\polar_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"polar_I"


"$(INTDIR)\polar_I.obj"	"$(INTDIR)\polar_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\polar_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\potcal_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"potcal_I"


"$(INTDIR)\potcal_I.obj"	"$(INTDIR)\potcal_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\potcal_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\powsav_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"powsav_I"


"$(INTDIR)\powsav_I.obj"	"$(INTDIR)\powsav_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\powsav_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\powsq_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"powsq_I"


"$(INTDIR)\powsq_I.obj"	"$(INTDIR)\powsq_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\powsq_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\printp_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"printp_I"


"$(INTDIR)\printp_I.obj"	"$(INTDIR)\printp_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\printp_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\prtdrc_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"prtdrc_I"


"$(INTDIR)\prtdrc_I.obj"	"$(INTDIR)\prtdrc_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\prtdrc_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\prthco_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"prthco_I"


"$(INTDIR)\prthco_I.obj"	"$(INTDIR)\prthco_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\prthco_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\prthes_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"prthes_I"


"$(INTDIR)\prthes_I.obj"	"$(INTDIR)\prthes_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\prthes_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\prtpar_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"prtpar_I"


"$(INTDIR)\prtpar_I.obj"	"$(INTDIR)\prtpar_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\prtpar_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\prttim_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"prttim_I"


"$(INTDIR)\prttim_I.obj"	"$(INTDIR)\prttim_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\prttim_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\pulay_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"pulay_I"


"$(INTDIR)\pulay_I.obj"	"$(INTDIR)\pulay_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\pulay_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\quadr_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"quadr_I"


"$(INTDIR)\quadr_I.obj"	"$(INTDIR)\quadr_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\quadr_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\react1_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"react1_I"


"$(INTDIR)\react1_I.obj"	"$(INTDIR)\react1_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\react1_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\reada_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"reada_I"


"$(INTDIR)\reada_I.obj"	"$(INTDIR)\reada_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\reada_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\readmo_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"readmo_I"


"$(INTDIR)\readmo_I.obj"	"$(INTDIR)\readmo_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\readmo_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\redatm_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"redatm_I"


"$(INTDIR)\redatm_I.obj"	"$(INTDIR)\redatm_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\redatm_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\refer_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"refer_I"


"$(INTDIR)\refer_I.obj"	"$(INTDIR)\refer_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\refer_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\reppd2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"reppd2_I"


"$(INTDIR)\reppd2_I.obj"	"$(INTDIR)\reppd2_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\reppd2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\reppd_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"reppd_I"


"$(INTDIR)\reppd_I.obj"	"$(INTDIR)\reppd_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\reppd_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\resolv_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"resolv_I"


"$(INTDIR)\resolv_I.obj"	"$(INTDIR)\resolv_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\resolv_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\rijkl_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"rijkl_I"


"$(INTDIR)\rijkl_I.obj"	"$(INTDIR)\rijkl_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\rijkl_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\rotat_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"rotat_I"


"$(INTDIR)\rotat_I.obj"	"$(INTDIR)\rotat_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\rotat_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\rotatd_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"rotatd_I"


"$(INTDIR)\rotatd_I.obj"	"$(INTDIR)\rotatd_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\rotatd_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\rotate_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"rotate_I"


"$(INTDIR)\rotate_I.obj"	"$(INTDIR)\rotate_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\rotate_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\rotmat_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"rotmat_I"


"$(INTDIR)\rotmat_I.obj"	"$(INTDIR)\rotmat_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\rotmat_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\rotmol_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"rotmol_I"


"$(INTDIR)\rotmol_I.obj"	"$(INTDIR)\rotmol_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\rotmol_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\rsc_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"rsc_I"


"$(INTDIR)\rsc_I.obj"	"$(INTDIR)\rsc_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\rsc_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\rsp_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"rsp_I"


"$(INTDIR)\rsp_I.obj"	"$(INTDIR)\rsp_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\rsp_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\schmib_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"schmib_I"


"$(INTDIR)\schmib_I.obj"	"$(INTDIR)\schmib_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\schmib_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\schmit_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"schmit_I"


"$(INTDIR)\schmit_I.obj"	"$(INTDIR)\schmit_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\schmit_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\scprm_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"scprm_I"


"$(INTDIR)\scprm_I.obj"	"$(INTDIR)\scprm_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\scprm_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\search_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"search_I"


"$(INTDIR)\search_I.obj"	"$(INTDIR)\search_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\search_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\second_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"second_I"


"$(INTDIR)\second_I.obj"	"$(INTDIR)\second_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\second_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\set_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"set_I"


"$(INTDIR)\set_I.obj"	"$(INTDIR)\set_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\set_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\setup3_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"setup3_I"


"$(INTDIR)\setup3_I.obj"	"$(INTDIR)\setup3_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\setup3_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\solrot_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"solrot_I"


"$(INTDIR)\solrot_I.obj"	"$(INTDIR)\solrot_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\solrot_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\sort_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"sort_I"


"$(INTDIR)\sort_I.obj"	"$(INTDIR)\sort_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\sort_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\spcore_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"spcore_I"


"$(INTDIR)\spcore_I.obj"	"$(INTDIR)\spcore_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\spcore_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\spline_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"spline_I"


"$(INTDIR)\spline_I.obj"	"$(INTDIR)\spline_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\spline_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\ss_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"ss_I"


"$(INTDIR)\ss_I.obj"	"$(INTDIR)\ss_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\ss_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\suma2_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"suma2_I"


"$(INTDIR)\suma2_I.obj"	"$(INTDIR)\suma2_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\suma2_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\supdot_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"supdot_I"


"$(INTDIR)\supdot_I.obj"	"$(INTDIR)\supdot_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\supdot_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\superd_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"superd_I"


"$(INTDIR)\superd_I.obj"	"$(INTDIR)\superd_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\superd_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\surfa_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"surfa_I"


"$(INTDIR)\surfa_I.obj"	"$(INTDIR)\surfa_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\surfa_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\surfac_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"surfac_I"


"$(INTDIR)\surfac_I.obj"	"$(INTDIR)\surfac_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\surfac_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\swap_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"swap_I"


"$(INTDIR)\swap_I.obj"	"$(INTDIR)\swap_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\swap_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\symdec_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"symdec_I"


"$(INTDIR)\symdec_I.obj"	"$(INTDIR)\symdec_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\symdec_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\symh_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"symh_I"


"$(INTDIR)\symh_I.obj"	"$(INTDIR)\symh_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\symh_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\symoir_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"symoir_I"


"$(INTDIR)\symoir_I.obj"	"$(INTDIR)\symoir_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\symoir_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\symopr_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"symopr_I"


"$(INTDIR)\symopr_I.obj"	"$(INTDIR)\symopr_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\symopr_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\symp_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"symp_I"


"$(INTDIR)\symp_I.obj"	"$(INTDIR)\symp_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\symp_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\sympop_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"sympop_I"


"$(INTDIR)\sympop_I.obj"	"$(INTDIR)\sympop_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\sympop_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\symr_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"symr_I"


"$(INTDIR)\symr_I.obj"	"$(INTDIR)\symr_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\symr_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\symt_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"symt_I"


"$(INTDIR)\symt_I.obj"	"$(INTDIR)\symt_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\symt_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\symtry_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"symtry_I"


"$(INTDIR)\symtry_I.obj"	"$(INTDIR)\symtry_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\symtry_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\symtrz_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"symtrz_I"


"$(INTDIR)\symtrz_I.obj"	"$(INTDIR)\symtrz_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\symtrz_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\tf_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"tf_I"


"$(INTDIR)\tf_I.obj"	"$(INTDIR)\tf_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\tf_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\thermo_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"thermo_I"


"$(INTDIR)\thermo_I.obj"	"$(INTDIR)\thermo_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\thermo_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\time_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"time_I"


"$(INTDIR)\time_I.obj"	"$(INTDIR)\time_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\time_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\timer_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"timer_I"


"$(INTDIR)\timer_I.obj"	"$(INTDIR)\timer_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\timer_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\timout_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"timout_I"


"$(INTDIR)\timout_I.obj"	"$(INTDIR)\timout_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\timout_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\transf_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"transf_I"


"$(INTDIR)\transf_I.obj"	"$(INTDIR)\transf_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\transf_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\trsub_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"trsub_I"


"$(INTDIR)\trsub_I.obj"	"$(INTDIR)\trsub_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\trsub_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\trudgu_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"trudgu_I"


"$(INTDIR)\trudgu_I.obj"	"$(INTDIR)\trudgu_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\trudgu_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\trugdu_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"trugdu_I"


"$(INTDIR)\trugdu_I.obj"	"$(INTDIR)\trugdu_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\trugdu_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\trugud_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"trugud_I"


"$(INTDIR)\trugud_I.obj"	"$(INTDIR)\trugud_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\trugud_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\tx_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"tx_I"


"$(INTDIR)\tx_I.obj"	"$(INTDIR)\tx_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\tx_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\upcase_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"upcase_I"


"$(INTDIR)\upcase_I.obj"	"$(INTDIR)\upcase_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\upcase_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\update_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"update_I"


"$(INTDIR)\update_I.obj"	"$(INTDIR)\update_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\update_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\vastkind.F90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"vast_kind_param"


"$(INTDIR)\vastkind.obj"	"$(INTDIR)\vast_kind_param.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\vastkind.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\vecprt_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"vecprt_I"


"$(INTDIR)\vecprt_I.obj"	"$(INTDIR)\vecprt_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\vecprt_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\volume_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"volume_I"


"$(INTDIR)\volume_I.obj"	"$(INTDIR)\volume_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\volume_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\w2mat_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"w2mat_I"


"$(INTDIR)\w2mat_I.obj"	"$(INTDIR)\w2mat_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\w2mat_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\worder_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"worder_I"


"$(INTDIR)\worder_I.obj"	"$(INTDIR)\worder_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\worder_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\wrdkey_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"wrdkey_I"


"$(INTDIR)\wrdkey_I.obj"	"$(INTDIR)\wrdkey_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\wrdkey_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\write_trajectory_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"write_trajectory_I"


"$(INTDIR)\write_trajectory_I.obj"	"$(INTDIR)\write_trajectory_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\write_trajectory_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\writmo_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"writmo_I"


"$(INTDIR)\writmo_I.obj"	"$(INTDIR)\writmo_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\writmo_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\wrtkey_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"wrtkey_I"


"$(INTDIR)\wrtkey_I.obj"	"$(INTDIR)\wrtkey_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\wrtkey_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\wrttxt_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"wrttxt_I"


"$(INTDIR)\wrttxt_I.obj"	"$(INTDIR)\wrttxt_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\wrttxt_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\wstore_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"wstore_I"


"$(INTDIR)\wstore_I.obj"	"$(INTDIR)\wstore_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\wstore_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\wwstep_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"wwstep_I"


"$(INTDIR)\wwstep_I.obj"	"$(INTDIR)\wwstep_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\wwstep_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\xerbla_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"xerbla_I"


"$(INTDIR)\xerbla_I.obj"	"$(INTDIR)\xerbla_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\xerbla_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\xxx_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"xxx_I"


"$(INTDIR)\xxx_I.obj"	"$(INTDIR)\xxx_I.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\xxx_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\xyzcry_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"xyzcry_I"


"$(INTDIR)\xyzcry_I.obj"	"$(INTDIR)\xyzcry_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\xyzcry_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\xyzint_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"xyzint_I"


"$(INTDIR)\xyzint_I.obj"	"$(INTDIR)\xyzint_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\xyzint_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\src_interfaces\zerom_I.f90

!IF  "$(CFG)" == "Interfaces - Win32 Release"

F90_MODOUT=\
	"zerom_I"


"$(INTDIR)\zerom_I.obj"	"$(INTDIR)\zerom_I.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\vast_kind_param.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"


"$(INTDIR)\zerom_I.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

!IF  "$(CFG)" == "Interfaces - Win32 Release"

"Modules - Win32 Release" : 
   cd "\Software\mopac2009\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Release" 
   cd "..\Interfaces"

"Modules - Win32 ReleaseCLEAN" : 
   cd "\Software\mopac2009\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Release" RECURSE=1 CLEAN 
   cd "..\Interfaces"

!ELSEIF  "$(CFG)" == "Interfaces - Win32 Debug"

"Modules - Win32 Debug" : 
   cd "\Software\mopac2009\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Debug" 
   cd "..\Interfaces"

"Modules - Win32 DebugCLEAN" : 
   cd "\Software\mopac2009\Modules"
   $(MAKE) /$(MAKEFLAGS) /F .\Modules.mak CFG="Modules - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\Interfaces"

!ENDIF 


!ENDIF 

