library(dplyr)
library(stringr)
library(tibble)
library(readr)
library(readxl)

##### GET DATA #####
data_path <- "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/MAIN OUTCOME/02_data/02_data_Prep/AMIS_merged_analysis_dataset.xlsx"
dat_raw <- read_excel(data_path)

##### INSPECT DATA ####
dim(dat_raw)
names(dat_raw)

##### CHECK DUPLICATES
duplicate_ids <- dat_raw |>
  count(sic) |>
  filter(n > 1)

duplicate_ids

##### DEFINE FINAL ANALYSIS SAMPLE ####
dat_analysis <- dat_raw |>
  filter(
    !is.na(sdq_emotion_k_t5),
    !is.na(sdq_emotion_b_t5)
  )

dim(dat_analysis)


##### CHECK DATA COMPLETENESS FOR EACH VAR ####
completeness_table <- tibble(
  variable = names(dat_analysis),
  complete_n = colSums(!is.na(dat_analysis)),
  complete_pct = round(colMeans(!is.na(dat_analysis)) * 100, 1),
  missing_n = colSums(is.na(dat_analysis)),
  missing_pct = round(colMeans(is.na(dat_analysis)) * 100, 1)
) |>
  arrange(complete_pct)

View(completeness_table)

##### RENAME VARIABLES ##########

##### STORE ORIGINAL VARIABLE NAMES ####
original_names <- names(dat_raw)

##### RENAME ALL VARIABLES FOR MPLUS ####
dat_mplus <- dat_raw |>
  rename(
    SIC_N = sic,
    has_sdq = has_sdq_file,
    has_mt = has_maltreatment_file,
    has_cg = has_cortisol_genetics_file,
    has_diag = has_clinical_diag_file,
    diag_vor = klin_DIAG_VORH,
    int_vor = klin_INT_VORH,
    ext_vor = klin_EXT_VORH,
    int_ext = klin_INT_EXT,
    ie_diff = klin_INTEXT_DIFF,
    aff_vor = klin_AFFEKTIV_VORH,
    anx_vor = klin_ANGST_VORH,
    psy_vor = klin_PSYCHO_VORH,
    vh_vor = klin_VH_VORH,
    adhs_vor = klin_ADHS_VORH,
    weit_vor = klin_WEIT_VORH,
    abh_vor = klin_ABHAENG_VORH,
    ess_vor = klin_ESSST_VORH,
    tic_vor = klin_TIC_VORH,
    enur_vor = klin_ENUR_VORH,
    enk_vor = klin_ENK_VORH,
    anp_vor = klin_ANP_VORH,
    ptbs_vor = klin_PTBS_VORH,
    bipo_vor = klin_BIPO_VORH,
    md_vor = klin_MD_VORH,
    dys_vor = klin_DYS_VORH,
    pan_vor = klin_PANIK_VORH,
    gad_vor = klin_GAD_VORH,
    ago_vor = klin_AGO_VORH,
    soz_vor = klin_SOZ_VORH,
    phob_vor = klin_PHOB_VORH,
    zwa_vor = klin_ZWANG_VORH,
    trn_vor = klin_TRENN_VORH,
    cd_vor = klin_CD_VORH,
    odd_vor = klin_ODD_VORH,
    dnos_vor = klin_DNOS_VORH,
    bind_vor = klin_BIND_VORH,
    mut_vor = klin_MUT_VORH,
    sext5 = sdq_sex,
    age_b2 = sdq_age_parent_t2,
    age_k2 = sdq_age_child_t2,
    age_p2 = sdq_age_partner_t2,
    age_t2 = sdq_age_teacher_t2,
    age_b5 = sdq_age_parent_t5,
    age_k5 = sdq_age_child_t5,
    age_p5 = sdq_age_partner_t5,
    age_t5 = sdq_age_teacher_t5,
    emo_b2 = sdq_emotion_b_t2,
    emo_b5 = sdq_emotion_b_t5,
    emo_k2 = sdq_emotion_k_t2,
    emo_k5 = sdq_emotion_k_t5,
    emo_p2 = sdq_emotion_p_t2,
    emo_p5 = sdq_emotion_p_t5,
    emo_t2 = sdq_emotion_t_t2,
    emo_t5 = sdq_emotion_t_t5,
    con_b2 = sdq_conduct_b_t2,
    con_b5 = sdq_conduct_b_t5,
    con_k2 = sdq_conduct_k_t2,
    con_k5 = sdq_conduct_k_t5,
    con_p2 = sdq_conduct_p_t2,
    con_p5 = sdq_conduct_p_t5,
    con_t2 = sdq_conduct_t_t2,
    con_t5 = sdq_conduct_t_t5,
    hyp_b2 = sdq_hyper_b_t2,
    hyp_b5 = sdq_hyper_b_t5,
    hyp_k2 = sdq_hyper_k_t2,
    hyp_k5 = sdq_hyper_k_t5,
    hyp_p2 = sdq_hyper_p_t2,
    hyp_p5 = sdq_hyper_p_t5,
    hyp_t2 = sdq_hyper_t_t2,
    hyp_t5 = sdq_hyper_t_t5,
    pee_b2 = sdq_peer_b_t2,
    pee_b5 = sdq_peer_b_t5,
    pee_k2 = sdq_peer_k_t2,
    pee_k5 = sdq_peer_k_t5,
    pee_p2 = sdq_peer_p_t2,
    pee_p5 = sdq_peer_p_t5,
    pee_t2 = sdq_peer_t_t2,
    pee_t5 = sdq_peer_t_t5,
    pro_b2 = sdq_prosoc_b_t2,
    pro_b5 = sdq_prosoc_b_t5,
    pro_k2 = sdq_prosoc_k_t2,
    pro_k5 = sdq_prosoc_k_t5,
    pro_p2 = sdq_prosoc_p_t2,
    pro_p5 = sdq_prosoc_p_t5,
    pro_t2 = sdq_prosoc_t_t2,
    pro_t5 = sdq_prosoc_t_t5,
    mt_sdq5 = mt_has_sdq_t5,
    mt_covag = mt_coverage_age,
    n6_sa = mt_n_subtypes_6_sa,
    n5_sa = mt_n_subtypes_5_sa,
    nev_sa = mt_n_events_sa,
    sev_sa = mt_severity_sa,
    fq_sa = mt_frequency_sa,
    z6_sa = mt_z_n_subtypes_6_sa,
    z5_sa = mt_z_n_subtypes_5_sa,
    zev_sa = mt_z_n_events_sa,
    zsv_sa = mt_z_severity_sa,
    zfq_sa = mt_z_frequency_sa,
    zind_sa = mt_z_burden_sa,
    n6_kk = mt_n_subtypes_6_kk,
    n5_kk = mt_n_subtypes_5_kk,
    nev_kk = mt_n_events_kk,
    sev_kk = mt_severity_kk,
    fq_kk = mt_frequency_kk,
    z6_kk = mt_z_n_subtypes_6_kk,
    z5_kk = mt_z_n_subtypes_5_kk,
    zev_kk = mt_z_n_events_kk,
    zsv_kk = mt_z_severity_kk,
    zfq_kk = mt_z_frequency_kk,
    zind_kk = mt_z_burden_kk,
    n6_vsa = mt_n_subtypes_6_vsa,
    n5_vsa = mt_n_subtypes_5_vsa,
    nev_vsa = mt_n_events_vsa,
    sev_vsa = mt_severity_vsa,
    fq_vsa = mt_frequency_vsa,
    z6_vsa = mt_z_n_subtypes_6_vsa,
    z5_vsa = mt_z_n_subtypes_5_vsa,
    zev_vsa = mt_z_n_events_vsa,
    zsv_vsa = mt_z_severity_vsa,
    zfq_vsa = mt_z_frequency_vsa,
    zind_vsa = mt_z_burden_vsa,
    n6_fsz = mt_n_subtypes_6_fsz,
    n5_fsz = mt_n_subtypes_5_fsz,
    nev_fsz = mt_n_events_fsz,
    sev_fsz = mt_severity_fsz,
    fq_fsz = mt_frequency_fsz,
    z6_fsz = mt_z_n_subtypes_6_fsz,
    z5_fsz = mt_z_n_subtypes_5_fsz,
    zev_fsz = mt_z_n_events_fsz,
    zsv_fsz = mt_z_severity_fsz,
    zfq_fsz = mt_z_frequency_fsz,
    zind_fsz = mt_z_burden_fsz,
    n6_ssz = mt_n_subtypes_6_ssz,
    n5_ssz = mt_n_subtypes_5_ssz,
    nev_ssz = mt_n_events_ssz,
    sev_ssz = mt_severity_ssz,
    fq_ssz = mt_frequency_ssz,
    z6_ssz = mt_z_n_subtypes_6_ssz,
    z5_ssz = mt_z_n_subtypes_5_ssz,
    zev_ssz = mt_z_n_events_ssz,
    zsv_ssz = mt_z_severity_ssz,
    zfq_ssz = mt_z_frequency_ssz,
    zind_ssz = mt_z_burden_ssz,
    n6_ja = mt_n_subtypes_6_ja,
    n5_ja = mt_n_subtypes_5_ja,
    nev_ja = mt_n_events_ja,
    sev_ja = mt_severity_ja,
    fq_ja = mt_frequency_ja,
    z6_ja = mt_z_n_subtypes_6_ja,
    z5_ja = mt_z_n_subtypes_5_ja,
    zev_ja = mt_z_n_events_ja,
    zsv_ja = mt_z_severity_ja,
    zfq_ja = mt_z_frequency_ja,
    zind_ja = mt_z_burden_ja,
    n6_jea = mt_n_subtypes_6_jea,
    n5_jea = mt_n_subtypes_5_jea,
    nev_jea = mt_n_events_jea,
    sev_jea = mt_severity_jea,
    fq_jea = mt_frequency_jea,
    z6_jea = mt_z_n_subtypes_6_jea,
    z5_jea = mt_z_n_subtypes_5_jea,
    zev_jea = mt_z_n_events_jea,
    zsv_jea = mt_z_severity_jea,
    zfq_jea = mt_z_frequency_jea,
    zind_jea = mt_z_burden_jea,
    mal_t1 = mt_mal_status_t1,
    n6_t1 = mt_n_subtypes_6_t1,
    n5_t1 = mt_n_subtypes_5_t1,
    nev_t1 = mt_n_events_t1,
    sev_t1 = mt_severity_t1,
    fq_t1 = mt_frequency_t1,
    chr_t1 = mt_chronicity_t1,
    nexp_t1 = mt_n_exposed_periods_t1,
    ncov_t1 = mt_n_periods_covered_t1,
    mal_t12 = mt_mal_status_t1t2,
    n6_t12 = mt_n_subtypes_6_t1t2,
    n5_t12 = mt_n_subtypes_5_t1t2,
    nev_t12 = mt_n_events_t1t2,
    sev_t12 = mt_severity_t1t2,
    fq_t12 = mt_frequency_t1t2,
    chr_t12 = mt_chronicity_t1t2,
    nexp_t12 = mt_n_exposed_periods_t1t2,
    ncov_t12 = mt_n_periods_covered_t1t2,
    mal_all = mt_mal_status_t2all,
    n6_all = mt_n_subtypes_6_t2all,
    n5_all = mt_n_subtypes_5_t2all,
    nev_all = mt_n_events_t2all,
    sev_all = mt_severity_t2all,
    fq_all = mt_frequency_t2all,
    chr_all = mt_chronicity_t2all,
    nexp_all = mt_n_exposed_periods_t2all,
    ncov_all = mt_n_periods_covered_t2all,
    c6_t1 = mt_cat_n_subtypes_6_t1,
    c5_t1 = mt_cat_n_subtypes_5_t1,
    cev_t1 = mt_cat_n_events_t1,
    csv_t1 = mt_cat_severity_t1,
    cfq_t1 = mt_cat_frequency_t1,
    cch_t1 = mt_cat_chronicity_t1,
    c6_t12 = mt_cat_n_subtypes_6_t1t2,
    c5_t12 = mt_cat_n_subtypes_5_t1t2,
    cev_t12 = mt_cat_n_events_t1t2,
    csv_t12 = mt_cat_severity_t1t2,
    cfq_t12 = mt_cat_frequency_t1t2,
    cch_t12 = mt_cat_chronicity_t1t2,
    c6_all = mt_cat_n_subtypes_6_t2all,
    c5_all = mt_cat_n_subtypes_5_t2all,
    cev_all = mt_cat_n_events_t2all,
    csv_all = mt_cat_severity_t2all,
    cfq_all = mt_cat_frequency_t2all,
    cch_all = mt_cat_chronicity_t2all,
    d1a_s1 = mt_d1_abuse_mal_status_t1,
    d1a_n1 = mt_d1_abuse_n_subtypes_t1,
    d1a_e1 = mt_d1_abuse_n_events_t1,
    d1a_v1 = mt_d1_abuse_severity_t1,
    d1a_c1 = mt_d1_abuse_chronicity_t1,
    d1a_x1 = mt_d1_abuse_n_exposed_periods_t1,
    d1a_p1 = mt_d1_abuse_n_periods_covered_t1,
    d1n_s1 = mt_d1_neglect_mal_status_t1,
    d1n_n1 = mt_d1_neglect_n_subtypes_t1,
    d1n_e1 = mt_d1_neglect_n_events_t1,
    d1n_v1 = mt_d1_neglect_severity_t1,
    d1n_c1 = mt_d1_neglect_chronicity_t1,
    d1n_x1 = mt_d1_neglect_n_exposed_periods_t1,
    d1n_p1 = mt_d1_neglect_n_periods_covered_t1,
    d2a_s1 = mt_d2_abuse_mal_status_t1,
    d2a_n1 = mt_d2_abuse_n_subtypes_t1,
    d2a_e1 = mt_d2_abuse_n_events_t1,
    d2a_v1 = mt_d2_abuse_severity_t1,
    d2a_c1 = mt_d2_abuse_chronicity_t1,
    d2a_x1 = mt_d2_abuse_n_exposed_periods_t1,
    d2a_p1 = mt_d2_abuse_n_periods_covered_t1,
    d2n_s1 = mt_d2_neglect_mal_status_t1,
    d2n_n1 = mt_d2_neglect_n_subtypes_t1,
    d2n_e1 = mt_d2_neglect_n_events_t1,
    d2n_v1 = mt_d2_neglect_severity_t1,
    d2n_c1 = mt_d2_neglect_chronicity_t1,
    d2n_x1 = mt_d2_neglect_n_exposed_periods_t1,
    d2n_p1 = mt_d2_neglect_n_periods_covered_t1,
    d2e_s1 = mt_d2_emotion_mal_status_t1,
    d2e_n1 = mt_d2_emotion_n_subtypes_t1,
    d2e_e1 = mt_d2_emotion_n_events_t1,
    d2e_v1 = mt_d2_emotion_severity_t1,
    d2e_c1 = mt_d2_emotion_chronicity_t1,
    d2e_x1 = mt_d2_emotion_n_exposed_periods_t1,
    d2e_p1 = mt_d2_emotion_n_periods_covered_t1,
    d1a_s2 = mt_d1_abuse_mal_status_t1t2,
    d1a_n2 = mt_d1_abuse_n_subtypes_t1t2,
    d1a_e2 = mt_d1_abuse_n_events_t1t2,
    d1a_v2 = mt_d1_abuse_severity_t1t2,
    d1a_c2 = mt_d1_abuse_chronicity_t1t2,
    d1a_x2 = mt_d1_abuse_n_exposed_periods_t1t2,
    d1a_p2 = mt_d1_abuse_n_periods_covered_t1t2,
    d1n_s2 = mt_d1_neglect_mal_status_t1t2,
    d1n_n2 = mt_d1_neglect_n_subtypes_t1t2,
    d1n_e2 = mt_d1_neglect_n_events_t1t2,
    d1n_v2 = mt_d1_neglect_severity_t1t2,
    d1n_c2 = mt_d1_neglect_chronicity_t1t2,
    d1n_x2 = mt_d1_neglect_n_exposed_periods_t1t2,
    d1n_p2 = mt_d1_neglect_n_periods_covered_t1t2,
    d2a_s2 = mt_d2_abuse_mal_status_t1t2,
    d2a_n2 = mt_d2_abuse_n_subtypes_t1t2,
    d2a_e2 = mt_d2_abuse_n_events_t1t2,
    d2a_v2 = mt_d2_abuse_severity_t1t2,
    d2a_c2 = mt_d2_abuse_chronicity_t1t2,
    d2a_x2 = mt_d2_abuse_n_exposed_periods_t1t2,
    d2a_p2 = mt_d2_abuse_n_periods_covered_t1t2,
    d2n_s2 = mt_d2_neglect_mal_status_t1t2,
    d2n_n2 = mt_d2_neglect_n_subtypes_t1t2,
    d2n_e2 = mt_d2_neglect_n_events_t1t2,
    d2n_v2 = mt_d2_neglect_severity_t1t2,
    d2n_c2 = mt_d2_neglect_chronicity_t1t2,
    d2n_x2 = mt_d2_neglect_n_exposed_periods_t1t2,
    d2n_p2 = mt_d2_neglect_n_periods_covered_t1t2,
    d2e_s2 = mt_d2_emotion_mal_status_t1t2,
    d2e_n2 = mt_d2_emotion_n_subtypes_t1t2,
    d2e_e2 = mt_d2_emotion_n_events_t1t2,
    d2e_v2 = mt_d2_emotion_severity_t1t2,
    d2e_c2 = mt_d2_emotion_chronicity_t1t2,
    d2e_x2 = mt_d2_emotion_n_exposed_periods_t1t2,
    d2e_p2 = mt_d2_emotion_n_periods_covered_t1t2,
    d1a_sa = mt_d1_abuse_mal_status_t2all,
    d1a_na = mt_d1_abuse_n_subtypes_t2all,
    d1a_ea = mt_d1_abuse_n_events_t2all,
    d1a_va = mt_d1_abuse_severity_t2all,
    d1a_ca = mt_d1_abuse_chronicity_t2all,
    d1a_xa = mt_d1_abuse_n_exposed_periods_t2all,
    d1a_pa = mt_d1_abuse_n_periods_covered_t2all,
    d1n_sa = mt_d1_neglect_mal_status_t2all,
    d1n_na = mt_d1_neglect_n_subtypes_t2all,
    d1n_ea = mt_d1_neglect_n_events_t2all,
    d1n_va = mt_d1_neglect_severity_t2all,
    d1n_ca = mt_d1_neglect_chronicity_t2all,
    d1n_xa = mt_d1_neglect_n_exposed_periods_t2all,
    d1n_pa = mt_d1_neglect_n_periods_covered_t2all,
    d2a_sa = mt_d2_abuse_mal_status_t2all,
    d2a_na = mt_d2_abuse_n_subtypes_t2all,
    d2a_ea = mt_d2_abuse_n_events_t2all,
    d2a_va = mt_d2_abuse_severity_t2all,
    d2a_ca = mt_d2_abuse_chronicity_t2all,
    d2a_xa = mt_d2_abuse_n_exposed_periods_t2all,
    d2a_pa = mt_d2_abuse_n_periods_covered_t2all,
    d2n_sa = mt_d2_neglect_mal_status_t2all,
    d2n_na = mt_d2_neglect_n_subtypes_t2all,
    d2n_ea = mt_d2_neglect_n_events_t2all,
    d2n_va = mt_d2_neglect_severity_t2all,
    d2n_ca = mt_d2_neglect_chronicity_t2all,
    d2n_xa = mt_d2_neglect_n_exposed_periods_t2all,
    d2n_pa = mt_d2_neglect_n_periods_covered_t2all,
    d2e_sa = mt_d2_emotion_mal_status_t2all,
    d2e_na = mt_d2_emotion_n_subtypes_t2all,
    d2e_ea = mt_d2_emotion_n_events_t2all,
    d2e_va = mt_d2_emotion_severity_t2all,
    d2e_ca = mt_d2_emotion_chronicity_t2all,
    d2e_xa = mt_d2_emotion_n_exposed_periods_t2all,
    d2e_pa = mt_d2_emotion_n_periods_covered_t2all,
    km_e1 = mt_sub_km_n_events_t1,
    km_v1 = mt_sub_km_severity_t1,
    km_c1 = mt_sub_km_chronicity_t1,
    km_x1 = mt_sub_km_n_exposed_periods_t1,
    km_p1 = mt_sub_km_n_periods_covered_t1,
    sm_e1 = mt_sub_sm_n_events_t1,
    sm_v1 = mt_sub_sm_severity_t1,
    sm_c1 = mt_sub_sm_chronicity_t1,
    sm_x1 = mt_sub_sm_n_exposed_periods_t1,
    sm_p1 = mt_sub_sm_n_periods_covered_t1,
    ea_e1 = mt_sub_ea_n_events_t1,
    ea_v1 = mt_sub_ea_severity_t1,
    ea_c1 = mt_sub_ea_chronicity_t1,
    ea_x1 = mt_sub_ea_n_exposed_periods_t1,
    ea_p1 = mt_sub_ea_n_periods_covered_t1,
    en_e1 = mt_sub_en_n_events_t1,
    en_v1 = mt_sub_en_severity_t1,
    en_c1 = mt_sub_en_chronicity_t1,
    en_x1 = mt_sub_en_n_exposed_periods_t1,
    en_p1 = mt_sub_en_n_periods_covered_t1,
    em_e1 = mt_sub_em_n_events_t1,
    em_v1 = mt_sub_em_severity_t1,
    em_c1 = mt_sub_em_chronicity_t1,
    em_x1 = mt_sub_em_n_exposed_periods_t1,
    em_p1 = mt_sub_em_n_periods_covered_t1,
    mv_e1 = mt_sub_mv_n_events_t1,
    mv_v1 = mt_sub_mv_severity_t1,
    mv_c1 = mt_sub_mv_chronicity_t1,
    mv_x1 = mt_sub_mv_n_exposed_periods_t1,
    mv_p1 = mt_sub_mv_n_periods_covered_t1,
    mb_e1 = mt_sub_mb_n_events_t1,
    mb_v1 = mt_sub_mb_severity_t1,
    mb_c1 = mt_sub_mb_chronicity_t1,
    mb_x1 = mt_sub_mb_n_exposed_periods_t1,
    mb_p1 = mt_sub_mb_n_periods_covered_t1,
    bm_e1 = mt_sub_bm_n_events_t1,
    bm_v1 = mt_sub_bm_severity_t1,
    bm_c1 = mt_sub_bm_chronicity_t1,
    bm_x1 = mt_sub_bm_n_exposed_periods_t1,
    bm_p1 = mt_sub_bm_n_periods_covered_t1,
    mre_e1 = mt_sub_mre_n_events_t1,
    mre_v1 = mt_sub_mre_severity_t1,
    mre_c1 = mt_sub_mre_chronicity_t1,
    mre_x1 = mt_sub_mre_n_exposed_periods_t1,
    mre_p1 = mt_sub_mre_n_periods_covered_t1,
    mrbm_e1 = mt_sub_mre_bm_n_events_t1,
    mrbm_v1 = mt_sub_mre_bm_severity_t1,
    mrbm_c1 = mt_sub_mre_bm_chronicity_t1,
    mrbm_x1 = mt_sub_mre_bm_n_exposed_periods_t1,
    mrbm_p1 = mt_sub_mre_bm_n_periods_covered_t1,
    km_e2 = mt_sub_km_n_events_t1t2,
    km_v2 = mt_sub_km_severity_t1t2,
    km_c2 = mt_sub_km_chronicity_t1t2,
    km_x2 = mt_sub_km_n_exposed_periods_t1t2,
    km_p2 = mt_sub_km_n_periods_covered_t1t2,
    sm_e2 = mt_sub_sm_n_events_t1t2,
    sm_v2 = mt_sub_sm_severity_t1t2,
    sm_c2 = mt_sub_sm_chronicity_t1t2,
    sm_x2 = mt_sub_sm_n_exposed_periods_t1t2,
    sm_p2 = mt_sub_sm_n_periods_covered_t1t2,
    ea_e2 = mt_sub_ea_n_events_t1t2,
    ea_v2 = mt_sub_ea_severity_t1t2,
    ea_c2 = mt_sub_ea_chronicity_t1t2,
    ea_x2 = mt_sub_ea_n_exposed_periods_t1t2,
    ea_p2 = mt_sub_ea_n_periods_covered_t1t2,
    en_e2 = mt_sub_en_n_events_t1t2,
    en_v2 = mt_sub_en_severity_t1t2,
    en_c2 = mt_sub_en_chronicity_t1t2,
    en_x2 = mt_sub_en_n_exposed_periods_t1t2,
    en_p2 = mt_sub_en_n_periods_covered_t1t2,
    em_e2 = mt_sub_em_n_events_t1t2,
    em_v2 = mt_sub_em_severity_t1t2,
    em_c2 = mt_sub_em_chronicity_t1t2,
    em_x2 = mt_sub_em_n_exposed_periods_t1t2,
    em_p2 = mt_sub_em_n_periods_covered_t1t2,
    mv_e2 = mt_sub_mv_n_events_t1t2,
    mv_v2 = mt_sub_mv_severity_t1t2,
    mv_c2 = mt_sub_mv_chronicity_t1t2,
    mv_x2 = mt_sub_mv_n_exposed_periods_t1t2,
    mv_p2 = mt_sub_mv_n_periods_covered_t1t2,
    mb_e2 = mt_sub_mb_n_events_t1t2,
    mb_v2 = mt_sub_mb_severity_t1t2,
    mb_c2 = mt_sub_mb_chronicity_t1t2,
    mb_x2 = mt_sub_mb_n_exposed_periods_t1t2,
    mb_p2 = mt_sub_mb_n_periods_covered_t1t2,
    bm_e2 = mt_sub_bm_n_events_t1t2,
    bm_v2 = mt_sub_bm_severity_t1t2,
    bm_c2 = mt_sub_bm_chronicity_t1t2,
    bm_x2 = mt_sub_bm_n_exposed_periods_t1t2,
    bm_p2 = mt_sub_bm_n_periods_covered_t1t2,
    mre_e2 = mt_sub_mre_n_events_t1t2,
    mre_v2 = mt_sub_mre_severity_t1t2,
    mre_c2 = mt_sub_mre_chronicity_t1t2,
    mre_x2 = mt_sub_mre_n_exposed_periods_t1t2,
    mre_p2 = mt_sub_mre_n_periods_covered_t1t2,
    mrbm_e2 = mt_sub_mre_bm_n_events_t1t2,
    mrbm_v2 = mt_sub_mre_bm_severity_t1t2,
    mrbm_c2 = mt_sub_mre_bm_chronicity_t1t2,
    mrbm_x2 = mt_sub_mre_bm_n_exposed_periods_t1t2,
    mrbm_p2 = mt_sub_mre_bm_n_periods_covered_t1t2,
    km_ea = mt_sub_km_n_events_t2all,
    km_va = mt_sub_km_severity_t2all,
    km_ca = mt_sub_km_chronicity_t2all,
    km_xa = mt_sub_km_n_exposed_periods_t2all,
    km_pa = mt_sub_km_n_periods_covered_t2all,
    sm_ea = mt_sub_sm_n_events_t2all,
    sm_va = mt_sub_sm_severity_t2all,
    sm_ca = mt_sub_sm_chronicity_t2all,
    sm_xa = mt_sub_sm_n_exposed_periods_t2all,
    sm_pa = mt_sub_sm_n_periods_covered_t2all,
    ea_ea = mt_sub_ea_n_events_t2all,
    ea_va = mt_sub_ea_severity_t2all,
    ea_ca = mt_sub_ea_chronicity_t2all,
    ea_xa = mt_sub_ea_n_exposed_periods_t2all,
    ea_pa = mt_sub_ea_n_periods_covered_t2all,
    en_ea = mt_sub_en_n_events_t2all,
    en_va = mt_sub_en_severity_t2all,
    en_ca = mt_sub_en_chronicity_t2all,
    en_xa = mt_sub_en_n_exposed_periods_t2all,
    en_pa = mt_sub_en_n_periods_covered_t2all,
    em_ea = mt_sub_em_n_events_t2all,
    em_va = mt_sub_em_severity_t2all,
    em_ca = mt_sub_em_chronicity_t2all,
    em_xa = mt_sub_em_n_exposed_periods_t2all,
    em_pa = mt_sub_em_n_periods_covered_t2all,
    mv_ea = mt_sub_mv_n_events_t2all,
    mv_va = mt_sub_mv_severity_t2all,
    mv_ca = mt_sub_mv_chronicity_t2all,
    mv_xa = mt_sub_mv_n_exposed_periods_t2all,
    mv_pa = mt_sub_mv_n_periods_covered_t2all,
    mb_ea = mt_sub_mb_n_events_t2all,
    mb_va = mt_sub_mb_severity_t2all,
    mb_ca = mt_sub_mb_chronicity_t2all,
    mb_xa = mt_sub_mb_n_exposed_periods_t2all,
    mb_pa = mt_sub_mb_n_periods_covered_t2all,
    bm_ea = mt_sub_bm_n_events_t2all,
    bm_va = mt_sub_bm_severity_t2all,
    bm_ca = mt_sub_bm_chronicity_t2all,
    bm_xa = mt_sub_bm_n_exposed_periods_t2all,
    bm_pa = mt_sub_bm_n_periods_covered_t2all,
    mre_ea = mt_sub_mre_n_events_t2all,
    mre_va = mt_sub_mre_severity_t2all,
    mre_ca = mt_sub_mre_chronicity_t2all,
    mre_xa = mt_sub_mre_n_exposed_periods_t2all,
    mre_pa = mt_sub_mre_n_periods_covered_t2all,
    mrbm_ea = mt_sub_mre_bm_n_events_t2all,
    mrbm_va = mt_sub_mre_bm_severity_t2all,
    mrbm_ca = mt_sub_mre_bm_chronicity_t2all,
    mrbm_xa = mt_sub_mre_bm_n_exposed_periods_t2all,
    mrbm_pa = mt_sub_mre_bm_n_periods_covered_t2all,
    km_s1 = mt_sub_km_mal_status_t1,
    sm_s1 = mt_sub_sm_mal_status_t1,
    ea_s1 = mt_sub_ea_mal_status_t1,
    en_s1 = mt_sub_en_mal_status_t1,
    em_s1 = mt_sub_em_mal_status_t1,
    mv_s1 = mt_sub_mv_mal_status_t1,
    mb_s1 = mt_sub_mb_mal_status_t1,
    bm_s1 = mt_sub_bm_mal_status_t1,
    mre_s1 = mt_sub_mre_mal_status_t1,
    mrbm_s1 = mt_sub_mre_bm_mal_status_t1,
    km_s2 = mt_sub_km_mal_status_t1t2,
    sm_s2 = mt_sub_sm_mal_status_t1t2,
    ea_s2 = mt_sub_ea_mal_status_t1t2,
    en_s2 = mt_sub_en_mal_status_t1t2,
    em_s2 = mt_sub_em_mal_status_t1t2,
    mv_s2 = mt_sub_mv_mal_status_t1t2,
    mb_s2 = mt_sub_mb_mal_status_t1t2,
    bm_s2 = mt_sub_bm_mal_status_t1t2,
    mre_s2 = mt_sub_mre_mal_status_t1t2,
    mrbm_s2 = mt_sub_mre_bm_mal_status_t1t2,
    km_sa = mt_sub_km_mal_status_t2all,
    sm_sa = mt_sub_sm_mal_status_t2all,
    ea_sa = mt_sub_ea_mal_status_t2all,
    en_sa = mt_sub_en_mal_status_t2all,
    em_sa = mt_sub_em_mal_status_t2all,
    mv_sa = mt_sub_mv_mal_status_t2all,
    mb_sa = mt_sub_mb_mal_status_t2all,
    bm_sa = mt_sub_bm_mal_status_t2all,
    mre_sa = mt_sub_mre_mal_status_t2all,
    mrbm_sa = mt_sub_mre_bm_mal_status_t2all,
    c2p1raw = cortgen_cort_t2_pg1_raw,
    c2p2raw = cortgen_cort_t2_pg2_raw,
    c2p3raw = cortgen_cort_t2_pg3_raw,
    c2p1log = cortgen_cort_t2_pg1_log,
    c2p2log = cortgen_cort_t2_pg2_log,
    c2p3log = cortgen_cort_t2_pg3_log,
    c2p1_z = cortgen_cort_t2_pg1_z,
    c2p2_z = cortgen_cort_t2_pg2_z,
    c2p3_z = cortgen_cort_t2_pg3_z,
    c5_raw = cortgen_cort_t5_raw,
    c5_log = cortgen_cort_t5_log,
    c5_z = cortgen_cort_t5_z,
    prs_e02 = cortgen_MDD_EUR_no23_Adams2025_pst_eff_a1_b0_5_phi1e_02,
    prs_e04 = cortgen_MDD_EUR_no23_Adams2025_pst_eff_a1_b0_5_phi1e_04,
    prs_e06 = cortgen_MDD_EUR_no23_Adams2025_pst_eff_a1_b0_5_phi1e_06,
    prs_eau = cortgen_MDD_EUR_no23_Adams2025_pst_eff_a1_b0_5_phiauto,
    prs_m02 = cortgen_MDD_MultiAnc_no23_Adams2025_pst_eff_a1_b0_5_phi1e_02,
    prs_m04 = cortgen_MDD_MultiAnc_no23_Adams2025_pst_eff_a1_b0_5_phi1e_04,
    prs_m06 = cortgen_MDD_MultiAnc_no23_Adams2025_pst_eff_a1_b0_5_phi1e_06,
    prs_mau = cortgen_MDD_MultiAnc_no23_Adams2025_pst_eff_a1_b0_5_phiauto,
    PC1 = cortgen_PC1,
    PC2 = cortgen_PC2,
    PC3 = cortgen_PC3,
    PC4 = cortgen_PC4,
    dob = mt_dob,
    intdt2 = mt_int_date_t2,
    intdt5 = mt_int_date_t5,
    stat_t5 = mt_status_t5,
    aget2 = mt_age_t2,
    aget5m = mt_age_t5
  )

##### CREATE INITIAL VARIABLE DICTIONARY ####
variable_dictionary <- tibble(
  original_position = seq_along(original_names),
  original_name = original_names,
  mplus_name = names(dat_mplus),
  original_class = vapply(
    dat_raw,
    function(x) class(x)[1],
    character(1)
  )
)

##### RECODE T5 STATUS AND CREATE NUMERIC ID ####
id_dictionary <- dat_mplus |>
  distinct(SIC_N) |>
  arrange(SIC_N) |>
  mutate(
    SIC_num = row_number()
  )

dat_mplus <- dat_mplus |>
  left_join(
    id_dictionary,
    by = "SIC_N"
  ) |>
  mutate(
    stat_t5 = case_when(
      stat_t5 == "drop out"  ~ 0,
      stat_t5 == "completed" ~ 2,
      !is.na(stat_t5)        ~ 1,
      TRUE                   ~ NA_real_
    )
  ) |>
  select(
    -SIC_N
  ) |>
  rename(
    SIC_N = SIC_num
  ) |>
  select(
    where(
      ~ is.numeric(.x) &&
        !inherits(.x, "POSIXt") &&
        !inherits(.x, "Date")
    )
  )

##### UPDATE VARIABLE DICTIONARY ####
variable_dictionary <- variable_dictionary |>
  mutate(
    retained = mplus_name %in% names(dat_mplus),
    final_position = match(mplus_name, names(dat_mplus)),
    final_class = if_else(
      retained,
      vapply(
        mplus_name,
        function(x) class(dat_mplus[[x]])[1],
        character(1)
      ),
      NA_character_
    )
  ) |>
  arrange(
    desc(retained),
    final_position,
    original_position
  )

##### CHECK FINAL MPLUS DATASET ####
stopifnot(
  all(vapply(dat_mplus, is.numeric, logical(1))),
  !any(vapply(dat_mplus, inherits, logical(1), what = "POSIXt")),
  !any(vapply(dat_mplus, inherits, logical(1), what = "Date")),
  all(nchar(names(dat_mplus)) <= 8),
  anyDuplicated(names(dat_mplus)) == 0,
  "SIC_N" %in% names(dat_mplus),
  "stat_t5" %in% names(dat_mplus)
)

##### INSPECT REMOVED VARIABLES ####
removed_variables <- variable_dictionary |>
  filter(!retained)

# View(variable_dictionary)
# View(removed_variables)

##### SAVE VARIABLE DICTIONARY ####
write_csv(
  variable_dictionary,
  "C:/Users/keil/Documents/main_outcome_amis2/mplus_variable_dictionary.csv"
)

##### REPLACE MISSING VALUES FOR MPLUS EXPORT ####
dat_mplus_export <- dat_mplus |>
  mutate(
    across(
      everything(),
      ~ replace(.x, is.na(.x), -999)
    )
  )

stopifnot(
  sum(is.na(dat_mplus_export)) == 0
)

##### DEFINE OUTPUT DIRECTORIES ####

output_dir <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/Meine Bibliotheken/",
  "MAIN OUTCOME/02_data/02_data_Prep/MPlus_Dataset"
)

mplus_input_dir <- "C:/MPLUS/Inputs"

invisible(
  sapply(
    c(
      output_dir,
      mplus_input_dir
    ),
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

##### SAVE MPLUS DATASET ####

mplus_data_file <- file.path(
  output_dir,
  "AMIS_mplus_dataset.dat"
)

write.table(
  dat_mplus_export,
  file = mplus_data_file,
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)

##### SAVE MPLUS VARIABLE NAMES ####

mplus_names <- names(dat_mplus)

mplus_names_file <- file.path(
  output_dir,
  "AMIS_mplus_names.rds"
)

saveRDS(
  mplus_names,
  file = mplus_names_file
)

##### SAVE MPLUS NAMES SYNTAX ####

mplus_names_text <- paste(
  mplus_names,
  collapse = "\n    "
)

mplus_names_syntax_file <- file.path(
  output_dir,
  "AMIS_mplus_names.txt"
)

writeLines(
  c(
    "NAMES ARE",
    paste0(
      "    ",
      mplus_names_text
    ),
    ";"
  ),
  mplus_names_syntax_file
)

##### COPY MPLUS FILES TO LOCAL INPUT DIRECTORY ####

files_to_copy <- c(
  mplus_data_file,
  mplus_names_file,
  mplus_names_syntax_file
)

copy_success <- file.copy(
  from = files_to_copy,
  to = mplus_input_dir,
  overwrite = TRUE
)

stopifnot(
  all(copy_success)
)

##### CHECK LOCAL MPLUS FILES ####

local_mplus_data_file <- file.path(
  mplus_input_dir,
  "AMIS_mplus_dataset.dat"
)

local_mplus_names_file <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names.rds"
)

local_mplus_names_syntax_file <- file.path(
  mplus_input_dir,
  "AMIS_mplus_names.txt"
)

stopifnot(
  file.exists(local_mplus_data_file),
  file.exists(local_mplus_names_file),
  file.exists(local_mplus_names_syntax_file)
)

##### CHECK DATASET AND VARIABLE NAMES ####

number_of_data_columns <- length(
  strsplit(
    readLines(
      local_mplus_data_file,
      n = 1
    ),
    split = "\t",
    fixed = TRUE
  )[[1]]
)

stopifnot(
  number_of_data_columns == length(mplus_names)
)