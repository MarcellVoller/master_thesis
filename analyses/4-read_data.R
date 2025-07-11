#------------------------------------------------------------------------------|
# EXISTING OBJECTS                                                         ####
#------------------------------------------------------------------------------|

# read in finished analysis objects

# CP and BC
cpt_res <- readRDS(file = "cpa_results.rds")
bc_res <- readRDS(file = "bimodality_results.rds") # BC
bimodal_test <- readRDS(file = "bimodality_test_result.rds") # LaplacesDemon is.bimodal()

# 1s and 3s models

gamma_1s_res <- readRDS(file = "1shmm_results.rds")
gamma_3s_res <- readRDS(file = "3shmm_results.rds")

# external change
gamma_extchange_res <- readRDS(file = "external_change_2shmm_results.rds")
gamma_2shyst_extchange_res <- readRDS(file = "extchange_hyst_results.rds")
llratio_extchange <- readRDS(file = "extchange_llratio_pvalues.rds")
sig_hysteresis_extchange <- readRDS(file = "extchange_llratio_objectnr.rds")

# aversive internal state
gamma_avstate_res <- readRDS(file = "aversivestates_2shmm_results.rds")
gamma_hyst_avstate <- readRDS(file = "avstate_hystfit_results.rds")
llratio_avstate <- readRDS(file = "avstate_llratio_pvalues.rds")
sig_hysteresis_avstate <- readRDS(file = "avstate_llratio_objectnr.rds")

# model BIC

bic_frame <- readRDS(file = "model_bic_values.rds")
