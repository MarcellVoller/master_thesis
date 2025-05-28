#------------------------------------------------------------------------------|
# PACKAGES                                                                 ####
#------------------------------------------------------------------------------|


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Sys.setenv(LIBARROW_MINIMAL = FALSE); install.packages('arrow', type = "source")
library(tidyverse) # data wrangling
library(vroom) # fast data reading
library(arrow) # parquet data reading
library(changepoint) # changepoint analysis
library(hmmr) # hidden Markov model analysis
library(mousetrap) # bimodality coefficient
library(cusp) # aggregated cusp analysis
library(tictoc) # for fun timing experiments
library(mlVAR) # multilevel vector autoregressive models


#------------------------------------------------------------------------------|
# PARALLEL PROCESSING                                                      ####
#------------------------------------------------------------------------------|

# set up clusters for parallel processing

cl <- snow::makeCluster(6, type = "SOCK")
doSNOW::registerDoSNOW(cl)


#------------------------------------------------------------------------------|
# INITIATION                                                               ####
#------------------------------------------------------------------------------|


# read in data
data <- open_dataset("sim_data.parquet", partitioning = "pp_nr")
data <- data %>%
  mutate(stress_lvl = case_when(stress_lvl == 0 ~ "low",
                                stress_lvl == 1 ~ "high"),
         par_changed = case_when(par_changed == 0 ~ "d4",
                                 par_changed == 1 ~ "K2",
                                 par_changed == 2 ~ "e5", 
                                 par_changed == 3 ~ "c42",
                                 par_changed == 4 ~ "c52",
                                 par_changed == 5 ~ "c3")) %>%
  collect()

# use test_data to calibrate analysis
test_data <- data %>%
  filter(pp_nr %in% sample(data$pp_nr, 15)) %>%
  collect()

#------------------------------------------------------------------------------|
# EXISTING OBJECTS                                                         ####
#------------------------------------------------------------------------------|

# read in analyzed data files

# CP and BC
cpt_res <- readRDS(file = "cpa_results.rds")
bc_res <- readRDS(file = "bimodality_results.rds") # BC
bimodal <- readRDS(file = "bimodality_test_result.rds") # LaplacesDemon is.bimodal()

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
