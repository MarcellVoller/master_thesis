#------------------------------------------------------------------------------|
# PACKAGES                                                                 ####
#------------------------------------------------------------------------------|


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Sys.setenv(LIBARROW_MINIMAL = FALSE); install.packages('arrow', type = "source")
library(tidyverse) # data wrangling
library(ggh4x) # axis label guides for ggplot
library(vroom) # fast data reading
library(arrow) # parquet data reading
library(changepoint) # changepoint analysis
library(hmmr) # hidden Markov model analysis
library(mousetrap) # bimodality coefficient
library(cusp) # aggregated cusp analysis
library(tictoc) # for fun timing experiments
#library(devtools)
#install_github("LauraBringmann/tvvarGAM")  
library(tvvarGAM) # time-varying VAR


#------------------------------------------------------------------------------|
# PARALLEL PROCESSING                                                      ####
#------------------------------------------------------------------------------|

# set up clusters for parallel processing

cl <- snow::makeCluster(4, type = "SOCK")
doSNOW::registerDoSNOW(cl)

#------------------------------------------------------------------------------|
# INITIATION                                                               ####
#------------------------------------------------------------------------------|

# load data for analysis

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
# %>%
#   mutate(beep = rep(rep(1:1440, times = 28), times = 1008),
#          daynr = rep(rep(1:28, each = 1440), times = 1008))


# use test_data to calibrate analysis
test_data <- data %>%
  filter(pp_nr %in% sample(data$pp_nr, 15)) %>%
  mutate(beep = rep(rep(1:1440, times = 28), times = 15),
         daynr = rep(rep(1:28, each = 1440), times = 15))
  

