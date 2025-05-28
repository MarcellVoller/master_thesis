setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("0-analysis_init.R")

#------------------------------------------------------------------------------|
# CHANGE POINT ANALYSIS                                                    ####
#------------------------------------------------------------------------------|


# try mean and variance in isolation
# CUSUM is a distribution-free method for changepoint analysis but only available for change in mean

tic()
cpt_pelt <- list()
set.seed(36)
for(i in sort(unique(data$pp_nr))) {
  work_data <- data %>%
    filter(pp_nr == i) %>% 
    dplyr::select(sui_thoughts) %>%
    unlist() 
  cpt_pelt[i] <- cpt.mean(work_data, penalty = "Manual", method = "PELT",
                              pen.value = "4 * log(n)")
  print(i)
}
toc()

# BINARY SEGMENTATION
tic()
cpt_res <- list()
set.seed(36)
for(i in sort(unique(data$pp_nr))) {
  work_data <- data %>%
    filter(pp_nr == i) %>% 
    dplyr::select(sui_thoughts) %>%
    unlist() 
  cpt_res[i + 1] <- cpt.mean(work_data, 
                            method = "BinSeg", test.stat = "Normal", minseglen = 120, Q = 5)
  print(i)
}
toc() # 3106.67 sec elapsed


# BINARY SEGMENTATION
tic()
cpt_res <- list()
set.seed(36)
for(i in sort(unique(data$pp_nr))) {
  work_data <- data %>%
    filter(pp_nr == i) %>% 
    dplyr::select(sui_thoughts) %>%
    unlist() 
  cpt_res[i + 1] <- cpt.var(work_data, 
                method = "BinSeg", test.stat = "Normal", minseglen = 120, Q = 5)
  print(i)
}
toc() # 3106.67 sec elapsed

# pelt_res <- cpt.meanvar(snippet$sui_thoughts, penalty = "MBIC", 
#                     method = "PELT", minseglen = 120)
# plot(pelt)
# 
# # exponential test statistic - normal test statistic is not sensitive to fluctuations in beginning,
# # only longer-term change periods
# binary <- cpt.meanvar(snippet$sui_thoughts, penalty = "None", Q = 10, method = "BinSeg",
#                       minseglen = 120, test.stat = "Exponential")
# 
# summary(binary)
# plot(binary)
# 
#------------------------------------------------------------------------------|
# BIMODALITY COEFFICIENT                                                   ####
#------------------------------------------------------------------------------|


# bimodality coefficient
tic()
set.seed(36)
bc_res <- list()
for(i in sort(unique(data$pp_nr))) {
  work_data <- data %>%
    filter(pp_nr == i) %>% 
    dplyr::select(sui_thoughts) %>%
    unlist() 
  bc_res[i + 1] <- mousetrap::bimodality_coefficient(work_data)
}
toc() # 2734.08 sec elapsed

# is.bimodal function from LaplacesDemon
tic()
set.seed(36)
bimodal_test <- list()
for(i in sort(unique(data$pp_nr))) {
  work_data <- data %>%
    filter(pp_nr == i) %>% 
    dplyr::select(sui_thoughts) %>%
    unlist() 
  bimodal_test[i + 1] <- LaplacesDemon::is.bimodal(work_data)
}
toc() # 2734.08 sec elapsed


#------------------------------------------------------------------------------|
# HIDDEN MARKOV MODELS                                                     ####
#------------------------------------------------------------------------------|

# sort participant numbers
sorted_ppnrs <- sort(unique(data$pp_nr))

# find participants with sufficient sui_thoughts
sufficient_suithoughts <- data %>% 
  group_by(pp_nr) %>% 
  filter(max(sui_thoughts) > 0.2) %>% 
  dplyr::select(pp_nr) %>% 
  mutate(pp_nr = pp_nr + 1) %>%
  unique() %>%
  unlist() %>%
  sort()


# 1-state comparison model
tic()
set.seed(36)
gamma_1s_res <- list()
for(i in sorted_ppnrs) {
  work_data <- data %>%
    filter(pp_nr == i) %>% 
    dplyr::select(sui_thoughts) %>%
    mutate(sui_thoughts = sui_thoughts + 0.0005)
  hmm_two_cov <- try(depmix(response = sui_thoughts ~ 1, 
                            data = work_data, nstates = 1, 
                            initdata = work_data,
                            respstart = 0.5,
                            family = Gamma()))
  gamma_1s_res[i + 1] <- try(depmixS4::fit(hmm_two_cov))
  print(i)
}
toc() # 1368.95 sec elapsed

# 2-state comparison model
tic()
set.seed(36)
gamma_2s_res <- list()
for(i in sorted_ppnrs) {
  work_data <- data %>%
    filter(pp_nr == i) %>% 
    dplyr::select(sui_thoughts) %>%
    mutate(sui_thoughts = sui_thoughts + 0.0005)
  hmm_two_cov <- try(depmix(response = sui_thoughts ~ 1, 
                            data = work_data, nstates = 2, 
                            initdata = work_data,
                            respstart = c(0.0005, 1),
                            family = Gamma()))
  gamma_2s_res[i + 1] <- try(depmixS4::fit(hmm_two_cov))
  print(i)
}
toc() # 


# 3-state comparison model
tic()
set.seed(36)
gamma_3s_res <- list()
for(i in sorted_ppnrs) {
  work_data <- data %>%
    filter(pp_nr == i) %>% 
    dplyr::select(sui_thoughts) %>%
    mutate(sui_thoughts = sui_thoughts + 0.0005)
  hmm_two_cov <- try(depmix(response = sui_thoughts ~ 1, 
                            data = work_data, nstates = 3, 
                            initdata = work_data,
                            respstart = c(0.5, 1, 0.0005),
                            family = Gamma()))
  gamma_3s_res[i + 1] <- try(depmixS4::fit(hmm_two_cov))
  print(i)
}
toc() # 5279.64 sec elapsed

################# single models  ###################

analysis_data <- data %>%
  mutate(sui_thoughts = sui_thoughts + 0.0005,
         ext_change = ext_change + 0.0005)

hmm2s_extchange_sing <- depmix(response = sui_thoughts ~ 1, 
                          data = analysis_data, nstates = 2, 
                          initdata = analysis_data,
                          respstart = c(0.0005, 1),
                          family = Gamma(),
                          transition =~ ext_change,
                          ntimes = rep.int(40320, 1008))
hmm2s_extchange_fitsing <- depmixS4::fit(hmm2s_extchange_sing)


################# ext_change  ###################

# create markov models baseline - EXT_CHANGE

# makes sure that values are valid by adding 0.0005 to variables
tic()
set.seed(36)
gamma_extchange_res <- list()
for(i in sorted_ppnrs) {
  work_data <- data %>%
    filter(pp_nr == i) %>% 
    dplyr::select(sui_thoughts, ext_change) %>%
    mutate(sui_thoughts = sui_thoughts + 0.0005, ext_change = ext_change + 0.0005)
  hmm_two_cov <- try(depmix(response = sui_thoughts ~ 1, 
                            data = work_data, nstates = 2, 
                            initdata = work_data,
                            respstart = c(0.0005, 1),
                            family = Gamma(),
                            transition =~ ext_change))
  gamma_extchange_res[i + 1] <- try(depmixS4::fit(hmm_two_cov))
  print(i)
}
toc() # 10441.35 sec elapsed

# identify correct model numbers

# which depmix objects were correctly fitted and converged? - EXT_CHANGE
extchange_fit <- which(sapply(gamma_extchange_res, typeof) == "S4") 
extchange_converged <- which(lapply(
  lapply(gamma_extchange_res[extchange_fit], function(i){slot(i, "message")}), 
  function(z){grep("converged", z)}) == 1)

# indices of fitted and converged
fitted_converged_extchange <- extchange_fit[extchange_converged]

# indices of converged and high enough sui_thoughts
hyst_test_extchange <- intersect(fitted_converged_extchange, sufficient_suithoughts)

# create hysteresis model comparison - EXT_CHANGE

tic()
set.seed(36)
z <- 1
gamma_hyst_extchange <- list()
for(i in hyst_test_extchange) {
  work_data <- data %>%
    filter(pp_nr == (i - 1)) %>% 
    dplyr::select(sui_thoughts, ext_change) %>%
    mutate(sui_thoughts = sui_thoughts + 0.0005, ext_change = ext_change + 0.0005)
  
  # create depmix object
  hmm_two_cov <- try(depmix(response = sui_thoughts ~ 1, 
                            data = work_data, nstates = 2, 
                            initdata = work_data,
                            family = Gamma(),
                            transition =~ ext_change))
  
  # create constrained model for hysteresis test
  pars <- getpars(gamma_extchange_res[[i]])
  # constraints 
  pars[c(1,2)] <- c(.5,.5) # set uniform initial probabilities 
  pars[c(11,12)] <- pars[c(11,12)] + rnorm(2,sd=.5) # random noise for the means
  hyst <- setpars(hmm_two_cov,pars) # define new model with different parameter values
  # tell model which are fixed = 0 and free = 1 parameters  
  conpat <- c(1,1,rep(c(0,1),4),1,1) # equality constraints in the hysteresis model
  # equality constraint on betas of transitions 
  conpat[6] <- conpat[10] <- 3 # same integer means these are equal
  
  gamma_hyst_extchange[z] <- try(depmixS4::fit(hyst,equal=conpat))
  print(z)
  z <- z + 1
}
toc() # 973.74 sec elapsed 

# critical test of non-equality between hysteresis constraints and 
# unconstrained model - EXT_CHANGE
tic()
llratio_extchange <- vector(length = length(gamma_hyst_extchange))
z <- 1
for(i in hyst_test_extchange) {
  lltest <- llratio(gamma_extchange_res[[i]], 
                    gamma_hyst_extchange[[which(hyst_test_extchange == i)]])
  llratio_extchange[z] <- pchisq(lltest@value, lltest@df, lower.tail = FALSE)
  z <- z + 1
}
toc() # 42.72 sec elapsed

# these are the object numbers with significant hysteresis
sig_hysteresis_extchange <- hyst_test_extchange[llratio_extchange > 0.05]

################# av_state  ###################

# try aversive internal states as covariate since external coping only seems to
# favor a single state when it converges
tic()
set.seed(36)
gamma_avstate_res <- list()
for(i in sorted_ppnrs) {
  work_data <- data %>%
    filter(pp_nr == i) %>% 
    dplyr::select(sui_thoughts, av_state) %>%
    mutate(sui_thoughts = sui_thoughts + 0.0005, av_state = av_state + 0.0005)
  hmm_two_cov <- try(depmix(response = sui_thoughts ~ 1, 
                            data = work_data, nstates = 2, 
                            initdata = work_data,
                            respstart = c(1, 5),
                            family = Gamma(),
                            transition =~ av_state))
  gamma_avstate_res[which(sorted_ppnrs == i)] <- try(depmixS4::fit(hmm_two_cov))
  print(i)
}
toc() # 9142.69 sec elapsed

# identify correct model numbers

# which depmix objects were correctly fitted and converged? - AV_STATE
avstate_fit <- which(sapply(gamma_avstate_res, typeof) == "S4") 
avstate_converged <- which(lapply(
  lapply(gamma_avstate_res[avstate_fit], function(i){slot(i, "message")}), 
  function(z){grep("converged", z)}) == 1)

# indices of fitted and converged
fitted_converged_avstate <- avstate_fit[avstate_converged]

# indices of converged and high enough sui_thoughts
hyst_test_avstate <- intersect(fitted_converged_avstate, sufficient_suithoughts)

# create hysteresis fits for aversive internal state

tic()
set.seed(36)
z <- 1
gamma_hyst_avstate <- list()
for(i in hyst_test_avstate) {
  work_data <- data %>%
    filter(pp_nr == (i - 1)) %>% 
    dplyr::select(sui_thoughts, av_state) %>%
    mutate(sui_thoughts = sui_thoughts + 0.0005, av_state = av_state + 0.0005)
  
  # create depmix object
  hmm_two_cov <- try(depmix(response = sui_thoughts ~ 1, 
                            data = work_data, nstates = 2, 
                            initdata = work_data,
                            family = Gamma(),
                            transition =~ av_state))
  
  # create constrained model for hysteresis test
  pars <- getpars(gamma_avstate_res[[i]])
  # constraints 
  pars[c(1,2)] <- c(.5,.5) # set uniform initial probabilities 
  pars[c(11,12)] <- pars[c(11,12)] + rnorm(2,sd=.5) # random noise for the means
  hyst <- setpars(hmm_two_cov,pars) # define new model with different parameter values
  # tell model which are fixed = 0 and free = 1 parameters  
  conpat <- c(1,1,rep(c(0,1),4),1,1) # equality constraints in the hysteresis model
  # equality constraint on betas of transitions 
  conpat[6] <- conpat[10] <- 3 # same integer means these are equal
  
  gamma_hyst_avstate[z] <- try(depmixS4::fit(hyst,equal=conpat))
  print(z)
  z <- z + 1
}
toc() # 737.08 sec elapsed 

# critical test of non-equality between hysteresis constraints and 
# unconstrained model - AV_STATE
tic()
llratio_avstate <- vector(length = length(hyst_test_avstate))
z <- 1
for(i in hyst_test_avstate) {
  lltest <- llratio(gamma_avstate_res[[i]], 
                    gamma_hyst_avstate[[which(hyst_test_avstate == i)]])
  llratio_avstate[z] <- pchisq(lltest@value, lltest@df, lower.tail = FALSE)
  z <- z + 1
}
toc() # 300.92 sec elapsed

# these are the participant numbers with significant hysteresis
sig_hysteresis_avstate <- hyst_test_avstate[llratio_avstate > 0.05]


# plot how probability changes based on aversive state as covariate
pars <- getpars(gamma_avstate_res[[629]]) 
logit1 <- function(value) plogis(pars[4]+value*pars[6]) 
logit2 <- function(value) plogis(pars[8]+value*pars[10]) 
plot(logit1, ylab="Probability", xlab="Aversive State", lty = 1, axes=F)
axis(1,c(0,0.5,1)) 
axis(2,c(0,0.5,1),las=1)
curve(logit2, lty = 2, add = TRUE)


#------------------------------------------------------------------------------|
# BIC VALUES                                                               ####
#------------------------------------------------------------------------------|

# get correctly fitted and converged objects

# which depmix objects were correctly fitted and converged? - HMM1S
hmm1s_fit <- which(sapply(gamma_1s_res, typeof) == "S4") 
hmm1s_converged <- which(lapply(
  lapply(gamma_1s_res[hmm1s_fit], function(i){slot(i, "message")}), 
  function(z){grep("converged", z)}) == 1)

# indices of fitted and converged
hmm1s_fitted_converged <- hmm1s_fit[hmm1s_converged]

# which depmix objects were correctly fitted and converged? - HMM3S
hmm3s_fit <- which(sapply(gamma_3s_res, typeof) == "S4") 
hmm3s_converged <- which(lapply(
  lapply(gamma_3s_res[hmm3s_fit], function(i){slot(i, "message")}), 
  function(z){grep("converged", z)}) == 1)

# indices of fitted and converged
hmm3s_fitted_converged <- hmm3s_fit[hmm3s_converged]


hmm1s_bic <- mean(sapply(gamma_1s_res[hmm1s_fitted_converged], BIC))
hmm3s_bic <- mean(sapply(gamma_3s_res[hmm3s_fitted_converged], BIC))
hmm2s_extchange_bic <- mean(sapply(gamma_extchange_res[fitted_converged_extchange], BIC))
hmm2s_extchangehyst_bic <- mean(sapply(gamma_extchange_res[hyst_test_extchange], BIC))
hmm2s_avstate_bic <- mean(sapply(gamma_avstate_res[fitted_converged_avstate], BIC))
hmm2s_avstatehyst_bic <- mean(sapply(gamma_avstate_res[hyst_test_avstate], BIC))


# number of changepoints identified
# BC 
# is hysteresis present according to model comparison

# # create aggregated plot for exporting
# par(mfrow = c(5, 3))
# lapply(cpa_res[hyst_avstate_ppnrs], plot)

#------------------------------------------------------------------------------|
# AGGREGATED CUSP ANALYSIS                                                 ####
#------------------------------------------------------------------------------|

# aggregate data
cross_data <- data %>%
  group_by(pp_nr) %>%
  filter(time == max(time))

# try first model where two variables together determine state of the model
tic()
set.seed(36)
cusp_multivariate <- cusp(y ~ sui_thoughts, 
                          alpha ~ ext_change + int_change, 
                          beta ~ ext_change + int_change,
                          cross_data)
toc()
summary(cusp_multivariate)
plot(cusp_multivariate)

# try second model where the change mechanisms are separated
tic()
set.seed(36)
cusp_multivariate <- cusp(y ~ sui_thoughts, 
                          alpha ~ ext_change, 
                          beta ~ int_change,
                          cross_data)
toc()
summary(cusp_multivariate)
plot(cusp_multivariate)

# try third model with everything
tic()
set.seed(36)
cusp_multivariate <- cusp(y ~ sui_thoughts, 
                          alpha ~ stressor + ext_change + av_state + urge_escape + other_escape + int_change, 
                          beta ~ stressor + ext_change + av_state + urge_escape + other_escape + int_change,
                          cross_data)
toc()
summary(cusp_multivariate)
plot(cusp_multivariate)

# try fourth model with nonsig from 3rd model removed
tic()
set.seed(36)
cusp_multivariate <- cusp(y ~ sui_thoughts, 
                          alpha ~ stressor + ext_change + av_state + urge_escape + other_escape + int_change, 
                          beta ~ stressor + av_state + urge_escape,
                          cross_data)
toc()
summary(cusp_multivariate)
plot(cusp_multivariate)
# best possible fit, all variables are significant, yet the linear model still much lower BIC


# try full model on timepoint 1 for comparison 
cross_data_t1 <- data %>%
  group_by(pp_nr) %>%
  filter(time == 1)

tic()
set.seed(36)
cusp_multivariate <- cusp(y ~ sui_thoughts, 
                          alpha ~ stressor + ext_change + av_state + urge_escape + other_escape + int_change, 
                          beta ~ stressor + ext_change + av_state + urge_escape + other_escape + int_change,
                          cross_data_t1)
toc()
# does not converge


# compare with models using single variables

set.seed(123)
# example with regressors
x1 = runif(150)
x2 = runif(150)
z = Vectorize(rcusp)(1, 4*x1-2, 4*x2-1)
data <- data.frame(x1, x2, z)
tic()
fit <- cusp(y ~ z, alpha ~ x1+x2, beta ~ x1+x2, data)
toc()


#------------------------------------------------------------------------------|
# mlVAR INDIVIDUAL NETWORKS                                                ####
#------------------------------------------------------------------------------|

mlVAR()
# main arguments include:
# estimator: defaults to "LMER" but other options exist
# contemporaneous/temporal
# beepvar, dayvar, idvar - this indicates the people
# nonsig allows you to decide how to remove edges

# Estimate model:
res_ml <- mlVAR(data, vars = vars, idvar = idvar, dayvar = dayvar, beepvar
                = beepvar, contemporaneous = ”correlated”, temporal = “correlated”)
# Fixed-effects temporal network:
plot(res_ml , “temporal”, title = “Temporal Network”)
# Fixed-effects contemporaneous network:
plot(res_ml , “Contemporaneous”, title = “Contemporaneous network”, rule =
       "and")

# Between-subjects network:
plot(res_ml , “between”, title = “Between-subjects network”, rule = "and")

# Random effects temporal network:
plot(res_ml , type= “temporal”, subject = 1, title = “Temporal Network”)

# Random effects contemporaneous network:
plot(res_ml , type = Contemporaneous”, subject = 1, title =
       “Contemporaneous network”)

# Individual differences network:
plot(res_ml , type = “temporal”, SD = TRUE, title = “Individual differences
     temporal network”)
plot(res_ml , type = “contemporaneous”, SD = TRUE, title = “Individual
     differences contemporaneous network”)

