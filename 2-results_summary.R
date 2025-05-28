#------------------------------------------------------------------------------|
# DESCRIPTIVES                                                             ####
#------------------------------------------------------------------------------|

bimodal <- which(unlist(bimodal_test) == TRUE)

################# neither ###################


# descriptives for external change hysteresis individuals
all_sig_hysteresis <- union(sig_hysteresis_avstate, sig_hysteresis_extchange)

nohysteresis_subset_overall <- data %>%
  filter(!((pp_nr + 1) %in% all_sig_hysteresis)) %>%
  filter(time == 1)

# create table
nohysteresis_table <- arsenal::tableby(par_changed ~ stress_lvl + par_value, 
                                    data = nohysteresis_subset_overall, 
                                    numeric.stats = c("N", "mean", "median", "range"),
                                    test = FALSE) 

# create meaningful labels
arsenal::labels(nohysteresis_table)  <- c(par_changed = 'Varied Parameter', 
                                       stress_lvl = "Stress Level",
                                       par_value = "Parameter Value")

# examine table
summary(nohysteresis_table, text = TRUE, title = "No Hysteresis - Parameters")

nohysteresis_subset_vars <- data %>%
  filter(!((pp_nr + 1) %in% all_sig_hysteresis))

nohysteresis_vars_table <- arsenal::tableby(par_changed ~ stressor + 
                                           av_state + urge_escape + sui_thoughts + 
                                           other_escape + ext_change + 
                                           int_change, data = nohysteresis_subset_vars,
                                         numeric.stats = c("mean", "median", "range"),
                                         test = FALSE, total = FALSE)

arsenal::labels(nohysteresis_vars_table) <- c(par_changed = 'Varied Parameter', 
                                           stressor = "Stress",
                                           av_state = "Aversive State",
                                           urge_escape = "Urge to Escape",
                                           sui_thoughts = "Suicidal Thoughts",
                                           other_escape = "Other Escape Strategies",
                                           ext_change = "External-Focused Change",
                                           int_change = "Internal-Focused Change")

summary(nohysteresis_vars_table, text = TRUE, title = "No Hysteresis - Variables")




################# ext_change hysteresis ###################


# descriptives for external change hysteresis individuals
extchange_subset_overall <- data %>%
  filter(((pp_nr + 1) %in% sig_hysteresis_extchange) & ((pp_nr + 1) %in% bimodal)) %>%
  filter(time == 1)

# create table
extchange_table <- arsenal::tableby(par_changed ~ stress_lvl + par_value, 
                                    data = extchange_subset_overall, 
                 numeric.stats = c("N", "mean", "median", "range"),
                 test = FALSE) 

# create meaningful labels
arsenal::labels(extchange_table)  <- c(par_changed = 'Varied Parameter', 
                              stress_lvl = "Stress Level",
                              par_value = "Parameter Value")

# examine table
summary(extchange_table, text = TRUE, title = "External Change - Parameters")

extchange_subset_vars <- data %>%
  filter((pp_nr + 1) %in% sig_hysteresis_extchange) 

extchange_vars_table <- arsenal::tableby(par_changed ~ stressor + 
                                           av_state + urge_escape + sui_thoughts + 
                                           other_escape + ext_change + 
                                           int_change, data = extchange_subset_vars,
                                         numeric.stats = c("mean", "median", "range"),
                                         test = FALSE, total = FALSE)

arsenal::labels(extchange_vars_table) <- c(par_changed = 'Varied Parameter', 
                                           stressor = "Stress",
                                           av_state = "Aversive State",
                                           urge_escape = "Urge to Escape",
                                           sui_thoughts = "Suicidal Thoughts",
                                           other_escape = "Other Escape Strategies",
                                           ext_change = "External-Focused Change",
                                           int_change = "Internal-Focused Change")

summary(extchange_vars_table, text = TRUE, title = "External Change - Variables")


################# av_state hysteresis ###################


# descriptives for aversive state hysteresis individuals
avstate_subset_overall <- data %>%
  filter(((pp_nr + 1) %in% sig_hysteresis_avstate) & ((pp_nr + 1) %in% bimodal)) %>%
  filter(time == 1)

# create table
avstate_table <- arsenal::tableby(par_changed ~ stress_lvl + par_value, 
                                    data = avstate_subset_overall, 
                                    numeric.stats = c("N", "mean", "median", "range"),
                                    test = FALSE) 

# create meaningful labels
arsenal::labels(avstate_table)  <- c(par_changed = 'Varied Parameter', 
                                       stress_lvl = "Stress Level",
                                       par_value = "Parameter Value")

# examine table
summary(avstate_table, text = TRUE, title = "Aversive State - Parameters")

avstate_subset_vars <- data %>%
  filter((pp_nr + 1) %in% sig_hysteresis_avstate) 

avstate_vars_table <- arsenal::tableby(par_changed ~ stressor + 
                                           av_state + urge_escape + sui_thoughts + 
                                           other_escape + ext_change + 
                                           int_change, data = avstate_subset_vars,
                                         numeric.stats = c("mean", "median", "range"),
                                         test = FALSE, total = FALSE)

arsenal::labels(avstate_vars_table) <- c(par_changed = 'Varied Parameter', 
                                           stressor = "Stress",
                                           av_state = "Aversive State",
                                           urge_escape = "Urge to Escape",
                                           sui_thoughts = "Suicidal Thoughts",
                                           other_escape = "Other Escape Strategies",
                                           ext_change = "External-Focused Change",
                                           int_change = "Internal-Focused Change")

summary(avstate_vars_table, text = TRUE, title = "Aversive State - Variables")

#------------------------------------------------------------------------------|
# PLOTTING                                                                 ####
#------------------------------------------------------------------------------|


################# limit cycle ###################

# set plot margins
par(mar = c(5, 5, 5, 7), xpd = TRUE)

# select data
cycle_data <- data %>%
  filter(pp_nr == 252) 

# get changepoint locations
cycle_cpts <- cpt_res[[253]]@cpts

# plot variables across time and set basic settings
plot(x = 0:40319, y = cycle_data$sui_thoughts, type = "l", xlab = "Time in Minutes", 
     ylab = "Value",main = "Limit Cycle (Simulation 253)", las = 1, lwd = 2, cex.axis = 0.75, 
     bty = "l")
abline(v = cycle_cpts, col = "tomato", xpd = FALSE)
lines(cycle_data$stressor, col = "deepskyblue")
lines(cycle_data$av_state, col = "darkviolet")
lines(cycle_data$other_escape, col = "chartreuse")

# add legend
cycle_cols <- c("black","deepskyblue", "darkviolet", "chartreuse")
cycle_labels <- c("Suicidal Thoughts", "Stress", 
                  "Aversive State", "Other Escape Strategies")
legend("topright", inset = c(-0.335, 0.2), lty = 1, lwd = 2, cex = 0.6, col = cycle_cols, 
       legend = cycle_labels, title = "Variables", bty = "n")

################# hysteresis ###################

# set plot margins
par(mar = c(5, 5, 5, 7), xpd = TRUE)

# select data
hyst_data <- data %>%
  filter(pp_nr == 633) 

# get changepoint locations
hyst_cpts <- cpt_res[[634]]@cpts

# plot variables across time and set basic settings
plot(x = 0:40319, y = hyst_data$sui_thoughts, type = "l", xlab = "Time in Minutes", 
     ylab = "Value", lwd = 2, main = "Hysteresis 1 (Simulation 633)", las = 1, cex.axis = 0.75, 
     bty = "l", ylim = c(0, max(hyst_data$stressor)))
abline(v = hyst_cpts, col = "tomato", xpd = FALSE, lwd = 2)
lines(hyst_data$stressor, col = "deepskyblue")
lines(hyst_data$av_state, col = "darkviolet")
lines(hyst_data$other_escape, col = "chartreuse")

# add legend
legend("topright", inset = c(-0.335, 0.2), lty = 1, lwd = 2, cex = 0.6, col = cycle_cols, 
       legend = cycle_labels, title = "Variables", bty = "n")

################# critical hysteresis ###################

# set plot margins
par(mar = c(5, 5, 5, 7), xpd = TRUE)

# select data
hyst2_data <- data %>%
  filter(pp_nr == 768) 

# get changepoint locations
hyst2_cpts <- cpt_res[[769]]@cpts

# plot variables across time and set basic settings
plot(x = 0:40319, y = hyst2_data$sui_thoughts, type = "l", xlab = "Time in Minutes", 
     ylab = "Value",main = "Hysteresis 2 (Simulation 769)", las = 1, cex.axis = 0.75, 
     bty = "l", ylim = c(0, max(hyst2_data$stressor)))
abline(v = hyst2_cpts, col = "tomato", xpd = FALSE, lwd = 2)
lines(hyst2_data$stressor, col = "deepskyblue")
lines(hyst2_data$av_state, col = "darkviolet")
lines(hyst2_data$other_escape, col = "chartreuse")

# add legend
legend("topright", inset = c(-0.335, 0.2), lty = 1, lwd = 2, cex = 0.6, col = cycle_cols, 
       legend = cycle_labels, title = "Variables", bty = "n")


################# converging on values ###################

# set plot margins
par(mar = c(5, 5, 5, 7), xpd = TRUE)

# select data
conv_data <- data %>%
  filter(pp_nr == 258) 

# get changepoint locations
conv_cpts <- cpt_res[[259]]@cpts

# plot variables across time and set basic settings
plot(x = 0:40319, y = conv_data$sui_thoughts, type = "l", xlab = "Time in Minutes", 
     ylab = "Value",main = "Convergence (Simulation 260)", las = 1, lwd = 2, cex.axis = 0.75, 
     bty = "l", ylim = c(0, max(conv_data$sui_thoughts)))
abline(v = conv_cpts, col = "tomato", xpd = FALSE, lwd = 2)
lines(conv_data$stressor, col = "deepskyblue")
lines(conv_data$av_state, col = "darkviolet")
lines(conv_data$other_escape, col = "chartreuse")

# add legend
legend("topright", inset = c(-0.335, 0.2), lty = 1, lwd = 2, cex = 0.6, col = cycle_cols, 
       legend = cycle_labels, title = "Variables", bty = "n")

################# false positive ###################

# set plot margins
par(mar = c(5, 5, 5, 7), xpd = TRUE)

# select data
fp_data <- data %>%
  filter(pp_nr == 850) 

# get changepoint locations
fp_cpts <- cpt_res[[851]]@cpts

# plot variables across time and set basic settings
plot(x = 0:40319, y = fp_data$sui_thoughts, type = "l", xlab = "Time in Minutes", 
     ylab = "Value",main = "False Positive (Simulation 851)", las = 1, lwd = 2, cex.axis = 0.75, 
     bty = "l", ylim = c(0, max(fp_data$stressor)))
abline(v = fp_cpts, col = "tomato", xpd = FALSE, lwd = 2)
lines(fp_data$stressor, col = "deepskyblue")
lines(fp_data$av_state, col = "darkviolet")
lines(fp_data$other_escape, col = "chartreuse")

# add legend
legend("topright", inset = c(-0.335, 0.2), lty = 1, lwd = 2, cex = 0.6, col = cycle_cols, 
       legend = cycle_labels, title = "Variables", bty = "n")

################# bic plot ###################

bic_frame <- tibble("Fit" = c("1-state", "2-state_avstate", "2-state_avstate_hyst", "2-state_extchange", 
                 "2-state_extchange_hyst", "3-state"),
       "BIC" = c(hmm1s_bic, hmm2s_avstate_bic, hmm2s_avstatehyst_bic, hmm2s_extchange_bic, 
               hmm2s_extchangehyst_bic, hmm3s_bic))
bic_frame$BIC <- round(bic_frame$BIC)

options(scipen = 999)
plot(1:6, bic_frame$BIC, xlab = "Fit", ylab = "BIC", type = "b", xaxt = "n", cex.axis = 0.5, 
     las = 1, bty = "n", ylim = c(-50000,-100000))
axis(1, at = 1:6, labels = bic_frame$Fit, cex.axis = 0.5)
