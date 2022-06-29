##### Load Data and setwd
ll = FALSE
if (ll == TRUE) {
  home_directory <- "/proj/mnhallqlab/users/sophie/ReversalTask/"
  data_dir <- file.path(paste0(home_directory, "data/"))
} else {
  home_directory <- "~/github_repos/ReversalTask/"
  data_dir <- file.path(paste0(home_directory, "Data/"))
}
load(file = paste0(data_dir,'base_effects_data.RData'))

data <- df %>% dplyr::filter(rt !=0) %>% dplyr::filter(!isResponseCorrect == 0) %>% filter(subject != 270470)

data$rt_log <- log(data$rt)
data$rt_inv<- -1/1000*(data$rt)
data <- data %>% 
  mutate(trial_z = scale(trial_number),
         total_trial_z = scale(total_trialnum),
         task_phase = as.factor(task_phase),
         prevFeedback = as.factor(prevFeedback),
         phase_trialnum_z = scale(phase_trialnum),
         since_reversal = scale(-100/phase_trialnum),
         ResponseCorrect_num = dplyr::recode(ResponseCorrect_num, "-1" = 0, "1" = 1)) %>% 
  group_by(subject) %>% mutate(prevResponse = as.factor(lag(Response)),
                               prevrt_log = lag(rt_log)) %>% ungroup()

data$stim_num <- as.numeric(data$stim_num)
data$index_correctChoice <- as.numeric(data$index_correctChoice)
data$index_incorrectChoice <- as.numeric(data$index_incorrectChoice)

#stim_ident
data <- data %>% group_by(subject, task_phase_character) %>%
  mutate(stim_num = ifelse(isResponseCorrect == 1, index_correctChoice, index_incorrectChoice)) %>% ungroup() %>%
  mutate(choose_A = case_when(stim_num == index_correctChoice & task_phase_character == "Reversal" ~ "B",
                              stim_num == index_correctChoice & task_phase_character == "Acquisiton" ~ "A",
                              stim_num == index_incorrectChoice & task_phase_character == "Reversal" ~ "A",
                              stim_num == index_incorrectChoice & task_phase_character == "Acquisiton" ~ "B")) 

data$choose_A <- as.factor(data$choose_A)
data$prev_choice <- lag(data$choose_A)
#random shorter things
data$phase_early <- ifelse(data$phase_trialnum_z <0, "early", "late")
save(data, file = paste0(data_dir, "MNH_rewrite_data.RData"))

data$StaySwitch <- as.factor(ifelse(data$choose_A != data$prev_choice, "Switch", "Stay"))
data$StaySwitch_lag <-lag(data$StaySwitch)


#final choose_reversal model
choose_rev_2 <- glmer(choose_A ~ task_phase*prevFeedback + task_phase*prev_choice +  
                      prev_choice*prevFeedback + prev_choice*since_reversal +  
                      task_phase*since_reversal + rt_log + prevrt_log + 
                      since_reversal + (1 + prev_choice | subject),
                    family="binomial", control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e4)), 
                    nAGQ = 1, data = data, na.action = na.exclude)

choose_rev_3 <- glmer(choose_A ~ task_phase*prevFeedback + task_phase*prev_choice +  
                        prev_choice*prevFeedback + prev_choice*since_reversal +  
                        task_phase*since_reversal + rt_log + prevrt_log + 
                        since_reversal + (0 + prev_choice | subject),
                      family="binomial", control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e4)), 
                      nAGQ = 1, data = data, na.action = na.exclude)

choose_rev_4 <- glmer(choose_A ~ task_phase*prev_choice +  
                        prev_choice*prevFeedback + prev_choice*since_reversal +  
                        task_phase*since_reversal + rt_log + prevrt_log + 
                        since_reversal + (0 + prevFeedback | subject:prev_choice),
                      family="binomial", control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e4)), 
                      nAGQ = 1, data = data, na.action = na.exclude)

choose_rev_5 <- glmer(choose_A ~ prev_choice*prevFeedback + prev_choice*since_reversal +  
                        task_phase*since_reversal + rt_log + prevrt_log + 
                        since_reversal + (0 + prevFeedback | subject:prev_choice),
                      family="binomial", control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e4)), 
                      nAGQ = 1, data = data, na.action = na.exclude)

minf <- list()
minf[["choose_rev"]] <- c(fixed="task phase, previous Feedback, 
                           previous choice, time since reversal, 
                           reaction time, previous reaction time", 
                          l2="Intercept/Subject:task_phase", 
                          max_vif = as.character(max(c(car::vif(choose_rev)))))
minf[["choose_rev_2"]] <- c(fixed="task phase, previous Feedback, 
                           previous choice, time since reversal, 
                           reaction time, previous reaction time", 
                            l2="Intercept, previous choice/Subject",
                            max_vif = as.character(max(c(car::vif(choose_rev_2)))))

minf[["choose_rev_3"]] <- c(fixed="task phase, previous Feedback, 
                           previous choice, time since reversal, 
                           reaction time, previous reaction time", 
                            l2="previous feedback/Subject:feedback",
                            max_vif = as.character(max(c(car::vif(choose_rev_3)))))

# #minf[["choose_rev_5"]] <- c(fixed="task phase, previous Feedback, 
#                            previous choice, time since reversal, 
#                            reaction time, previous reaction time", 
#                              l2="previous choice, time since reversal /Subject",
#                             max_vif = as.character(max(c(car::vif(choose_rev_5)))))
mlist <- list()
mlist[["choose_rev"]] <- choose_rev
mlist[["choose_rev_2"]] <- choose_rev_2
mlist[["choose_rev_3"]] <- choose_rev_3
#mlist[["choose_rev_4"]] <- NULL
#mlist[["choose_rev_5"]] <- choose_rev_5
table_choose_rev_random <- dependlab::render_flex_aictab(minfo = minf, mlist = mlist) %>% autofit()
#final stay_switch model
stay_switch <- glmer(StaySwitch ~ rt_log + prevrt_log + task_phase*since_reversal + task_phase*prev_choice + 
                       prevFeedback*StaySwitch_lag + (0 + prevFeedback + prevrt_log | subject), 
                     family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1, data = data)

stay_switch_2 <- glmer(StaySwitch ~ rt_log + prevrt_log + task_phase*since_reversal + task_phase*prev_choice + 
                         prevFeedback*StaySwitch_lag + (0 + prevFeedback + since_reversal | subject), 
                       family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1, data = data)
stay_switch_3 <- glmer(StaySwitch ~ rt_log + prevrt_log + task_phase*since_reversal + task_phase*prev_choice + 
                         prevFeedback*StaySwitch_lag + (0 + prevFeedback | subject) + (0 + since_reversal | subject:task_phase), 
                       family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1, data = data)
stay_switch_4 <- glmer(StaySwitch ~ rt_log + prevrt_log + StaySwitch_lag + task_phase*since_reversal + task_phase*prev_choice + 
                         prevFeedback*prev_choice + (0 + prevFeedback | subject) + (0 + since_reversal | subject:task_phase), 
                       family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1, data = data)

stay_switch_5 <- glmer(StaySwitch ~ rt_log + prevrt_log + StaySwitch_lag + task_phase*since_reversal + task_phase*prev_choice + 
                         prevFeedback*prev_choice + (0 + prevFeedback | subject) + (0 + since_reversal | subject:task_phase), 
                       family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ = 1, data = data)
mlist <- list()
mlist[["stay_switch"]] <- stay_switch
mlist[["stay_switch_2"]] <- stay_switch_2
mlist[["stay_switch_3"]] <- stay_switch_3
mlist[["stay_switch_4"]] <- stay_switch_4
mlist[["stay_switch_5"]] <- stay_switch_5

minf <- list()
minf[["stay_switch"]] <- c(fixed="RT, prevRT", l2="previous feedback, previous RT /Subject", 
                           ixns = ' time since reversal x task phase, task phase x previous choice,  
                            previous feedback x stayswitch_lag ', 
                           max_vif =as.character(max(c(car::vif(stay_switch)))))
minf[["stay_switch_2"]] <- c(fixed="RT, prevRT", l2="previous feedback, since_reversal /Subject", 
                             ixns = ' time since reversal x task phase, task phase x previous choice,  
                            previous feedback x stayswitch_lag ', 
                             max_vif =as.character(max(c(car::vif(stay_switch_2)))))
minf[["stay_switch_3"]] <- c(fixed="RT, prevRT", l2="previous feedback/subject, since_reversal /Subject:taskphase", 
                             ixns = ' time since reversal x task phase, task phase x previous choice,  
                            previous feedback x stayswitch_lag ', 
                             max_vif =as.character(max(c(car::vif(stay_switch_3)))))
minf[["stay_switch_4"]] <- c(fixed="RT, prevRT, stayswitch_lagged", l2="previous feedback, previous RT /Subject", 
                             ixns = ' time since reversal x task phase, task phase x previous choice,  
                            previous feedback x previous choice ', 
                             max_vif =as.character(max(c(car::vif(stay_switch_4)))))
minf[["stay_switch_5"]] <- c(fixed="RT, prevRT, stayswitch_lagged", l2="previous feedback /Subject, time since reversal within subject with task phase", 
                             ixns = ' time since reversal x task phase, task phase x previous choice,  
                            previous feedback x previous choice ', 
                             max_vif =as.character(max(c(car::vif(stay_switch_5))))) 

table_stay_switch_random <- dependlab::render_flex_aictab(minfo = minf, mlist = mlist) %>% autofit()

#fully transformed data frame
save(table_stay_switch, table_stay_switch_random, table_choose_rev_base, 
     table_choose_rev_random, mod10, data, choose_rev_3, 
     stay_switch_4, file = paste0("~/github_repos/ReversalTask/Results/Reversal_models_6.29.RData"))
load(file = paste0("~/github_repos/ReversalTask/Results/MNH_rewrite_data.RData"))


## get coefficient for random effects structure
mod_matrix <- as.tibble(ranef(stay_switch_4), names= "term") %>% 
  separate(grpvar, into = c("base", "grouping_factor"), sep = ":") %>% 
  separate(term, into = c("term","type"), sep = "back") %>% 
  separate(grp, into = c("subject", "phase_label")) %>%
  mutate(specifier = ifelse(is.na(type), phase_label, type),
         grouping_factor = ifelse(is.na(grouping_factor), base, grouping_factor))


mod_matrix <- mod_matrix %>% dplyr::select(!c(type, phase_label, base))
coefficients <- mod_matrix %>% pivot_wider(
  names_from = c(grouping_factor, term, specifier),
  values_from = c(condsd, condval))

coefficients

with_coefs_stay_switch <- left_join(coefficients, data, by = 
                          "subject")

save(with_coefs_stay_switch, coefficients, file = "~/github_repos/ReversalTask/Data/stay_switch_df_with_coefficients.RData")



mod_matrix <- as.tibble(ranef(choose_rev), names= "term") %>% 
  separate(term, into = c("term","type"), sep = "oice") %>%
  separate(grp, into = c("subject","task_phase"), sep = ":")

coefficients <- mod_matrix %>% pivot_wider(
  names_from = c(type, task_phase),
  values_from = c(condsd, condval))

coefficients$extra <- NULL

with_coefs_choose_rev <- left_join(coefficients, data, by = "subject")
save(coefficients, with_coefs_choose_rev, file = "~/github_repos/ReversalTask/Data/choose_rev_df_with_coefficients.RData")
