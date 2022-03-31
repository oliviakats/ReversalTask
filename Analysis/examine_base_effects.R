pacman::p_load(tidyverse, readr, janitor, ggplot2,wesanderson, cowplot, flextable, 
               plotly, emmeans, data.table, dependlab,
               lme4, car, AICcmodavg, gtools, lattice)
setwd("~/github_repos/ReversalTask/")
load("data/reversal_proc.RData")

within_effects <- reversal %>% # dataframe of within-subject effects: this is the main stuff we're using 
  select(subject, 
         rt, #reaction time
         task_phase, #acquisition or reversal
         index_correctChoice,
         isResponseCorrect, #acc
         isFeedbackAccurate, #did they get correct feedback
         total_trialnum,
         trial_number,
         totalcorrect,
         block_number, #successful catch (1) or missed (1)?
         Feedback,
         Response,
         responseAndFeedbackCategory,
         rightleftcorrect, # num of Trials after cp
         correctstim_name, #absolute PE on previous trial
         centsearned, #total trial number (NOTE: currently by block, that's wrong)
         trial_response #did they choose left or right
  ) %>% mutate(prevRT = lag(rt),#last update
               prev_choice = lag(trial_response),
               fastRT = ifelse(rt <= 150, 1, 0),
               rightleftcorrect_num = as.character(rightleftcorrect),
               trial_response_num = as.character(trial_response),
               ResponseCorrect_num = as.character(isResponseCorrect),
               FeedbackAccurate_num = as.character(isFeedbackAccurate),
               rt_log = log(rt),
               rt_inv = -1/(rt)*1000,
               Feedback = ifelse(responseAndFeedbackCategory %in% c( "CRCF", "IRCF"), "correct_feedback", "incorrect_feedback"),
               Response = ifelse(responseAndFeedbackCategory %in% c("CRCF", "CRIF"), "correct_response", "incorrect_response"),
               prevResponse = lag(Response),#last update
               prevFeedback = lag(Feedback)) #last update

within_effects <- within_effects %>% dplyr::mutate(rightleftcorrect_num = dplyr::recode(rightleftcorrect_num, "right" = 1, "left" = 2))
within_effects <- within_effects %>%  mutate(trial_response_num = dplyr::recode(trial_response_num, "right" = 1, "left" = 2, "noresponse" = 0))
within_effects <- within_effects %>% mutate(fastRT = ifelse(rt <= 150, "fast", "notfast"))
         
cor_heatmap(within_effects %>% select(-c(rt_log, rt_inv)))

#relationships to note:
#so block is related to faster RTs, and ovbiously total trial number
#more $$ related to slower RTs, positively related to number correct. confused there
#inaccurate responses are related to faster RTs
#side correct is relatd to trial_response
# PredT and totTrial - seems like later trials = faster RTs. Makes sense if people feel like they've learned location of mu
# positive corr. between fastRT and later trials. 
# neg. corr. between fastRT and base RT - so people with more fastRTs have fast RTs overall
#prevRt and rt are pretty correlated - good to know

Hmisc::rcorr(as.matrix(within_effects %>% select(-c(subject,task_phase, 
                                                    index_correctChoice, isResponseCorrect,
                                                    isFeedbackAccurate,  rightleftcorrect, 
                                                    correctstim_name, trial_response,
                                                    prev_choice, fastRT, ResponseCorrect_num,
                                                    FeedbackAccurate_num, prevRT
                                                    ))))

sqrt(psych::smc(as.matrix(within_effects %>% select(-c(subject,task_phase, 
                                                       index_correctChoice, isResponseCorrect,
                                                       isFeedbackAccurate,  rightleftcorrect, 
                                                       correctstim_name, trial_response,
                                                       prev_choice, fastRT, ResponseCorrect_num,
                                                       FeedbackAccurate_num, prevRT)) %>% na.omit())))

#not seeing anything new here

#examine by accuracy and predT
hist(within_effects$rt) #interesting on the early side - what's up with that bin?
hist(within_effects$rt_inv) #interesting on the early side - what's up with that bin?
hist(within_effects$rt_log) #interesting on the early side - what's up with that bin?

histogram(~ rt | fastRT*trial_response, within_effects) #not different across side, lots of fast Rts?
histogram(~ rt | task_phase, within_effects) #similar across acquisiton/reversal

#looking at similar effects on acc side
histogram(~ isResponseCorrect | task_phase, within_effects) #a little more on the acq side
histogram(~ isResponseCorrect | fastRT, within_effects) #less accurate when fast, for sure

histogram(~ rt | Feedback, within_effects) #a little more on the acq side
histogram(~ rt | isResponseCorrect, within_effects) #less accurate when fast, for sure

save(within_effects, file = 'Data/base_effects_data.RData')


