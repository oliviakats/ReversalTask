

tidy_cannon <- function(data) {
  data <- data %>% 
    select(!c(build, experimentName, list.Condition.currentvalue)) %>% 
    group_by(subject, time) %>% ## be by subject
    dplyr::filter(trialcode == "cannon_outcome") %>% filter(practiceblock == 6) %>% ##only data from trials
    mutate(picture.shield.stimulusonset, shieldsize = parse_number(as.character(picture.shield.stimulusonset))) %>% ## make shield size numeric
    mutate_if(is.integer, as.numeric) %>% #convert all integers to numeric
    mutate(cond = as.factor(cond)) %>% mutate(driftConc = 30)
  
  #convert non-numeric columns  to numeric
  cols.char <-c("subject","cond", "condcolor", "date", "time", "blockcode", "trialcode", "shieldsize")

  data <- data %>% as_tibble() %>% mutate(across(-cols.char, ~as.numeric(as.character((.)))))
  sapply(data, class)
  return(data)
}

create_vars_reversal <- function(data){
  if(as.Date(data$date) < "2021-08-31"){
  data %>%
      group_by(subject, block_number) %>%
      mutate(numbercorrect = cumsum(as.numeric(isResponseCorrect))) %>%
      ungroup() %>% 
      group_by(subject, block_number, task_phase, grp = lag(cumsum(isResponseCorrect == -1), default = 0)) %>%
      mutate(ConsecutiveCorrect = ifelse(isResponseCorrect == -1, 0, cumsum(isResponseCorrect))) %>%
      mutate(ConsecutiveCorrect = ifelse(isResponseCorrect == 0, NA, ConsecutiveCorrect)) %>%
      filter(!block_number > 4) %>%
      ungroup() %>% select(-grp)
  } else if(as.Date(data$date) > "2021-08-31"){
    #data <-  data[,as.numeric(trial_number, reversalnumber, trial.choice.latency, trial.choice.response, total_trialnum)]
    data <- data %>% mutate(trial_number = as.numeric(as.character(trial_number)),
                            block_number = as.numeric(as.character(block_number))) %>% 
                              group_by(subject, block_number, trial_number) %>% ##group by subject and block to prevent weirdness
      arrange(subject, block_number, trial_number) %>% ## arrange in order based on grouping
      mutate(rt = trial.choice.latency) %>% ## combine latencies to create overall trial latency
      mutate(trial_response = dplyr::recode(trial.choice.response, 
                                     `36` = "right",
                                     `33` = "left",
                                     `0` = "noresponse")) %>%  #change name of response variable
      mutate(trial_response_numeric = dplyr::recode(trial.choice.response, 
                                            `36` = 2,
                                            `33` = 1,
                                            `0` = 0)) %>%  #change name of response variable
      mutate(rightleftcorrect_numeric = dplyr::recode(rightleftcorrect, 
                                                    "right" = 2,
                                                    "left" = 1)) %>%  #change name of response variable
      mutate(reversal_trial = ifelse(as.numeric(trial_number) == as.numeric(reversalnumber), as.numeric(trial_number), NA)) %>% #change name of response variable
      group_by(subject, block_number, task_phase) %>%
      arrange(subject) %>%
      mutate(phase_trialnum = ifelse(task_phase == "Reversal", cumsum(task_phase == "Reversal"), cumsum(task_phase == "Acquisiton"))) %>%  ## number of trials since reversal
      mutate(numbercorrect_phase = ifelse(task_phase == "Reversal", as.numeric(cumsum(isResponseCorrect == 1)), as.numeric(cumsum(isResponseCorrect == 1)))) %>% ## number of correct choices by phase
      mutate(ResponseCorrect = ifelse(rightleftcorrect_numeric ==  trial_response_numeric, 1, 0)) %>% ## transform to numeric
      mutate(reached_criterion = ifelse(as.numeric(numbercorrect_phase) == 8, phase_trialnum, NA)) %>% ## did subjects get 8 consecutive answers correct
      mutate(percent_correct_phase = numbercorrect_phase/max(phase_trialnum))  %>% # percent of correct choices by phase (acquisiton or reversal)
      ungroup() %>%
      group_by(subject, block_number) %>%
      mutate(numbercorrect = cumsum(ResponseCorrect),
             block_number = as.numeric(block_number)) %>%
      ungroup() %>%
      mutate(Feedback = ifelse("CF" %in% responseAndFeedbackCategory, "correct_feedback", "incorrect_feedback")) %>%
      mutate(Response = ifelse("CR" %in% responseAndFeedbackCategory, "correct_response", "incorrect_response"))
      }
  if (trim_cols == TRUE){
    drop <- c("stimulusitem1", "trialcode", 
              "picture.correctStim.currentvalue", 
              "picture.incorrectStim.currentvalue", "correctKey", 
              "incorrectKey", "trial.choice.percentcorrect",
              "trial.choice.response", "trial.choice.latency" )
    df = data[,!(names(data) %in% drop)]
  }
}

check_tidy <- function(data){
  check <- data %>% group_by(subject) %>%
  summarise(count = n_distinct(time),
            no_resp = )
  return(check)
}

tidy_reversal <- function(data) {
  if(as.Date(data$date) < "2021-08-31"){
    data <- data %>% 
      select(!c(build, experimentName, totalcorrect)) %>% 
      group_by(subject, time) %>% ## be by subject
      filter(!stimulusitem1 == "Press SPACE to continue")
    data <- as.data.table(data)
    data <- data[ , correctstim_name:= shift(stimulusitem1, 1)]
    data <- data %>% 
      mutate_if(is.integer, as.numeric) %>%
      filter(!correctstim_name== "+")
  } else if (as.Date(data$date) > "2021-08-31"){
    data <- data %>% 
      select(!c(build, experimentName)) %>% 
      group_by(subject, time) %>% ## be by subject
      filter(!stimulusitem1 == "Press SPACE to continue")
    data <- as.data.table(data)
    data <- data[ , correctstim_name:= shift(stimulusitem1, 1)]
    data <- data %>% 
      mutate_if(is.integer, as.numeric)%>%
      filter(!correctstim_name== "+")
  }
  return(data)
}

discrep <- function(angmu, r) {
  if(is.na(angmu)){
    phi <- NA
  } else if (is.na(angmu)){
    phi <- NA
  } else {
    phi <- abs(angmu - r) %% 360
    if (phi > 180) { phi <- 360 - phi }
    
  }
  return(phi)
}

create_vars <- function(data, set_vars){
  if (trim_cols == TRUE){
    drop <- c("block.InstructionBlock.timestamp", "trial.begin_block.timestamp",
              "trial.mainloop.timestamp","trial.placeshield_mouse.timestamp",
              "trial.showPE.timestamp","trial.cannon_outcome.timestamp", 
              "picture.shield.currentitem")
    df = data[,!(names(data) %in% drop)]
  }
  if("total_trialnum" %in% set_vars) {
    data <- data %>% group_by(subject, cond) %>% 
      mutate(total_trialnum = ifelse(cond == "ODDBALL", cumsum(cond == "ODDBALL"), cumsum(cond == "CHANGEPOINT")))
  }
  if("total_changepoints" %in% set_vars) {
    data <- data %>% group_by(subject, cond) %>% 
      mutate(total_trialnum = ifelse(cond == "ODDBALL", n(data$changepoint), cumsum(cond == "CHANGEPOINT")))
  }
  if("perf" %in% set_vars) {
    data['perf'] <- NA
    for(r in 1:nrow(data)){
      if(is.na(data$outcomeindex[r])){
        data$perf[r] <- 0
      } else if(is.na(data$outcomeindex[r])){
        data$perf[r] <- 0
      } else if(data$outcomeindex[r] == 5) {
        data$perf[r] <- .5
      } else {
        data$perf[r] <- 0
      }
    }
  }
  if("predErr" %in% set_vars) {
    data['predErr'] <- NA
    for(r in 1:nrow(data)){
      if(is.na(data$placementAngle[r])){
        data$predErr[r] <- NA
      } else if (is.na(data$outcome[r])){
        data$predErr[r] <- NA
      } else {
      data$predErr[r] <- discrep(as.numeric(data$outcome[r]), as.numeric(data$placementAngle[r]))} # deltat: prediction error of current trial. need to use lagged PE?
      }
    }
  if("distMean" %in% set_vars){
    data$distMean <- NA
    for(r in 1:nrow(data)){
      if(is.na(data$placementAngle[r])){
        data$distMean[r]
      } else if(is.na(data$angmu[r])){
        data$distMean[r]
      } else{
      tmp <- discrep(data$angmu[r], data$placementAngle[r])
      data$distMean[r] <- tmp
      }
    }
  }
  if ("catch_miss" %in% set_vars) {
    data$catch_miss <- data$outcomeindex 
    for(r in 1:nrow(data)){
      if(data$catch_miss[r] == 1){ ## check 
        if(is.na(data$trial.placeshield_mouse.latency[r])){
          data$catch_miss[r] <- "noresp"
          data$distMean[r] <- NA
          data$predErr[r] <- NA
          data$placementAngle[r] <- NA
          data$trial.placeshield_mouse.latency[r] <- NA
        }
        else if (data$trial.placeshield_mouse.latency[r] == 2500){
          data$trial.placeshield_mouse.latency[r] <- NA
        } 
      } else if (data$catch_miss[r] == 5){
        data$catch_miss[r] <- "hit"
      } else if (data$catch_miss[r] == 6){
        data$catch_miss[r] <- "miss"
      } 
    }
    for(r in 1:nrow(data)) {
      if(is.na(data$trial.placeshield_mouse.latency[r])){
        data$catch_miss[r] <- "noresp"
        data$distMean[r] <- NA
        data$predErr[r] <- NA
        data$placementAngle[r] <- NA
        data$trial.placeshield_mouse.latency[r] <- NA
        }
      }
    }
  if ("changepoint" %in% set_vars){
    data$changepoint <- NA
    for(r in 1:nrow(data)){
     data$changepoint[r] <- ifelse(data$StaySwitch[r] == 1, data$trialnum[r], NA)
      }
  }
  data$cond_num <- NA
  for(r in 1:nrow(data)){
    data$cond_num[r] <- ifelse(data$cond[r] == "ODDBALL", 1, 2)
  }
  return(data)
}


check_irreg <- function(data){
  data$Irreg <- NA
  for(r in 1:nrow(data)){
    if(is.na(data$predErr[r])) {
      if(data$outcomeindex[r] != 1){
        data$Irreg[r] <- "CHECK_NA"
      } else {
        data$Irreg[r] <- NA
      }
    } else if(is.na(data$hitmiss[r])){
      data$Irreg[r] <- NA
    } else if(is.na(data$shieldsize[r])) {
      data$Irreg[r] <- NA
    } else {
        if (data$predErr[r] <= (data$shieldsize[r]/2) && data$hitmiss[r] == 0){
        data$Irreg[r] <- "CHECK_MISS"
      } else if (data$predErr[r] >= 30 & data$hitmiss[r] == 1){
        data$Irreg[r] <- "CHECK_HIT"
      }
    }
  }
  return(data)
}
  



relabel_qnames <- function(data, string){
  map <- extract_colmap(data)
  cols <- c("RandomID")
  labels <- c("RandomID")
  for (i in 1:nrow(map)){
    if (grepl(paste0(string), as.character(map[i,2]))){
      cols <- append(cols, as.character(map[i,1]))
      labels <- append(labels, as.character(map[i,2]))
    }
  }
}

