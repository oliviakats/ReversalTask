ll = FALSE
if (ll == TRUE) {
  home_directory <- "/proj/mnhallqlab/users/sophie/ReversalTask/"
  data_dir <- file.path(paste0(home_directory, "data/"))
} else {
  home_directory <- "~/github_repos/ReversalTask/"
  data_dir <- file.path(paste0(home_directory, "Data/"))
}
load(file = paste0(data_dir,'base_effects_data.RData'))
load("~/github_repos/Cannon_Task/Data/qualtrics_qata/PUBS_Batch3_QR_Proc.Rdata")

selected <- measures_df %>% dplyr::select(RandomID, age, gen, PAI_Tot, PAI_AI,
                                          PAI_NR,
                                          PAI_SH,
                                          PID_neg_aff,
                                          PID_anh,
                                          PID_anx,
                                          PID_attn,
                                          PID_callous,
                                          PID_deceit,
                                          PID_depres,
                                          PID_distr,PID_ecc,PID_emo_lab,PID_grnd,
                                          PID_host,PID_impuls,PID_int_avd,PID_irr,
                                          PID_man,PID_perc_dysreg, PID_wthdrwl,
                                          PID_persev,PID_rest_aff,PID_perf,PID_rt,
                                          PID_sep_insec,PID_sub,PID_sus,PID_unusual,
                                          PID_neg_aff,PID_detach,PID_antag,PID_disinhib,
                                          PID_psycho, STAI_Tot,STAI_T,STAI_S,PSWQ_Tot,
                                          BFAS_Tot,BFAS_V,BFAS_W, age) %>% 
  rename(subject = RandomID) %>% mutate(subject = as.character(subject)) 
selected <- selected %>% mutate(sex = dplyr::recode(gen, '1' = "female", '2' = "male"))
 
df <- left_join(selected, within_effects, by = "subject", copy = TRUE)
                                                                                                                                                             

save(df, file = paste0(data_dir,'base_effects_data.RData'))
