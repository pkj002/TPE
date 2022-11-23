rm(list=ls())
library(dplyr)
library(purrr)

cnt <- 'ethiopia'
in_dir <- paste0('E:/Prakash/dssat_outputs/',cnt,'/bd_random_1lay/')
dd1 <- list.dirs(in_dir, full.names = F, recursive = F)
dd1 <- sort(as.numeric(dd1))
var_o <- list(); var_c <- list()

for(i in 1:length(dd1)){ ## for all grids
  print(i)
  ## Evaluate.csv
  ## 1. Orig
  if(!dir.exists(paste0(in_dir,dd1[i]))) {
    next
  } 
  
  if (!file.exists(paste0(in_dir,dd1[i],'/orig_Calima/Evaluate_',cnt,'_orig.RDS'))) {
    next
  } else {  
    ev<- readRDS(paste0(in_dir,dd1[i],'/orig_Calima/Evaluate_',cnt,'_orig.RDS'))
    ev[ev==-99]=NA
    ev1<- ev %>% group_by(RUN) %>% summarise(pwam=mean(PWAMS,na.rm=T), cwam=mean(CWAMS,na.rm=T), lai=mean(LAIXS,na.rm=T), yld=mean(HWAMS,na.rm=T))
  }
     
  ## 2. compacted
  if (!file.exists(paste0(in_dir,dd1[i],'/bd10_Calima/Evaluate_',cnt,'_bd10.RDS'))) {
    next
  } else { 
    ev_c<- readRDS(paste0(in_dir,dd1[i],'/bd10_Calima/Evaluate_',cnt,'_bd10.RDS'))
  }
  ev_c[ev_c==-99]=NA
  ev_c1<- ev_c %>% group_by(RUN) %>% summarise(pwam=mean(PWAMS,na.rm=T), cwam=mean(CWAMS,na.rm=T), lai=mean(LAIXS,na.rm=T), yld=mean(HWAMS,na.rm=T))
  dif <- (ev_c1[,2:5]-ev1[,2:5])*100/ev1[,2:5]
  sd_dif <- apply(dif,c(2),sd,na.rm=T)
 
  ## PlantGro.csv
  # 1. Orig
  if (!file.exists(paste0(in_dir,dd1[i],'/orig_Calima/PlantGro_',cnt,'_orig.RDS'))) {
    next
  } else {
    ptg<-readRDS(paste0(in_dir,dd1[i],'/orig_Calima/PlantGro_',cnt,'_orig.RDS'))
  }
  ptg[ptg==-99]=NA
  pt1<- ptg %>% group_by(RUN) %>% summarise(mx_hipd=max(HIPD), mx_slad=max(SLAD), mx_lnd=max(LN.D), mx_rdpd=max(RDPD), mx_rwad=max(RWAD),
                                            mx_rl1d=max(RL1D), mx_rl2d=max(RL2D), mx_rl3d=max(RL3D), mx_rl4d=max(RL4D))
  
  ## 2. compact
  if (!file.exists(paste0(in_dir,dd1[i],'/bd10_Calima/PlantGro_',cnt,'_bd10.RDS'))) {
    next
  } else {
    ptg_c<-readRDS(paste0(in_dir,dd1[i],'/bd10_Calima/PlantGro_',cnt,'_bd10.RDS'))
  }
  
  ptg_c[ptg_c==-99]=NA
  ptg_c1<- ptg_c %>% group_by(RUN) %>% summarise(mx_hipd=max(HIPD), mx_slad=max(SLAD), mx_lnd=max(LN.D), mx_rdpd=max(RDPD), mx_rwad=max(RWAD),
                                                 mx_rl1d=max(RL1D), mx_rl2d=max(RL2D), mx_rl3d=max(RL3D), mx_rl4d=max(RL4D))
  difpg <- (ptg_c1[,2:10] - pt1[,2:10])*100/pt1[,2:10]
  ptg_sd <- apply(difpg ,c(2),sd,na.rm=T)
  
  ## average evapotranspiratoin
  ## 1. Orig
  if (!file.exists(paste0(in_dir,dd1[i],'/orig_Calima/summary_',cnt,'_orig.RDS'))) {
    next
  } else {
    et<- readRDS(paste0(in_dir,dd1[i],'/orig_Calima/summary_',cnt,'_orig.RDS'))
  }
  et[et==-99]=NA
  et <- et %>% mutate(ET=PRCP/(MDAT-SDAT))
 
  ## 2. Compact
  if (!file.exists(paste0(in_dir,dd1[i],'/bd10_Calima/summary_',cnt,'_bd10.RDS'))) {
    next
  } else {
    et_c<- readRDS(paste0(in_dir,dd1[i],'/bd10_Calima/summary_',cnt,'_bd10.RDS'))
  }
  et_c[et_c==-99]=NA
  et_c <- et_c %>% mutate(ET=PRCP/(MDAT-SDAT))
  et_dif <- (et_c$ET - et$ET)*100/et$ET
  
  et_sd <- sd(et_dif)
  
  var_o[[dd1[i]]] <- data.frame(t(sd_dif), t(ptg_sd), et_sd)
}

all_pts_o <- map_df(var_o, ~as.data.frame(.x), .id = 'grids') %>% mutate_if(is.numeric, ~round(.,2))

saveRDS(all_pts_o, paste0('//catalogue/BaseLineDataCluster01/temp/compac_all_grids/plts_comb/all_grids/sd_dif_',cnt,'.RDS'))
