rm(list=ls())
library(dplyr)
library(purrr)

in_dir <- 'E:/Prakash/dssat_outputs/uganda/bd_random_lay1/'#11068/'
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
  
  if (!file.exists(paste0(in_dir,dd1[i],'/orig_Calima/Evaluate_uganda_orig.RDS'))) {
    next
  } else { 
  ev<- readRDS(paste0(in_dir,dd1[i],'/orig_Calima/Evaluate_uganda_orig.RDS'))
  ev[ev==-99]=NA
}
ev1<- ev %>% group_by(RUN) %>% summarise(pwam=mean(PWAMS,na.rm=T), cwam=mean(CWAMS,na.rm=T), lai=mean(LAIXS,na.rm=T), yld=mean(HWAMS,na.rm=T))
 ev_av1<-apply(ev1, c(2),mean,na.rm=T)
 ev_av1<-ev_av1[2:5]

 ## 2. compacted
 if (!file.exists(paste0(in_dir,dd1[i],'/bd10_Calima/Evaluate_uganda_bd10.RDS'))) {
   next
 } else { 
 ev_c<- readRDS(paste0(in_dir,dd1[i],'/bd10_Calima/Evaluate_uganda_bd10.RDS'))
 }
 ev_c[ev_c==-99]=NA
 ev_c1<- ev_c %>% group_by(RUN) %>% summarise(pwam=mean(PWAMS,na.rm=T), cwam=mean(CWAMS,na.rm=T), lai=mean(LAIXS,na.rm=T), yld=mean(HWAMS,na.rm=T))
 ev_c1<-apply(ev_c1, c(2),mean,na.rm=T)
 ev_c1 <- ev_c1[2:5]

## PlantGro.csv
 # 1. Orig
 if (!file.exists(paste0(in_dir,dd1[i],'/orig_Calima/PlantGro_uganda_orig.RDS'))) {
   next
 } else {
ptg<-readRDS(paste0(in_dir,dd1[i],'/orig_Calima/PlantGro_uganda_orig.RDS'))
}
ptg[ptg==-99]=NA
pt1<- ptg %>% group_by(RUN) %>% summarise(mx_hipd=max(HIPD), mx_slad=max(SLAD), mx_lnd=max(LN.D), mx_rdpd=max(RDPD), mx_rwad=max(RWAD),
                                          mx_rl1d=max(RL1D), mx_rl2d=max(RL2D), mx_rl3d=max(RL3D), mx_rl4d=max(RL4D))

ptg_av1 <- apply(pt1,c(2),mean,na.rm=T)
ptg_av1 <- ptg_av1[2:10]

## 2. compact
if (!file.exists(paste0(in_dir,dd1[i],'/bd10_Calima/PlantGro_uganda_bd10.RDS'))) {
  next
} else {
ptg_c<-readRDS(paste0(in_dir,dd1[i],'/bd10_Calima/PlantGro_uganda_bd10.RDS'))
}
ptg_c[ptg_c==-99]=NA
ptg_c1<- ptg_c %>% group_by(RUN) %>% summarise(mx_hipd=max(HIPD), mx_slad=max(SLAD), mx_lnd=max(LN.D), mx_rdpd=max(RDPD), mx_rwad=max(RWAD),
                                          mx_rl1d=max(RL1D), mx_rl2d=max(RL2D), mx_rl3d=max(RL3D), mx_rl4d=max(RL4D))
ptg_c1 <- apply(ptg_c1,c(2),mean,na.rm=T)
ptg_c1 <- ptg_c1[2:10]

## average evapotranspiratoin
## 1. Orig
if (!file.exists(paste0(in_dir,dd1[i],'/orig_Calima/summary_uganda_orig.RDS'))) {
  next
} else {
et<- readRDS(paste0(in_dir,dd1[i],'/orig_Calima/summary_uganda_orig.RDS'))
}

et[et==-99]=NA
et <- et %>% mutate(ET=PRCP/(MDAT-SDAT))
et <- mean(et$ET)

## 2. Compact
if (!file.exists(paste0(in_dir,dd1[i],'/bd10_Calima/summary_uganda_bd10.RDS'))) {
  next
} else {
et_c<- readRDS(paste0(in_dir,dd1[i],'/bd10_Calima/summary_uganda_bd10.RDS'))
}

et_c[et_c==-99]=NA
et_c <- et_c %>% mutate(ET=PRCP/(MDAT-SDAT))
et_c <- mean(et_c$ET)

var_o[[dd1[i]]] <- data.frame(t(ev_av1), t(ptg_av1), et)
var_c[[dd1[i]]] <- data.frame(t(ev_c1), t(ptg_c1), et_c)
}

all_pts_o <- map_df(var_o, ~as.data.frame(.x), .id = 'grids') %>% mutate_if(is.numeric, ~round(.,2))
all_pts_c <- map_df(var_c, ~as.data.frame(.x), .id = 'grids') %>% mutate_if(is.numeric, ~round(.,2))
saveRDS(all_pts_o, '//catalogue/BaseLineDataCluster01/temp/compac_all_grids/plts_comb/all_grids/data_orig_uganda.RDS')
saveRDS(all_pts_c, '//catalogue/BaseLineDataCluster01/temp/compac_all_grids/plts_comb/all_grids/data_compact_uganda.RDS')


