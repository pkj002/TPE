rm(list = ls())
library(dplyr)
library(purrr)
library(ggpubr)
library(ggplot2)
library(DSSAT)

soil<-c('orig','bd17_srgfO','bd17_srgfP')

strSS<-c('Stress-free', 'All season', 'Terminal moderate', 'Terminal severe','Extreme terminal')

#cultiv <- c('calima', 'Isabella', 'A 195','BAT 1393')
cultiv <- c('calima', 'BAT 1393')
pcols<-c('#377eb8','#4daf4a','#984ea3','#ff7f00','#fff7bc','#bdbdbd')

in_dir<- paste0('E:/Prakash/dssat_outputs/tanzania/')

dir_sf<-c(28320,29203,30070,31830,32520,35605,37607,39170,40700,41576,42259,43800,45342,46249,47336) ## SF
dir_as<-c(28684,29795,30254,30693,31133,31575,31806,32248,32687,33126,34006,35751,36860,38403,41693)  ## AS
dir_mts<- c(4576,7454,10283,13359,16611,19050,21150,24819,27444,30659,33118,36395,39068,42783,44990) ## MTS
dir_sts<-c(3218,6084,7626,11821,16644,18168,19051,19710,19949,20392,21051,21700,22149,22802,31311)  ## STS
dir_ets<- c(1273,3222,4982,7212,13370,15094,16408,18010,19786,21562,29987,34852,35736,38595,39950)  ## ETS

var<-list(); vari <- list()
var_all<-list(); var_orig1<-list(); var_orig2_al<- list()
srgfO<-list(); srgfP <- list()
srgfCO<-list(); srgfCP <- list()
orig_al_sts_cul<-list(); srgfCO_sts_cul<-list(); srgfCP_sts_cul<-list()

#st<-c('gf','sf','tm','ts')
for (k in 1:length(cultiv)){
  for (s in 1:5){ ## stress types
    for(i in 1:15){ ## 15 dir for each stresses
      for (j in 1:2){
        
        if (s==1){
          dir=dir_sf
        } else if(s==2){
          dir=dir_as
        } else if(s==3){
          dir=dir_mts
        } else if(s==4){
          dir=dir_sts
        } else {
          dir=dir_ets
        }
        ## Evaluate.csv
        ev<- readRDS(paste0(in_dir,'bd_random/',dir[i],'/',soil[j],'_',cultiv[k],'/Evaluate_tanzania_',soil[j],'.RDS'))
        ev[ev==-99]=NA
        ev1<- ev %>% group_by(RUN) %>% summarise(pwam=mean(PWAMS,na.rm=T), cwam=mean(CWAMS,na.rm=T), lai=mean(LAIXS,na.rm=T), yld=mean(HWAMS,na.rm=T))
        ev_av1<-apply(ev1, c(2),mean,na.rm=T)
        ev_av1 <- ev_av1[2:5]
        
        ## PlantGro.csv
        ptg<-readRDS(paste0(in_dir, 'bd_random/', dir[i],'/',soil[j],'_',cultiv[k],'/PlantGro_tanzania_',soil[j],'.RDS'))
        ptg[ptg==-99]=NA
        pt1<- ptg %>% group_by(RUN) %>% summarise(mx_hipd=max(HIPD), mx_slad=max(SLAD), mx_lnd=max(LN.D), mx_rdpd=max(RDPD), mx_rwad=max(RWAD),
                                                  mx_rl1d=max(RL1D), mx_rl2d=max(RL2D), mx_rl3d=max(RL3D), mx_rl4d=max(RL4D), mx_rl5d=max(RL5D),
                                                  mx_rl6d=max(RL6D),mx_rl7d=max(RL7D), mx_rl8d=max(RL8D), mx_rl9d=max(RL9D))
        
        ptg_av1 <- apply(pt1,c(2),mean,na.rm=T)
        ptg_av1 <- ptg_av1[2:15]
        
        
        ## average evapotranspiratoin
        et<- readRDS(paste0(in_dir,'bd_random/',dir[i],'/',soil[j],'_',cultiv[k],'/summary_tanzania_',soil[j],'.RDS'))
        et[et==-99]=NA
        et2 <- mean(et[,72],na.rm=T)
        
        var[[j]] <- c(ev_av1, ptg_av1, et2)
      }
      ## Soil total
      var_1pt <- as.data.frame(do.call(rbind,var))
      var_1pt <- var_1pt %>% mutate(r_s=mx_rwad/cwam)
      colnames(var_1pt)[19]='ET'
      var_orig <- matrix(round(var_1pt[1,],2), nrow=1)
      
      #rownames(var_orig) <- 'orig'
      colnames(var_orig) <- colnames(var_1pt)
      var_1pt<-data.frame(soil=c('orig','comp'),var_1pt)
      vari[[i]]<- var_1pt
    }
    
    ## each stress
    var_orig2_al[[s]]<-data.frame(stress=strSS[s],do.call(rbind, vari))
  }
  ## combine all stress
  ## combine all stress
  orig_al_sts<- do.call(rbind,var_orig2_al)
  #orig_al_sts<- round(orig_al_sts,2)
  orig_al_sts_cul[[k]]<- data.frame(cultiv=k,orig_al_sts)
}

bau<- do.call(rbind,orig_al_sts_cul)

osf<- bau %>% filter(soil=='orig', stress=='Stress-free')
cfgf<- bau %>% filter(soil=='comp', stress=='All season')
ext <-  bau %>% filter(soil=='comp', stress=='Extreme terminal')
ctm<- bau %>% filter(soil=='comp', stress=='Terminal moderate')
cts <- bau %>% filter(soil=='comp', stress=='Terminal severe')

(mean(cfgf$yld)-mean(osf$yld))/mean(osf$yld)
(mean(ext$yld)-mean(osf$yld))/mean(osf$yld)

(mean(cfgf$pwam)-mean(osf$pwam))/mean(osf$pwam)
(mean(ext$pwam)-mean(osf$pwam))/mean(osf$pwam)

(mean(cfgf$cwam)-mean(osf$cwam))/mean(osf$cwam)
(mean(ext$cwam)-mean(osf$cwam))/mean(osf$cwam)

(mean(cfgf$mx_rwad)-mean(osf$mx_rwad))/mean(osf$mx_rwad)
(mean(ext$mx_rwad)-mean(osf$mx_rwad))/mean(osf$mx_rwad)
(mean(ctm$mx_rwad)-mean(osf$mx_rwad))/mean(osf$mx_rwad)
(mean(cts$mx_rwad)-mean(osf$mx_rwad))/mean(osf$mx_rwad)

