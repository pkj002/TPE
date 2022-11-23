rm(list = ls())
library(dplyr)
library(purrr)
library(ggpubr)
library(ggplot2)
library(DSSAT)

soil<-c('orig','bd17_srgfO','bd17_srgfP')
strSS<-c('Stress free', 'Terminal moderate', 'Terminal severe', 'Flowering to Grain filling')
#cultiv <- c('calima', 'Isabella', 'A 195','BAT 1393')
cultiv <- c('calima', 'BAT 1393')
pcols<-c('#377eb8','#4daf4a','#984ea3','#ff7f00','#fff7bc','#bdbdbd')

in_dir<- paste0('E:/Prakash/dssat_outputs/ethiopia/bd_random/')

dir_sf<-c(13088,15144,18228,20191,23551,25458,28880,30655,33222,35283,38105,39200,43681,45601,49381) ## random points
dir_tm<-c(11316,23077,26430,28607,30768,32155,33497,35473,37024,39015,40771,42563,44306,47846,50932)
dir_ts<-c(7794,25305,26839,27751,28621,29504,30394,31741,40545,42308,44773,46080,47640,49417,50963)
dir_gf<-c(4962,7627,11800,14072,17635,21822,23752,26515,29377,32843,35080,38404,41286,44128,48764) 

var<-list(); var_all<-list(); var_orig1<-list(); var_orig2_al<- list()
srgfO<-list(); srgfP <- list()
srgfCO<-list(); srgfCP <- list()
orig_al_sts_cul<-list(); srgfCO_sts_cul<-list(); srgfCP_sts_cul<-list()

#st<-c('gf','sf','tm','ts')
vari<-list()
for (k in 1:length(cultiv)){
  for (s in 1:4){ ## stress types
    for(i in 1:15){ ## 15 dir for each stresses
      for (j in 1:2){
        
        if (s==1){
          dir=dir_sf
        } else if(s==2){
          dir=dir_tm
        } else if(s==3){
          dir=dir_ts
        } else {
          dir=dir_gf
        }
        ## Evaluate.csv
        ev<- readRDS(paste0(in_dir,'/',dir[i],'/',soil[j],'_',cultiv[k],'/Evaluate_ethiopia_',soil[j],'.RDS'))
        
        ev1<- ev %>% group_by(RUN) %>% summarise(pwam=mean(PWAMS), cwam=mean(CWAMS), lai=mean(LAIXS), yld=mean(HWAMS))
        ev_av1<-apply(ev1, c(2),mean)
        ev_av1 <- ev_av1[2:5]
        
        ## PlantGro.csv
        ptg<-readRDS(paste0(in_dir, '/', dir[i],'/',soil[j],'_',cultiv[k],'/PlantGro_ethiopia_',soil[j],'.RDS'))
        
        pt1<- ptg %>% group_by(RUN) %>% summarise(mx_hipd=max(HIPD), mx_slad=max(SLAD), mx_lnd=max(LN.D), mx_rdpd=max(RDPD), mx_rwad=max(RWAD),
                                                  mx_rl1d=max(RL1D), mx_rl2d=max(RL2D), mx_rl3d=max(RL3D), mx_rl4d=max(RL4D), mx_rl5d=max(RL5D),
                                                  mx_rl6d=max(RL6D),mx_rl7d=max(RL7D), mx_rl8d=max(RL8D), mx_rl9d=max(RL9D))
        
        ptg_av1 <- apply(pt1,c(2),mean)
        ptg_av1 <- ptg_av1[2:15]
        
        ## PlantC.csv
        ptc<-readRDS(paste0(in_dir, '/', dir[i],'/',soil[j],'_',cultiv[k],'/PlantC_ethiopia_',soil[j],'.RDS'))
        ptc1<- ptc %>% group_by(RUN) %>% summarise(mx_chad=max(CHAD), mx_cld=max(CL.D), mx_csd=max(CS.D))
        ptc_av1 <- apply(ptc1,c(2),mean)
        ptc_av1 <- ptc_av1[2:4]
        
        ## average evapotranspiratoin
        et<- readRDS(paste0(in_dir,'/',dir[i],'/',soil[j],'_',cultiv[k],'/summary_ethiopia_',soil[j],'.RDS'))
        et2 <- mean(et[,72])
        
        var[[j]] <- c(ev_av1, ptg_av1, ptc_av1, et2)
      }
      ## Soil total
      var_1pt <- as.data.frame(do.call(rbind,var))
      var_1pt <- var_1pt %>% mutate(r_s=mx_rwad/cwam)
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
   orig_al_sts<- do.call(rbind,var_orig2_al)
  #orig_al_sts<- round(orig_al_sts,2)
  orig_al_sts_cul[[k]]<- data.frame(cultiv=k,orig_al_sts)
}

bau<- do.call(rbind,orig_al_sts_cul)

osf<- bau %>% filter(soil=='orig', stress=='Stress free')
cfgf<- bau %>% filter(soil=='comp', stress=='Flowering to Grain filling')

mean(cfgf$mx_rwad-osf$mx_rwad)/mean(osf$mx_rwad)
(mean(cfgf$yld)-mean(osf$yld))/mean(osf$yld)
c(mean(cfgf$pwam),mean(osf$pwam))
c(mean(cfgf$cwam),mean(osf$cwam))
