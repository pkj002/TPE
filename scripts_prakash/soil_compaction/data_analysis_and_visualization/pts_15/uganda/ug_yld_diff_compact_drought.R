rm(list = ls())
library(dplyr)
library(purrr)
library(ggpubr)
library(ggplot2)
library(DSSAT)

soil<-c('orig','bd17_srgfO','bd17_srgfP')
strSS<-c('Stress free', 'Terminal severe')
cultiv <- c('Calima', 'BAT 1393')
pcols<-c('#377eb8','#4daf4a','#984ea3','#ff7f00','#fff7bc','#bdbdbd')

#in_dir<- paste0('E:/Prakash/dssat_outputs/uganda/bd_random/')
in_dir <- 'E:/Prakash/dssat_outputs/uganda/'

dir_sf<-readRDS(paste0(in_dir,'sample_pts1.RDS')) ## sf sample points
dir_ts<-readRDS(paste0(in_dir,'sample_pts2.RDS'))

var<-list(); vari <- list()
var_all<-list(); var_orig1<-list(); var_orig2_al<- list()
srgfO<-list(); srgfP <- list()
srgfCO<-list(); srgfCP <- list()
orig_al_sts_cul<-list(); srgfCO_sts_cul<-list(); srgfCP_sts_cul<-list()
#st<-c('gf','sf','tm','ts')

for (k in 1:length(cultiv)){
  for (s in 1:2){ ## stress types
    for(i in 1:15){ ## 15 dir for each stresses
      for (j in 1:2){
        
        if (s==1){
          dir=dir_sf
        } else if(s==2){
          dir=dir_ts
        } 
        ## Evaluate.csv
        ev<- readRDS(paste0(in_dir,'bd_random/',dir[i],'/',soil[j],'_',cultiv[k],'/Evaluate_uganda_',soil[j],'.RDS'))
        ev[ev==-99]=NA
        ev1<- ev %>% group_by(RUN) %>% summarise(pwam=mean(PWAMS,na.rm=T), cwam=mean(CWAMS,na.rm=T), lai=mean(LAIXS,na.rm=T), yld=mean(HWAMS,na.rm=T))
        ev_av1<-apply(ev1, c(2),mean,na.rm=T)
        ev_av1 <- ev_av1[2:5]
        
        ## PlantGro.csv
        ptg<-readRDS(paste0(in_dir, 'bd_random/', dir[i],'/',soil[j],'_',cultiv[k],'/PlantGro_uganda_',soil[j],'.RDS'))
        ptg[ptg==-99]=NA
        pt1<- ptg %>% group_by(RUN) %>% summarise(mx_hipd=max(HIPD), mx_slad=max(SLAD), mx_lnd=max(LN.D), mx_rdpd=max(RDPD), mx_rwad=max(RWAD),
                                                  mx_rl1d=max(RL1D), mx_rl2d=max(RL2D), mx_rl3d=max(RL3D), mx_rl4d=max(RL4D), mx_rl5d=max(RL5D),
                                                  mx_rl6d=max(RL6D),mx_rl7d=max(RL7D), mx_rl8d=max(RL8D), mx_rl9d=max(RL9D))
        
        ptg_av1 <- apply(pt1,c(2),mean,na.rm=T)
        ptg_av1 <- ptg_av1[2:15]
        
        ## average evapotranspiratoin
        et<- readRDS(paste0(in_dir,'bd_random/',dir[i],'/',soil[j],'_',cultiv[k],'/summary_uganda_',soil[j],'.RDS'))
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
  orig_al_sts<- do.call(rbind,var_orig2_al)
  #orig_al_sts<- round(orig_al_sts,2)
  orig_al_sts_cul[[k]]<- data.frame(cultiv=k,orig_al_sts)
}

bau<- do.call(rbind,orig_al_sts_cul)

osf<- bau %>% filter(soil=='orig', stress=='Stress free')
ext <-  bau %>% filter(soil=='comp', stress=='Terminal severe')
mean(ext$mx_rwad-osf$mx_rwad)/mean(osf$mx_rwad)

(mean(ext$yld)-mean(osf$yld))/mean(osf$yld)
(mean(ext$pwam)-mean(osf$pwam))/mean(osf$pwam)
(mean(ext$cwam)-mean(osf$cwam))/mean(osf$cwam)
