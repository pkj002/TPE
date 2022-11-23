rm(list = ls())
library(dplyr)
library(purrr)
library(ggpubr)
library(ggplot2)
library(DSSAT)

#soil<-c('orig','bd17_srgfO','bd17_srgfP')
soil<-c('orig','bd17_srgfO','bd8')
strSS<-rep(c('Flowering to Grain filling', 'Stress free', 'Terminal moderate', 'Terminal severe'), each=15)
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

for (k in 1:length(cultiv)){
  for (s in 1:4){ ## stress types
    for(i in 1:15){ ## 15 dir for each stresses
      for (j in 1:length(soil)){
        
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
        ev[ev==-99]=NA
        ev1<- ev %>% group_by(RUN) %>% summarise(pwam=mean(PWAMS,na.rm=T), cwam=mean(CWAMS,na.rm=T), lai=mean(LAIXS,na.rm=T), yld=mean(HWAMS,na.rm=T))
        ev_av1<-apply(ev1, c(2),mean,na.rm=T) 
        ev_av1 <- ev_av1[2:5]
        
        ## PlantGro.csv
        ptg<-readRDS(paste0(in_dir, '/', dir[i],'/',soil[j],'_',cultiv[k],'/PlantGro_ethiopia_',soil[j],'.RDS'))
        ptg[ptg==-99]=NA
        pt1<- ptg %>% group_by(RUN) %>% summarise(mx_hipd=max(HIPD), mx_slad=max(SLAD), mx_lnd=max(LN.D), mx_rdpd=max(RDPD), mx_rwad=max(RWAD),
                                                  mx_rl1d=max(RL1D), mx_rl2d=max(RL2D), mx_rl3d=max(RL3D), mx_rl4d=max(RL4D), mx_rl5d=max(RL5D),
                                                  mx_rl6d=max(RL6D),mx_rl7d=max(RL7D), mx_rl8d=max(RL8D), mx_rl9d=max(RL9D))
        
        ptg_av1 <- apply(pt1,c(2),mean,na.rm=T)
        ptg_av1 <- ptg_av1[2:15]
        
        ## average evapotranspiratoin
        et<- readRDS(paste0(in_dir,'/',dir[i],'/',soil[j],'_',cultiv[k],'/summary_ethiopia_',soil[j],'.RDS'))
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
      var_orig1[[i]] <- var_orig
      
      ch1<- 100*(var_1pt[2,]-var_1pt[1,])/var_1pt[1,]
      ch2 <- 100*(var_1pt[3,]-var_1pt[1,])/var_1pt[1,]
      
      srgfO[[i]] <- var_1pt[2,]
      srgfP[[i]] <- var_1pt[3,]
    }
    
    ## each stress
    var_orig2_al[[s]]<-do.call(rbind, var_orig1)
    srgfCO[[s]]<- do.call(rbind, srgfO)
    srgfCP[[s]]<-do.call(rbind, srgfP)
  }
  ## combine all stress
  sts_nam<-c('Stress free', 'Terminal moderate', 'Terminal severe', 'Flowering to Grain filling')
  strSS<-rep(sts_nam, each=15)
  
  orig_al_sts<- do.call(rbind,var_orig2_al)
  #orig_al_sts<- round(orig_al_sts,2)
  orig_al_sts_cul[[k]]<- data.frame(stress=strSS,orig_al_sts)
  
  srgfCO_sts<-do.call(rbind,srgfCO)
  srgfCO_sts<-round(srgfCO_sts,2)
  srgfCO_sts_cul[[k]]<-data.frame(stress=strSS,srgfCO_sts)
  
  srgfCP_sts<-do.call(rbind,srgfCP)
  srgfCP_sts<-round(srgfCP_sts,2)
  srgfCP_sts_cul[[k]]<-data.frame(stress=strSS,srgfCP_sts)
}

cultivar<- rep(cultiv, each=60)

var_orig2_cul<-do.call(rbind, orig_al_sts_cul)
var_orig2_cul<-data.frame(cultivar, var_orig2_cul)

srgfO_cul<- do.call(rbind, srgfCO_sts_cul)
srgfO_cul<-data.frame(cultivar, srgfO_cul)

srgfP_cul<- do.call(rbind, srgfCP_sts_cul)
srgfP_cul<- data.frame(cultivar, srgfP_cul)

## Rearrange order of stress stress types and cultivars
## 1. SRGFO
srgfO_cul<-arrange(transform(srgfO_cul, cultivar=factor(cultivar,levels=unique(srgfO_cul$cultivar))), cultivar)
srgfO_cul<-arrange(transform(srgfO_cul, stress=factor(stress,levels=c(sts_nam[1],sts_nam[2],sts_nam[3],sts_nam[4]))), stress)

srgfP_cul<-arrange(transform(srgfP_cul, cultivar=factor(cultivar,levels=unique(srgfP_cul$cultivar))), cultivar)
srgfP_cul<-arrange(transform(srgfP_cul, stress=factor(stress,levels=c(sts_nam[1],sts_nam[2],sts_nam[3],sts_nam[4]))), stress)

## 2. Var_orig2
var_orig2_cul<-arrange(transform(var_orig2_cul, cultivar=factor(cultivar,levels=unique(var_orig2_cul$cultivar))), cultivar)
var_orig2_cul<-arrange(transform(var_orig2_cul, stress=factor(stress,levels=c(sts_nam[1],sts_nam[2],sts_nam[3],sts_nam[4]))), stress)

var_orig2_cul<-data.frame(lapply(var_orig2_cul, function(x) unlist(x)))

var_orig2_cul$pwam= var_orig2_cul$pwam/1000; var_orig2_cul$cwam= var_orig2_cul$cwam/1000
var_orig2_cul$yld= var_orig2_cul$yld/1000; var_orig2_cul$mx_slad= var_orig2_cul$mx_slad/1000 
var_orig2_cul$mx_rwad= var_orig2_cul$mx_rwad/1000;

## bd 17
srgfO_cul$pwam= srgfO_cul$pwam/1000; srgfO_cul$cwam= srgfO_cul$cwam/1000
srgfO_cul$yld= srgfO_cul$yld/1000; srgfO_cul$mx_slad= srgfO_cul$mx_slad/1000 
srgfO_cul$mx_rwad= srgfO_cul$mx_rwad/1000;

## bd8
srgfP_cul$pwam= srgfP_cul$pwam/1000; srgfP_cul$cwam= srgfP_cul$cwam/1000
srgfP_cul$yld= srgfP_cul$yld/1000; srgfP_cul$mx_slad= srgfP_cul$mx_slad/1000 
srgfP_cul$mx_rwad= srgfP_cul$mx_rwad/1000;

all <- rbind(var_orig2_cul,srgfP_cul,srgfO_cul)
all1<- data.frame(comp=rep(c('orig','bd8','bd17'),each=120),all)
saveRDS(all1, 'E:/Prakash/dssat_outputs/plts_comb/data/et_all_var_with_cult.RDS')
