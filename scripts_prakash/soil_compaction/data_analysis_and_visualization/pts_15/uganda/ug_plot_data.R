rm(list = ls())
library(dplyr)
library(purrr)
library(ggpubr)
library(ggplot2)
library(DSSAT)

#soil<-c('orig','bd17_srgfO','bd17_srgfP')

soil<-c('orig','bd17_srgfO','bd8')
strSS<-rep(c('Stress free', 'Terminal severe'), each=15)
cultiv <- c('Calima', 'BAT 1393')
pcols<-c('#377eb8','#4daf4a','#984ea3','#ff7f00','#fff7bc','#bdbdbd')

#in_dir<- paste0('E:/Prakash/dssat_outputs/uganda/bd_random/')
in_dir <- 'E:/Prakash/dssat_outputs/uganda/'

dir_sf<-readRDS(paste0(in_dir,'sample_pts1.RDS')) ## sf sample points
dir_ts<-readRDS(paste0(in_dir,'sample_pts2.RDS'))

var<-list(); var_all<-list(); var_orig1<-list(); var_orig2_al<- list()
srgfO<-list(); srgfP <- list()
srgfCO<-list(); srgfCP <- list()
orig_al_sts_cul<-list(); srgfCO_sts_cul<-list(); srgfCP_sts_cul<-list()

for (k in 1:length(cultiv)){
  for (s in 1:2){ ## stress types
    for(i in 1:15){ ## 15 dir for each stresses
      for (j in 1:length(soil)){
      
        
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
        et <- et %>% mutate(ET=PRCP/(MDAT-SDAT))
        et2 <- mean(et[,76],na.rm=T)
        
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
      
      srgfO[[i]] <- var_1pt[2,]
      srgfP[[i]] <- var_1pt[3,]
    }
    
    ## each stress
    var_orig2_al[[s]]<-do.call(rbind, var_orig1)
    srgfCO[[s]]<- do.call(rbind, srgfO)
    srgfCP[[s]]<-do.call(rbind, srgfP)
  }
  ## combine all stress
  sts_nam<-c('Stress free', 'Terminal severe')
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

cultivar<- rep(cultiv, each=30)

var_orig2_cul<-do.call(rbind, orig_al_sts_cul)
var_orig2_cul<-data.frame(cultivar, var_orig2_cul)
#colnames(var_orig2_cul)[colnames(var_orig2_cul)== 'V22'] <- 'ET'

srgfO_cul<- do.call(rbind, srgfCO_sts_cul)
srgfO_cul<-data.frame(cultivar, srgfO_cul)
#colnames(srgfO_cul)[colnames(srgfO_cul) == 'V22'] <- 'ET'

srgfP_cul<- do.call(rbind, srgfCP_sts_cul)
srgfP_cul<- data.frame(cultivar, srgfP_cul)
#colnames(srgfP_cul)[colnames(srgfP_cul) == 'V22'] <- 'ET'

## Rearrange order of stress stress types and cultivars
## 1. SRGFO
srgfO_cul<-arrange(transform(srgfO_cul, cultivar=factor(cultivar,levels=unique(srgfO_cul$cultivar))), cultivar)
srgfO_cul<-arrange(transform(srgfO_cul, stress=factor(stress,levels=c(sts_nam[1],sts_nam[2]))), stress)

srgfP_cul<-arrange(transform(srgfP_cul, cultivar=factor(cultivar,levels=unique(srgfP_cul$cultivar))), cultivar)
srgfP_cul<-arrange(transform(srgfP_cul, stress=factor(stress,levels=c(sts_nam[1],sts_nam[2]))), stress)

## 2. Var_orig2
var_orig2_cul<-arrange(transform(var_orig2_cul, cultivar=factor(cultivar,levels=unique(var_orig2_cul$cultivar))), cultivar)
var_orig2_cul<-arrange(transform(var_orig2_cul, stress=factor(stress,levels=c(sts_nam[1],sts_nam[2]))), stress)

var_orig2_cul<-data.frame(lapply(var_orig2_cul, function(x) unlist(x)))

var_orig2_cul$pwam= var_orig2_cul$pwam/1000; var_orig2_cul$cwam= var_orig2_cul$cwam/1000
var_orig2_cul$yld= var_orig2_cul$yld/1000; var_orig2_cul$mx_slad= var_orig2_cul$mx_slad/1000 
var_orig2_cul$mx_rwad= var_orig2_cul$mx_rwad/1000;

out<-'E:/Prakash/dssat_outputs/'
# saveRDS(var_orig2_cul, paste0(out,'ug_var_orig2_cul.RDS'))
# saveRDS(srgfO_cul, paste0(out,'ug_srgfO_cul.RDS'))
# saveRDS(srgfP_cul, paste0(out, 'ug_srgfP_cul.RDS'))