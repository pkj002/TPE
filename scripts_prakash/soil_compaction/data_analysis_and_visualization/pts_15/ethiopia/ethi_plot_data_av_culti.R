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

var<-list(); var_1pt <- list(); pts<-list(); all_pts<- list()

for (s in 1:4){ ## stress types
  for(i in 1:15){ ## 15 dir for each stresses
    for (j in 1:length(soil)){
      for (k in 1:length(cultiv)){
        
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
        et <- et %>% mutate(ET=PRCP/(MDAT-SDAT))
        et2 <- mean(et[,76],na.rm=T)
        
        var[[k]] <- c(ev_av1, ptg_av1, et2)
      }
      var_1pt[[j]]<-  apply(do.call(rbind,var),c(2),mean,na.rm=T)  
      
    }
    ## each point
    pts[[i]]<- data.frame(comp=c('orig','bd17','bd8'),do.call(rbind, var_1pt))
  }
  
  ## combine all point
  all_pts[[s]] <- do.call(rbind,pts)
}

## combine all stress
sts_nam<-c('Stress free', 'Terminal moderate', 'Terminal severe', 'Flowering to Grain filling')
stress<-rep(sts_nam, each=45)
hi <- data.frame(stress, do.call(rbind,all_pts))
colnames(hi)[21] <- 'ET'

hi$pwam= hi$pwam/1000; hi$cwam= hi$cwam/1000
hi$yld= hi$yld/1000; hi$mx_slad= hi$mx_slad/1000 
hi$mx_rwad= hi$mx_rwad/1000;

out<-'E:/Prakash/dssat_outputs/'
#saveRDS(hi, paste0(out,'et_all_var.RDS'))

yld_hi <-data.frame(point=c(rep(dir_sf,each=3),rep(dir_tm,each=3),rep(dir_ts,each=3),rep(dir_gf,each=3)),hi[,c(1,2,6)])
saveRDS(yld_hi, paste0(out,'ethiopia/ethi_yld_with_pts.RDS'))
