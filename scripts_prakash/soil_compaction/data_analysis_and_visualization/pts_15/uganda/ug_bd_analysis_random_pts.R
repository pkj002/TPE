rm(list = ls())
library(dplyr)
library(purrr)
library(ggpubr)
library(ggplot2)
library(DSSAT)

soil<-c('orig','bd17_srgfO','bd17_srgfP')
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

#st<-c('gf','sf','tm','ts')

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
      
      srgfO[[i]] <- ch1
      srgfP[[i]] <- ch2
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

#ETCP
#pcols<-c('#377eb8','#4daf4a','#984ea3','#ff7f00')

pcols<-c('#377eb8','#ff7f00')

## box plot
ET= ggplot(srgfO_cul[,c(1,2,21)],aes(x=cultivar, y=ET, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(srgfO_cul$ET),max(srgfO_cul$ET))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Season total ET (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

ET_A= ggplot(var_orig2_cul[,c(1,2,21)],aes(x=cultivar, y=ET, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(var_orig2_cul$ET),max(var_orig2_cul$ET))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Season total ET (mm)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

PW= ggplot(srgfO_cul[,c(1,2,3)],aes(x=cultivar, y=pwam, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(-25,max(srgfO_cul$pwam))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Pod weight (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

PW_A= ggplot(var_orig2_cul[,c(1,2,3)],aes(x=cultivar, y=pwam, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(var_orig2_cul$pwam),max(var_orig2_cul$pwam))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Pod weight (t ha-1)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

CW=ggplot(srgfO_cul[,c(1,2,4)],aes(x=cultivar, y=cwam, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(-30,max(srgfO_cul$cwam)) +
  facet_wrap(~stress,nrow=1, scales="free") + 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="top weight (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

CW_A=ggplot(var_orig2_cul[,c(1,2,4)],aes(x=cultivar, y=cwam, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(var_orig2_cul$cwam),max(var_orig2_cul$cwam)) +
  facet_wrap(~stress,nrow=1, scales="free") + 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="top weight (t ha-1)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

LAI= ggplot(srgfO_cul[,c(1,2,5)],aes(x=cultivar, y=lai, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(-20, max(srgfO_cul$lai))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(subtitle ="", x = "", y="LAI (%)") +
  theme(strip.background = element_rect(colour="black", fill="white"))

LAI_A = ggplot(var_orig2_cul[,c(1,2,5)],aes(x=unlist(var_orig2_cul$cultivar), y=unlist(var_orig2_cul$lai), fill=unlist(var_orig2_cul$cultivar))) +
  geom_boxplot(aes(fill=unlist(var_orig2_cul$cultivar)),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(1, 3.02)+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(subtitle ="", x = "", y="LAI") +
  theme(strip.background = element_rect(colour="black", fill="white"))

yld= ggplot(srgfO_cul[,c(1,2,6)],aes(x=cultivar, y=yld, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(-30, 0)+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "bottom")+
  labs(x = "Cultivars", y="Yield (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

yld_A = ggplot(var_orig2_cul[,c(1,2,6)],aes(x=cultivar, y=yld, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(var_orig2_cul$yld), max(var_orig2_cul$yld))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "bottom")+
  labs(x = "Cultivars", y="Yield (t ha-1)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

PHI=ggplot(srgfO_cul[,c(1,2,7)],aes(x=cultivar, y=mx_hipd, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(-2,7) +
  facet_wrap(~stress,nrow=1, scales="free") + 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Pod Harv Ind (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

PHI_A=ggplot(var_orig2_cul[,c(1,2,7)],aes(x=cultivar, y=mx_hipd, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(var_orig2_cul$mx_hipd),max(var_orig2_cul$mx_hipd)) +
  facet_wrap(~stress,nrow=1, scales="free") + 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Pod Harv Ind")+
  theme(strip.background = element_rect(colour="black", fill="white"))

SLAD=ggplot(srgfO_cul[,c(1,2,8)],aes(x=cultivar, y=mx_slad, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(-8,max(srgfO_cul$mx_slad))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Sp. Leaf Area (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

SLAD_A=ggplot(var_orig2_cul[,c(1,2,8)],aes(x=cultivar, y=mx_slad, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(var_orig2_cul$mx_slad),max(var_orig2_cul$mx_slad))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Sp. Leaf Area (cm2 g-1)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

LN = ggplot(srgfO_cul[,c(1,2,9)],aes(x=cultivar, y=mx_lnd, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(srgfO_cul$mx_lnd), max(srgfO_cul$mx_lnd))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Leaf N (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RD= ggplot(srgfO_cul[,c(1,2,10)],aes(x=cultivar, y=mx_rdpd, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(srgfP_cul$mx_rdpd), max(srgfP_cul$mx_rdpd))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Root Depth (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RD_A = ggplot(var_orig2_cul[,c(1,2,10)],aes(x=cultivar, y=mx_rdpd, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(1, max(var_orig2_cul$mx_rdpd))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Root Depth (m)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RW= ggplot(srgfO_cul[,c(1,2,11)],aes(x=cultivar, y=mx_rwad, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(-25, max(srgfO_cul$mx_rwad))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = "", y="Root weight (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RW_A = ggplot(var_orig2_cul[,c(1,2,11)],aes(x=cultivar, y=mx_rwad, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(var_orig2_cul$mx_rwad), max(var_orig2_cul$mx_rwad))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = "", y="Root weight (t ha-1)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RL1D= ggplot(srgfO_cul[,c(1,2,12)],aes(x=cultivar, y=mx_rl1d, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  #ylim(min(srgfP_cul$mx_rl1d), max(srgfP_cul$mx_rl1d))+
  ylim(-20,10)+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = "", y="RL1D (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RL1D_A = ggplot(var_orig2_cul[,c(1,2,12)],aes(x=cultivar, y=mx_rl1d, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(var_orig2_cul$mx_rl1d), .8)+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = "", y="RL1D (cm cm-3)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RL2D= ggplot(srgfO_cul[,c(1,2,13)],aes(x=cultivar, y=mx_rl2d, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  #ylim(min(srgfP_cul$mx_rl2d), max(srgfP_cul$mx_rl2d))+
  ylim(-25,10)+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = "", y="RL2D (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RL2D_A = ggplot(var_orig2_cul[,c(1,2,13)],aes(x=cultivar, y=mx_rl2d, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(var_orig2_cul$mx_rl2d), max(var_orig2_cul$mx_rl2d))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = "", y="RL2D (5-15 cm)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RL3D= ggplot(srgfO_cul[,c(1,2,14)],aes(x=cultivar, y=mx_rl3d, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  #ylim(min(srgfO_cul$mx_rl3d), max(srgfO_cul$mx_rl3d))+
  ylim(-20,10)+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "bottom")+
  labs(x = "", y="RL3D (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RL3D_A = ggplot(var_orig2_cul[,c(1,2,14)],aes(x=cultivar, y=mx_rl3d, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(var_orig2_cul$mx_rl3d), max(var_orig2_cul$mx_rl3d))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "bottom")+
  labs(x = "Cultivars", y="RL3D (15-30 cm)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RL4D= ggplot(srgfO_cul[,c(1,2,15)],aes(x=cultivar, y=mx_rl4d, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(srgfO_cul$mx_rl4d), max(srgfO_cul$mx_rl4d))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "bottom")+
  labs(x = "Cultivars", y="RL4D (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RL4D_A = ggplot(var_orig2_cul[,c(1,2,15)],aes(x=cultivar, y=mx_rl4d, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(var_orig2_cul$mx_rl4d), max(var_orig2_cul$mx_rl4d))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "bottom")+
  labs(x = "Cultivars", y="RL4D")+
  theme(strip.background = element_rect(colour="black", fill="white"))

rootsh = ggplot(srgfO_cul[,c(1,2,22)],aes(x=cultivar, y=r_s, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(srgfO_cul$r_s), 25)+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = "", y="Root:Shoot (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

rootsh_A = ggplot(var_orig2_cul[,c(1,2,22)],aes(x=cultivar, y=r_s, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(0,0.6)+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = "", y="Root:Shoot")+
  theme(strip.background = element_rect(colour="black", fill="white"))

## Relative 
pp<-ggarrange(RD,RW,ET,RL1D,RL2D,RL3D, heights=c(0.16,0.16,0.16,0.16,0.16,0.20), nrow = 6)
pp2<-ggarrange(PW,CW,rootsh,SLAD,LAI,yld, heights=c(0.16,0.16,0.16,0.16,0.16,0.20), nrow = 6)

out_dir <- 'E:/Prakash/dssat_outputs/uganda/boxplots/bd_random/'

ggsave(paste0(out_dir,'boxplot_root_rel.png'), pp, width=5, height=10, dpi=600)
ggsave(paste0(out_dir,'boxplot_shoot_rel.png'), pp2, width=5, height=10, dpi=600)

## Absolute values
pp3<-ggarrange(RD_A,RW_A,ET_A,RL1D_A,RL2D_A,RL3D_A,heights=c(0.16,0.16,0.16,0.16,0.16,0.20), nrow = 6)
pp4<-ggarrange(PW_A,CW_A,rootsh_A,SLAD_A,LAI_A,yld_A, heights=c(0.16,0.16,0.16,0.16,0.16,0.20), nrow = 6)

ggsave(paste0(out_dir,'boxplot_root_Abs.png'), pp3, width=5, height=10, dpi=600)
ggsave(paste0(out_dir,'boxplot_shoot_Abs.png'), pp4, width=5, height=10, dpi=600)

#################################################################################################################################################
######################################## SRGF reduction in proportion to bulk density #########################################################
ET= ggplot(srgfP_cul[,c(1,2,21)],aes(x=cultivar, y=ET, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(srgfO_cul$ET),max(srgfO_cul$ET))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y=" Season total ET (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

PW= ggplot(srgfP_cul[,c(1,2,3)],aes(x=cultivar, y=pwam, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  #ylim(seq(-25,0,by=5))+
  scale_y_continuous(limits = c(-25,0))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Pod weight (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

CW=ggplot(srgfP_cul[,c(1,2,4)],aes(x=cultivar, y=cwam, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(-30,max(srgfP_cul$cwam)) +
  facet_wrap(~stress,nrow=1, scales="free") + 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="top weight (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

LAI= ggplot(srgfP_cul[,c(1,2,5)],aes(x=cultivar, y=lai, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(-20, max(srgfP_cul$lai))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(subtitle ="", x = "", y="LAI (%)") +
  theme(strip.background = element_rect(colour="black", fill="white"))

yld= ggplot(srgfP_cul[,c(1,2,6)],aes(x=cultivar, y=yld, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(-30, max(srgfP_cul$yld))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "bottom")+
  labs(x = "Cultivars", y="Yield (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

PHI=ggplot(srgfP_cul[,c(1,2,7)],aes(x=cultivar, y=mx_hipd, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(-2,4) +
  facet_wrap(~stress,nrow=1, scales="free") + 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Pod Harv Ind (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

SLAD=ggplot(srgfP_cul[,c(1,2,8)],aes(x=cultivar, y=mx_slad, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(-8,max(srgfP_cul$mx_slad))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Sp. Leaf Area (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

LN = ggplot(srgfP_cul[,c(1,2,9)],aes(x=cultivar, y=mx_lnd, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(srgfP_cul$mx_lnd), max(srgfP_cul$mx_lnd))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Leaf N (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RD= ggplot(srgfP_cul[,c(1,2,10)],aes(x=cultivar, y=mx_rdpd, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(srgfO_cul$mx_rdpd), max(srgfO_cul$mx_rdpd))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Root Depth (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RW= ggplot(srgfP_cul[,c(1,2,11)],aes(x=cultivar, y=mx_rwad, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  #ylim(min(srgfO_cul$mx_rwad), max(srgfO_cul$mx_rwad))+
  ylim(-24,10)+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = "", y="Root weight (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RL1D= ggplot(srgfP_cul[,c(1,2,12)],aes(x=cultivar, y=mx_rl1d, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  #ylim(min(srgfO_cul$mx_rl1d), max(srgfO_cul$mx_rl1d))+
  ylim(-25,10)+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = "", y="RL1D (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RL2D= ggplot(srgfP_cul[,c(1,2,13)],aes(x=cultivar, y=mx_rl2d, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  #ylim(min(srgfO_cul$mx_rl2d), max(srgfO_cul$mx_rl2d))+
  ylim(-25,10)+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = "", y="RL2D (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RL3D= ggplot(srgfP_cul[,c(1,2,14)],aes(x=cultivar, y=mx_rl3d, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  #ylim(min(srgfO_cul$mx_rl3d), max(srgfO_cul$mx_rl3d))+
  ylim(-25,10)+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "bottom", axis.text.x = element_blank())+
  labs(x = "Cultivars", y="RL3D (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

RL4D= ggplot(srgfP_cul[,c(1,2,15)],aes(x=cultivar, y=mx_rl4d, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(srgfP_cul$mx_rl4d), max(srgfP_cul$mx_rl4d))+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "bottom")+
  labs(x = "Cultivars", y="RL4D (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

rootsh = ggplot(srgfP_cul[,c(1,2,22)],aes(x=cultivar, y=r_s, fill=cultivar)) +
  geom_boxplot(aes(fill=cultivar),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  ylim(min(srgfO_cul$r_s), 25)+
  facet_wrap(~stress,nrow=1, scales="free")+ 
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = "", y="Root:Shoot (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

## Relative 
gg<-ggarrange(RD,RW,ET,RL1D,RL2D,RL3D,heights=c(0.16,0.16,0.16,0.16,0.16,0.20), nrow = 6)
gg2<-ggarrange(PW,CW,rootsh,SLAD,LAI,yld, heights=c(0.16,0.16,0.16,0.16,0.16,0.20), nrow = 6)

ggsave(paste0(out_dir,'boxplot_srgfP_root_rel.png'), gg, width=5, height=10, dpi=600)
ggsave(paste0(out_dir,'boxplot_srgfP_shoot_rel.png'), gg2, width=5, height=10, dpi=600)




