rm(list=ls())
library(DSSAT)
library(dplyr)
library(ggplot2)
library(ggpubr)

### soil texture analysis. Since texture is same after compaction,  
## we take just one value from every point
in_dir<- 'E:/Prakash/dssat_outputs/ethiopia/bd_random/'
out_dir<- 'E:/Prakash/dssat_outputs/ethiopia/boxplots/bd_random/'

dir_sf<-c(13088,15144,18228,20191,23551,25458,28880,30655,33222,35283,38105,39200,43681,45601,49381) ## random points
dir_tm<-c(11316,23077,26430,28607,30768,32155,33497,35473,37024,39015,40771,42563,44306,47846,50932)
dir_ts<-c(7794,25305,26839,27751,28621,29504,30394,31741,40545,42308,44773,46080,47640,49417,50963)
dir_gf<-c(4962,7627,11800,14072,17635,21822,23752,26515,29377,32843,35080,38404,41286,44128,48764) 

soil<-c('orig','bd17_srgfO','bd17_srgfP')
strSS<-rep(c('Stress free', 'Terminal moderate', 'Terminal severe', 'Flowering to Grain filling'), each=15)
cultiv <- c('calima', 'Isabella', 'A 195','BAT 1393')

clay<-list(); silt<-list(); bd<-list(); aw<-list()
clay2<-list(); sand1<-list(); bd1<-list(); aw1<-list()

for (s in 1:4){ ## stress types
  for(i in 1:15){ ##
    
    if (s==1){
      dir=dir_sf
    } else if(s==2){
      dir=dir_tm
    } else if(s==3){
      dir=dir_ts
    } else {
      dir=dir_gf
    }
    
    sl <- read_sol(paste0(in_dir,dir[i],'/',soil[2],'_',cultiv[1],'/SOIL.SOL'))
    bd[[i]]<- as.numeric(as.character(unlist(sl$SBDM)))
    ul<-as.numeric(as.character(unlist(sl$SDUL)))
    ll<-as.numeric(as.character(unlist(sl$SLLL)))
    ew<-ul-ll
    aw[[i]]<- ew
    clay[[i]] <- as.numeric(as.character(unlist(sl$SLCL)))
    silt[[i]] <- as.numeric(as.character(unlist(sl$SLSI)))
  }
  
  clay1 <- do.call(rbind,clay)
  silt1<-  do.call(rbind,silt)
  sand1[[s]] <- 100 - (clay1+silt1)
  clay2[[s]] <- clay1
  bd1[[s]] <-do.call(rbind,bd)
  aw1[[s]] <-do.call(rbind,aw)
}

sand2<-do.call(rbind,sand1)
clay3<-do.call(rbind,clay2)
bd2<-do.call(rbind,bd1)
aw2<-do.call(rbind,aw1)


clay4 <- data.frame(stress=rep(strSS,6),depth=rep(paste0('layer_',1:6),each=60),texture=rep('clay',360),
                    value=c(clay3[,1],clay3[,2],clay3[,3],clay3[,4],clay3[,5],clay3[,6]))

sand3<-data.frame(stress=rep(strSS,6),depth=rep(paste0('layer_',1:6),each=60), texture=rep('sand',360),
                  value=c(sand2[,1],sand2[,2],sand2[,3],sand2[,4],sand2[,5],sand2[,6]))

bd3<-data.frame(stress=rep(strSS,6),depth=rep(paste0('layer_',1:6),each=60),bd=c(bd2[,1],bd2[,2],bd2[,3],bd2[,4],bd2[,5],bd2[,6]))
aw3<-data.frame(stress=rep(strSS,6),depth=rep(paste0('layer_',1:6),each=60),aw=c(aw2[,1],aw2[,2],aw2[,3],aw2[,4],aw2[,5],aw2[,6]))

clay4<- clay4 %>% filter(depth %in% c('layer_1', 'layer_2','layer_3'))
sand3<- sand3 %>% filter(depth %in% c('layer_1', 'layer_2','layer_3'))
pcols<-c('#377eb8','#4daf4a','#984ea3')

clay4<-arrange(transform(clay4, stress=factor(stress,levels=c(strSS[1], strSS[16],strSS[31],strSS[46]))), stress)
sand3<-arrange(transform(sand3, stress=factor(stress,levels=c(strSS[1], strSS[16],strSS[31],strSS[46]))), stress)
bd3<- arrange(transform(bd3,stress=factor(stress,levels=c(strSS[1], strSS[16],strSS[31],strSS[46]))), stress)
aw3<-arrange(transform(aw3,stress=factor(stress,levels=c(strSS[1], strSS[16],strSS[31],strSS[46]))), stress)

bd3 <- bd3 %>% filter(depth %in% c('layer_1', 'layer_2','layer_3'))
aw3 <- aw3 %>% filter(depth %in% c('layer_1', 'layer_2','layer_3'))

tex1= ggplot(clay4, aes(x=depth, y=value)) +
  geom_boxplot(aes(fill=depth),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  #ylim(min(clay4$value),max(clay4$value))+
  ylim(17,46)+
  facet_wrap(~stress,nrow=1, scales="free")+
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Clay (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

tex2=ggplot(sand3, aes(x=depth, y=value)) +
  geom_boxplot(aes(fill=depth),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  #ylim(min(sand3$value),max(sand3$value))+
  ylim(30,69)+
  facet_wrap(~stress,nrow=1, scales="free")+
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Sand (%)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

bd_plot=ggplot(bd3, aes(x=depth, y=bd)) +
  geom_boxplot(aes(fill=depth),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  #ylim(min(bd3$bd),max(bd3$bd))+
  ylim(.97,1.72)+
  facet_wrap(~stress,nrow=1, scales="free")+
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "none", axis.text.x = element_blank())+
  labs(x = " ", y="Bulk density (g cm-3)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

aw_plot=ggplot(aw3, aes(x=depth, y=aw)) +
  geom_boxplot(aes(fill=depth),notch=F, outlier.size = 0.5, outlier.colour = NA)+
  #ylim(min(aw3$aw),max(aw3$aw))+
  ylim(.058,.128)+
  facet_wrap(~stress,nrow=1, scales="free")+
  theme_bw()+
  scale_fill_manual(values=pcols)+
  scale_colour_manual(values=pcols)+
  theme(legend.position = "bottom")+
  labs(x = " ", y="Available water (cm3 cm-3)")+
  theme(strip.background = element_rect(colour="black", fill="white"))

tex3<-ggarrange(tex1,tex2, bd_plot, aw_plot, nrow = 4, heights = c(.22,.22,.22,.34))

ggsave(paste0(out_dir,'soil_texture.png'), tex3, width=10, height=10, dpi=600)
