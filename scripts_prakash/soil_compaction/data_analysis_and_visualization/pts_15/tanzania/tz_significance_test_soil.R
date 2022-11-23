rm(list=ls())
library(DSSAT)
library(dplyr)
library(ggplot2)
library(ggpubr)

### soil texture analysis. Since texture is same after compaction,  
## we take just one value from every point
in_dir<- 'E:/Prakash/dssat_outputs/tanzania/bd_random/'
out_dir<- 'E:/Prakash/dssat_outputs/tanzania/boxplots/bd_random/'

dir_sf<-c(28320,29203,30070,31830,32520,35605,37607,39170,40700,41576,42259,43800,45342,46249,47336) ## SF
dir_as<-c(28684,29795,30254,30693,31133,31575,31806,32248,32687,33126,34006,35751,36860,38403,41693)  ## AS
dir_mts<- c(4576,7454,10283,13359,16611,19050,21150,24819,27444,30659,33118,36395,39068,42783,44990) ## MTS
dir_sts<-c(3218,6084,7626,11821,16644,18168,19051,19710,19949,20392,21051,21700,22149,22802,31311)  ## STS
dir_ets<- c(1273,3222,4982,7212,13370,15094,16408,18010,19786,21562,29987,34852,35736,38595,39950)  ## ETS

soil<-c('orig','bd17_srgfO','bd17_srgfP')
strSS<-rep(c('Stress-free', 'All season', 'Terminal moderate', 'Terminal severe','Extreme terminal'),each=15)
cultiv <- c('calima', 'BAT 1393')

clay<-list(); silt<-list(); bd<-list(); aw<-list()
clay2<-list(); sand1<-list(); bd1<-list(); aw1<-list()

for (s in 1:5){ ## stress types
  for(i in 1:15){ ##
    
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

clay4 <- data.frame(stress=rep(strSS,6),depth=rep(paste0('layer_',1:6),each=75),texture=rep('clay',450),
                    value=c(clay3[,1],clay3[,2],clay3[,3],clay3[,4],clay3[,5],clay3[,6]))

sand3<-data.frame(stress=rep(strSS,6),depth=rep(paste0('layer_',1:6),each=75), texture=rep('sand',450),
                  value=c(sand2[,1],sand2[,2],sand2[,3],sand2[,4],sand2[,5],sand2[,6]))

bd3<-data.frame(stress=rep(strSS,6),depth=rep(paste0('layer_',1:6),each=75),bd=c(bd2[,1],bd2[,2],bd2[,3],bd2[,4],bd2[,5],bd2[,6]))
aw3<-data.frame(stress=rep(strSS,6),depth=rep(paste0('layer_',1:6),each=75),aw=c(aw2[,1],aw2[,2],aw2[,3],aw2[,4],aw2[,5],aw2[,6]))

clay4<- clay4 %>% filter(depth %in% c('layer_1', 'layer_2','layer_3'))
sand3<- sand3 %>% filter(depth %in% c('layer_1', 'layer_2','layer_3'))
pcols<-c('#377eb8','#4daf4a','#984ea3')

clay4<-arrange(transform(clay4, stress=factor(stress,levels=c(strSS[1], strSS[16],strSS[31],strSS[46],strSS[61]))), stress)
sand3<-arrange(transform(sand3, stress=factor(stress,levels=c(strSS[1], strSS[16],strSS[31],strSS[46],strSS[61]))), stress)
bd3<- arrange(transform(bd3,stress=factor(stress,levels=c(strSS[1], strSS[16],strSS[31],strSS[46],strSS[61]))), stress)
aw3<-arrange(transform(aw3,stress=factor(stress,levels=c(strSS[1], strSS[16],strSS[31],strSS[46],strSS[61]))), stress)

bd3 <- bd3 %>% filter(depth %in% c('layer_1', 'layer_2','layer_3'))
aw3 <- aw3 %>% filter(depth %in% c('layer_1', 'layer_2','layer_3'))

############## Available water ##############
aw1<- aw3 %>% filter(depth %in% c('layer_1','layer_2','layer_3'))

test3<-aov(as.numeric(as.character(aw)) ~ depth + stress, data=aw1)
summary(test3)

sts<-c('Stress-free', 'All season', 'Terminal moderate', 'Terminal severe','Extreme terminal')

s<-list()
for (i in 1:5){
  gg1<- aw1 %>% filter(stress==sts[i], depth=='layer_1')
  gg2<- aw1 %>% filter(stress==sts[i], depth=='layer_2')
  gg3<- aw1 %>% filter(stress==sts[i], depth=='layer_3')
  s[[i]]<- c(mean(gg1$aw), mean(gg2$aw), mean(gg2$aw)) 
}

aww<-t(do.call(rbind,s))

aw<- data.frame(c('0-5cm', '5-15cm', '15-30cm'), aww, sources=c('depth','stress***','depthxstress'))
colnames(aw) <- c('Available water',sts,'Significant?')

################ clay ###
aw1 <- clay4 %>% filter(depth %in% c('layer_1','layer_2','layer_3'))
test3<-aov(as.numeric(as.character(value)) ~ depth + stress, data=clay4)
summary(test3)

c<-list()

for (i in 1:5){
  gg1<- aw1 %>% filter(stress==sts[i], depth=='layer_1')
  gg2<- aw1 %>% filter(stress==sts[i], depth=='layer_2')
  gg3<- aw1 %>% filter(stress==sts[i], depth=='layer_3')
  c[[i]]<- c(mean(gg1$value), mean(gg2$value), mean(gg2$value)) 
}

cww<-t(do.call(rbind,c))
cl<- data.frame(c('0-5cm', '5-15cm', '15-30cm'), cww, sources=c('depth ***','stress ***','depthxstress'))
colnames(cl) <- c('clay',sts,'Significant?')

## sand
aw1 <- sand3 %>% filter(depth %in% c('layer_1','layer_2','layer_3'))
test3<-aov(as.numeric(as.character(value)) ~ depth + stress, data=aw1)
summary(test3)

s <- list()

for (i in 1:5){
  gg1<- aw1 %>% filter(stress==sts[i], depth=='layer_1')
  gg2<- aw1 %>% filter(stress==sts[i], depth=='layer_2')
  gg3<- aw1 %>% filter(stress==sts[i], depth=='layer_3')
  s[[i]]<- c(mean(gg1$value), mean(gg2$value), mean(gg2$value)) 
}

sww<- t(do.call(rbind,s))
sd<- data.frame(c('0-5cm', '5-15cm', '15-30cm'), sww, sources=c('depth**','stress***','depthxstress'))
colnames(sd) <- c('clay',sts,'Significant?')

## bulk density
aw1 <- bd3 %>% filter(depth %in% c('layer_1','layer_2','layer_3'))
test3<-aov(as.numeric(as.character(bd)) ~ depth + stress, data=aw1)
summary(test3)

db<-list()
for (i in 1:5){
  gg1<- aw1 %>% filter(stress==sts[i], depth=='layer_1')
  gg2<- aw1 %>% filter(stress==sts[i], depth=='layer_2')
  gg3<- aw1 %>% filter(stress==sts[i], depth=='layer_3')
  db[[i]]<- c(mean(gg1$bd), mean(gg2$bd), mean(gg2$bd)) 
}

sdb<- t(do.call(rbind,db))
bd<- data.frame(c('0-5cm', '5-15cm', '15-30cm'), sdb, sources=c('depth***','stress.','depthxstress'))
colnames(bd) <- c('clay',sts,'Significant?')

write.csv(cl, 'E:/Prakash/dssat_outputs/tanzania/cl.csv')
write.csv(sd, 'E:/Prakash/dssat_outputs/tanzania/sd.csv')
write.csv(bd, 'E:/Prakash/dssat_outputs/tanzania/bd.csv')
write.csv(aw, 'E:/Prakash/dssat_outputs/tanzania/aw.csv')
