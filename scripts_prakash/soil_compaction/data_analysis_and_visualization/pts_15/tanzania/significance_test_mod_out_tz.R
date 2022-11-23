rm(list = ls())
library(dplyr)
library(purrr)
library(ggpubr)
library(ggplot2)
library(DSSAT)

soil<-c('orig','bd17_srgfO','bd17_srgfP')
strSS<-rep(c('Flowering to Grain filling', 'Stress free', 'Terminal moderate', 'Terminal severe'), each=15)
#cultiv <- c('calima', 'Isabella', 'A 195','BAT 1393')
cultiv <- c('calima', 'BAT 1393')
pcols<-c('#377eb8','#4daf4a','#984ea3','#ff7f00','#fff7bc','#bdbdbd')

in_dir<- paste0('E:/Prakash/dssat_outputs/tanzania/')

dir_sf<-c(28320,29203,30070,31830,32520,35605,37607,39170,40700,41576,42259,43800,45342,46249,47336) ## SF
dir_as<-c(28684,29795,30254,30693,31133,31575,31806,32248,32687,33126,34006,35751,36860,38403,41693)  ## AS
dir_mts<- c(4576,7454,10283,13359,16611,19050,21150,24819,27444,30659,33118,36395,39068,42783,44990) ## MTS
dir_sts<-c(3218,6084,7626,11821,16644,18168,19051,19710,19949,20392,21051,21700,22149,22802,31311)  ## STS
dir_ets<- c(1273,3222,4982,7212,13370,15094,16408,18010,19786,21562,29987,34852,35736,38595,39950)  ## ETS

var<-list(); var_all<-list(); var_orig1<-list(); var_orig2_al<- list()
srgfO<-list(); srgfP <- list()
srgfCO<-list(); srgfCP <- list()
orig_al_sts_cul<-list(); srgfCO_sts_cul<-list(); srgfCP_sts_cul<-list()

#st<-c('gf','sf','tm','ts')
for (k in 1:length(cultiv)){
  for (s in 1:5){ ## stress types
    for(i in 1:15){ ## 15 dir for each stresses
      for (j in 1:length(soil)){
        
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
  
  sts_nam<-c('Stress-free', 'All season', 'Terminal moderate', 'Terminal severe','Extreme terminal')
  
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

cultivar<- rep(cultiv, each=75)

var_orig2_cul<-do.call(rbind, orig_al_sts_cul)
var_orig2_cul<-data.frame(cultivar, var_orig2_cul)
#colnames(var_orig2_cul)[colnames(var_orig2_cul)== 'V22'] <- 'ET'

srgfO_cul<- do.call(rbind, srgfCO_sts_cul)
srgfO_cul<-data.frame(cultivar, srgfO_cul)
#colnames(srgfO_cul)[colnames(srgfO_cul) == 'V22'] <- 'ET'

srgfP_cul<- do.call(rbind, srgfCP_sts_cul)
srgfP_cul<- data.frame(cultivar, srgfP_cul)

## Rearrange order of stress stress types and cultivars
## 1. SRGFO
srgfO_cul<-arrange(transform(srgfO_cul, cultivar=factor(cultivar,levels=unique(srgfO_cul$cultivar))), cultivar)
srgfO_cul<-arrange(transform(srgfO_cul, stress=factor(stress,levels=c(sts_nam[1],sts_nam[2],sts_nam[3],sts_nam[4],sts_nam[5]))), stress)

srgfP_cul<-arrange(transform(srgfP_cul, cultivar=factor(cultivar,levels=unique(srgfP_cul$cultivar))), cultivar)
srgfP_cul<-arrange(transform(srgfP_cul, stress=factor(stress,levels=c(sts_nam[1],sts_nam[2],sts_nam[3],sts_nam[4],sts_nam[5]))), stress)

## 2. Var_orig2
var_orig2_cul<-arrange(transform(var_orig2_cul, cultivar=factor(cultivar,levels=unique(var_orig2_cul$cultivar))), cultivar)
var_orig2_cul<-arrange(transform(var_orig2_cul, stress=factor(stress,levels=c(sts_nam[1],sts_nam[2],sts_nam[3],sts_nam[4],sts_nam[5]))), stress)

var_orig2_cul<-data.frame(lapply(var_orig2_cul, function(x) unlist(x)))

var_orig2_cul$pwam= var_orig2_cul$pwam/1000; var_orig2_cul$cwam= var_orig2_cul$cwam/1000
var_orig2_cul$yld= var_orig2_cul$yld/1000; var_orig2_cul$mx_slad= var_orig2_cul$mx_slad/1000

## Significance test
## Pod weight
pww<- srgfO_cul[,c(1,2,3)]

test3<-aov(as.numeric(as.character(pwam)) ~ cultivar + stress, data=pww)
summary(test3)

sts<-c('Stress-free', 'All season', 'Terminal moderate', 'Terminal severe','Extreme terminal')

s<-list()
for (i in 1:5){
  gg1<- pww %>% filter(stress==sts[i], cultivar=='calima')
  gg2<- pww %>% filter(stress==sts[i], cultivar=='BAT 1393')
  s[[i]]<- c(median(gg1$pwam), median(gg2$pwam)) 
}

pw<-round(t(do.call(rbind,s)),2)
colnames(pw)=sts
pww<- data.frame(cultivar=c('calima','BAT 1393'),pw,Significance=c('cultivar**','stress***'))

## Top weight
cw=srgfO_cul[,c(1,2,4)]
test3<-aov(as.numeric(as.character(cwam)) ~ cultivar + stress, data=cw)
summary(test3)

c<-list()
for (i in 1:5){
  gg1<- cw %>% filter(stress==sts[i], cultivar=='calima')
  gg2<- cw %>% filter(stress==sts[i], cultivar=='BAT 1393')
  c[[i]]<- c(median(gg1$cwam), median(gg2$cwam)) 
}

cw<-round(t(do.call(rbind,c)),2)
colnames(cw)=sts
cww<- data.frame(cultivar=c('calima','BAT 1393'),cw,Significance=c('cultivar**','stress***'))

## Root:shoot
sw<- srgfO_cul[,c(1,2,22)]
test3<- aov(as.numeric(as.character(r_s)) ~ cultivar + stress, data=sw)
summary(test3)

c<-list()
for (i in 1:5){
  gg1<- sw %>% filter(stress==sts[i], cultivar=='calima')
  gg2<- sw %>% filter(stress==sts[i], cultivar=='BAT 1393')
  c[[i]]<- c(median(gg1$r_s), median(gg2$r_s)) 
}

rs<-round(t(do.call(rbind,c)),2)
colnames(rs)=sts
rsr<- data.frame(cultivar=c('calima','BAT 1393'),rs,Significance=c('cultivar','stress***'))

## specific leaf area
sl=srgfO_cul[,c(1,2,8)]
test3<- aov(as.numeric(as.character(mx_slad)) ~ cultivar + stress, data=sl)
summary(test3)

c<-list()
for (i in 1:5){
  gg1<- sl %>% filter(stress==sts[i], cultivar=='calima')
  gg2<- sl %>% filter(stress==sts[i], cultivar=='BAT 1393')
  c[[i]]<- c(median(gg1$mx_slad), median(gg2$mx_slad)) 
}

sl <- round(t(do.call(rbind,c)),2)
colnames(sl)=sts
sla=data.frame(cultivar=c('calima','BAT 1393'),sl,Significance=c('cultivar','stress***'))

### LAI
la=srgfO_cul[,c(1,2,5)]
test3<- aov(as.numeric(as.character(lai)) ~ cultivar + stress, data=la)
summary(test3)

c<-list()
for (i in 1:5){
  gg1<- la %>% filter(stress==sts[i], cultivar=='calima')
  gg2<- la %>% filter(stress==sts[i], cultivar=='BAT 1393')
  if(i==4){
    gg1=gg1[-7,]; gg2<- gg2[-7,]
  }
  c[[i]]<- c(median(gg1$lai), median(gg2$lai)) 
}

sl <- round(t(do.call(rbind,c)),2)
colnames(sl)=sts

lai=data.frame(cultivar=c('calima','BAT 1393'),sl,Significance=c('cultivar**','stress***'))

## yield
yl=srgfO_cul[,c(1,2,6)]
test3<- aov(as.numeric(as.character(yld)) ~ cultivar + stress, data=yl)
summary(test3)

c<-list()
for (i in 1:5){
  gg1<- yl %>% filter(stress==sts[i], cultivar=='calima')
  gg2<- yl %>% filter(stress==sts[i], cultivar=='BAT 1393')
  c[[i]]<- c(median(gg1$yld), median(gg2$yld)) 
}

sl <- round(t(do.call(rbind,c)),2)
colnames(sl)=sts

yld=data.frame(cultivar=c('calima','BAT 1393'),sl,Significance=c('cultivar**','stress***'))

top<-rbind(pww,cww,rsr,sla,lai,yld)
nam<- rep(c('pww','cww','rsr','sla','lai','yld'),each=2)
top1<-data.frame(vari=nam,top)
write.csv(top1, 'E:/Prakash/dssat_outputs/tanzania/significance/top.csv')


## Significance of root related variables
rd=srgfO_cul[,c(1,2,10)]
test3<- aov(as.numeric(as.character(mx_rdpd)) ~ cultivar + stress, data=rd)
summary(test3)

c<-list()
for (i in 1:5){
  gg1<- rd %>% filter(stress==sts[i], cultivar=='calima')
  gg2<- rd %>% filter(stress==sts[i], cultivar=='BAT 1393')
  c[[i]]<- c(median(gg1$mx_rdpd), median(gg2$mx_rdpd)) 
}

sl <- round(t(do.call(rbind,c)),2)
colnames(sl)=sts
rd=data.frame(cultivar=c('calima','BAT 1393'),sl,Significance=c('cultivar','stress.'))

## root weight
rw=srgfO_cul[,c(1,2,11)]
test3<- aov(as.numeric(as.character(mx_rwad)) ~ cultivar + stress, data=rw)
summary(test3)

c<-list()
for (i in 1:5){
  gg1<- rw %>% filter(stress==sts[i], cultivar=='calima')
  gg2<- rw %>% filter(stress==sts[i], cultivar=='BAT 1393')
  c[[i]]<- c(median(gg1$mx_rwad), median(gg2$mx_rwad)) 
}

sl <- round(t(do.call(rbind,c)),2)
colnames(sl)=sts
rw=data.frame(cultivar=c('calima','BAT 1393'),sl,Significance=c('cultivar***','stress***'))

## total ET
et=srgfO_cul[,c(1,2,21)]
test3<- aov(as.numeric(as.character(ET)) ~ cultivar + stress, data=et)
summary(test3)

c<-list()
for (i in 1:5){
  gg1<- et %>% filter(stress==sts[i], cultivar=='calima')
  gg2<- et %>% filter(stress==sts[i], cultivar=='BAT 1393')
  c[[i]]<- c(median(gg1$ET), median(gg2$ET)) 
}

sl <- round(t(do.call(rbind,c)),2)
colnames(sl)=sts
et=data.frame(cultivar=c('calima','BAT 1393'),sl,Significance=c('cultivar','stress***'))

## RL1D
rl1<- srgfO_cul[,c(1,2,12)]
test3<- aov(as.numeric(as.character(mx_rl1d)) ~ cultivar + stress, data=rl1)
summary(test3)

c<-list()
for (i in 1:5){
  gg1<- rl1 %>% filter(stress==sts[i], cultivar=='calima')
  gg2<- rl1 %>% filter(stress==sts[i], cultivar=='BAT 1393')
  c[[i]]<- c(median(gg1$mx_rl1d), median(gg2$mx_rl1d)) 
}

sl <- round(t(do.call(rbind,c)),2)
colnames(sl)=sts
rl1=data.frame(cultivar=c('calima','BAT 1393'),sl,Significance=c('cultivar**','stress***'))

## RL2D
rl2=srgfO_cul[,c(1,2,13)]
test3<- aov(as.numeric(as.character(mx_rl2d)) ~ cultivar + stress, data=rl2)
summary(test3)

c<-list()
for (i in 1:5){
  gg1<- rl2 %>% filter(stress==sts[i], cultivar=='calima')
  gg2<- rl2 %>% filter(stress==sts[i], cultivar=='BAT 1393')
  c[[i]]<- c(median(gg1$mx_rl2d), median(gg2$mx_rl2d)) 
}

sl <- round(t(do.call(rbind,c)),2)
colnames(sl)=sts
rl2=data.frame(cultivar=c('calima','BAT 1393'),sl,Significance=c('cultivar***','stress***'))


## RL3D
rl3=srgfO_cul[,c(1,2,14)]
test3<- aov(as.numeric(as.character(mx_rl3d)) ~ cultivar + stress, data=rl3)
summary(test3)

c<-list()
for (i in 1:5){
  gg1<- rl3 %>% filter(stress==sts[i], cultivar=='calima')
  gg2<- rl3 %>% filter(stress==sts[i], cultivar=='BAT 1393')
  c[[i]]<- c(median(gg1$mx_rl3d), median(gg2$mx_rl3d)) 
}

sl <- round(t(do.call(rbind,c)),2)
colnames(sl)=sts
rl3=data.frame(cultivar=c('calima','BAT 1393'),sl,Significance=c('cultivar***','stress***'))

top2<-rbind(rd,rw,et,rl1,rl2,rl3)
nam1<-rep(c('rd','rw','et','rl1','rl2','rl3'),each=2)
top2<-data.frame(nam1,top2)

write.csv(top2, 'E:/Prakash/dssat_outputs/tanzania/significance/root.csv')
