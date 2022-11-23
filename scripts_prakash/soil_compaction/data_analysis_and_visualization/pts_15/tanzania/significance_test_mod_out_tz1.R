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
var_orig2_cul$yld= var_orig2_cul$yld/1000; var_orig2_cul$mx_slad= var_orig2_cul$mx_slad/10

## Significance test
## combine 
all <- data.frame(comp=c(rep('orig',150),rep('bd8',150),rep('bd17',150)),rbind(var_orig2_cul,srgfP_cul,srgfO_cul))
pww<- all[,1:4]

test3<-aov(as.numeric(as.character(pwam)) ~ comp + stress + cultivar, data=pww)
summary(test3)

fit = lm( pwam ~ comp + stress + cultivar, data=pww )
anova(fit)

af <- anova(fit)
afss <- af$"Sum Sq"
df<-print(cbind(af,PctExp=round(afss/sum(afss)*100,2)))

sts<- unique(as.character(pww$stress))
comp1<-unique(pww$comp)
cult1<-unique(as.character(pww$cultivar))

s<-array(numeric(),c(length(comp1),length(sts),length(cult1)))

for (j in 1:3){ ## comp
  for (i in 1:length(sts)){ ## stress
    for (k in 1:2){ ## cultiv
      gg<- pww %>% filter(stress==sts[i] & comp==comp1[j] & cultivar==cult1[k])
      s[j,i,k]<-median(gg$pwam)
    }
  }
}
pw=round(rbind(s[,,1],s[,,2]),1)
rownames(pw)<- rep(c('no comp', '8%bd', '17%bd'), 2)
colnames(pw) <- sts
pw <- data.frame(culti=rep(c('Calima','BAT 1393'),each=3),pw, var=rep(df$PctExp[1:3],2), Signi=rep(c('comp***','sts***','cult***'),2))

## Top weight
cw=all[,c(1,2,3,5)]
test3<-aov(as.numeric(as.character(cwam)) ~ comp + stress + cultivar, data=cw)

summary(test3)

fit = lm( cwam ~ comp + stress + cultivar, data=cw )
anova(fit)

af <- anova(fit)
afss <- af$"Sum Sq"
df<-print(cbind(af,PctExp=round(afss/sum(afss)*100,2)))

sts<- unique(as.character(cw$stress))
comp1<-unique(cw$comp)
cult1<-unique(as.character(cw$cultivar))


c<-array(numeric(),c(length(comp1),length(sts),length(cult1)))

for (j in 1:3){ ## comp
  for (i in 1:length(sts)){ ## stress
    for (k in 1:2){ ## cultiv
      gg<- cw %>% filter(stress==sts[i] & comp==comp1[j] & cultivar==cult1[k])
      c[j,i,k]<-median(gg$cwam)
    }
  }
}

cw=round(rbind(c[,,1],c[,,2]),1)
rownames(cw)<- rep(c('no comp', '8%bd', '17%bd'), 2)
colnames(cw) <- sts
cw <- data.frame(culti=rep(c('Calima','BAT 1393'),each=3),cw, Signi=rep(c('comp***','sts***','cult***'),2),var=rep(df$PctExp[1:3],2))

## specific leaf area
sl=all[,c(1,2,3,9)]
test3<-aov(as.numeric(as.character(mx_slad)) ~ comp + stress + cultivar, data=sl)
summary(test3)

fit = lm( mx_slad ~ comp + stress + cultivar, data=sl)
anova(fit)

af <- anova(fit)
afss <- af$"Sum Sq"
df<-print(cbind(af,PctExp=round(afss/sum(afss)*100,2)))

sts<- unique(as.character(sl$stress))
comp1<-unique(sl$comp)
cult1<-unique(as.character(sl$cultivar))

c<-array(numeric(),c(length(comp1),length(sts),length(cult1)))

for (j in 1:3){ ## comp
  for (i in 1:length(sts)){ ## stress
    for (k in 1:2){ ## cultiv
      gg<- sl %>% filter(stress==sts[i] & comp==comp1[j] & cultivar==cult1[k])
      c[j,i,k]<-median(gg$mx_slad)
    }
  }
}

sl=round(rbind(c[,,1],c[,,2]),1)
rownames(sl)<- rep(c('no comp', '8%bd', '17%bd'), 2)
colnames(sl) <- sts
sl <- data.frame(culti=rep(c('Calima','BAT 1393'),each=3),sl, Signi=rep(c('comp***','sts***','cult'),2),var=rep(df$PctExp[1:3],2))

### LAI
la=all[,c(1,2,3,6)]
test3<-aov(as.numeric(as.character(lai)) ~ comp + stress + cultivar, data=la)
summary(test3)

fit = lm( lai ~ comp + stress + cultivar, data=la)
anova(fit)

af <- anova(fit)
afss <- af$"Sum Sq"
df<-print(cbind(af,PctExp=round(afss/sum(afss)*100,2)))

sts<- unique(as.character(la$stress))
comp1<-unique(la$comp)
cult1<-unique(as.character(la$cultivar))

c<-array(numeric(),c(length(comp1),length(sts),length(cult1)))

for (j in 1:3){ ## comp
  for (i in 1:length(sts)){ ## stress
    for (k in 1:2){ ## cultiv
      gg<- la %>% filter(stress==sts[i] & comp==comp1[j] & cultivar==cult1[k])
      c[j,i,k]<-median(gg$lai)
    }
  }
}

la=round(rbind(c[,,1],c[,,2]),1)
rownames(la)<- rep(c('no comp', '8%bd', '17%bd'), 2)
colnames(la) <- sts
la <- data.frame(culti=rep(c('Calima','BAT 1393'),each=3),la, Signi=rep(c('comp***','sts***','cult***'),2),var=rep(df$PctExp[1:3],2))


## yield
yl=all[,c(1,2,3,7)]
test3<- aov(as.numeric(as.character(yld)) ~ comp + stress + cultivar, data=yl)
summary(test3)

fit = lm( yld ~ comp + stress + cultivar, data=yl)
anova(fit)

af <- anova(fit)
afss <- af$"Sum Sq"
df<-print(cbind(af,PctExp=round(afss/sum(afss)*100,2)))

sts<- unique(as.character(yl$stress))
comp1<-unique(yl$comp)
cult1<-unique(as.character(yl$cultivar))

c<-array(numeric(),c(length(comp1),length(sts),length(cult1)))

for (j in 1:3){ ## comp
  for (i in 1:length(sts)){ ## stress
    for (k in 1:2){ ## cultiv
      gg<- yl %>% filter(stress==sts[i] & comp==comp1[j] & cultivar==cult1[k])
      c[j,i,k]<-median(gg$yld)
    }
  }
}

yl=round(rbind(c[,,1],c[,,2]),1)
rownames(yl)<- rep(c('no comp', '8%bd', '17%bd'), 2)
colnames(yl) <- sts
yl <- data.frame(culti=rep(c('Calima','BAT 1393'),each=3),yl, Signi=rep(c('comp***','sts***','cult**'),2),var=rep(df$PctExp[1:3],2))

## total ET
et=all[,c(1,2,3,22)]
test3<- aov(as.numeric(as.character(ET)) ~ comp + stress + cultivar, data=et)
summary(test3)

af <- anova(fit)
afss <- af$"Sum Sq"
df<-print(cbind(af,PctExp=round(afss/sum(afss)*100,2)))

sts<- unique(as.character(et$stress))
comp1<-unique(et$comp)
cult1<-unique(as.character(et$cultivar))

c<-array(numeric(),c(length(comp1),length(sts),length(cult1)))

for (j in 1:3){ ## comp
  for (i in 1:length(sts)){ ## stress
    for (k in 1:2){ ## cultiv
      gg<- et %>% filter(stress==sts[i] & comp==comp1[j] & cultivar==cult1[k])
      c[j,i,k]<-median(gg$ET)
    }
  }
}

ET=round(rbind(c[,,1],c[,,2]),1)
rownames(ET)<- rep(c('no comp', '8%bd', '17%bd'), 2)
colnames(ET) <- sts
ET <- data.frame(culti=rep(c('Calima','BAT 1393'),each=3),ET, Signi=rep(c('comp***','sts***','cult'),2),var=rep(df$PctExp[1:3],2))

#top<-rbind(pw,cw,rs,sl,la,yl)
#nam<- rep(c('pww','cww','rsr','sla','lai','yld'),each=3)
top <- rbind(yl,pw,cw,la,sl,ET)
nam<- rep(c('yld','pww','cww','lai','sla','ET'),each=6)

top1<-data.frame(vari=nam,top)

write.csv(top1, 'E:/Prakash/dssat_outputs/tanzania/significance/top.csv')

## Significance of root related variables
## Root:shoot
rs<- all[,c(1,2,3,23)]
test3<-aov(as.numeric(as.character(r_s)) ~ comp + stress + cultivar, data=rs)
summary(test3)

fit = lm( r_s ~ comp + stress + cultivar, data=rs )
anova(fit)

af <- anova(fit)
afss <- af$"Sum Sq"
df<-print(cbind(af,PctExp=round(afss/sum(afss)*100,2)))

sts<- unique(as.character(rs$stress))
comp1<-unique(rs$comp)
cult1<-unique(as.character(rs$cultivar))


c<-array(numeric(),c(length(comp1),length(sts),length(cult1)))

for (j in 1:3){ ## comp
  for (i in 1:length(sts)){ ## stress
    for (k in 1:2){ ## cultiv
      gg<- rs %>% filter(stress==sts[i] & comp==comp1[j] & cultivar==cult1[k])
      c[j,i,k]<-median(gg$r_s)
    }
  }
}

rs=round(rbind(c[,,1],c[,,2]),1)
rownames(rs)<- rep(c('no comp', '8%bd', '17%bd'), 2)
colnames(rs) <- sts
rs <- data.frame(culti=rep(c('Calima','BAT 1393'),each=3),rs, Signi=rep(c('comp***','sts***','cult'),2),var=rep(df$PctExp[1:3],2))

rd=all[,c(1,2,3,11)]
test3<- aov(as.numeric(as.character(mx_rdpd)) ~ comp + stress + cultivar, data=rd)
summary(test3)

fit = lm( mx_rdpd ~ comp + stress + cultivar, data=rd)
anova(fit)

af <- anova(fit)
afss <- af$"Sum Sq"
df<-print(cbind(af,PctExp=round(afss/sum(afss)*100,2)))

sts<- unique(as.character(rd$stress))
comp1<-unique(rd$comp)
cult1<-unique(as.character(rd$cultivar))

c<-array(numeric(),c(length(comp1),length(sts),length(cult1)))

for (j in 1:3){ ## comp
  for (i in 1:length(sts)){ ## stress
    for (k in 1:2){ ## cultiv
      gg<- rd %>% filter(stress==sts[i] & comp==comp1[j] & cultivar==cult1[k])
      c[j,i,k]<-median(gg$mx_rdpd)
    }
  }
}

rd=round(rbind(c[,,1],c[,,2]),1)
rownames(rd)<- rep(c('no comp', '8%bd', '17%bd'), 2)
colnames(rd) <- sts
rd <- data.frame(culti=rep(c('Calima','BAT 1393'),each=3),rd, Signi=rep(c('comp***','sts**','cult'),2),var=rep(df$PctExp[1:3],2))

## root weight
rw=all[,c(1,2,3,12)]
test3<- aov(as.numeric(as.character(mx_rwad)) ~ comp + stress + cultivar, data=rw)
summary(test3)

fit = lm( mx_rwad ~ comp + stress + cultivar, data=rw)
anova(fit)

af <- anova(fit)
afss <- af$"Sum Sq"
df<-print(cbind(af,PctExp=round(afss/sum(afss)*100,2)))

sts<- unique(as.character(rw$stress))
comp1<-unique(rw$comp)
cult1<-unique(as.character(rw$cultivar))

c<-array(numeric(),c(length(comp1),length(sts),length(cult1)))

for (j in 1:3){ ## comp
  for (i in 1:length(sts)){ ## stress
    for (k in 1:2){ ## cultiv
      gg<- rw %>% filter(stress==sts[i] & comp==comp1[j] & cultivar==cult1[k])
      c[j,i,k]<-median(gg$mx_rwad)
    }
  }
}

rw=round(rbind(c[,,1],c[,,2]),1)
rownames(rw)<- rep(c('no comp', '8%bd', '17%bd'), 2)
colnames(rw) <- sts
rw <- data.frame(culti=rep(c('Calima','BAT 1393'),each=3),rw, Signi=rep(c('comp***','sts***','cult***'),2),var=rep(df$PctExp[1:3],2))

## RL1D
rl1<- all[,c(1,2,3,13)]
test3<- aov(as.numeric(as.character(mx_rl1d)) ~ comp + stress + cultivar, data=rl1)
summary(test3)

fit = lm( mx_rl1d ~ comp + stress + cultivar, data=rl1)
anova(fit)

af <- anova(fit)
afss <- af$"Sum Sq"
df<-print(cbind(af,PctExp=round(afss/sum(afss)*100,2)))

sts<- unique(as.character(rl1$stress))
comp1<-unique(rl1$comp)
cult1<-unique(as.character(rl1$cultivar))

c<-array(numeric(),c(length(comp1),length(sts),length(cult1)))

for (j in 1:3){ ## comp
  for (i in 1:length(sts)){ ## stress
    for (k in 1:2){ ## cultiv
      gg<- rl1 %>% filter(stress==sts[i] & comp==comp1[j] & cultivar==cult1[k])
      c[j,i,k]<-median(gg$mx_rl1d)
    }
  }
}

rl1=round(rbind(c[,,1],c[,,2]),1)
rownames(rl1)<- rep(c('no comp', '8%bd', '17%bd'), 2)
colnames(rl1) <- sts
rl1 <- data.frame(culti=rep(c('Calima','BAT 1393'),each=3),rl1, Signi=rep(c('comp***','sts***','cult***'),2),var=rep(df$PctExp[1:3],2))

## RL2D
rl2=all[,c(1,2,3,14)]
test3<- aov(as.numeric(as.character(mx_rl2d)) ~ comp + stress + cultivar, data=rl2)
summary(test3)

fit = lm( mx_rl2d ~ comp + stress + cultivar, data=rl2)
anova(fit)

af <- anova(fit)
afss <- af$"Sum Sq"
df<-print(cbind(af,PctExp=round(afss/sum(afss)*100,2)))

sts<- unique(as.character(rl2$stress))
comp1<-unique(rl2$comp)
cult1<-unique(as.character(rl2$cultivar))

c<-array(numeric(),c(length(comp1),length(sts),length(cult1)))

for (j in 1:3){ ## comp
  for (i in 1:length(sts)){ ## stress
    for (k in 1:2){ ## cultiv
      gg<- rl2 %>% filter(stress==sts[i] & comp==comp1[j] & cultivar==cult1[k])
      c[j,i,k]<-median(gg$mx_rl2d)
    }
  }
}

rl2=round(rbind(c[,,1],c[,,2]),1)
rownames(rl2)<- rep(c('no comp', '8%bd', '17%bd'), 2)
colnames(rl2) <- sts
rl2 <- data.frame(culti=rep(c('Calima','BAT 1393'),each=3),rl2, Signi=rep(c('comp','sts***','cult***'),2),var=rep(df$PctExp[1:3],2))


## RL3D
rl3=all[,c(1,2,3,15)]
test3<- aov(as.numeric(as.character(mx_rl3d)) ~ comp + stress + cultivar, data=rl3)
summary(test3)

fit = lm( mx_rl3d ~ comp + stress + cultivar, data=rl3)
anova(fit)

af <- anova(fit)
afss <- af$"Sum Sq"
df<-print(cbind(af,PctExp=round(afss/sum(afss)*100,2)))

sts<- unique(as.character(rl3$stress))
comp1<-unique(rl3$comp)
cult1<-unique(as.character(rl3$cultivar))

c<-array(numeric(),c(length(comp1),length(sts),length(cult1)))

for (j in 1:3){ ## comp
  for (i in 1:length(sts)){ ## stress
    for (k in 1:2){ ## cultiv
      gg<- rl3 %>% filter(stress==sts[i] & comp==comp1[j] & cultivar==cult1[k])
      c[j,i,k]<-median(gg$mx_rl3d)
    }
  }
}

rl3=round(rbind(c[,,1],c[,,2]),1)
rownames(rl3)<- rep(c('no comp', '8%bd', '17%bd'), 2)
colnames(rl3) <- sts
rl3 <- data.frame(culti=rep(c('Calima','BAT 1393'),each=3),rl3, Signi=rep(c('comp***','sts***','cult***'),2),var=rep(df$PctExp[1:3],2))

top2<-rbind(rs,rd,rw,rl1,rl2,rl3)
nam1<-rep(c('rsr','rd','rw','rl1','rl2','rl3'),each=6)
top2<-data.frame(nam1,top2)

write.csv(top2, 'E:/Prakash/dssat_outputs/tanzania/significance/root.csv')
