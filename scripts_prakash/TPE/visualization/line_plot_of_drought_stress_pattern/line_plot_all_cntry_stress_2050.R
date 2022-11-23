rm(list=ls())
library(dplyr)
library(ggplot2)
library(stringi)
library(ggpubr)
theme_set(theme_pubr())


path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/tanzania/tpe/PlantGro/cluster/'
#path<-'D:/OneDrive - CGIAR/Desktop/cluster/cluster_tanzania/'

## clustering data
mod <- c('BCC-CSM2-MR','EC-Earth3-Veg','GFDL-ESM4','IPSL-CM6A-LR','MRI-ESM2-0')
level<-c('126', '245', '370', '585')

# dimension of domain
comb <- data.frame(year = c(rep(2030,20),rep(2050,20)), mod = c(rep(mod,each=4),rep(mod,each=4)), sc = rep(c(126,245,370,585),10),
                   period=c(rep('202101-204012',20),rep('204101-206012',20)), co2=c(rep(c(440,446,454,456),5),rep(c(469,508,544,569),5)),
                   stringsAsFactors = FALSE)
fig='main'
#fig='supp'

name<-list()
for (n in 1:5){
  name[[n]]<-c(paste0(mod[n],'_ssp_',level[1:4]))
}
name<-c(unlist(name),'Historical')
lab=c('V1', 'V2', 'V3', 'R1', 'R3','R5','R7','R8')
allf<-readRDS(paste0(path,'tz_combined_line_plot_data_2050.RDS'))

allf1<-cbind(allf,rep(name,each=120))
colnames(allf1)<-c('stages','prob','cluster','model')
## take out historical
hist<-allf1 %>% filter(model=='Historical')
## change color of historical
hist$cluster[hist$cluster=='clust_3']='clust_6'
hist$cluster[hist$cluster=='clust_4']='clust_7'
hist$cluster[hist$cluster=='clust_5']='clust_8'

hist$cluster[hist$cluster=='clust_2']='clust_3'
hist$cluster[hist$cluster=='clust_6']='clust_5'
hist$cluster[hist$cluster=='clust_7']='clust_2'
hist$cluster[hist$cluster=='clust_8']='clust_4'

mm1<-allf1[!grepl("historical", allf1$model),]  # exclude historical from model
mm1<-allf1 %>% mutate(ssp=stri_sub(model,-3,-1))

gr<-c('clust_1','clust_2','clust_3','clust_4','clust_5')

qall<-list()
q_ss_all<-list()
ssp<-c(126,245,370,585)

for(ss in 1:4){ ## ssp
  for (l in 1:5){ ## cluster
   	mm2<-mm1[which(mm1$cluster==gr[l] & mm1$ssp==ssp[ss]),]
    q1<-data.frame(mod1=mm2[1:8,2],mod2=mm2[25:32,2],mod3=mm2[49:56,2],mod4=mm2[73:80,2],mod5=mm2[97:104,2]) ## 75percentile
    q1_mx<-q1 %>% rowwise() %>% mutate(mx=mean(c_across(mod1:mod5))) %>% dplyr::select(mx)
    q2<-data.frame(mod1=mm2[17:24,2],mod2=mm2[41:48,2],mod3=mm2[65:72,2],mod4=mm2[89:96,2],mod5=mm2[113:120,2]) ##25percentile
    q1_min<-q2 %>% rowwise() %>% mutate(minm=mean(c_across(mod1:mod5))) %>% dplyr::select(minm)
    q1_avg<-rowMeans(cbind(q1_min,q1_mx))
    q1_f<-c(t(as.matrix(q1_min)),q1_avg,t(as.matrix(q1_mx)))
    qall[[l]]<-q1_f
  }
  prob_mod1<-c(qall[[1]],qall[[2]],qall[[3]],qall[[4]],qall[[5]])
  q_ss_all[[ss]]<-prob_mod1
}

prob_tz<-c(q_ss_all[[1]],q_ss_all[[2]],q_ss_all[[3]],q_ss_all[[4]])
stg<-rep(rep(rep(1:8,3),5),4)
clst<-rep(c(rep('clust_1',24),rep('clust_2',24),rep('clust_3',24),rep('clust_4',24),rep('clust_5',24)),4)
sc<-c(rep(126,120),rep(245,120),rep(370,120),rep(585,120))
dat_mod<-data.frame(stages=stg,prob=prob_tz,cluster=clst,ssp=sc)
dat_mod$ssp<-as.character(dat_mod$ssp)

dat_mod1<-dat_mod %>% mutate(ssp,scenario=ifelse(ssp=='126','SSP_126',
                                                 ifelse(ssp=='245','SSP_245',
                                                        ifelse(ssp=='370','SSP_370','SSP_585')))) %>%
  dplyr::select(stages,prob,cluster,scenario)

## Historical
colnames(hist)<-colnames(dat_mod1)
dat_all<-rbind(hist,dat_mod1)

dat_all$cluster[dat_all$cluster=='clust_1']='1: SF'
dat_all$cluster[dat_all$cluster=='clust_2']='2: AS'
dat_all$cluster[dat_all$cluster=='clust_3']='3: MTS'
dat_all$cluster[dat_all$cluster=='clust_4']='4: STS'
dat_all$cluster[dat_all$cluster=='clust_5']='5: ETS'

## select only Hist, SSP 126 and SSP585
histr<-dat_all %>% filter(scenario==c('Historical')) 

if(fig=='supp'){
  dt_50_1<- dat_all %>% filter(scenario=='SSP_245')
  dt_50_2<- dat_all %>% filter(scenario=='SSP_370') 
} else {
  dt_50_1<- dat_all %>% filter(scenario=='SSP_126')
  dt_50_2<- dat_all %>% filter(scenario=='SSP_585')
}

dt_50<-rbind(dt_50_1,dt_50_2) %>% rowwise %>% mutate(scen=paste0(scenario,'_',2050)) %>%dplyr::select(-scenario)

## Load 2030 data of SSP 126 and 585
if (fig=='supp'){
  dt23<-read.csv('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/data_line_plot_2030_245_370_all_cntry.csv')
} else {
  dt23<-read.csv('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/data_line_plot_2030_126_585_all_cntry.csv')
}

## filter tz
dt23_tz<-dt23 %>% filter(cntry=='Tanz') %>% rowwise %>% mutate(scen=paste0(scenario,'_',2030)) %>%dplyr::select(-c(cntry,scenario))
dt23_tz$cluster[dt23_tz$cluster=='clust_1: Stress-free']='1: SF'
dt23_tz$cluster[dt23_tz$cluster=='clust_2: Veg+flwr+grain filling']='2: AS'
dt23_tz$cluster[dt23_tz$cluster=='clust_3: Terminal moderate']='3: MTS'
dt23_tz$cluster[dt23_tz$cluster=='clust_4: Terminal severe']='4: STS'
dt23_tz$cluster[dt23_tz$cluster=='clust_5: Terminal very severe']='5: ETS'

dt23_tz<-dt23_tz[,2:5]

## combine hist, 2030 and 2050
colnames(histr)<-colnames(dt_50)
dt_tz<- rbind(histr, dt23_tz, dt_50)

if(fig=='supp'){
  ### For supplementary
  dt_tz$scen[dt_tz$scen=='SSP_245_2030']='SSP2-4.5 (2030)'
  dt_tz$scen[dt_tz$scen=='SSP_370_2030']='SSP3-7.0 (2030)'
  dt_tz$scen[dt_tz$scen=='SSP_245_2050']='SSP2-4.5 (2050)'
  dt_tz$scen[dt_tz$scen=='SSP_370_2050']='SSP3-7.0 (2050)'
  
  dt_tz<-transform(dt_tz,
                   scen=factor(scen,levels=c('Historical','SSP2-4.5 (2030)','SSP3-7.0 (2030)','SSP2-4.5 (2050)','SSP3-7.0 (2050)')))
} else {
  dt_tz$scen[dt_tz$scen=='SSP_126_2030']='SSP1-2.6 (2030)'
  dt_tz$scen[dt_tz$scen=='SSP_585_2030']='SSP5-8.5 (2030)'
  dt_tz$scen[dt_tz$scen=='SSP_126_2050']='SSP1-2.6 (2050)'
  dt_tz$scen[dt_tz$scen=='SSP_585_2050']='SSP5-8.5 (2050)'
  
  dt_tz<-transform(dt_tz,
                   scen=factor(scen,levels=c('Historical','SSP1-2.6 (2030)','SSP5-8.5 (2030)','SSP1-2.6 (2050)','SSP5-8.5 (2050)')))
}

jj<-ggplot(data = dt_tz) +
  geom_smooth(mapping = aes(x = stages, y = prob, fill=cluster, color=cluster)) + 
  facet_wrap(vars(scen), nrow = 5) +
  scale_color_manual(values=c('#377eb8','#feb24c','#e31a1c','#67000d','#984ea3')) + coord_cartesian(ylim=c(0,1)) +
  scale_fill_manual(values=c('#3182bd','#ffeda0','#fc4e2a','#a50f15','#9e9ac8')) +
  xlab('Growth Stages') + ylab('Water stress index (WSPD)') + theme_bw() + 
  theme(legend.position = 'bottom',legend.text=element_text(size=8), 
        legend.key.size = unit(0.4, "cm"),legend.key.width = unit(0.4,"cm"), legend.title = element_blank()) + 
    scale_x_continuous(breaks=1:8,labels=lab)  

# ggsave(plot = jj,
#        filename = paste0(path,'test_line_plot_multi_mod_mean_tz_2050.png'),
#        width = 7, height = 10, units = 'in', dpi = 300)

################################################################################################################
path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ethiopia/tpe/PlantGro/final/cluster/'
allf<-readRDS(paste0(path,'et_combined_line_plot_data_2050.RDS'))

et_allf1<-cbind(allf,rep(name,each=96))
colnames(et_allf1)<-c('stages','prob','cluster','model')
## take out historical
et_hist<-et_allf1 %>% filter(model=='Historical')
## change color of historical
et_mm1<-et_allf1[!grepl("Historical", et_allf1$model),]  # exclude historical from model
et_mm1<-et_allf1 %>% mutate(ssp=stri_sub(model,-3,-1))

## cluster2 is stress free
# et_mm1$cluster[et_mm1$cluster=='clust_2']='clust_5'
# et_mm1$cluster[et_mm1$cluster=='clust_1']='clust_2'
# et_mm1$cluster[et_mm1$cluster=='clust_5']='clust_1'

## repeat this for each cluster and ssp
## 1. min
gr<-c('clust_1','clust_2','clust_3','clust_4')
qall<-list()
q_ss_all<-list()
ssp<-c(126,245,370,585)

for(ss in 1:4){ ## ssp
  for (l in 1:4){ ### cluster
    et_mm2<-et_mm1[which(et_mm1$cluster==gr[l] & et_mm1$ssp==ssp[ss]),]
    q1<-data.frame(mod1=et_mm2[1:8,2],mod2=et_mm2[25:32,2],mod3=et_mm2[49:56,2],mod4=et_mm2[73:80,2],mod5=et_mm2[97:104,2])
    q1_mx<-q1 %>% rowwise() %>% mutate(mx=mean(c_across(mod1:mod5))) %>% dplyr::select(mx)
    q2<-data.frame(mod1=et_mm2[17:24,2],mod2=et_mm2[41:48,2],mod3=et_mm2[65:72,2],mod4=et_mm2[89:96,2],mod5=et_mm2[113:120,2])##25th percentile
    q1_min<-q2 %>% rowwise() %>% mutate(minm=mean(c_across(mod1:mod5))) %>% dplyr::select(minm)
    q1_avg<-rowMeans(cbind(q1_min,q1_mx))
    q1_f<-c(t(as.matrix(q1_min)),q1_avg,t(as.matrix(q1_mx)))
    qall[[l]]<-q1_f
  }
  prob_mod1<-c(qall[[1]],qall[[2]],qall[[3]],qall[[4]])
  q_ss_all[[ss]]<-prob_mod1
}

prob_et<-c(q_ss_all[[1]],q_ss_all[[2]],q_ss_all[[3]],q_ss_all[[4]])
stg<-rep(rep(rep(1:8,3),4),4)
clst<-rep(c(rep('clust_1',24),rep('clust_2',24),rep('clust_3',24),rep('clust_4',24)),4)
sc<-c(rep(126,96),rep(245,96),rep(370,96),rep(585,96))
dat_mod<-data.frame(stages=stg,prob=prob_et,cluster=clst,ssp=sc)
dat_mod$ssp<-as.character(dat_mod$ssp)

et_dat_mod1<-dat_mod %>% mutate(ssp,scenario=ifelse(ssp=='126','SSP_126',
                                                    ifelse(ssp=='245','SSP_245',
                                                           ifelse(ssp=='370','SSP_370','SSP_585')))) %>%
  dplyr::select(stages,prob,cluster,scenario)


## Historical
colnames(et_hist)<-colnames(et_dat_mod1)
dat_all<-rbind(et_hist,et_dat_mod1)

dat_all$cluster[dat_all$cluster=='clust_2']='clust_5'
dat_all$cluster[dat_all$cluster=='clust_1']='clust_2'
dat_all$cluster[dat_all$cluster=='clust_5']='clust_1'

dat_all$cluster[dat_all$cluster=='clust_1']='1: SF'
dat_all$cluster[dat_all$cluster=='clust_2']='2: GS'
dat_all$cluster[dat_all$cluster=='clust_3']='3: MTS'
dat_all$cluster[dat_all$cluster=='clust_4']='4: ETS'

## select only Hist, SSP 126 and SSP585
histr<-dat_all %>% filter(scenario==c('Historical'))

if(fig=='supp'){
  dt_50_1<- dat_all %>% filter(scenario=='SSP_245')
  dt_50_2<- dat_all %>% filter(scenario=='SSP_370')
} else{
  dt_50_1<- dat_all %>% filter(scenario=='SSP_126')
  dt_50_2<- dat_all %>% filter(scenario=='SSP_585')
}

dt_50<-rbind(dt_50_1,dt_50_2) %>% rowwise %>% mutate(scen=paste0(scenario,'_',2050)) %>%dplyr::select(-scenario)

## select Ethiopia
dt23_et<-dt23 %>% filter(cntry=='Ethi') %>% rowwise %>% mutate(scen=paste0(scenario,'_',2030)) %>%dplyr::select(-c(cntry,scenario))
dt23_et$cluster[dt23_et$cluster=='clust_1: Stress-free']='1: SF'
dt23_et$cluster[dt23_et$cluster=='clust_2: Veg+flwr+grain filling']='2: GS'
dt23_et$cluster[dt23_et$cluster=='clust_3: Terminal moderate']='3: MTS'
dt23_et$cluster[dt23_et$cluster=='clust_4: Terminal severe']='4: ETS'

dt23_et<-dt23_et[,2:5]

## combine hist, 2030 and 2050
colnames(histr)<-colnames(dt_50)
dt_et<- rbind(histr, dt23_et, dt_50)

if(fig=='supp'){
  ### For supplementary
  dt_et$scen[dt_et$scen=='SSP_245_2030']='SSP2-4.5 (2030)'
  dt_et$scen[dt_et$scen=='SSP_370_2030']='SSP3-7.0 (2030)'
  dt_et$scen[dt_et$scen=='SSP_245_2050']='SSP2-4.5 (2050)'
  dt_et$scen[dt_et$scen=='SSP_370_2050']='SSP3-7.0 (2050)'
  
  dt_et<-transform(dt_et,
                   scen=factor(scen,levels=c('Historical','SSP2-4.5 (2030)','SSP3-7.0 (2030)','SSP2-4.5 (2050)','SSP3-7.0 (2050)')))
} else {
  dt_et$scen[dt_et$scen=='SSP_126_2030']='SSP1-2.6 (2030)'
  dt_et$scen[dt_et$scen=='SSP_585_2030']='SSP5-8.5 (2030)'
  dt_et$scen[dt_et$scen=='SSP_126_2050']='SSP1-2.6 (2050)'
  dt_et$scen[dt_et$scen=='SSP_585_2050']='SSP5-8.5 (2050)'
  
  dt_et<-transform(dt_et,
                   scen=factor(scen,levels=c('Historical','SSP1-2.6 (2030)','SSP5-8.5 (2030)','SSP1-2.6 (2050)','SSP5-8.5 (2050)')))
}


kk<-ggplot(data = dt_et) +
  geom_smooth(mapping = aes(x = stages, y = prob, fill=cluster, color=cluster)) + 
  facet_wrap(vars(scen), nrow = 5) +
  scale_color_manual(values=c('#377eb8','#ec7014','#e31a1c','#984ea3')) + coord_cartesian(ylim=c(0,1)) +
  scale_fill_manual(values=c('#3182bd','#fe9929','#fc4e2a','#9e9ac8')) +
  xlab('Growth Stages') + ylab('') + theme_bw() + 
  theme(legend.position = 'bottom',legend.text=element_text(size=8), legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),legend.key.width = unit(0.4,"cm"), axis.text.y = element_blank()) + 
   scale_x_continuous(breaks=1:8,labels=lab)  

# ggsave(plot = kk,
#        filename = paste0(path,'test_line_plot_multi_mod_mean_et_2050.png'),
#        width = 7, height = 10, units = 'in', dpi = 300)
###########################################################################################################
path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ugand/tpe/PlantGro/final/cluster/cluster_pltgro_yr_sowdt/'
allf<-readRDS(paste0(path,'ug_combined_line_plot_data_2050.RDS'))

ug_allf1<-cbind(allf,rep(name,each=48))
colnames(ug_allf1)<-c('stages','prob','cluster','model')
## take out historical
ug_hist<-ug_allf1 %>% filter(model=='Historical')

## change color of historical
ug_mm1<-ug_allf1[!grepl("Historical", ug_allf1$model),]  # exclude historical from model
ug_mm1<-ug_allf1 %>% mutate(ssp=stri_sub(model,-3,-1))

## change colors
# ug_mm1$cluster[ug_mm1$cluster=='clust_1']='clust_3'
# ug_mm1$cluster[ug_mm1$cluster=='clust_2']='clust_1'
# ug_mm1$cluster[ug_mm1$cluster=='clust_3']='clust_2'

## repeat this for each cluster and ssp
## 1. min
gr<-c('clust_1','clust_2')

qall<-list()
q_ss_all<-list()
ssp<-c(126,245,370,585)

for(ss in 1:4){ ## ssp
  for (l in 1:2){ ### cluster
    ug_mm2<-ug_mm1[which(ug_mm1$cluster==gr[l] & ug_mm1$ssp==ssp[ss]),]
    q1<-data.frame(mod1=ug_mm2[1:8,2],mod2=ug_mm2[25:32,2],mod3=ug_mm2[49:56,2],mod4=ug_mm2[73:80,2],mod5=ug_mm2[97:104,2])
    q1_mx<-q1 %>% rowwise() %>% mutate(mx=mean(c_across(mod1:mod5))) %>% dplyr::select(mx)
    q2<-data.frame(mod1=ug_mm2[17:24,2],mod2=ug_mm2[41:48,2],mod3=ug_mm2[65:72,2],mod4=ug_mm2[89:96,2],mod5=ug_mm2[113:120,2])##25th percentile
    q1_min<-q2 %>% rowwise() %>% mutate(minm=mean(c_across(mod1:mod5))) %>% dplyr::select(minm)
    q1_avg<-rowMeans(cbind(q1_min,q1_mx))
    q1_f<-c(t(as.matrix(q1_min)),q1_avg,t(as.matrix(q1_mx)))
    qall[[l]]<-q1_f
  }
  prob_mod1<-c(qall[[1]],qall[[2]])
  q_ss_all[[ss]]<-prob_mod1
}

prob_ug<-c(q_ss_all[[1]],q_ss_all[[2]],q_ss_all[[3]],q_ss_all[[4]])
stg<-rep(rep(rep(1:8,3),2),4)

clst<-rep(c(rep('clust_1',24),rep('clust_2',24)),4)
sc<-c(rep(126,48),rep(245,48),rep(370,48),rep(585,48))

dat_mod<-data.frame(stages=stg,prob=prob_ug,cluster=clst,ssp=sc)
dat_mod$ssp<-as.character(dat_mod$ssp)

ug_dat_mod1<-dat_mod %>% mutate(ssp,scenario=ifelse(ssp=='126','SSP_126',
                                                    ifelse(ssp=='245','SSP_245',
                                                           ifelse(ssp=='370','SSP_370','SSP_585')))) %>%
  dplyr::select(stages,prob,cluster,scenario)


## Historical
colnames(ug_hist)<-colnames(ug_dat_mod1)
dat_all_f<-rbind(ug_hist,ug_dat_mod1)

dat_all_f$cluster[dat_all_f$cluster=='clust_1']='1: SF'
dat_all_f$cluster[dat_all_f$cluster=='clust_2']='2: STS'


## select only Hist, SSP 126 and SSP585
histr<-dat_all_f %>% filter(scenario==c('Historical'))

if(fig=='supp'){
  dt_50_1<- dat_all_f %>% filter(scenario=='SSP_245')
  dt_50_2<- dat_all_f %>% filter(scenario=='SSP_370')
} else {
  dt_50_1<- dat_all_f %>% filter(scenario=='SSP_126')
  dt_50_2<- dat_all_f %>% filter(scenario=='SSP_585')
}


dt_50<-rbind(dt_50_1,dt_50_2) %>% rowwise %>% mutate(scen=paste0(scenario,'_',2050)) %>%dplyr::select(-scenario)

## select Ethiopia
dt23_ug<-dt23 %>% filter(cntry=='Ug') %>% rowwise %>% mutate(scen=paste0(scenario,'_',2030)) %>%dplyr::select(-c(cntry,scenario))
dt23_ug$cluster[dt23_ug$cluster=='clust_1: Stress-free']='1: SF'
dt23_ug$cluster[dt23_ug$cluster=='clust_2: Terminal severe']='2: STS'

dt23_ug<-dt23_ug[,2:5]

## combine hist, 2030 and 2050
colnames(histr)<-colnames(dt_50)
dt_ug<- rbind(histr, dt23_ug, dt_50)

if(fig=='supp'){
  ### For supplementary
  dt_ug$scen[dt_ug$scen=='SSP_245_2030']='SSP2-4.5 (2030)'
  dt_ug$scen[dt_ug$scen=='SSP_370_2030']='SSP3-7.0 (2030)'
  dt_ug$scen[dt_ug$scen=='SSP_245_2050']='SSP2-4.5 (2050)'
  dt_ug$scen[dt_ug$scen=='SSP_370_2050']='SSP3-7.0 (2050)'
  
  dt_ug<-transform(dt_ug,
                   scen=factor(scen,levels=c('Historical','SSP2-4.5 (2030)','SSP3-7.0 (2030)','SSP2-4.5 (2050)','SSP3-7.0 (2050)')))
} else {
  dt_ug$scen[dt_ug$scen=='SSP_126_2030']='SSP1-2.6 (2030)'
  dt_ug$scen[dt_ug$scen=='SSP_585_2030']='SSP5-8.5 (2030)'
  dt_ug$scen[dt_ug$scen=='SSP_126_2050']='SSP1-2.6 (2050)'
  dt_ug$scen[dt_ug$scen=='SSP_585_2050']='SSP5-8.5 (2050)'
  
  dt_ug<-transform(dt_ug,
                   scen=factor(scen,levels=c('Historical','SSP1-2.6 (2030)','SSP5-8.5 (2030)','SSP1-2.6 (2050)','SSP5-8.5 (2050)')))
}

ug<-ggplot(data = dt_ug) +
  geom_smooth(mapping = aes(x = stages, y = prob, fill=cluster, color=cluster)) + 
  facet_wrap(vars(scen), nrow = 5) +
  scale_color_manual(values=c('#377eb8','#67000d')) + coord_cartesian(ylim=c(0,1)) +
  scale_fill_manual(values=c('#3182bd','#a50f15')) +
  xlab('Growth Stages') + ylab('') + theme_bw() + 
  theme(legend.position = 'bottom', legend.title = element_blank(), legend.text=element_text(size=8), 
        axis.text.y = element_blank(),legend.key.size = unit(0.4, "cm"),legend.key.width = unit(0.4,"cm") ) +  
  scale_x_continuous(breaks=1:8,labels=lab)  

plot<-ggarrange(
  jj,kk, ug, labels = c("A", "B", 'C'), ncol = 3,widths=c(0.4,0.34,0.32),common.legend = F
  
)
# ggsave(plot = ug, 
#        filename = paste0(path,'corrected_line_plot_multi_mod_mean_all_cntry_245_370.png'), 
#        width = 10, height = 10, units = 'in', dpi = 300) 


if(fig=='supp'){
  ggsave(plot = plot, 
         filename = paste0(path,'corrected_line_plot_multi_mod_mean_all_cntry_245_370.png'), 
         width = 10, height = 10, units = 'in', dpi = 300) 
} else {
  ggsave(plot = plot, 
         filename = paste0(path,'corrected_line_plot_multi_mod_mean_all_cntry_126_585.png'), 
         width = 10, height = 10, units = 'in', dpi = 300)
}

