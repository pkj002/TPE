rm(list=ls())
gc()
library(abind)
require(pacman)
library(tmap) 
library(classInt)
library(sf)
library(stringi)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

pacman::p_load(sp, raster, rgdal, rgeos, gtools, tidyverse)
path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ugand/tpe/PlantGro/final/cluster/cluster_pltgro_yr_sowdt/'

cntry<-'ugand'

# GCM Models
mod <- c('BCC-CSM2-MR','EC-Earth3-Veg','GFDL-ESM4','IPSL-CM6A-LR','MRI-ESM2-0')
level<-c('126', '245', '370', '585')
name<-paste0(rep(mod,each=4),'_ssp_',level[1:4])

# dimension of domain
comb <- data.frame(year = c(rep(2030,20),rep(2050,20)), mod = c(rep(mod,each=4),rep(mod,each=4)), sc = rep(c(126,245,370,585),10),
                   period=c(rep('202101-204012',20),rep('204101-206012',20)), co2=c(rep(c(440,446,454,456),5),rep(c(469,508,544,569),5)),
                   stringsAsFactors = FALSE)

## domains of ugand
lon<-seq(from=29.525,to=35.025,by=0.05)
lat<-seq(from=-1.525, to=4.475,by=0.05)
ln<-length(lon); lt<-length(lat)
len <- length(lon)*length(lat)

mps <- shapefile('C:/Users/pjha/Desktop/UGA_adm/UGA_adm0.shp')


# Load bean corridor polygons
atlas <- shapefile('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/bean_atlas/AtlasBean_data_copy.shp')
atlas <- atlas[atlas$Country == "UGANDA",]
atlas <- st_as_sf(atlas)

final<-list()
for (i in 1:40){
  
  ## Obs Yield 
  ## main season
  df1<-readRDS(paste0(path,'freq_cluster/new1_stress_totclus2_clust1_perc_in_each_grid_ug_',comb$mod[i],'_ssp_',comb$sc[i],'_',comb$year[i],'.RDS'))
  df1<-df1$clst
  
  df2<-readRDS(paste0(path,'freq_cluster/new1_stress_clust2_totclus2_perc_in_each_grid_ug_',comb$mod[i],'_ssp_',comb$sc[i],'_',comb$year[i],'.RDS'))
  df2<-df2$clst
  
  if (i==11){
    cl1<-c(df1,NA,NA) 
    cl2<-c(df2,NA,NA)
  } else {
    cl1<-df1
    cl2<-df2
  }
  
  if(i==1 | i==6 | i==7 | i==13 | i==17 | i==18 | i==20 |i==21 |i==23 |i==24 |i==25 |i==26 |i==27
     |i==29 |i==30 |i==31 |i==34 |i==35 |i==38){
    cl2<-df1
    cl1<-df2
  }
  
  ## cluster 1
  cl1<-array(cl1,c(ln,lt))
  dd<-cl1[,order(ncol(cl1):1)]
  dd1<-t(dd)
  r1 <- raster(dd1)
  extent(r1)<-c(xmn=29.125, xmx=35.025, ymn=-1.525, ymx=4.475)
  
  ## cluster 2
  cl2<-array(cl2,c(ln,lt))
  ee<-cl2[,order(ncol(cl2):1)]
  ee1<-t(ee)
  r2<- raster(ee1)
  extent(r2)<-c(xmn=29.125, xmx=35.025, ymn=-1.525, ymx=4.475)
  
  #extract raster cell count (sum) within each polygon area (poly)
  atlas$cluser1 <- raster::extract(r1, atlas, fun=mean, na.rm=T, df=F)
  atlas$cluser2 <- raster::extract(r2, atlas, fun=mean, na.rm=T, df=F)
  
  vv <- atlas
  st_geometry(vv) <- NULL
  vv<- vv %>% dplyr::select(MBPA,cluser1,cluser2)
   
  final[[i]]<-vv
}

final1<-do.call(rbind,final)
period<-rep(c(2030,2050),each=200)
final1<-data.frame(final1,model=c(rep(name,each=10),rep(name,each=10)),period=period)
colnames(final1)<-c('Corridor','cl1%','cl2%','model','period')
mm1<-final1 %>% mutate(ssp=stri_sub(model,-3,-1)) 
mm30<-mm1[which(mm1$period==2030),]; mm50<-mm1[which(mm1$period==2050),]

mm30_1<-aggregate(mm30[, 2:3], list(mm30$Corridor,mm30$ssp), mean) ## multi model mean. Now, Only SSPs 
mm50_1<-aggregate(mm50[, 2:3], list(mm50$Corridor,mm50$ssp), mean)
mm_all<-rbind(mm30_1,mm50_1)
period<-c(rep(c(2030,2050),each=40))
mm_all<-data.frame(mm_all, period=period)
mm2<-mm_all[order(mm_all$Group.1),]

mm2<- mm2 %>%  mutate_if(is.numeric, ~round(., 2))
colnames(mm2)<-c('Corridor','ssp','c1','c2','period')

## Historical
#dir1<-'//dapadfs/workspace_cluster_12/AVISA/dssat_outputs/ugand/tpe/PlantGro/final/cluster/'
## 1st cluster
#gg<-readRDS(paste0(dir1,'obs_new1_stress_totclus2_clust1_perc_in_each_grid_ug.RDS'))
gg<-readRDS(paste0(path,'freq_cluster/obs_new1_stress_totclus2_clust1_perc_in_each_grid_ug.RDS'))
gg<-unlist(gg$clst)
gg<-array(gg,c(ln,lt))
gg1<-gg[,order(ncol(gg):1)]
r1 <- raster(t(gg1))
extent(r1)<-c(xmn=29.125, xmx=35.025, ymn=-1.525, ymx=4.475)

## 2nd cluster
#hh<-readRDS(paste0(dir1,'obs_new1_stress_clust2_totclus2_perc_in_each_grid_ug.RDS'))
hh<-readRDS(paste0(path,'freq_cluster/obs_new1_stress_clust2_totclus2_perc_in_each_grid_ug.RDS'))
hh<-unlist(hh$clst)
hh<-array(hh,c(ln,lt))
hh1<-hh[,order(ncol(hh):1)]
r2<-raster(t(hh1))
extent(r2)<-c(xmn=29.125, xmx=35.025, ymn=-1.525, ymx=4.475)

#extract raster cell count (sum) within each polygon area (poly)
atlas$his_cl1<- raster::extract(r1, atlas, fun=mean, na.rm=T, df=F)
atlas$his_cl2<- raster::extract(r2, atlas, fun=mean, na.rm=T, df=F)

his <- atlas
st_geometry(his) <- NULL
his<- his %>%  dplyr::select(MBPA,his_cl1,his_cl2) %>% 
  mutate_if(is.numeric, ~round(., 2))

colnames(his)<-c('Corridor','c1','c2')
obs_co<-his

region<-as.character(obs_co$Corridor)
out_dir<- '//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ugand/tpe/PlantGro/final/cluster/cluster_pltgro_yr_sowdt/grid_cluster/'
for (n in 1:10){
  mm3<-mm2 %>% filter(Corridor==region[n])
  mm3<-mm3[,c(-1,-2,-5)]
  mm3<-data.matrix(mm3)
  
  obs_co1<- obs_co %>% filter(Corridor==region[n])
  obs_co1<-data.matrix(obs_co1[,-1])
  mm4<-c(obs_co1,mm3[1,],mm3[2,],mm3[3,],mm3[4,],mm3[5,],mm3[6,],mm3[7,],mm3[8,])
  clust<-rep(c('SF', 'STS'),9)
  scen<-c('obs','obs',rep(c('ssp_126','ssp_245','ssp_370','ssp_585'),each=2),rep(c('ssp_126','ssp_245','ssp_370','ssp_585'),each=2))
  dd<-data.frame(scenario=scen, cluster=clust,Proportion=mm4)
  
  hist<-data.frame(period=c('Historical','Historical'),dd[1:2,2:3])
  fut<-data.frame(period=rep(c(2030,2050),each=8),dd[3:18,])
  fut<-fut %>% mutate(ssp=stri_sub(scenario,-3,-1))  %>% dplyr::select(cluster,Proportion,period,ssp)
  
  
   if(n==3){
     fut$Proportion[15]=fut$Proportion[15]-.01
   } else if(n==10){
     fut$Proportion[3]=fut$Proportion[3]-.01;
   }
  
  
  p_hist<-ggplot(hist, aes(x=period, y = Proportion, fill = cluster, label =Proportion)) +
    geom_bar(stat = "identity") + ggtitle("Historical") +
    xlab('') + ylab('Proportion') + scale_fill_manual(values=c('#377eb8', '#e41a1c')) +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) + labs(fill='') +
    theme_bw() + theme(legend.position='bottom', plot.title = element_text(size=9))
  
  P_fut<-ggplot(fut, aes(x = ssp, y = Proportion, fill = cluster, label =Proportion)) +
    geom_bar(stat = "identity") + facet_grid(.~period) +
    xlab('SSP') + ylab('') + scale_fill_manual(values=c('#377eb8', '#e41a1c')) +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) + labs(fill='') +
    theme_bw() + theme(legend.position='bottom', axis.text.y = element_blank()) 
  
  plot<-ggarrange(p_hist,P_fut, common.legend = TRUE, legend = 'bottom', widths=c(.23,.77)) 
  
  ggsave(plot =plot,
         filename = paste0(out_dir,'new_bar2/',region[n],'_',comb$year[i],'.png'),
         width = 5, height = 5, units = 'in', dpi = 300)
}
#