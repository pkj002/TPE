rm(list=ls())
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
dir<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ethiopia/tpe/PlantGro/final/cluster/clust_perc_in_each_grid/'
out_dir<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ethiopia/tpe/PlantGro/final/cluster/data_plot_stress/corridor_cluster_prob/space_time_new1/'
cntry<-'ethiopia'

# GCM Models
mod <- c('BCC-CSM2-MR','EC-Earth3-Veg','GFDL-ESM4','IPSL-CM6A-LR','MRI-ESM2-0')
level<-c('126', '245', '370', '585')
name<-paste0(rep(mod,each=4),'_ssp_',level[1:4])

# dimension of domain
comb <- data.frame(year = c(rep(2030,20),rep(2050,20)), mod = c(rep(mod,each=4),rep(mod,each=4)), sc = rep(c(126,245,370,585),10),
                   period=c(rep('202101-204012',20),rep('204101-206012',20)), co2=c(rep(c(440,446,454,456),5),rep(c(469,508,544,569),5)),
                   stringsAsFactors = FALSE)

## domains of ethiopia
lon<-seq(from=33.025,to=44.025,by=0.05)
lat<-seq(from=3.025, to=15.025,by=0.05)
len=length(lon)*length(lat)
ln<-length(lon); lt<-length(lat)

mps <- shapefile('C:/Users/pjha/Desktop/ETH_adm/ETH_adm0.shp')

# Load bean corridor polygons
atlas <- shapefile('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/bean_atlas/AtlasBean_data_copy.shp')
atlas <- atlas[atlas$Country == "ETHIOPIA",]
atlas <- st_as_sf(atlas)

final<-list()
for (n in 1:40){
  
  ## Obs Yield 
  ## main season
  df1<-readRDS(paste0(dir,'clust1prop_',comb$mod[n],'_ssp_',comb$sc[n],'_',comb$year[n],'.RDS'))
  df1<-df1[order(df1$grids),]; df1<-df1$clst
  
  df2<-readRDS(paste0(dir,'clust2prop_',comb$mod[n],'_ssp_',comb$sc[n],'_',comb$year[n],'.RDS')) 
  df2<-df2[order(df2$grids),]; df2<-df2$clst
  
  df3<-readRDS(paste0(dir,'clust3prop_',comb$mod[n],'_ssp_',comb$sc[n],'_',comb$year[n],'.RDS'))
  df3<-df3[order(df3$grids),]; df3<-df3$clst
  
  df4<-readRDS(paste0(dir,'clust4prop_',comb$mod[n],'_ssp_',comb$sc[n],'_',comb$year[n],'.RDS'))
  df4<-df4[order(df4$grids),]; df4<-df4$clst
  
  if(n==1){
    cl2<-df1; cl4<-df2; cl1<-df3; cl3<-df4
  } else if(n==2){
    cl4<-df1; cl2<-df2; cl1<-df3; cl3<-df4
  } else if(n==3){
    cl4<-df1; cl3<-df2; cl2<-df3; cl1<-df4
  } else if(n==4){
    cl3<-df1; cl4<-df2; cl2<-df3; cl1<-df4
  } else if(n==5){
    cl4<-df1; cl1<-df2; cl2<-df3; cl4<-df4
  } else if(n==6){
    cl2<-df1; cl1<-df2; cl4<-df3; cl3<-df4
  } else if(n==7){
    cl4<-df1; cl2<-df2; cl1<-df3; cl3<-df4
  } else if(n==8){
    cl2<-df1; cl3<-df2; cl1<-df3; cl4<-df4
  } else if(n==9){
    cl2<-df1; cl4<-df2; cl3<-df3; cl1<-df4
  } else if(n==10){
    cl1<-df1; cl2<-df2; cl4<-df3; cl3<-df4
  } else if(n==11){
    cl2<-df1; cl3<-df2; cl1<-df3; cl4<-df4
  } else if(n==12){
    cl3<-df1; cl2<-df2; cl1<-df3; cl4<-df4
  } else if(n==13){
    cl1<-df1; cl4<-df2; cl2<-df3; cl3<-df4
  } else if(n==14){
    cl2<-df1; cl3<-df2; cl4<-df3; cl1<-df4
  } else if(n==15){
    cl4<-df1; cl1<-df2; cl2<-df3; cl3<-df4
  } else if(n==16){
    cl4<-df1; cl2<-df2; cl1<-df3; cl3<-df4
  } else if(n==17){
    cl2<-df1; cl3<-df2; cl1<-df3; cl4<-df4
  } else if(n==18){
    cl2<-df1; cl1<-df2; cl4<-df3; cl3<-df4
  } else if(n==19){
    cl3<-df1; cl2<-df2; cl4<-df3; cl1<-df4
  } else if(n==20){
    cl3<-df1; cl1<-df2; cl2<-df3; cl4<-df4
  } else if(n==21){
    cl2<-df1; cl1<-df2; cl4<-df3; cl3<-df4
  } else if(n==22){
    cl2<-df1; cl1<-df2; cl3<-df3; cl4<-df4
  } else if(n==23){
    cl3<-df1; cl4<-df2; cl2<-df3; cl1<-df4
  } else if(n==24){
    cl3<-df1; cl2<-df2; cl1<-df3; cl4<-df4
  } else if(n==25){
    cl4<-df1; cl2<-df2; cl3<-df3; cl1<-df4
  } else if(n==26){
    cl2<-df1; cl1<-df2; cl4<-df3; cl3<-df4
  } else if(n==27){
    cl4<-df1; cl3<-df2; cl3<-df3; cl1<-df4
  } else if(n==28){
    cl4<-df1; cl3<-df2; cl2<-df3; cl1<-df4
  } else if(n==29){
    cl1<-df1; cl2<-df2; cl3<-df3; cl4<-df4
  } else if(n==30){
    cl3<-df1; cl1<-df2; cl2<-df3; cl4<-df4
  } else if(n==31){
    cl1<-df1; cl2<-df2; cl4<-df3; cl3<-df4
  } else if(n==32){
    cl1<-df1; cl2<-df2; cl3<-df3; cl4<-df4
  } else if(n==33){
    cl2<-df1; cl1<-df2; cl4<-df3; cl3<-df4
  } else if(n==34){
    cl2<-df1; cl4<-df2; cl3<-df3; cl1<-df4
  } else if(n==35){
    cl3<-df1; cl2<-df2; cl1<-df3; cl4<-df4
  } else if(n==36){
    cl4<-df1; cl3<-df2; cl2<-df3; cl1<-df4
  } else if(n==37){
    cl2<-df1; cl3<-df2; cl1<-df3; cl4<-df4
  } else if(n==38){
    cl3<-df1; cl4<-df2; cl2<-df3; cl1<-df4
  } else if(n==39){
    cl2<-df1; cl1<-df2; cl3<-df3; cl4<-df4
  } else {
    cl4<-df1; cl2<-df2; cl1<-df3; cl3<-df4
  }
  
  ## cluster 1
  cl1<-array(cl1,c(ln,lt))
  dd<-cl1[,order(ncol(cl1):1)]
  dd1<-t(dd)
  r1 <- raster(dd1)
  extent(r1)<-c(xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025) 
  
  ## cl 2
  cl2<-array(cl2,c(ln,lt))
  ee<-cl2[,order(ncol(cl2):1)]
  ee1<-t(ee)
  r2 <- raster(ee1)
  extent(r2)<-c(xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025) 
  
  ## cl3
  cl3<-array(cl3,c(ln,lt))
  ff<-cl3[,order(ncol(cl3):1)]
  ff1<-t(ff)
  r3<-raster(ff1)
  extent(r3)<-c(xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025)
  
  ## cl4
  cl4<-array(cl4,c(ln,lt))
  gg<-cl4[,order(ncol(cl4):1)]
  gg1<-t(gg)
  r4<-raster(gg1)
  extent(r4)<-c(xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025)
  
  #extract raster cell count (sum) within each polygon area (poly)
  atlas$cluser1 <- raster::extract(r1, atlas, fun=mean, na.rm=T, df=F)
  atlas$cluser2 <- raster::extract(r2, atlas, fun=mean, na.rm=T, df=F)
  atlas$cluser3 <- raster::extract(r3, atlas, fun=mean, na.rm=T, df=F)
  atlas$cluser4 <- raster::extract(r4, atlas, fun=mean, na.rm=T, df=F)
  
  vv <- atlas
  st_geometry(vv) <- NULL
  vv<- vv %>% dplyr::select(MBPA,cluser1,cluser2, cluser3, cluser4)
  
  final[[n]]<-vv
} 

final1<-do.call(rbind,final)
period<-rep(c(2030,2050),each=200)
final1<-data.frame(final1,model=c(rep(name,each=10),rep(name,each=10)),period=period)
colnames(final1)<-c('Corridor','cl1%','cl2%','cl3%','cl4%','model','period')
mm1<-final1 %>% mutate(ssp=stri_sub(model,-3,-1))
mm30<-mm1[which(mm1$period==2030),]; mm50<-mm1[which(mm1$period==2050),]
mm30_1<-aggregate(mm30[, 2:5], list(mm30$Corridor,mm30$ssp), mean, na.rm=T) ## multi model mean. Now, Only SSPs 
mm50_1<-aggregate(mm50[, 2:5], list(mm50$Corridor,mm50$ssp), mean, na.rm=T)
mm_all<-rbind(mm30_1,mm50_1)
period<-c(rep(c(2030,2050),each=40))
mm_all<-data.frame(mm_all, period=period)
mm2<-mm_all[order(mm_all$Group.1),]
colnames(mm2)<-c('Corridor','ssp','c1','c2','c3','c4','period')

mm2<-mm2 %>% mutate_if(is.numeric, ~round(., 2))

## Historical
## cluster 1
hh<-readRDS(paste0(dir,'clust1prop_ethi_obs_new.RDS'))
hh<-hh[order(hh$grids),]; hh<-hh$clst
hh<-array(hh,c(ln,lt))
hh1<-hh[,order(ncol(hh):1)]
r1 <- raster(t(hh1))
extent(r1)<-c(xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025) 

## cl 2
ii<-readRDS(paste0(dir,'clust2prop_ethi_obs_new.RDS'))
ii<-ii[order(ii$grids),]; ii<-ii$clst
ii<-array(ii,c(ln,lt))
ii1<-ii[,order(ncol(ii):1)]
r2<-raster(t(ii1))
extent(r2)<-c(xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025)

## cl3
jj<-readRDS(paste0(dir,'clust3prop_ethi_obs_new.RDS'))
jj<-jj[order(jj$grids),]; jj<-jj$clst
jj<-array(jj,c(ln,lt))
jj1<-jj[,order(ncol(jj):1)]
r3<-raster(t(jj1))
extent(r3)<-c(xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025)

## cl4
kk<-readRDS(paste0(dir,'clust4prop_ethi_obs_new.RDS'))
kk<-kk[order(kk$grids),]; kk<-kk$clst
kk<-array(kk,c(ln,lt))
kk1<-kk[,order(ncol(kk):1)]
r4<-raster(t(kk1))
extent(r4)<-c(xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025)

#extract raster cell count (sum) within each polygon area (poly)
atlas$his_cl1<- raster::extract(r1, atlas, fun=mean, na.rm=T, df=F)
atlas$his_cl2<- raster::extract(r2, atlas, fun=mean, na.rm=T, df=F)
atlas$his_cl3<- raster::extract(r3, atlas, fun=mean, na.rm=T, df=F)
atlas$his_cl4<- raster::extract(r4, atlas, fun=mean, na.rm=T, df=F)

his <- atlas
st_geometry(his) <- NULL
his<- his %>% dplyr::select(MBPA,his_cl1,his_cl2,his_cl3,his_cl4) %>% rowwise %>% mutate(bias=(sum(his_cl1,his_cl2,his_cl3,his_cl4)-1)/4)%>%
  mutate(cl1=(his_cl1-bias), cl2=(his_cl2-bias), cl3=(his_cl3-bias), cl4=(his_cl4-bias)) %>% 
  dplyr::select(MBPA,cl1,cl2,cl3,cl4) %>% mutate_if(is.numeric, ~round(., 2))

## sum of all clusters should be 1
#his$cl2[1]=0.45; his$cl2[2]=0.35; his$cl1[5]=0; his$cl3[5]=0.42; his$cl4[5]=0.35;  his$cl4[7]=0.02; his$cl1[9]=0.01; his$cl1[10]=0
his$cl1[5]=0; his$cl4[5]=his$cl4[5]-.02; his$cl4[7]=his$cl4[7]-.01; his$cl1[9]=his$cl1[9]+.01; his$cl1[10]=his$cl1[10]-.01;


colnames(his)<-c('Corridor','c1','c2','c3','c4')
obs_co<-his

region<-as.character(obs_co$Corridor)

## calculate area harvested for each corridor
## Need to multiply cluster proportion with harvested areas
ext<-c(xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025)
dd<-crop(raster('C:/Users/pjha/Desktop/harv_area/spam2017V2r1_SSA_H_BEAN_A.tif'),ext)
atlas$harv_area<- raster::extract(dd, atlas, fun=sum, na.rm=T, df=F)
vv<-atlas
st_geometry(vv) <- NULL
hv_area<-vv$harv_area 


c_all<-list()

for (n in 1:10){
  mm3<-mm2 %>% filter(Corridor==region[n])
  mm3<-mm3[,c(-1,-2,-7)]
  mm3<-data.matrix(mm3)
  
  obs_co1<- obs_co %>% filter(Corridor==region[n])
  obs_co1<-data.matrix(obs_co1[,-1])
  mm4<-c(obs_co1,mm3[1,],mm3[2,],mm3[3,],mm3[4,],mm3[5,],mm3[6,],mm3[7,],mm3[8,])
  
  clust<-rep(c('GS','SF','MTS','ETS'),9)
  scen<-c('obs','obs','obs','obs',rep(c('ssp_126','ssp_245','ssp_370','ssp_585'),each=4),rep(c('ssp_126','ssp_245','ssp_370','ssp_585'),each=4))
  dd<-data.frame(scenario=scen, cluster=clust,Probability=mm4)
  fut<-data.frame(period=rep(c(2030,2050),each=16),dd[5:36,])
  fut<-fut %>% mutate(ssp=stri_sub(scenario,-3,-1))  %>% dplyr::select(cluster,Probability,period,ssp)
  fut$Probability <- round(fut$Probability,2)
  
  hist<-data.frame(period=c('Historical','Historical','Historical','Historical'),dd[1:4,2:3])
  hist$Probability <- round(hist$Probability, 2)
  
  ### fill correction here 
  if(n==1){
    fut$Probability[1]=fut$Probability[1]+0.01; fut$Probability[10]=fut$Probability[10]-.01; fut$Probability[11]=fut$Probability[11]-.01;
    fut$Probability[14]= fut$Probability[14]-.01;  fut$Probability[15]= fut$Probability[15]-.01;
    fut$Probability[18]=fut$Probability[18]-.01; fut$Probability[22]=fut$Probability[22]-.01; fut$Probability[26]=fut$Probability[26]-.07
    fut$Probability[25]=fut$Probability[25]-.01;  fut$Probability[27]=fut$Probability[27]-.01; fut$Probability[28]=fut$Probability[28]-.01;                                                                                                                           
    fut$Probability[30]=fut$Probability[30]-.01
  } else if (n==2){
    fut$Probability[1]=fut$Probability[1]-.01;  fut$Probability[2]=fut$Probability[2]-.06; fut$Probability[3]=fut$Probability[3]-.02;
    fut$Probability[4]=fut$Probability[4]-.02;
    
    fut$Probability[5]=fut$Probability[5]-.01;  fut$Probability[6]=fut$Probability[6]-.06; fut$Probability[7]=fut$Probability[7]-.03;
    fut$Probability[8]=fut$Probability[8]-.03;
    
    fut$Probability[9]=fut$Probability[9]-.01; fut$Probability[10]=fut$Probability[10]-.06; fut$Probability[11]=fut$Probability[11]-.02; 
    fut$Probability[12]=fut$Probability[12]-.01;
    
    fut$Probability[14]=fut$Probability[14]-.06; fut$Probability[15]=fut$Probability[15]-.03; fut$Probability[16]=fut$Probability[16]-.03;
    
    fut$Probability[18]=fut$Probability[18]-.05;  fut$Probability[19]=fut$Probability[19]-.03;  fut$Probability[20]=fut$Probability[20]-.03;
    fut$Probability[21]=fut$Probability[21]-.01; fut$Probability[22]=fut$Probability[22]-.07; fut$Probability[23]=fut$Probability[23]-.02;
    fut$Probability[24]=fut$Probability[24]-.02;
    fut$Probability[26]=fut$Probability[26]-.12; fut$Probability[27]=fut$Probability[27]-.03; fut$Probability[28]=fut$Probability[28]-.03; 
    fut$Probability[30]=fut$Probability[30]-.07; fut$Probability[31]=fut$Probability[31]-.02; fut$Probability[32]=fut$Probability[32]-.02;
  } else if(n==3){
    fut$Probability[2]=fut$Probability[2]-.01; fut$Probability[3]=fut$Probability[3]-.01;
    fut$Probability[6]=fut$Probability[6]-.01; fut$Probability[7]=fut$Probability[7]-.01;
    fut$Probability[9]=fut$Probability[9]-.01; fut$Probability[10]=fut$Probability[10]-.01; fut$Probability[11]=fut$Probability[11]-.01;
    fut$Probability[14]=fut$Probability[14]-.01; fut$Probability[15]=fut$Probability[15]-.01; 
    fut$Probability[18]=fut$Probability[18]-.01; fut$Probability[19]=fut$Probability[19]-.01;
    fut$Probability[22]=fut$Probability[22]-.01;
    fut$Probability[25]=fut$Probability[25]-.01; fut$Probability[26]=fut$Probability[26]-.05; fut$Probability[27]=fut$Probability[27]-.02;
    fut$Probability[28]=fut$Probability[28]-.01;
    fut$Probability[30]=fut$Probability[30]-.02; fut$Probability[31]=fut$Probability[31]-.01;
    
  } else if(n==4){
    fut$Probability[1]=fut$Probability[1]-.01; fut$Probability[2]=fut$Probability[2]-.04; fut$Probability[3]=fut$Probability[3]-.03;
    fut$Probability[4]=fut$Probability[4]-.04;
    fut$Probability[5]=fut$Probability[5]-.03; fut$Probability[6]=fut$Probability[6]-.07; fut$Probability[7]=fut$Probability[7]-.03;
    fut$Probability[8]=fut$Probability[8]-.03;
    
    fut$Probability[9]=fut$Probability[9]-.03; fut$Probability[10]=fut$Probability[10]-.04; fut$Probability[11]=fut$Probability[11]-.04;
    fut$Probability[12]=fut$Probability[12]-.04;
    
    fut$Probability[13]=fut$Probability[13]-.03; fut$Probability[14]=fut$Probability[14]-.05; fut$Probability[15]=fut$Probability[15]-.05;
    fut$Probability[16]=fut$Probability[16]-.05;
    
    fut$Probability[17]=fut$Probability[17]-.03; fut$Probability[18]=fut$Probability[18]-.08; fut$Probability[19]=fut$Probability[19]-.04;
    fut$Probability[20]=fut$Probability[20]-.04;
    
    fut$Probability[21]=fut$Probability[21]-.02; fut$Probability[22]=fut$Probability[22]-.06; fut$Probability[23]=fut$Probability[23]-.03;
    fut$Probability[24]=fut$Probability[24]-.04;
    
    fut$Probability[25]=fut$Probability[25]-.02; fut$Probability[26]=fut$Probability[26]-.09; fut$Probability[27]=fut$Probability[27]-.05;
    fut$Probability[28]=fut$Probability[28]-.05;
    
    fut$Probability[29]=fut$Probability[29]-.03; fut$Probability[30]=fut$Probability[30]-.12; fut$Probability[31]=fut$Probability[31]-.08;
    fut$Probability[30]=fut$Probability[30]-.03;
    
  } else if(n==5){
    fut$Probability[1]=fut$Probability[1]-.01; fut$Probability[2]=fut$Probability[2]-.02; fut$Probability[3]=fut$Probability[3]-.02;
    fut$Probability[4]=fut$Probability[4]-.02;
    fut$Probability[6]=fut$Probability[6]-.05; fut$Probability[7]=fut$Probability[7]-.05; fut$Probability[8]=fut$Probability[8]-.05;
    fut$Probability[10]=fut$Probability[10]-.04;fut$Probability[11]=fut$Probability[11]-.04; fut$Probability[12]=fut$Probability[12]-.03;
    fut$Probability[14]=fut$Probability[14]-.06; fut$Probability[15]=fut$Probability[15]-.06; fut$Probability[16]=fut$Probability[16]-.05;
    fut$Probability[18]=fut$Probability[18]-.06; fut$Probability[19]=fut$Probability[19]-.04; fut$Probability[20]=fut$Probability[20]-.04;
    fut$Probability[22]=fut$Probability[22]-.06; fut$Probability[23]=fut$Probability[23]-.04; fut$Probability[24]=fut$Probability[24]-.04;
    fut$Probability[26]=fut$Probability[26]-.11; fut$Probability[27]=fut$Probability[27]-.05; fut$Probability[28]=fut$Probability[28]-.05;
    fut$Probability[30]=fut$Probability[30]-.06; fut$Probability[31]=fut$Probability[31]-.05; fut$Probability[32]=fut$Probability[32]-.01;
  } else if(n==6){
    fut$Probability[2]=fut$Probability[2]-.06; fut$Probability[3]=fut$Probability[3]-.01;
    fut$Probability[6]=fut$Probability[6]-.04; fut$Probability[7]=fut$Probability[7]-.01;
    fut$Probability[10]=fut$Probability[10]-.05; fut$Probability[11]=fut$Probability[11]-.02;
    fut$Probability[14]=fut$Probability[14]-.05; fut$Probability[15]=fut$Probability[15]-.01;
    fut$Probability[18]=fut$Probability[18]-.05;
    fut$Probability[22]=fut$Probability[22]-.04;
    fut$Probability[26]=fut$Probability[26]-.19; fut$Probability[27]=fut$Probability[27]-.03; 
    fut$Probability[30]=fut$Probability[30]-.05;
  } else if(n==7){
    fut$Probability[1]=fut$Probability[1]-.01; fut$Probability[2]=fut$Probability[2]-.04;
    fut$Probability[6]=fut$Probability[6]-.01; fut$Probability[7]=fut$Probability[7]-.04;
    fut$Probability[10]=fut$Probability[10]-.01; fut$Probability[11]=fut$Probability[11]-.04;
    fut$Probability[14]=fut$Probability[14]-.01; fut$Probability[15]=fut$Probability[15]-.04;
    fut$Probability[18]=fut$Probability[18]-.01; fut$Probability[19]=fut$Probability[19]-.04;
    fut$Probability[22]=fut$Probability[22]-.02; fut$Probability[23]=fut$Probability[23]-.04;
    fut$Probability[25]=fut$Probability[25]-.02; fut$Probability[26]=fut$Probability[26]-.15; fut$Probability[27]=fut$Probability[27]-.02;
    fut$Probability[29]=fut$Probability[29]-.01; fut$Probability[30]=fut$Probability[30]-.03; fut$Probability[31]=fut$Probability[31]-.01;
  } else if(n==8){
    fut$Probability[1]=fut$Probability[1]-.01; fut$Probability[2]=fut$Probability[2]-.01; fut$Probability[3]=fut$Probability[3]-.01;
    fut$Probability[6]=fut$Probability[6]-.01; fut$Probability[7]=fut$Probability[7]-.01; 
    fut$Probability[10]=fut$Probability[10]-.03; fut$Probability[11]=fut$Probability[11]-.01;
    fut$Probability[14]=fut$Probability[14]-.01; fut$Probability[15]=fut$Probability[15]-.01;
    fut$Probability[18]=fut$Probability[18]-.01; fut$Probability[19]=fut$Probability[19]-.01; fut$Probability[20]=fut$Probability[20]-.01;
    fut$Probability[22]=fut$Probability[22]-.01;
    fut$Probability[26]=fut$Probability[26]-.08; fut$Probability[27]=fut$Probability[27]-.03;
    fut$Probability[30]=fut$Probability[30]-.04; fut$Probability[31]=fut$Probability[31]-.01;
  } else if(n==9){
    fut$Probability[2]=fut$Probability[2]-.07;    fut$Probability[6]=fut$Probability[6]-.06; fut$Probability[10]=fut$Probability[10]-.07;  fut$Probability[14]=fut$Probability[14]-.07;
    fut$Probability[18]=fut$Probability[18]-.06;  fut$Probability[22]=fut$Probability[22]-.06; fut$Probability[26]=fut$Probability[26]-.09;
    fut$Probability[27]=fut$Probability[27]-.15;  fut$Probability[30]=fut$Probability[30]-.07;
  } else {
    fut$Probability[2]=fut$Probability[2]-.09; fut$Probability[3]=fut$Probability[3]-.02; fut$Probability[4]=fut$Probability[4]-.01;
    fut$Probability[6]=fut$Probability[6]-.08; fut$Probability[7]=fut$Probability[7]-.02; 
    fut$Probability[10]=fut$Probability[10]-.06; fut$Probability[11]=fut$Probability[11]-.02;
    fut$Probability[14]=fut$Probability[14]-.06; fut$Probability[15]=fut$Probability[15]-.02;
    fut$Probability[18]=fut$Probability[18]-.06; fut$Probability[19]=fut$Probability[19]-.02;
    fut$Probability[22]=fut$Probability[22]-.06; fut$Probability[23]=fut$Probability[23]-.02;
    fut$Probability[26]=fut$Probability[26]-.10; fut$Probability[27]=fut$Probability[27]-.12;
    fut$Probability[30]=fut$Probability[30]-.06; fut$Probability[31]=fut$Probability[31]-.02;
  }
  
  fut_f<-array(fut$Probability,c(4,8))
  colnames(fut_f)<-c('ssp1_30','ssp2_30','ssp3_30','ssp4_30','ssp1_50','ssp2_50','ssp3_50','ssp4_50')
  all<-round(data.frame(History=hist$Probability,fut_f)*hv_area[n],0)
  c_all[[n]]<-data.frame(Production_hub=region[n],clus=hist$cluster,all)
  
}

prop_all<-do.call(rbind,c_all)
prop_all<-data.frame(country='ETH',prop_all)
saveRDS(prop_all, '//catalogue/BaseLineDataCluster01/temp/area_stress_corridor_ethi.RDS')
