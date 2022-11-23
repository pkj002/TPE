rm(list=ls())
library(ncdf4)
library(abind)
require(pacman)
library(tmap) 
library(classInt)
library(purrr)
library(tidyr)
library(sf)

pacman::p_load(raster, rgdal, rgeos, gtools, tidyverse)
dir<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/tanzania/tpe/PlantGro/cluster/'
cntry<-'tanzania'

## domains of Tanzania
lon<-seq(from=29.125,to=40.125,by=0.05)
lat<-seq(from=-11.975, to=-0.975,by=0.05)
len=length(lon)*length(lat)
ln<-length(lon); lt<-length(lat)

## Obs Yield 
## two season
#seas<-readRDS(paste0(dir,'season_mask.RDS'))
obs<-readRDS(paste0(dir,'list_grid_tz_clust5_obs_SplitSeas.RDS'))
obs <- map_df(obs, ~as.data.frame(.x), .id="grids")

## change in cluster according to the line plot. From script 'line_plot_all_cntry_stress_2050.R'
obs[obs==3]=6; obs[obs==4]=7; obs[obs==5]=8; obs[obs==2]=3; obs[obs==6]=5; obs[obs==7]=2; obs[obs==8]=4
## clusters
## 1=SF; 2=AS; 3=MTS; 4=STS; 5=ETS


## extract season 1
obs1s<- obs %>% dplyr::select(grids,clst,seas) %>% filter(seas=='seas1') %>% dplyr::select(grids,clst)
obs1s$grids<-as.numeric(obs1s$grids); obs1s$clst <- as.numeric(obs1s$clst)

# complete grids
main<-obs1s %>% complete(grids = full_seq(as.numeric(grids), 1))
top<-data.frame(grids=1:2618,clst=NA)
bot<-data.frame(grids=48061:len,clst=NA)
s1<-rbind(top,main,bot)

## extract 2 seasons 1st planting 
s2c1<- obs %>% dplyr::select(grids,seas,clst1,clst2) %>% filter(seas=='seas2') %>% dplyr::select(grids,clst1)
s2c2<-obs %>% dplyr::select(grids,seas,clst1,clst2) %>% filter(seas=='seas2') %>% dplyr::select(grids,clst2)
colnames(s2c1)[2]<- 'clst'; colnames(s2c2)[2]<- 'clst'
s2c1$clst <- as.numeric(s2c1$clst); s2c2$clst <- as.numeric(s2c2$clst)
s2c1$grids <- as.numeric(s2c1$grids); s2c2$grids <- as.numeric(s2c2$grids)


## complete grids
top <- data.frame(grids=1:1247,clst=NA); bot<-data.frame(grids=48718:len,clst=NA)
s2c1<-s2c1 %>% complete(grids = full_seq(as.numeric(grids), 1))
s2c2<-s2c2 %>% complete(grids = full_seq(as.numeric(grids), 1))

s2c1 <- rbind(top,s2c1,bot)
s2c2 <-rbind(top,s2c2,bot)

s2c1<- array(s2c1$clst,c(ln,lt)); s2c2<-array(s2c2$clst,c(ln,lt)); s1<-array(s1$clst,c(ln,lt))

yld<-abind(s2c1,s2c2,s1,along=3)

## Now plotting
df2=yld
for (n in 1:dim(df2)[3]){
  df2[, ,n]=yld[,order(ncol(yld[,,n]):1),n]
}

data<-brick(df2, xmn=29.125, xmx=40.125, ymn=-11.975, ymx=-0.975, 
            crs=NA, transpose=TRUE)

mps <- getData("GADM", country = "TZA", level = 0, path = dir)
name<-c('Two Seasons: 1st season','Two Seasons: 2nd season', 'One season only')

# Selecting only Valle del Cauca ------------------------------------------
crs(data)<-crs(mps)
data1 <- crop(data, mps) %>% mask(mps)
names(data1)<-name

#tit=paste0('Number of clusters based on yield under recent climate')
tit='Water Stress Cluster'
unit='Cluster'

#breaks=1:6
pcols<-c('#377eb8','#feb24c','#e31a1c','#67000d','#984ea3') ## colors are consistent with 'line_plot_all_cntry_stress_2050.R'
breaks=1:5

##############################################################################################
## bulk density simulation points
cord<-data.frame(long=rep(lon,lt),lati=rep(lat,each=ln))
## load sample points
 pts1<-c(28320,29203,30070,31830,32520,35605,37607,39170,40700,41576,42259,43800,45342,46249,47336) ## SF
 pts2<-c(28684,29795,30254,30693,31133,31575,31806,32248,32687,33126,34006,35751,36860,38403,41693)  ## AS
 pts3<- c(4576,7454,10283,13359,16611,19050,21150,24819,27444,30659,33118,36395,39068,42783,44990) ## MTS
 pts4<-c(3218,6084,7626,11821,16644,18168,19051,19710,19949,20392,21051,21700,22149,22802,31311)  ## STS
 pts5<- c(1273,3222,4982,7212,13370,15094,16408,18010,19786,21562,29987,34852,35736,38595,39950)  ## ETS
pts<-c(pts1,pts2,pts3,pts4,pts5)

dt<-cord[pts,]
colnames(dt)<- c('LONGITUDE', 'LATITUDE')
tif_data <- dt[!is.na(dt$LATITUDE), ]
tif_sf <- st_as_sf(tif_data, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)


## add shape file of bean atlas
atlas <- shapefile('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/bean_atlas/AtlasBean_data_copy.shp')

atlas <- gBuffer(atlas, byid=TRUE, width=0)
atlas <- spTransform(atlas, CRS( "+proj=longlat +datum=WGS84 +no_defs")) 
atlas <- raster::crop(atlas, mps)

## draw plot
curmap <- tm_shape(data1) +
  
  tm_raster(style='cat', breaks = breaks,legend.is.portrait = T,
            palette = pcols, alpha = 0.75, title = unit) +
  
  tm_layout(main.title=tit, main.title.size=1.5, panel.show = TRUE,   
            inner.margins = c(0.01, 0.01, 0.02, 0.05), 
            outer.margins = c(0.001, 0.001, 0.001, 0.001), 
            panel.labels = name, panel.label.size = 2, 
            legend.outside = T, legend.bg.color = "white", legend.bg.alpha = 1) +
  tm_facets(ncol=3) +
  
  ## border
  tm_shape(mps) + 
  #tm_borders(lwd=0.1, col=gray(0.2), alpha=0.5) +
  tm_borders(lwd=0.1, col='black', alpha=0.5) +
  
  ## bean area from atlas
  tm_shape(atlas) +
  tm_borders(lwd=1.5, col='blue', alpha=0.9) +
  
  ### sample points
  tm_shape(tif_sf) + tm_dots(size = 0.1, shape = 1)+
  
  tm_xlab("Longitude", size =2) +  tm_ylab("Latitude", size =2)

tmap_save(curmap, filename = 'E:/Prakash/dssat_outputs/tanzania/map_pts_tz_obs_tmin_clust5.png',
          width = 10, height = 10, units = 'in', dpi = 300)
