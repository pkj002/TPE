rm(list=ls())
gc()
library(abind)
require(pacman)
library(tmap) 
library(classInt)
library(sf)

pacman::p_load(raster, rgdal, rgeos, gtools, tidyverse)
cntry<-'ethiopia'

dir<-'//dapadfs/workspace_cluster_12/AVISA/dssat_outputs/ethiopia/tpe/PlantGro/final/cluster/'

## clustering data
## domains of ethiopia
lon<-seq(from=33.025,to=44.025,by=0.05)
lat<-seq(from=3.025, to=15.025,by=0.05)
len=length(lon)*length(lat)
ln<-length(lon); lt<-length(lat)

## Obs Yield 
## Belg season
bleg<-readRDS(paste0(dir,'clst4_2seas_ethiopia_obs_new.RDS'))
belg<-bleg$clst2
# belg[belg==1]=5; belg[belg==3]=6
# belg[belg==2]=3; belg[belg==6]=1; belg[belg==5]=2

## main seas
main<-readRDS(paste0(dir,'clst4_main_seas_ethiopia_obs_new.RDS'))
main<-main$clst
# main[main==1]=5; main[main==3]=6
# main[main==2]=3; main[main==6]=1; main[main==5]=2

##############################################################################################
## bulk density simulation points
cord<-data.frame(long=rep(lon,lt),lati=rep(lat,each=ln))
dt<-data.frame(cord, main)


dir_sf<-c(13088,15144,18228,20191,23551,25458,28880,30655,33222,35283,38105,39200,43681,45601,49381) ## random points
dir_tm<-c(11316,23077,26430,28607,30768,32155,33497,35473,37024,39015,40771,42563,44306,47846,50932)
dir_ts<-c(7794,25305,26839,27751,28621,29504,30394,31741,40545,42308,44773,46080,47640,49417,50963)
dir_gf<-c(4962,7627,11800,14072,17635,21822,23752,26515,29377,32843,35080,38404,41286,44128,48764) 

pts <- c(dir_sf,dir_tm,dir_ts,dir_gf)

dt<-dt[pts,]
dt<-dt[,1:2]
colnames(dt)<- c('LONGITUDE', 'LATITUDE')

tif_data <- dt[!is.na(dt$LATITUDE), ]
tif_sf <- st_as_sf(tif_data, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
#############################################################################################

## two season
obs3<-array(unlist(belg),c(ln,lt))
obs4<-array(unlist(main),c(ln,lt))

yld<-abind(obs4,obs3,along=3)


## Now plotting
df2=yld
for (n in 1:dim(df2)[3]){
  df2[, ,n]=yld[,order(ncol(yld[,,n]):1),n]
}

data<-brick(df2, xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025, 
            crs=NA, transpose=TRUE)

mps <- getData("GADM", country = "ETH", level = 1, path = dir)
name<-c('main season','Belg')

# Selecting only Valle del Cauca ------------------------------------------
crs(data)<-crs(mps)
v1 <- mps[mps$NAME_1 %in% mps$NAME_1[c(1:8,10:11)],] ## exclude Somali desert
data1 <- crop(data, v1) %>% mask(v1)

#tit=paste0('Number of clusters based on yield under recent climate')
tit='Water Stress Cluster'
unit='Cluster'

#pcols<-c('#984ea3','#377eb8','#e41a1c','#7f2704')
#pcols<-c('#e41a1c','#377eb8','#7f2704','#41ab5d')
pcols<-c('#fff7bc','#377eb8','#e41a1c','#7f2704')
breaks=1:4


## add shape file of bean atlas
atlas <- shapefile('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/bean_atlas/AtlasBean_data_copy.shp')

crs(atlas)<-crs(mps)
atlas <- raster::crop(atlas, mps) #%>% mask(mps)

mps <- getData("GADM", country = "ETH", level = 0, path = dir)

## draw plot
curmap <- tm_shape(data1) +
  
  tm_raster(style='cat', breaks = breaks,legend.is.portrait = T,
            palette = pcols, alpha = 0.75, title = unit) +
  
  tm_layout(main.title=tit, main.title.size=1.5, panel.show = TRUE,   
            inner.margins = c(0.01, 0.01, 0.02, 0.05), 
            outer.margins = c(0.001, 0.001, 0.001, 0.001), 
            panel.labels = name, panel.label.size = 2, 
            legend.outside = T, legend.bg.color = "white", legend.bg.alpha = 1) +
  tm_facets(ncol=2) +
  ## border
  tm_shape(mps) + 
  tm_borders(lwd=0.3, col=gray(0.2), alpha=0.5) +
  
  ## bean area from atlas
  tm_shape(atlas) +
  tm_borders(lwd=2.5, col='yellow', alpha=0.9) +
  
  ### points
  tm_shape(tif_sf) + tm_dots(size = 0.1, shape = 1)+
  
  tm_xlab("Longitude", size =2) +  tm_ylab("Latitude", size =2)
tmap_save(curmap, filename = 'E:/Prakash/dssat_outputs/ethiopia/new_clust4_stress_splitSeas.png',
          width = 10, height = 10, units = 'in', dpi = 300)
