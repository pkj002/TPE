rm(list=ls())
library(ncdf4)
library(abind)
require(pacman)
library(tmap) 
library(classInt)
library(sf)

pacman::p_load(raster, rgdal, rgeos, gtools, tidyverse)
path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ugand/tpe/PlantGro/final/cluster/cluster_pltgro_yr_sowdt/'
cntry<-'uganda'

## domain Uganda
lon<-seq(from=29.525,to=35.025,by=0.05)
lat<-seq(from=-1.525, to=4.475,by=0.05)
len=length(lon)*length(lat)
ln<-length(lon); lt<-length(lat)

#cl<-readRDS(paste0(path, 'grid_veg_rep_stress_cluster2_uganda_.RDS'))
cl<-readRDS(paste0(path,'grid_cluster/obs_grid_veg_rep_stress_cluster2_uganda.RDS'))
#cl<-readRDS(paste0(path, 'grid_veg_rep_stress_cluster5_uganda.RDS'))

cl1<-cl$clust
cl2<-array(cl1,c(ln,lt))
cl3<-cl2
yld<-abind(cl2,cl3,along=3)

df2=yld
for (n in 1:dim(df2)[3]){
  df2[, ,n]=yld[,order(ncol(yld[,,n]):1),n]
}


data<-brick(df2, xmn=29.125, xmx=35.025, ymn=-1.525, ymx=4.475, 
            crs=NA, transpose=TRUE)

mps <- getData("GADM", country = "UGA", level = 1, path = path)

## name for panel plots
name<-c('cluster 2','cluster 2')

# Selecting only Valle del Cauca ------------------------------------------
crs(data)<-crs(mps)
v1 <- mps[mps$NAME_1 %in% mps$NAME_1[-32],] #
data1 <- crop(data, v1) %>% mask(v1)
names(data1)<-name

tit=paste0('Number of clusters based on stress under recent climate')

breaks=1:2
pcols<-c('#377eb8','#7f2704')

##############################################################################################
## bulk density simulation points
cord<-data.frame(long=rep(lon,lt),lati=rep(lat,each=ln))
## load sample points
pts<-readRDS('E:/Prakash/dssat_outputs/uganda/sample_pts2.RDS')
pts<- c(pts[c(2,3,4,9)], 8766, 8989, 9100,9204,9210,9321,9536,9647,9758,9867,9982)
pts1<-readRDS('E:/Prakash/dssat_outputs/uganda/sample_pts1.RDS')
pts<-c(pts1,pts)

dt<-cord[pts,]
colnames(dt)<- c('LONGITUDE', 'LATITUDE')
tif_data <- dt[!is.na(dt$LATITUDE), ]
tif_sf <- st_as_sf(tif_data, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

## add shape file of bean atlas
atlas <- shapefile('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/bean_atlas/AtlasBean_data_copy.shp')

mps <- getData("GADM", country = "UGA", level = 0, path = path)

crs(atlas)<-crs(mps)
atlas <- crop(atlas, mps) 
#atlas <- crop(mps, atlas) 

## draw plot
curmap <- tm_shape(data1) +
  
  tm_raster(style='cat', breaks = breaks, legend.is.portrait = T,legend.show = F,
            palette = pcols, alpha = 0.65, title = 'Cluster') +
  
  tm_layout(main.title=tit, main.title.size=1.5, panel.show = TRUE,   
            inner.margins = c(0.001, 0.01, 0.005, 0.01), 
            outer.margins = c(0.001, 0.001, 0.001, 0.001), 
            panel.labels = name, panel.label.size = 2, legend.position = c("right","top"),
            #legend.format = list(fun = function(x) formatC(x, digits = digit[v], format = "f")),
            legend.outside = F, legend.bg.color = "white", legend.bg.alpha = 1) +
  tm_facets(ncol=2) +
  ## border
  tm_shape(mps) + 
  tm_borders(lwd=0.3, col=gray(0.2), alpha=0.5) +
  
  ## bean area from atlas
  tm_shape(atlas) +
  tm_borders(lwd=2.5, col='yellow', alpha=0.9) +
  
   ### sample points
  tm_shape(tif_sf) + tm_dots(size = 0.1, shape = 1)+
  
  # tm_grid(projection=4326, col ='black', n.x=12, n.y=12, labels.size = 1, alpha=1,
  #         labels.margin.x=0, labels.margin.y=0, labels.inside.frame=F)  +
  
  tm_xlab("Longitude", size =2) +  tm_ylab("Latitude", size =2)

tmap_save(curmap,
          'E:/Prakash/dssat_outputs/uganda/map_sample_pts_ug.png',
          width= 10, height = 10, units = "in",  dpi = 300)

#q(save='no')

