rm(list=ls())
library(ncdf4)
library(abind)
require(pacman)
library(tmap) 
library(classInt)
library(sf)
pacman::p_load(raster, rgdal, rgeos, gtools, tidyverse)

path<-'E:/Prakash/dssat_out_culti/uganda/PlantGro/cluster/'
cntry<-'uganda'

## domain Uganda
lon<-seq(from=29.525,to=35.025,by=0.05)
lat<-seq(from=-1.525, to=4.475,by=0.05)
len=length(lon)*length(lat)
ln<-length(lon); lt<-length(lat)

cultivar <- c('Cal_96','DAB_489','HTA_4','KAT_B9','MCM_1015','SCR_26','SELIAN_97','SER_119','SUG_73','UYOLE_94')
yld1 <- list(); yld2 <- list()

for (n in 1:10){
  ff1<-readRDS(paste0(path, 'freq_cluster/SF_perc_in_each_grid_ug_',cultivar[n],'_uganda.RDS'))[,2]
  ff2<-readRDS(paste0(path, 'freq_cluster/STS_perc_in_each_grid_ug_',cultivar[n],'_uganda.RDS'))[,2]
  
  cl1 <- ff1; cl2 <- ff2
  yl1 <- array(cl1,c(ln,lt))*100
    
  yl2 <- array(cl2,c(ln,lt))*100
   
  yld1[[n]]<- yl1
  yld2[[n]]<- yl2
}

yld<-abind(yld1[[1]],yld2[[1]],yld1[[2]],yld2[[2]],
           yld1[[3]],yld2[[3]],yld1[[4]],yld2[[4]],
           yld1[[7]],yld2[[7]],yld1[[9]],yld2[[9]],
           yld1[[10]],yld2[[10]],yld1[[6]],yld2[[6]],
           yld1[[8]],yld2[[8]],yld1[[5]],yld2[[5]],along=3)

df2=yld
for (n in 1:dim(df2)[3]){
  df2[, ,n]=yld[,order(ncol(yld[,,n]):1),n]
}

data<-brick(df2, xmn=29.125, xmx=35.025, ymn=-1.525, ymx=4.475, 
            crs=NA, transpose=TRUE)

mps <- getData("GADM", country = "UGA", level = 1, path = path)
# name for panel plots
name<-paste0(rep(cultivar[c(1:4,7,9,10,6,8,5)],each=2),c('_SF','_STS'))

# Selecting only Valle del Cauca ------------------------------------------
crs(data)<-crs(mps)
v1 <- mps[mps$NAME_1 %in% mps$NAME_1[-32],] #
data1 <- crop(data, v1) %>% mask(v1)
names(data1)<-name

tit=paste0('Cluster frequency')
#pcols<-c('#bdbdbd','#377eb8','#4daf4a','#984ea3','#ff7f00')
pcols<-c('#f7f7f7','#cccccc','#969696','#636363','#252525')
pcols <- c('#e0ecf4', '#8856a7','#31a354','#ffeda0','#f03b20')

breaks=c(0,10,25,50,75,100)
unit="%"

# Load bean corridor polygons
atlas <- shapefile('//catalogue/BaseLineDataCluster01/temp/scripts/bean_atlas/AtlasBean_data_copy.shp')
atlas <- atlas[atlas$Country == "UGANDA",]

mps <- getData("GADM", country = "UGA", level = 0, path = path)
crs(atlas)<-crs(mps)
atlas <- crop(atlas, mps)


## draw plot
curmap <- tm_shape(data1) +
  
  tm_raster(style='fixed', breaks = breaks, legend.is.portrait = F,
            palette = pcols, alpha = 0.65, title = unit) +
  
  tm_layout(main.title='', main.title.size=1.5, panel.show = TRUE,   
            inner.margins = c(0.001, 0.01, 0.005, 0.01), 
            outer.margins = c(0.001, 0.001, 0.001, 0.001), 
            panel.labels = name, panel.label.size = 1.2, legend.outside.position = "bottom",
            #legend.format = list(fun = function(x) formatC(x, digits = digit[v], format = "f")),
            legend.outside = T, legend.bg.color = "white", legend.bg.alpha = 1) +
  tm_facets(ncol=4) +
  ## border
  tm_shape(mps) + 
  tm_borders(lwd=0.1, col=gray(0.2), alpha=0.5) +
  
  ## bean area from atlas
  tm_shape(atlas) +
  tm_borders(lwd=1.5, col='#3182bd', alpha=0.9) +
  tm_xlab("Longitude", size =2) +  tm_ylab("Latitude", size =2)

tmap_save(curmap,
          'E:/Prakash/dssat_out_culti/uganda/maps/allgeno_combined_cluster_freq.png', 
          width= 7, height = 10, units = "in",  dpi = 300)

