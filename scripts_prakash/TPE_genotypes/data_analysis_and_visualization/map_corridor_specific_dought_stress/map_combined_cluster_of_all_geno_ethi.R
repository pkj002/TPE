rm(list=ls())
library(ncdf4)
library(abind)
require(pacman)
library(tmap) 
library(classInt)
library(sf)
pacman::p_load(raster, rgdal, rgeos, gtools, tidyverse)

path<-'E:/Prakash/dssat_out_culti/ethiopia/PlantGro/cluster/'
cntry<-'ethiopia'

## domains of ethiopia
lon<-seq(from=33.025,to=44.025,by=0.05)
lat<-seq(from=3.025, to=15.025,by=0.05)
len <- length(lon)*length(lat)
ln<-length(lon); lt<-length(lat)

cultivar <- c('Cal_96','DAB_489','HTA_4','KAT_B9','MCM_1015','SCR_26','SELIAN_97','SER_119','SUG_73','UYOLE_94')

yld1 <- list(); yld2 <- list(); yld3 <- list(); yld4 <- list()

for (i in 1:10){
  #for (i in 6:10){
  ## cl1
  gg<-readRDS(paste0(path,'clust_perc_in_each_grid/clust1prop_c',i,'_ethiopia_all_ge.RDS'))
  df1<-unlist(gg$clst)
  df1[is.na(df1)]=0
  
  ## cl2
  hh<-readRDS(paste0(path,'clust_perc_in_each_grid/clust2prop_c',i,'_ethiopia_all_ge.RDS'))
  df2<-unlist(hh$clst)
  df2[is.na(df2)]=0
  
  ## cl3
  ii<-readRDS(paste0(path,'clust_perc_in_each_grid/clust3prop_c',i,'_ethiopia_all_ge.RDS'))
  df3<-unlist(ii$clst)
  df3[is.na(df3)]=0
  
  ## cl4
  jj<-readRDS(paste0(path,'clust_perc_in_each_grid/clust4prop_c',i,'_ethiopia_all_ge.RDS'))
  df4<-unlist(jj$clst)
  df4[is.na(df4)]=0
  
  cl2 <- df1; cl4 <- df2; cl1 <- df3; cl3 <- df4
 
  yld1[[i]]<- array(cl1,c(ln,lt))*100
  yld2[[i]]<- array(cl2,c(ln,lt))*100  
  yld3[[i]]<- array(cl3,c(ln,lt))*100
  yld4[[i]]<- array(cl4,c(ln,lt))*100
}

################################################################################################
############# Make map #####################################################################
#map='1st'
map='2nd'

if(map=='1st'){
  yld<-abind(yld1[[1]],yld2[[1]],yld3[[1]],yld4[[1]],
             yld1[[2]],yld2[[2]],yld3[[2]],yld4[[2]], 
             yld1[[3]],yld2[[3]],yld3[[3]],yld4[[3]],
             yld1[[4]],yld2[[4]],yld3[[4]],yld4[[4]],
             yld1[[7]],yld2[[7]],yld3[[7]],yld4[[7]],along=3)  
} else {
  yld<-abind(yld1[[9]],yld2[[9]],yld3[[9]],yld4[[9]],
             yld1[[10]],yld2[[10]],yld3[[10]],yld4[[10]],
             yld1[[6]],yld2[[6]],yld3[[6]],yld4[[6]],
             yld1[[8]],yld2[[8]],yld3[[8]],yld4[[8]],
             yld1[[5]],yld2[[5]],yld3[[5]],yld4[[5]], along=3) 
  
}


df2=yld
for (n in 1:dim(df2)[3]){
  df2[, ,n]=yld[,order(ncol(yld[,,n]):1),n]
}

data<-brick(df2, xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025, 
            crs=NA, transpose=TRUE)

mps <- getData("GADM", country = "ETH", level = 1, path = path)
# name for panel plots
if(map=='1st'){
  name<-paste0(rep(cultivar[c(1:4,7)],each=4),c('_SF','_GS','_MTS','_ETS')) 
} else {
  name<-paste0(rep(cultivar[c(9:10,6,8,5)],each=4),c('_SF','_GS','_MTS','_ETS')) 
}


## assign coordinate and exclude Somali desert
crs(data)<-crs(mps)
v1 <- mps[mps$NAME_1 %in% mps$NAME_1[c(1:8,10:11)],] ## exclude Somali desert
data1 <- crop(data, v1) %>% raster::mask(v1)
names(data1)<-name

pcols <- c('#e0ecf4', '#8856a7','#31a354','#ffeda0','#f03b20')
breaks=c(0,10,25,50,75,100)
unit="%"

# Load bean corridor polygons
atlas <- shapefile('//catalogue/BaseLineDataCluster01/temp/scripts/bean_atlas/AtlasBean_data_copy.shp')
atlas <- atlas[atlas$Country == "ETHIOPIA",]

crs(atlas)<-crs(mps)
atlas <- crop(atlas, mps)

mps <- getData("GADM", country = "ETH", level = 0, path = path)
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

if(map=='1st'){
  tmap_save(curmap,
            'E:/Prakash/dssat_out_culti/ethiopia/maps/ethi_geno1to5_combined_cluster_freq.png', 
            width= 7, height = 10, units = "in",  dpi = 300)
} else {
  tmap_save(curmap,
            'E:/Prakash/dssat_out_culti/ethiopia/maps/ethi_geno6to10_combined_cluster_freq.png', 
            width= 7, height = 10, units = "in",  dpi = 300)
}

