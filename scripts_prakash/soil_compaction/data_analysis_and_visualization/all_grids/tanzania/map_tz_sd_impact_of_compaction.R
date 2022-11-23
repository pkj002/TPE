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
in_path<-'//catalogue/BaseLineDataCluster01/temp/compac_all_grids/plts_comb/all_grids/'
cnt<-'tanzania'

## domains of Tandif = round((comp[,2:15] - orig[,2:15])*100/orig[,2:15],1)
dif = data.frame(grids=orig$grids,dif)zania
lon<-seq(from=29.125,to=40.125,by=0.05)
lat<-seq(from=-11.975, to=-0.975,by=0.05)
len=length(lon)*length(lat)
ln<-length(lon); lt<-length(lat)

## Load data
dif <- readRDS(paste0(in_path,'tz_sd_dif_',cnt,'.RDS'))

## fill empty grids
dif$grids <- as.numeric(dif$grids)
bin1<- dif %>% complete(grids = full_seq(grids, 1))

mt <- array(as.numeric(),c(bin1$grids[1]-1,length(colnames(bin1[-1]))))
colnames(mt) <- colnames(bin1[-1])
top<-data.frame(grids=1:(bin1$grids[1]-1),mt)
mt1 <- array(as.numeric(),c(len-bin1$grids[length(bin1$grids)],length(colnames(bin1[-1]))))
colnames(mt1) <- colnames(bin1[-1])
bot<-data.frame(grids=(bin1$grids[length(bin1$grids)]+1):len,mt1)
df1<-rbind(top,bin1,bot)

yld <- array(df1$yld,c(ln,lt)); pwam <- array(df1$pwam,c(ln,lt)); hipd <- array(df1$mx_hipd,c(ln,lt));
cwam <- array(df1$cwam,c(ln,lt)); lai <- array(df1$lai,c(ln,lt)); et_c <- array(df1$et_sd,c(ln,lt));
slad <- array(df1$mx_slad,c(ln,lt)); lnd <- array(df1$mx_lnd,c(ln,lt)); rdpd <- array(df1$mx_rdpd,c(ln,lt));
rwad <- array(df1$mx_rwad,c(ln,lt)); rl1d <- array(df1$mx_rl1d,c(ln,lt)); rl2d <- array(df1$mx_rl2d,c(ln,lt));
rl3d <- array(df1$mx_rl3d,c(ln,lt)); rl4d <- array(df1$mx_rl4d,c(ln,lt)); 


all<-abind(yld,pwam,hipd,cwam,lai,et_c,slad,lnd,rdpd,rwad,rl1d,rl2d,rl3d,rl4d,
           along=3)  

df2=all
for (n in 1:dim(df2)[3]){
  df2[, ,n]=all[,order(ncol(all[,,n]):1),n]
}

data<-brick(df2, xmn=29.125, xmx=40.125, ymn=-11.975, ymx=-0.975, 
            crs=NA, transpose=TRUE)

mps <- getData("GADM", country = "TZA", level = 0, path = in_path)
name<-c('Yield','Pod weight','PHI','Biomass','LAI','ET av','Sp Leaf area','Leaf num','Root depth','Root weight','RLD1','RLD2','RLD3','RLD4')

# Selecting only Valle del Cauca ------------------------------------------
crs(data)<-crs(mps)
data1 <- crop(data, mps) %>% mask(mps)
names(data1)<-name

# Load bean corridor polygons
atlas <- shapefile('//catalogue/BaseLineDataCluster01/temp/scripts/bean_atlas/AtlasBean_data_copy.shp')
atlas <- atlas[atlas$CountryA3 == "TZA",]

crs(atlas)<-crs(mps)
atlas <- crop(atlas, mps)

pcols <- c('#bae4b3','#31a354','#fdd49e', '#fc8d59', '#b30000')
breaks=c(0,1,2,3,4,Inf)
unit="%"

## draw plot
curmap <- tm_shape(data1) +
    tm_raster(style='fixed', breaks = breaks, legend.is.portrait = F,
            palette = pcols, alpha = 0.65, title = unit) +
  tm_layout(main.title='Standard Deviation', main.title.size=1.5, panel.show = TRUE,   
            inner.margins = c(0.001, 0.01, 0.005, 0.01), 
            outer.margins = c(0.001, 0.001, 0.001, 0.001), 
            panel.labels = name, panel.label.size = 2, legend.outside.position = "bottom",
            legend.outside = T, legend.bg.color = "white", legend.bg.alpha = 1) +
  tm_facets(ncol=4) +
  ## border
  tm_shape(mps) + 
  tm_borders(lwd=0.1, col=gray(0.2), alpha=0.5) +
  
  ## bean area from atlas
  tm_shape(atlas) +
  tm_borders(lwd=1.5, col='#3182bd', alpha=0.9)# +
#tm_xlab("Longitude", size =2) +  tm_ylab("Latitude", size =2)
tmap_save(curmap,
          paste0(in_path,'tz_sd_impact_of_compaction.png'), 
          width= 10, height = 10, units = "in",  dpi = 300)
