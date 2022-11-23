rm(list=ls())
library(abind)
require(pacman)
library(tmap) 
library(classInt)

pacman::p_load(sp, raster, rgdal, rgeos, gtools, tidyverse)

# GCM Models
mod <- c('BCC-CSM2-MR','EC-Earth3-Veg','GFDL-ESM4','IPSL-CM6A-LR','MRI-ESM2-0')
sc<-c(126,245,370,585)

# dimension of domain
lon<-seq(from=33.025,to=44.025,by=0.05)
lat<-seq(from=3.025, to=15.025,by=0.05)
ln<-length(lon); lt<-length(lat)
len <- length(lon)*length(lat)
cntry<-'ethiopia'

# dimension of domain
comb <- data.frame(year = c(rep(2030,20),rep(2050,20)), mod = c(rep(mod,each=4),rep(mod,each=4)), sc = rep(c(126,245,370,585),10),
                   period=c(rep('202101-204012',20),rep('204101-206012',20)), co2=c(rep(c(440,446,454,456),5),rep(c(469,508,544,569),5)),
                   stringsAsFactors = FALSE)

path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ethiopia/tpe/'

## Historical
dd<-readRDS(paste0(path,'yld_main_seas_ethiopia_obs_new.RDS'))
his<-unlist(dd$yld)
his_ethi<-array(his,c(ln,lt))
his_ethi1<-his_ethi
his_ethi2<-abind(his_ethi,his_ethi1,along=3)

## Ethi
df2=his_ethi2
for (n in 1:dim(df2)[3]){
  df2[, ,n]=df2[,order(ncol(df2[,,n]):1),n]
}

data<-brick(df2, xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025, 
            crs=NA, transpose=TRUE)

#name <- paste0('ssp_',sc[c(1,4)],'_',rep(c(2030,2050),each=2))
mps_et<-shapefile('C:/Users/pjha/Desktop/ETH_adm/ETH_adm0.shp')

crs(data)<-crs(mps_et)
v1 <- mps_et[mps_et$NAME_0 %in% mps_et$NAME_0[c(1:8,10:11)],] ## exclude Somali desert
data1 <- crop(data, v1) %>% raster::mask(v1)
#names(data1)<-name

atlas <- shapefile('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/bean_atlas/AtlasBean_data_copy.shp')
crs(atlas)<-crs(mps_et)
atlas <- crop(atlas, mps_et) #%>% mask(mps)

# pcols <- c('#fc8d59','#f0f0f0','#ffffe5','#d9ef8b','#a1d99b','#00441b')
# breaks=c(-500,seq(0,1200,by=300),Inf)

pcols <- c('#969696','#d73027','#fc8d59','#fee08b','#ffffbf','#d9ef8b','#91cf60')
breaks=c(-Inf,seq(500,3000,by=500), Inf)
unit<- 'kg ha-1'

## draw plot
ethi_plot <- tm_shape(data1) +
  
  tm_raster(style='fixed', breaks = breaks, legend.show = F,
            palette = pcols,alpha = 0.75, title = unit) +
  
  tm_layout(main.title='', main.title.size=1.5, panel.show = TRUE,   
            # inner.margins = c(0.01, 0.01, 0.02, 0.05), 
            # outer.margins = c(0.001, 0.001, 0.001, 0.001), 
            panel.labels = '', panel.label.size = 1.2) +
  
  tm_facets(ncol=1) +
  
  ## border
  tm_shape(mps_et) + 
  tm_borders(lwd=0.1, col='black') +
  
  ## bean area from atlas
  tm_shape(atlas) +
  tm_borders(lwd=0.6, col='blue') 


# tmap_save(ethi_plot, filename='C:/Users/pjha/Desktop/hist_eth.png',
#                      width= 7, height = 12, units = "in",  dpi = 300)

###############################################################################################################
######### Tanz
## Tanzania begins
## domains of tanzania
lon<-seq(from=29.125,to=40.125,by=0.05)
lat<-seq(from=-11.975, to=-0.975,by=0.05)
ln<-length(lon); lt<-length(lat)
len <- ln*lt

path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/tanzania/tpe/yld_tpe/'

## Historical
dd<-readRDS(paste0(path,'yld_tpe_tanzania_obs_new2.RDS'))
md <- map_df(dd, ~as.data.frame(.x), .id="grids")
# create a vector with all the missing days
missing <- setdiff(1:len,md$grids)
# append the missing rows to the end of df data.frame (creating a new one)
md_comp <- rbind(md,data.frame(grids=missing,yield=NA,season=NA))
# sort the rows 
md_comp$grids<-as.numeric(md_comp$grids)
md_comp <- md_comp[order(md_comp$grids),]
his<-array(md_comp$yield,c(ln,lt))
his_tanz<-his
his_tanz1<-his_tanz
his_tanz2<-abind(his_tanz,his_tanz1,along=3)

## main
df2=his_tanz2
for (n in 1:dim(df2)[3]){
  df2[, ,n]=df2[,order(ncol(df2[,,n]):1),n]
}

data<-brick(df2, xmn=29.125, xmx=40.125, ymn=-11.975, ymx=-0.975, 
            crs=NA, transpose=TRUE)
mps_tz <- getData("GADM", country = "TZA", level = 0, path = path)
# Selecting only Valle del Cauca ------------------------------------------
crs(data)<-crs(mps_tz)
data2<-crop(data,mps_tz) %>% mask(mps_tz)
#names(data2)<-name
atlas <- shapefile('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/bean_atlas/AtlasBean_data_copy.shp')
atlas <- gBuffer(atlas, byid=TRUE, width=0)
atlas <- spTransform(atlas, CRS( "+proj=longlat +datum=WGS84 +no_defs"))
atlas <- raster::crop(atlas, mps_tz) #%>% ma

tz_plot <- tm_shape(data2) +
  
  tm_raster(style='fixed', breaks = breaks, legend.show = F,
            palette = pcols,alpha = 0.75, title = unit) +
  
  tm_layout(main.title='', main.title.size=1.5, panel.show = TRUE,   
            # inner.margins = c(0.01, 0.01, 0.02, 0.05), 
            # outer.margins = c(0.001, 0.001, 0.001, 0.001), 
            panel.labels = '', panel.label.size = 1.2) +
  
  tm_facets(ncol=1) +
  
  ## border
  tm_shape(mps_tz) + 
  tm_borders(lwd=0.1, col='black') +
  
  ## bean area from atlas
  tm_shape(atlas) +
  tm_borders(lwd=0.6, col='blue') 

# tmap_save(tz_plot, filename='C:/Users/pjha/Desktop/hist_tz.png',
#                      width= 7, height = 12, units = "in",  dpi = 300)

### Uganda
path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ugand/tpe/model_yld_tpe/'

lon<-seq(from=29.525,to=35.025,by=0.05)
lat<-seq(from=-1.525, to=4.475,by=0.05)
ln<-length(lon); lt<-length(lat)
len <- ln*lt

his<-readRDS(paste0(path,'yld_grid_tpe_uganda_obs.RDS'))
his_ug<-array(his,c(ln,lt))
his_ug[23:50,20]<- his_ug[23:50,19]
his_ug1<-his_ug
his_ug2<-abind(his_ug,his_ug1,along=3)

## Uganda
df2=his_ug2
for (n in 1:dim(df2)[3]){
  df2[, ,n]=df2[,order(ncol(df2[,,n]):1),n]
}

data<-brick(df2, xmn=29.125, xmx=35.025, ymn=-1.525, ymx=4.475, 
            crs=NA, transpose=TRUE)

mps_ug <- getData("GADM", country = "UGA", level = 1, path = path)

# Selecting only Valle del Cauca ------------------------------------------
crs(data)<-crs(mps_ug)
v1 <- mps_ug[mps_ug$NAME_0 %in% mps_ug$NAME_0[-32],] #
data3 <- crop(data, v1) %>% mask(v1)
#names(data3)<-name
mps_ug <- getData("GADM", country = "UGA", level = 0, path = path)

atlas <- shapefile('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/bean_atlas/AtlasBean_data_copy.shp')
crs(atlas)<-crs(mps_ug)
atlas <- crop(atlas, mps_ug) #%>% mask(mps)

## draw plot
ug_plot <- tm_shape(data3) +
  
  tm_raster(style='fixed', breaks = breaks,legend.show = F, 
            palette = pcols, alpha = 0.75, title = unit) +
  
  tm_layout(main.title='', main.title.size=1.5, panel.show = TRUE,   
            # inner.margins = c(0.01, 0.01, 0.02, 0.05), 
            # outer.margins = c(0.001, 0.001, 0.001, 0.001), 
            panel.labels = '', panel.label.size = 1.2) + 
  
  tm_facets(ncol=1) +
  
  ## border
  tm_shape(mps_ug) + 
  tm_borders(lwd=0.1, col='black', alpha=0.5) +
  
  ## bean area from atlas
  tm_shape(atlas) +
  tm_borders(lwd=0.5, col='blue') 

#tm_xlab(" ") +  tm_ylab("Latitude", size=1.5)


plot <- tmap_arrange(ug_plot, tz_plot, ethi_plot, ncol=3,heights=c(0.2,0.2,0.2,0.2,0.2),outer.margins=NULL)
tmap_save(plot, filename = 'C:/Users/pjha/Desktop/hist_yld_all_cntry.png', width= 10, height = 10, units = "in", dpi = 300)


##Separate legend plot
legend.map <- tm_shape(data3) +
  tm_raster(style = 'fixed',breaks = breaks,legend.is.portrait = F,
            palette = pcols, alpha = 0.75, title = unit) +
  tm_layout(legend.only = TRUE, legend.outside = T, legend.outside.position = 'bottom',
            legend.bg.color = "white", legend.bg.alpha = 1)

tmap_save(legend.map, filename = 'C:/Users/pjha/Desktop/legd_hist_yld_all_cntry.png',units = "in",dpi = 300)

