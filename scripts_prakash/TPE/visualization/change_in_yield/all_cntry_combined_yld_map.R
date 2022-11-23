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

fig='main'
#fig='supp'

## Historical
dd<-readRDS(paste0(path,'yld_main_seas_ethiopia_obs_new.RDS'))
his<-unlist(dd$yld)
his<-array(his,c(ln,lt))

## Model
main<-array(numeric(),c(len,40)); belg<-main
for (pp in 1:40){
  dd<-readRDS(paste0(path,'yld_main_seas_ethiopia_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
  if(pp==27 | pp == 39){ ## Last grid is missing
    main[,pp]<-c(unlist(dd$yld),NA)
  } else {
    main[,pp]<-unlist(dd$yld)
  }
  
  ## Belg season
  bb<-readRDS(paste0(path,'yld_2seas_ethiopia_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
  belg[,pp]<-unlist(bb$yld_seas2_2)
}

## Select only 126 and 585
## main
m130<-main[,c(1,5,9,13,17)] ## ssp126_2030
m130<-apply(m130,c(1),mean, na.rm=T)
m130<-array(m130,c(ln,lt))

m245<-main[,c(2,6,10,14,18)] ## ssp245_2030
m245<-apply(m245,c(1),mean, na.rm=T)
m245<-array(m245,c(ln,lt))

m370<-main[,c(3,7,11,15,19)] ## ssp_370_2030
m370<-apply(m370,c(1),mean, na.rm=T)
m370<-array(m370,c(ln,lt))

m430<-main[,c(4,8,12,16,20)] ## ssp585_2030
m430<-apply(m430,c(1),mean, na.rm=T)
m430<-array(m430,c(ln,lt))

m150<-main[,c(21,25,29,33,37)] ## ssp126_2050
m150<-apply(m150,c(1),mean, na.rm=T)
m150<-array(m150,c(ln,lt))

m250<-main[,c(22,26,30,34,38)] ## ssp245_2050
m250<-apply(m250,c(1),mean, na.rm=T)
m250<-array(m250,c(ln,lt))

m350<-main[,c(23,27,31,35,39)] ## ssp_370
m350<-apply(m350,c(1),mean, na.rm=T)
m350<-array(m350,c(ln,lt))

m450<-main[,c(24,28,32,36,40)] ## ssp585_2050
m450<-apply(m450,c(1),mean, na.rm=T)
m450<-array(m450,c(ln,lt))

# ## Belg
# b130<-belg[,c(1,5,9,13,17)] ## ssp126_2030
# b130<-apply(b130,c(1),mean, na.rm=T)
# b130<-array(b130,c(ln,lt))
# 
# b430<-belg[,c(4,8,12,16,20)] ## ssp585_2030
# b430<-apply(b430,c(1),mean, na.rm=T)
# b430<-array(b430,c(ln,lt))
# 
# b150<-belg[,c(21,25,29,33,37)] ## ssp126_2050
# b150<-apply(b150,c(1),mean, na.rm=T)
# b150<-array(b150,c(ln,lt))
# 
# b450<-belg[,c(24,28,32,36,40)] ## ssp585_2050
# b450<-apply(b450,c(1),mean, na.rm=T)
# b450<-array(b450,c(ln,lt))

## combine main hist and model
if(fig=='supp'){
  ch245=m245-his
  ch370=m370-his
  ch250=m250-his
  ch350=m350-his
  comb_main<-abind(ch245,ch370,ch250,ch350,along=3)
} else {
  ch130=m130-his
  ch430=m430-his
  ch150=m150-his
  ch450=m450-his
  comb_main<-abind(ch130,ch430,ch150,ch450,along=3) 
}

## Ethi
df2=comb_main
for (n in 1:dim(df2)[3]){
  df2[, ,n]=df2[,order(ncol(df2[,,n]):1),n]
}

data<-brick(df2, xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025, 
            crs=NA, transpose=TRUE)

name <- paste0('ssp_',sc[c(1,4)],'_',rep(c(2030,2050),each=2))

if (fig=='supp'){
  name<-c('SSP2-4.5 - 2030','SSP3-7.0 - 2030','SSP2-4.5 - 2050','SSP3-7.0 - 2050')
} else {
  name<-c('SSP1-2.6 - 2030','SSP5-8.5 - 2030','SSP1-2.6 - 2050','SSP5-8.5 - 2050')
}

mps_et<-shapefile('C:/Users/pjha/Desktop/ETH_adm/ETH_adm0.shp')

crs(data)<-crs(mps_et)
v1 <- mps_et[mps_et$NAME_0 %in% mps_et$NAME_0[c(1:8,10:11)],] ## exclude Somali desert
data1 <- crop(data, v1) %>% raster::mask(v1)
names(data1)<-name

## add shape file of bean atlas
atlas <- shapefile('//catalogue/BaseLineDataCluster01/temp/scripts/bean_atlas/AtlasBean_data_copy.shp')
#at_pts<-shapefile('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/bean_atlas/beanatlas_PRTot500ha_2017.shp')

crs(atlas)<-crs(mps_et)
atlas <- crop(atlas, mps_et) #%>% raster::mask(mps_et)

#pcols <- c('#fc8d59','#ffffbf','#d9ef8b','#91cf60','#a1d99b','#00441b')
#pcols <- c('#fc8d59','#f0f0f0','#ffffe5','#d9ef8b','#91cf60','#a1d99b','#00441b')
pcols <- c('#fc8d59','#f0f0f0','#ffffe5','#d9ef8b','#a1d99b','#00441b')
breaks=c(-500,seq(0,1200,by=300),Inf)
unit<- 'kg ha-1'


## draw plot
ethi_plot <- tm_shape(data1) +
  
  tm_raster(style='fixed', breaks = breaks, legend.show = F,
            palette = pcols,alpha = 0.75, title = unit) +
  
  tm_layout(main.title='', main.title.size=1.5, panel.show = TRUE,   
            # inner.margins = c(0.01, 0.01, 0.02, 0.05), 
            # outer.margins = c(0.001, 0.001, 0.001, 0.001), 
            panel.labels = name, panel.label.size = 1.2) +
  
  tm_facets(ncol=1) +
  
  ## border
  tm_shape(mps_et) + 
  tm_borders(lwd=0.1, col='black') +
  
  ## bean area from atlas
  tm_shape(atlas) +
  tm_borders(lwd=0.6, col='blue') 

#tm_xlab(" ") +  tm_ylab(" ")

# tmap_save(ethi_plot, filename='C:/Users/pjha/Desktop/eth.png',
#           width= 7, height = 12, units = "in",  dpi = 300)

## End of Ethiopia
##################################################################################################
## Tanzania begins
path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ethiopia/tpe/'
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

## Model
mod_tz<-array(numeric(),c(len,40))
for (pp in 1:40){
  bb<-readRDS(paste0(path,'yld_new_dssat_runs_tanzania_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
  # create a vector with all the missing days
  missing <- setdiff(1:len,bb$grids)
  # append the missing rows to the end of df data.frame (creating a new one)
  bb_comp <- rbind(bb,data.frame(grids=missing,yield=NA,season=NA))
  ## sort the rows
  bb_comp$grids<-as.numeric(bb_comp$grids)
  bb_comp<-bb_comp[order(bb_comp$grids),]
  mod_tz[,pp]<-array(bb_comp$yield,c(ln,lt))
}

## subsetting
t130<-mod_tz[,c(1,5,9,13,17)] ## ssp126_2030
t130<-apply(t130,c(1),mean, na.rm=T)
t130<-array(t130,c(ln,lt))

t245<-mod_tz[,c(2,6,10,14,18)] ## ssp245_2030
t245<-apply(t245,c(1),mean, na.rm=T)
t245<-array(t245,c(ln,lt))

t370<-mod_tz[,c(3,7,11,15,19)] ## ssp_370_2030
t370<-apply(t370,c(1),mean, na.rm=T)
t370<-array(t370,c(ln,lt))

t430<-mod_tz[,c(4,8,12,16,20)] ## ssp585_2030
t430<-apply(t430,c(1),mean, na.rm=T)
t430<-array(t430,c(ln,lt))

t150<-mod_tz[,c(21,25,29,33,37)] ## ssp126_2050
t150<-apply(t150,c(1),mean, na.rm=T)
t150<-array(t150,c(ln,lt))

t250<-mod_tz[,c(22,26,30,34,38)] ## ssp245_2050
t250<-apply(t250,c(1),mean, na.rm=T)
t250<-array(t250,c(ln,lt))

t350<-mod_tz[,c(23,27,31,35,39)] ## ssp_370
t350<-apply(t350,c(1),mean, na.rm=T)
t350<-array(t350,c(ln,lt))

t450<-mod_tz[,c(24,28,32,36,40)] ## ssp585_2050
t450<-apply(t450,c(1),mean, na.rm=T)
t450<-array(t450,c(ln,lt))

## change future -present
if(fig=='supp'){
  tch245=t245-his
  tch370=t370-his
  tch250=t250-his
  tch350=t350-his
  comb_tz<-abind(tch245,tch370,tch250,tch350,along=3)
} else {
  tch130=t130-his
  tch430=t430-his
  tch150=t150-his
  tch450=t450-his
  comb_tz<-abind(tch130,tch430,tch150,tch450,along=3) 
}

## main
df2=comb_tz
for (n in 1:dim(df2)[3]){
  df2[, ,n]=df2[,order(ncol(df2[,,n]):1),n]
}

data<-brick(df2, xmn=29.125, xmx=40.125, ymn=-11.975, ymx=-0.975, 
            crs=NA, transpose=TRUE)
mps_tz <- getData("GADM", country = "TZA", level = 0, path = path)
# Selecting only Valle del Cauca ------------------------------------------
crs(data)<-crs(mps_tz)
data2<-crop(data,mps_tz) %>% mask(mps_tz)
names(data2)<-name

atlas <- shapefile('//catalogue/BaseLineDataCluster01/temp/scripts/bean_atlas/AtlasBean_data_copy.shp')
atlas <- gBuffer(atlas, byid=TRUE, width=0)
atlas <- spTransform(atlas, CRS( "+proj=longlat +datum=WGS84 +no_defs"))
atlas <- raster::crop(atlas, mps_tz) #%>% mask(mps)

## draw plot
tz_plot <- tm_shape(data2) +
  
  tm_raster(style='fixed', breaks = breaks, legend.show = F, 
            palette = pcols,alpha = 0.75, title = unit) +
  
  tm_layout(main.title='', main.title.size=1.5, panel.show = TRUE,   
            # inner.margins = c(0.01, 0.01, 0.02, 0.05), 
            # outer.margins = c(0.001, 0.001, 0.001, 0.001), 
            panel.labels = name, panel.label.size = 1.2) + 
  
  tm_facets(ncol=1) +
  
  ## border
  tm_shape(mps_tz) + 
  tm_borders(lwd=0.1, col='black') +
  
  ## bean area from atlas
  tm_shape(atlas) +
  tm_borders(lwd=0.5, col='blue') 

#tm_xlab("Longitude", size =1.5) +  tm_ylab(" ")

#tmap_save(tz_plot, filename = 'C:/Users/pjha/Desktop/tz_test.png',width= 7, height = 12, units = "in")
#           
## End of Tanzania
##################################################################################################
## Uganda begins
path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ugand/tpe/model_yld_tpe/'

lon<-seq(from=29.525,to=35.025,by=0.05)
lat<-seq(from=-1.525, to=4.475,by=0.05)
ln<-length(lon); lt<-length(lat)
len <- ln*lt

his<-readRDS(paste0(path,'yld_grid_tpe_uganda_obs.RDS'))
his<-array(his,c(ln,lt))

## Model
mod_ug<-array(numeric(),c(len,40))
for (pp in 1:40){
  bb<-readRDS(paste0(path,'yld_grid_tpe_uganda_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
  mod_ug[,pp]<-array(bb,c(ln,lt))
}


###
u130<-mod_ug[,c(1,5,9,13,17)] ## ssp126_2030
u130<-apply(u130,c(1),mean, na.rm=T)
u130<-array(u130,c(ln,lt))

u245<-mod_ug[,c(2,6,10,14,18)] ## ssp245_2030
u245<-apply(u245,c(1),mean, na.rm=T)
u245<-array(u245,c(ln,lt))

u370<-mod_ug[,c(3,7,11,15,19)] ## ssp_370_2030
u370<-apply(u370,c(1),mean, na.rm=T)
u370<-array(u370,c(ln,lt))

u430<-mod_ug[,c(4,8,12,16,20)] ## ssp585_2030
u430<-apply(u430,c(1),mean, na.rm=T)
u430<-array(u430,c(ln,lt))

u150<-mod_ug[,c(21,25,29,33,37)] ## ssp126_2050
u150<-apply(u150,c(1),mean, na.rm=T)
u150<-array(u150,c(ln,lt))

u250<-mod_ug[,c(22,26,30,34,38)] ## ssp245_2050
u250<-apply(u250,c(1),mean, na.rm=T)
u250<-array(u250,c(ln,lt))

u350<-mod_ug[,c(23,27,31,35,39)] ## ssp_370
u350<-apply(u350,c(1),mean, na.rm=T)
u350<-array(u350,c(ln,lt))

u450<-mod_ug[,c(24,28,32,36,40)] ## ssp585_2050
u450<-apply(u450,c(1),mean, na.rm=T)
u450<-array(u450,c(ln,lt))

## change future - present
if(fig=='supp'){
  uch245=u245-his
  uch370=u370-his
  uch250=u250-his
  uch350=u350-his
  comb_ug<-abind(uch245,uch370,uch250,uch350,along=3)
} else {
  uch130=u130-his
  uch430=u430-his
  uch150=u150-his
  uch450=u450-his
  comb_ug<-abind(uch130,uch430,uch150,uch450,along=3) 
}

for (f in 1:4){
  comb_ug[23:50,20,f]<-comb_ug[23:50,19,f]
}

## Uganda
df2=comb_ug
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
names(data3)<-name

mps_ug <- getData("GADM", country = "UGA", level = 0, path = path)
atlas <- shapefile('//catalogue/BaseLineDataCluster01/temp/scripts/bean_atlas/AtlasBean_data_copy.shp')
crs(atlas)<-crs(mps_ug)
atlas <- crop(atlas, mps_ug) #%>% mask(mps)


## draw plot
ug_plot <- tm_shape(data3) +
  
  tm_raster(style='fixed', breaks = breaks,legend.show = F, 
            palette = pcols, alpha = 0.75, title = unit) +
  
  tm_layout(main.title='', main.title.size=1.5, panel.show = TRUE,   
            # inner.margins = c(0.01, 0.01, 0.02, 0.05), 
            # outer.margins = c(0.001, 0.001, 0.001, 0.001), 
            panel.labels = name, panel.label.size = 1.2) + 
  
  tm_facets(ncol=1) +
  
  ## border
  tm_shape(mps_ug) + 
  tm_borders(lwd=0.1, col='black', alpha=0.5) +
  
  ## bean area from atlas
  tm_shape(atlas) +
  tm_borders(lwd=0.5, col='blue') 

#tm_xlab(" ") +  tm_ylab("Latitude", size=1.5)


plot <- tmap_arrange(ug_plot, tz_plot, ethi_plot, ncol=3,heights=c(0.2,0.2,0.2,0.2,0.2),outer.margins=NULL)

if(fig=='supp'){
  tmap_save(plot, filename = 'C:/Users/pjha/Desktop/yld_ch_all_cntry_245_370.png', width= 8, height = 10, units = "in", dpi = 300)
}else{
  tmap_save(plot, filename = 'C:/Users/pjha/Desktop/yld_ch_all_cntry_126_585.png', width= 8, height = 10, units = "in", dpi = 300)
}

##Separate legend plot
legend.map <- tm_shape(data3) +
  tm_raster(style = 'fixed',breaks = breaks,legend.is.portrait = F,
            palette = pcols, alpha = 0.75, title = unit) +
  tm_layout(legend.only = TRUE, legend.outside = T, legend.outside.position = 'bottom',
            legend.bg.color = "white", legend.bg.alpha = 1)

tmap_save(legend.map, filename = 'C:/Users/pjha/Desktop/legd_yld_ch.png',units = "in",dpi = 300)

