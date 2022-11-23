rm(list=ls())
library(raster)
library(abind)
library(dplyr)
library(tmap) 
library(classInt)
library(sf)

pacman::p_load(sp, raster, rgdal, rgeos, gtools, tidyverse)

dir <-'//catalogue/BaseLineDataCluster01/temp/climate_change/'
#dir <- "C:/Users/anibi/Documents/test/beans/"
level<-c('126', '245', '370', '585')

#fig='main'
fig='supp'

toRaster <- function(fname, prfiles, xmn, xmx, ymn, ymx, ...){
  dt <- readRDS(fname)
  prfls<-readRDS(prfiles)
  if (fig=='supp'){
    pr <- prfls[,,2:3,]
    tmx <- dt[,,2,2:3,] 
    tmin <- dt[,,3,2:3,] 
    srd <- dt[,,4,2:3,] ## only select ssp 126 and 580
  } else {
    pr <- prfls[,,c(1,4),]
    tmx <- dt[,,2,c(1,4),] 
    tmin <- dt[,,3,c(1,4),] 
    srd <- dt[,,4,c(1,4),] ## only select ssp 126 and 580
  }
  
  
  tmx1 <- abind(tmx[,,,1],tmx[,,,2],along=3) ## 0 to 3.6 degC
  tmin1 <- abind(tmin[,,,1],tmin[,,,2],along=3) ## 0 to 4.5 degC
  pr<-abind(pr[,,,1],pr[,,,2],along=3)
  all<-list(pr,tmx1,tmin1)
  ## convert to raster temp
  r1<-list()
  for (i in 1:3){
    df2 <- all[[i]]
    for (n in 1:dim(df2)[3]){
      df2[, ,n]=df2[,order(ncol(df2[,,n]):1),n]
    }
    
    r1[[i]] <- brick(df2, xmn, xmx, ymn, ymx, crs=NA, transpose=TRUE)
  }
  
  return(r1)
}

fnames <- list.files(dir, "_change_in_weath.RDS", full.names = TRUE)
prfiles <- list.files(dir, "_fut_pres_prec_multi_mod_mean.RDS", full.names = TRUE)

r1 <- toRaster(fnames[1], prfiles[1], xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025)
r2 <- toRaster(fnames[2], prfiles[2], xmn=29.125, xmx=40.125, ymn=-11.975, ymx=-0.975)
r3 <- toRaster(fnames[3], prfiles[3], xmn=29.125, xmx=35.025, ymn=-1.525, ymx=4.475)

p1<-r1[[1]]; tx1<-r1[[2]]; tmn1<- r1[[3]]
p2<-r2[[1]]; tx2<-r2[[2]]; tmn2<- r2[[3]]
p3<-r3[[1]]; tx3<-r3[[2]]; tmn3<- r3[[3]]

## boundary map of three countries
v1 <- lapply(c("ETH", "TZA", "UGA", 'COD','KEN','SOM','SSD'), function(iso)getData("GADM", country = iso, level = 0, path = dir))
v1<-bind(v1)
ref1<-raster(res = c(0.05, 0.05))
ref1<-crop(ref1,v1)

v <- lapply(c("ETH", "TZA", "UGA"), function(iso)getData("GADM", country = iso, level = 0, path = dir))
v <- bind(v)
ref <- raster(res = c(0.05, 0.05))
ref <- crop(ref, v)
###
## Combine precip bricks
pr<-lapply(list(p1,p2,p3), resample, ref)
pr$fun<-mean
pr$na.rm<-TRUE
pr_comb<-do.call(mosaic,pr)

## Combine tmx
tx <- lapply(list(tx1, tx2, tx3), resample, ref)
tx$fun <- mean
tx$na.rm <- TRUE
tx_comb <- do.call(mosaic, tx)

## combine tmin
tmn <- lapply(list(tmn1, tmn2, tmn3), resample, ref)
tmn$fun <- mean
tmn$na.rm <- TRUE
tmn_comb <- do.call(mosaic, tmn)

## name of panel label
# var<-c('Precip','Tmax','Tmin')
# name<-list(); name1<-list()
# for (i in 1:3){
#   if(fig=='supp'){
#     name[[i]]<-c(paste0(var[i]," ", '- SSP2-4.5 (2030)'), paste0(var[i]," ", '- SSP3-7.0 (2030)'), 
#                  paste0(var[i]," ", '- SSP2-4.5 (2050)'), paste0(var[i]," ", '- SSP3-7.0 (2050)'))
#   } else {
#     name[[i]]<-c(paste0(var[i]," ", '- SSP1-2.6 (2030)'), paste0(var[i]," ", '- SSP5-8.5 (2030)'), 
#                  paste0(var[i]," ", '- SSP1-2.6 (2050)'), paste0(var[i]," ", '- SSP5-8.5 (2050)'))
#   }
# }

if(fig=='supp'){
  name<-rep(c('SSP2-4.5', 'SSP3-7.0'),2) 
} else {
  name<-rep(c(paste0('SSP1','-','2.6'), paste0('SSP5','-','8.5')),2)
}

## mask precip bricks with maps of 3 countries
crs(pr_comb)<-crs(v)
data1 <- crop(pr_comb,v) %>% mask(v)
#name_pr <- paste0('Precip_ssp_',level[c(1,4)],'_',rep(c(2030,2050),each=2))
#name_pr <- name[[1]]
names(data1)<-name

## mask tmx bricks with maps of 3 countries
crs(tx_comb)<-crs(v)
data2<-crop(tx_comb,v) %>% mask(v)
#name_tx <- paste0('Tmax_ssp_',level[c(1,4)],'_',rep(c(2030,2050),each=2))
#name_tx <- name[[2]]
names(data2)<-name

## mask tmin bricks with maps of 3 countries
crs(tmn_comb)<-crs(v)
data3<-crop(tmn_comb,v) %>% mask(v)
#name_tmn <- paste0('Tmin_ssp_',level[c(1,4)],'_',rep(c(2030,2050),each=2))
#name_tmn <- name[[3]]
names(data3)<-name

atlas <- shapefile('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/bean_atlas/AtlasBean_data_copy.shp')

atlas <- gBuffer(atlas, byid=TRUE, width=0)
atlas <- spTransform(atlas, CRS( "+proj=longlat +datum=WGS84 +no_defs")) 
atlas <- raster::crop(atlas, v) #%>% mask(mps)

### Set the extent of the map to minimize white spaces from the panel
bbox_new <- st_bbox(v1) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin 

bbox_new[1] <- bbox_new[1] + (0.42 * xrange) # xmin - left
bbox_new[2] <- bbox_new[2] + (0.059 * yrange) # ymin - bottom
bbox_new[3] <- bbox_new[3] - (0.21 * xrange) # xmax - right

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # 

#### 

## Plot Precip
pcols<-c('#fd8d3c','#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c')
breaks=c(-15,0,10,20,30,40,100)
unit='%'

plt_pr <- tm_shape(data1, bbox = bbox_new) +
  
  tm_raster(style='fixed', breaks = breaks,legend.show = F,
            palette = pcols, alpha = 0.75, title = unit) +
  
  tm_layout(main.title='', main.title.size=1.5, panel.show = T,   
            inner.margins = c(0.01, 0.01, 0.02, 0.05),
            outer.margins = c(0.001, 0.001, 0.001, 0.001),
            panel.labels = name, panel.label.size = 1.2,
            legend.outside = T,  legend.outside.position = 'bottom',
            legend.bg.color = "white", legend.bg.alpha = 1) + 
  
  tm_facets(ncol=2) +
  
  ## border
  tm_shape(v1) + 
  #tm_borders(lwd=0.1, col=gray(0.2), alpha=0.5) +
  tm_borders(lwd=0.1, col='black', alpha=0.5) +
  
  ## bean area from atlas
  tm_shape(atlas) +
  tm_borders(lwd=0.6, col='blue')

### seperate legend
legend.pr <- tm_shape(data1) +
  tm_raster(style = 'fixed',breaks = breaks,legend.is.portrait = F,
            palette = pcols, alpha = 0.75, title = unit) +
  tm_layout(legend.only = TRUE, legend.outside = T, legend.outside.position = 'bottom',
            legend.bg.color = "white", legend.bg.alpha = 1)

## Plot Tmx
## draw plot
#pcols<-c('#fff7ec','#fef0d9','#fdd49e','#fdbb84','#fc4e2a','#800026')
pcols<-c('#e5f5e0','#fff7bc','#fdd49e','#fdbb84','#fc4e2a','#800026')
breaks=c(0.3,seq(1,3,by=0.5),Inf)
unit='DegC'

plt_tmx <- tm_shape(data2, bbox = bbox_new) +
  
  tm_raster(style='fixed', breaks = breaks,legend.show = F,
            palette = pcols, alpha = 0.75, title = unit) +
  
  tm_layout(main.title='', main.title.size=1.5, panel.show = T,   
            inner.margins = c(0.01, 0.01, 0.02, 0.05),
            outer.margins = c(0.001, 0.001, 0.001, 0.001),
            panel.labels = name, panel.label.size = 1.2,
            legend.outside = T,  legend.outside.position = 'bottom',
            legend.bg.color = "white", legend.bg.alpha = 1) + 
  
  tm_facets(ncol=2) +
  
  ## border
  tm_shape(v1) + 
  #tm_borders(lwd=0.1, col=gray(0.2), alpha=0.5) +
  tm_borders(lwd=0.1, col='black', alpha=0.5) +
  
  ## bean area from atlas
  tm_shape(atlas) +
  tm_borders(lwd=0.6, col='blue') 

### seperate legend
legend.tmp <- tm_shape(data2) +
  tm_raster(style = 'fixed',breaks = breaks,legend.is.portrait = F,
            palette = pcols, alpha = 0.75, title = unit) +
  tm_layout(legend.only = TRUE, legend.outside = T, legend.outside.position = 'bottom',
            legend.bg.color = "white", legend.bg.alpha = 1)

###############################
## Plot tmin
plt_tmin <- tm_shape(data3, bbox = bbox_new) +
  
  tm_raster(style='fixed', breaks = breaks,legend.show = F,
            palette = pcols, alpha = 0.75, title = unit) +
  
  tm_layout(main.title='', main.title.size=1.5, panel.show = T,   
            inner.margins = c(0.01, 0.01, 0.02, 0.05),
            outer.margins = c(0.001, 0.001, 0.001, 0.001),
            panel.labels = name, panel.label.size = 1.2,
            legend.outside = T,  legend.outside.position = 'bottom',
            legend.bg.color = "white", legend.bg.alpha = 1) + 
  
  tm_facets(ncol=2) +
  
  ## border
  tm_shape(v1) + 
  #tm_borders(lwd=0.1, col=gray(0.2), alpha=0.5) +
  tm_borders(lwd=0.1, col='black', alpha=0.5) +
  
  ## bean area from atlas
  tm_shape(atlas) +
  tm_borders(lwd=0.6, col='blue') 

## save main plot
plot <- tmap_arrange(plt_pr, plt_tmx, plt_tmin, ncol=3, asp=NULL)

if(fig=='supp'){
  tmap_save(plot, filename = 'C:/Users/pjha/Desktop/cc_comb_245_370.png', width= 10, height = 10, units = "in",dpi = 300)
} else {
  tmap_save(plot, filename = 'C:/Users/pjha/Desktop/cc_comb_126_585.png', width= 10, height = 10, units = "in",dpi = 300)
}


# save legend
if(fig=='supp'){
  tmap_save(legend.pr, filename = 'C:/Users/pjha/Desktop/legd_pr_cc_comb_245_370.png',units = "in",dpi = 300)
  tmap_save(legend.tmp, filename = 'C:/Users/pjha/Desktop/legd_temp_cc_comb_245_370.png',units = "in",dpi = 300)
} else {
  tmap_save(legend.pr, filename = 'C:/Users/pjha/Desktop/legd_pr_cc_comb_126_585.png',units = "in",dpi = 300)
  tmap_save(legend.tmp, filename = 'C:/Users/pjha/Desktop/legd_temp_cc_comb_126_585.png',units = "in",dpi = 300)
}

#tmap_save(plot, filename = 'C:/Users/pjha/Desktop/tr_cc_al_126_585.png', width= 10, height = 10, units = "in", dpi = 300)
#tmap_save(plot, filename = 'C:/Users/pjha/Desktop/cc_al_126_585.png', width= 12, height = 8, units = "in", dpi = 300)

