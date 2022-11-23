rm(list=ls())
gc()
require(raster);require(ggplot2);require(dplyr);require(maptools)
require(rgdal);require(cluster);require(fastcluster); library(factoextra); require(purrr)
library(parallel)

cntry<-'ethiopia'
## Load data base yield
#path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ethiopia/tpe/PlantGro/final/'
path<-'/dapadfs/BaseLineData_cluster01/temp/dssat_outputs/ethiopia/tpe/PlantGro/tmin_plantgro/'
mod <- c('BCC-CSM2-MR','EC-Earth3-Veg','GFDL-ESM4','IPSL-CM6A-LR','MRI-ESM2-0')

# dimension of domain
comb <- data.frame(year = c(rep(2030,20),rep(2050,20)), mod = c(rep(mod,each=4),rep(mod,each=4)), sc = rep(c(126,245,370,585),10),
                   period=c(rep('202101-204012',20),rep('204101-206012',20)), co2=c(rep(c(440,446,454,456),5),rep(c(469,508,544,569),5)),
                   stringsAsFactors = FALSE)

# load helper functions
#source('//dapadfs/workspace_cluster_12/AVISA/dssat_outputs/ethiopia/others/funs_cluster/ethi_clust4.R')
source('/dapadfs/BaseLineData_cluster01/temp/dssat_outputs/ethiopia/others/funs_cluster/ethi_tmin_clust4.R')

parallel::mclapply(21:40,ethi_tmin_clust4, comb, path, mc.preschedule = FALSE, mc.cores = 20)
                   

#plan(multisession, workers = 20)
#future.apply::future_lapply(21:40, ethi_clust4, comb, path, future.seed = TRUE)
