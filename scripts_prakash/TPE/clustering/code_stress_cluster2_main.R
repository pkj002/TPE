rm(list=ls())
gc()
require(raster);require(ggplot2);require(dplyr);require(maptools)
require(rgdal);require(cluster);require(fastcluster); library(factoextra); library(purrr)
#library(parallel)
library(future.apply)

## Load data base yield
#path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ugand/tpe/PlantGro/'
path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ugand/tpe/PlantGro/tmin_plantgro/'

mod <- c('BCC-CSM2-MR','EC-Earth3-Veg','GFDL-ESM4','IPSL-CM6A-LR','MRI-ESM2-0')

# dimension of domain
comb <- data.frame(year = c(rep(2030,20),rep(2050,20)), mod = c(rep(mod,each=4),rep(mod,each=4)), sc = rep(c(126,245,370,585),10),
                   period=c(rep('202101-204012',20),rep('204101-206012',20)), co2=c(rep(c(440,446,454,456),5),rep(c(469,508,544,569),5)),
                   stringsAsFactors = FALSE)

# load helper functions
source('//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ugand/others/code_stress_cluster2.R')
#source('/dapadfs/BaseLineData_cluster01/temp/dssat_outputs/ugand/others/code_stress_cluster2.R')

## For Windows
plan(multisession, workers = 20)
future.apply::future_lapply(21:40,ug_clust2, comb, path, future.seed = TRUE)

# parallel::mclapply(1:20,ug_clust2, comb, path, 
#                    mc.preschedule = FALSE, mc.cores = 20)
