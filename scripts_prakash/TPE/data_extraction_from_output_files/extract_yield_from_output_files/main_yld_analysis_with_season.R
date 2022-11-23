rm(list=ls())
library(dplyr)
library(purrr)
library(tidyr)
library(future.apply)

path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ethiopia/tpe/'
# dimension of domain
lon<-seq(from=33.025,to=44.025,by=0.05)
lat<-seq(from=3.025, to=15.025,by=0.05)

len <- length(lon)*length(lat)

cntry<-'ethiopia'

# GCM Models
mod <- c('BCC-CSM2-MR','EC-Earth3-Veg','GFDL-ESM4','IPSL-CM6A-LR','MRI-ESM2-0')

# dimension of domain
comb <- data.frame(year = c(rep(2030,20),rep(2050,20)), mod = c(rep(mod,each=4),rep(mod,each=4)), sc = rep(c(126,245,370,585),10),
                   period=c(rep('202101-204012',20),rep('204101-206012',20)), co2=c(rep(c(440,446,454,456),5),rep(c(469,508,544,569),5)),
                   stringsAsFactors = FALSE)

source('//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ethiopia/others/fun_yld_analysis_with_season.R')

plan(multisession, workers = 33)
future.apply::future_lapply(c(1,9:40), yld_ana_with_season, comb, path, future.seed = TRUE)