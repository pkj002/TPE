rm(list=ls())
library(purrr)
library(future.apply)

path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/tanzania/'
cntry<-'tanzania'

# dimension of domain
lon<-seq(from=29.125,to=40.125,by=0.05)
lat<-seq(from=-11.975, to=-0.975,by=0.05)
len <- length(lon)*length(lat)

# GCM Models
mod <- c('BCC-CSM2-MR','EC-Earth3-Veg','GFDL-ESM4','IPSL-CM6A-LR','MRI-ESM2-0')

# dimension of domain
comb <- data.frame(year = c(rep(2030,20),rep(2050,20)), mod = c(rep(mod,each=4),rep(mod,each=4)), sc = rep(c(126,245,370,585),10),
                   period=c(rep('202101-204012',20),rep('204101-206012',20)), co2=c(rep(c(440,446,454,456),5),rep(c(469,508,544,569),5)),
                   stringsAsFactors = FALSE)

# load helper functions
source('//catalogue/BaseLineDataCluster01/temp/dssat_outputs/tanzania/others/fun_save_yld_tz_season.R')

plan(multisession, workers = 40)
future.apply::future_lapply(1:40, save_mod_yld_tz, comb, path, future.seed = TRUE)
