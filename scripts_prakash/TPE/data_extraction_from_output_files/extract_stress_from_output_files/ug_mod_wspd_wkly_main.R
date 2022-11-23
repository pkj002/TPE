rm(list=ls())
gc()
library(lubridate)
library(dplyr)
library(future.apply)
## WSPD = water stress -photosynthesis; WSGD: same water stress for expansion, partitioning and development
path<-'//catalogue/BaseLineDataCluster01/temp/dssat_outputs/ugand/'

cntry<-'uganda'
mod <- c('BCC-CSM2-MR','EC-Earth3-Veg','GFDL-ESM4','IPSL-CM6A-LR','MRI-ESM2-0')
level<-c('126', '245', '370', '585')

# dimension of domain
lon<-seq(from=29.525,to=35.025,by=0.05)
lat<-seq(from=-1.525, to=4.475,by=0.05)

len <- length(lon)*length(lat)

# dimension of domain
comb <- data.frame(year = c(rep(2030,20),rep(2050,20)), mod = c(rep(mod,each=4),rep(mod,each=4)), sc = rep(c(126,245,370,585),10),
                   period=c(rep('202101-204012',20),rep('204101-206012',20)), co2=c(rep(c(440,446,454,456),5),rep(c(469,508,544,569),5)),
                   stringsAsFactors = FALSE)

# load helper functions
basefuns<- list.files(paste0(path,'others/funs_wspd_wkly'), pattern = ".R$", full.names = TRUE)
sapply(basefuns, source)
# 
# ## In Windows
plan(multisession, workers = 20)
future.apply::future_lapply(21:40,wspd_chunk_ug, comb,path,future.seed = TRUE)
