rm(list=ls())
#options(DSSAT.CSM="/home/jovyan/dssat-csm-os/build/bin")
library(DSSAT)
#library(Dasst)
library(dplyr)
library(anytime)
library(lubridate)
library(stringr)
library(readr)
library(tidyr)
library(purrr)
library(future.apply)
#args <-commandArgs(TRUE)

# specify directories and load 
cntry<-'tanzania'
indir <- '//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/dssat_input/Ethiopia'
outdir <- 'E:/Prakash/dssat_outputs/ethiopia'
scripts<- 'E:/Prakash'

# load helper functions
basefuns <- list.files(file.path(scripts, "funs_ethi"), pattern = ".R$", full.names = TRUE)
sapply(basefuns, source)


# Lat and lon for each model
# GCM Models
mod <- c('BCC-CSM2-MR','EC-Earth3-Veg','GFDL-ESM4','IPSL-CM6A-LR','MRI-ESM2-0')

# dimension of domain
comb <- data.frame(year = c(rep(2030,20),rep(2050,20)), mod = c(rep(mod,each=4),rep(mod,each=4)), sc = rep(c(126,245,370,585),10),
                   period=c(rep('202101-204012',20),rep('204101-206012',20)), co2=c(rep(c(440,446,454,456),5),rep(c(469,508,544,569),5)),
                   stringsAsFactors = FALSE)
 
     
for (i in 1:40){
## 1st chunk
dfile <- readRDS(paste0(indir,'/',comb$year[i],'/n1_',comb$mod[i],'_ssp',comb$sc[i],'_r1i1p1f1_',comb$period[i],'.RDS'))
dfile$jday_end_avg<-ifelse(dfile$jday_end_avg==0,NA,dfile$jday_end_avg)

## 2nd chunk
dfile2<-readRDS(paste0(indir,'/',comb$year[i],'/n2_',comb$mod[i],'_ssp',comb$sc[i],'_r1i1p1f1_',comb$period[i],'.RDS'))
dfile2$jday_end_avg<-ifelse(dfile2$jday_end_avg==0,NA,dfile2$jday_end_avg)
# in parallel

dfile<- bind_rows(dfile,dfile2)
rm(dfile2)

plan(multisession, workers = 30)
future.apply::future_lapply(1:nrow(dfile), runDSSATSingle_window, dfile,outdir,comb,i,indir)
   }
