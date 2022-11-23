rm(list=ls())
gc()
#options(DSSAT.CSM="/bin/dscsm047")
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
cntry<-'uganda'
indir <- '//catalogue/BaseLineDataCluster01/temp/data_for_dssat_eaf/dssat_input/Uganda/obs'
#outdir <- 'E:/Prakash/dssat_outputs/uganda'
outdir <- 'E:/Prakash/dssat_out_culti/uganda'
#outdir <- '/catalogue/BaseLineData_cluster01/temp'
scripts<- 'E:/Prakash'

# load helper functions
basefuns <- list.files(file.path(scripts, "funs_ugan"), pattern = ".R$", full.names = TRUE)
sapply(basefuns, source)

## cultivars and ecotypes
## Run 1
# cul1='SUG_73'; eco1='IB2022'
# cul2='UYOLE_94'; eco2='IB2014'

## Run 2
cul1='SER_119'; eco1='IB2015'
cul2='SELIAN_97'; eco2='IB2016'

## Run 3
 #cul1='SCR_26'; eco1='IB2017'
 #cul2='MCM_1015'; eco2='IB2018'
# 
# ## Run 4
 # cul1='KAT_B9'; eco1='IB2019'
 # cul2='HTA_4'; eco2='IB2020'
# 
# ## Run 5
# cul1='DAB_489'; eco1='IB2021'
# cul2='Cal_96'; eco2='IB2023'
 
# Lat and lon for each model
dfile <- readRDS(paste0(indir,'/agMeChrips_ugan_1991_2010.RDS'))
dfile <-dfile[,-7]

## run only for specific pts
## done points
# dd1 <- list.dirs("E:/Prakash/dssat_out_culti/uganda/obs_cul3", full.names = F)
# dd1 <- dd1[2:length(dd1)]
# dd1 <- as.numeric(dd1)
## all points 

ee1 <- sort(readRDS('E:/Prakash/dssat_out_culti/uganda/grids_mt_elegen.RDS'))
ee2 <- sort(readRDS('E:/Prakash/dssat_out_culti/uganda/grids_western_highlands.RDS'))
pts <- c(ee1,ee2)
#pts <- setdiff(dd,dd1)

plan(multisession, workers = 5)
future.apply::future_lapply(pts, runDSSATSingle_obs_cultivar, dfile,outdir,future.seed=TRUE)
