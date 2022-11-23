rm(list=ls())
gc()
rm(list=ls())
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
cntry<-'ethiopia'
#indir <- '//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/dssat_input/Ethiopia'
indir <- '//catalogue/BaseLineDataCluster01/temp/data_for_dssat_eaf/dssat_input/Ethiopia/obs/'
outdir <- 'E:/Prakash/dssat_outputs/ethiopia'
#outdir <- '/catalogue/BaseLineData_cluster01/temp'
scripts<- 'E:/Prakash'

# load helper functions
basefuns <- list.files(file.path(scripts, "funs_ethi"), pattern = ".R$", full.names = TRUE)
sapply(basefuns, source)

dfile<-readRDS(paste0(indir, 'new_agMeChrips_ethi_obs_1991_2010.RDS'))

## load points for simulation
## done points
dd1 <- list.dirs(paste0(outdir,"/bd_random_1lay/"), full.names = F, recursive = F)
#dd1 <- dd1[2:length(dd1)]
dd1 <- as.numeric(dd1)

## grid points with bulk density (0.5 < bd < 1.45 g/cm3)
dd <- readRDS('E:/Prakash/dssat_outputs/ethiopia/et_pts_0.5_1.35.RDS')
dd <- dd$X1.len
pts <- setdiff(dd,dd1)
pts <- sort(pts)

plan(multisession, workers = 10)
#future.apply::future_lapply(pts, runDSSATSingle_obs_bd, dfile, outdir)
future.apply::future_lapply(pts, runDSSATSingle_obs_bd_1lay, dfile, outdir)


