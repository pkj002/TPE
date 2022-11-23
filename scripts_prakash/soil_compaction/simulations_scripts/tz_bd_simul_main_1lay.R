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

# specify directories and load 
cntry<-'tanzania'
indir <- '//catalogue/BaseLineDataCluster01/temp/data_for_dssat_eaf/dssat_input/Tanzania/obs/'
outdir <- 'E:/Prakash/dssat_outputs/tanzania'
scripts<- 'E:/Prakash'

# load helper functions
basefuns <- list.files(file.path(scripts, "funs_mod"), pattern = ".R$", full.names = TRUE)
sapply(basefuns, source)

dfile<-readRDS(paste0(indir, 'obs_raster_sowdt_agMeChrips_tanz_1991_2010.RDS'))

## load points for simulation
## done points
dd1 <- list.dirs(paste0(outdir,"/bd_random_1lay"), full.names = F, recursive = F)
dd1 <- as.numeric(dd1)

## all points (0.5 < bd < 1.35)
dd <- readRDS('E:/Prakash/dssat_outputs/tanzania/tz_pts_0.5_1.35.RDS')
dd <- dd$X1.len
pts <- setdiff(dd,dd1)
pts <- sort(pts)

plan(multisession, workers = 10)
#future.apply::future_lapply(pts, runDSSATSingle_obs_bd, dfile, outdir)
future.apply::future_lapply(pts, runDSSATSingle_obs_bd_1layer, dfile, outdir)

