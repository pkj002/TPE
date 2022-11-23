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
indir <- '//catalogue/BaseLineDataCluster01/temp/data_for_dssat_eaf/dssat_input/Tanzania/obs/'
outdir <- 'E:/Prakash/dssat_out_culti/tanzania'
scripts<- 'E:/Prakash'

# load helper functions
basefuns <- list.files(file.path(scripts, "funs_mod"), pattern = ".R$", full.names = TRUE)
sapply(basefuns, source)

## input
dfile<-readRDS(paste0(indir, 'obs_raster_sowdt_agMeChrips_tanz_1991_2010.RDS'))

## done points
dd1 <- list.dirs("E:/Prakash/dssat_out_culti/tanzania/obs_cul2", full.names = F)
dd1 <- dd1[2:length(dd1)]
dd1 <- as.numeric(dd1)
## all points 
dd <- readRDS('E:/Prakash/dssat_out_culti/tz_all_pts.RDS')
pts <- setdiff(dd,dd1)

## run
plan(multisession, workers = 10)
#future.apply::future_lapply(1:nrow(dfile), runDSSATSingle_obs_cul, dfile, outdir)
future.apply::future_lapply(pts, runDSSATSingle_obs_cul_remg, dfile, outdir)
## This script is for cultivars 1 to 5.
## For remaining cultivars (6-10), modify the name of cultivars in the 
## experimental files CCPA9105.BNX and TANZ9105.BNX using XBuild of DSSAT.
## Also in the function ('dsssat_single_obs_tz_cul_selection.R') while saving output file, change the name to '6to10' instead of the existing '1to5'.













