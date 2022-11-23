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
outdir <- 'E:/Prakash/dssat_out_culti/ethiopia'
#outdir <- '/catalogue/BaseLineData_cluster01/temp'
scripts<- 'E:/Prakash'

# load helper functions
basefuns <- list.files(file.path(scripts, "funs_ethi"), pattern = ".R$", full.names = TRUE)
sapply(basefuns, source)

dfile<-readRDS(paste0(indir, 'new_agMeChrips_ethi_obs_1991_2010.RDS'))

## done points
dd1 <- list.dirs("E:/Prakash/dssat_out_culti/ethiopia/obs_cul", full.names = F)
dd1 <- dd1[2:length(dd1)]
dd1 <- as.numeric(dd1)
## all points 
dd <- readRDS('E:/Prakash/dssat_out_culti/ethi_all_pts.RDS')
pts <- setdiff(dd,dd1)

plan(multisession, workers = 10)
#future.apply::future_lapply(1:nrow(dfile), runDSSATSingle_obs_cul, dfile, outdir) ## all grids even where no planting
#future.apply::future_lapply(pts, runDSSATSingle_obs_cul, dfile, outdir) ## only where planting occurs
future.apply::future_lapply(pts, runDSSATSingle_obs_cul, dfile, outdir) ## for remg cultivars



