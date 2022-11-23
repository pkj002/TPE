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

## Points with different types of stresses
#gf <- 10223:10305; sf <- 16386:16454;  tm <- 36583:36597; ts <- 50505:50525 ## set of points
#gf<-  10271:10285; sf <- 16401:16415;  tm <- c(36583:36597); ts<- c(50505:50514, 50519:50523) ## simulated pts

# gf1<-c(6061:6075,16941:16955,26920:26934) ## distributing gf pts. 

## random pts
gf<-c(4962,7627,11800,14072,17635,21822,23752,26515,29377,32843,35080,38404,41286,44128,48764) ## random points
sf<-c(13088,15144,18228,20191,23551,25458,28880,30655,33222,35283,38105,39200,43681,45601,49381)
tm<-c(11316,23077,26430,28607,30768,32155,33497,35473,37024,39015,40771,42563,44306,47846,50932)
ts<-c(7794,25305,26839,27751,28621,29504,30394,31741,40545,42308,44773,46080,47640,49417,50963)
pts <- c(gf,sf,tm,ts)

plan(multisession, workers = 10)
#future.apply::future_lapply(pts, runDSSATSingle_obs_bd, dfile, outdir)
future.apply::future_lapply(pts, runDSSATSingle_obs_bd_2lay, dfile, outdir)


