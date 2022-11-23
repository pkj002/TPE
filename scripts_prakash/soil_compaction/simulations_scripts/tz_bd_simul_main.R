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
pts1<-c(28320,29203,30070,31830,32520,35605,37607,39170,40700,41576,42259,43800,45342,46249,47336) ## SF
pts2<-c(28684,29795,30254,30693,31133,31575,31806,32248,32687,33126,34006,35751,36860,38403,41693)  ## AS
pts3<- c(4576,7454,10283,13359,16611,19050,21150,24819,27444,30659,33118,36395,39068,42783,44990) ## MTS
pts4<-c(3218,6084,7626,11821,16644,18168,19051,19710,19949,20392,21051,21700,22149,22802,31311)  ## STS
pts5<- c(1273,3222,4982,7212,13370,15094,16408,18010,19786,21562,29987,34852,35736,38595,39950)  ## ETS
pts<-c(pts1,pts2,pts3,pts4,pts5)

plan(multisession, workers = 15)
future.apply::future_lapply(pts, runDSSATSingle_obs_bd, dfile, outdir)


