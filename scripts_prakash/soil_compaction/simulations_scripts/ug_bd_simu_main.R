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
indir <- '//catalogue/BaseLineDataCluster01/temp/data_for_dssat_eaf/dssat_input/Uganda'
outdir <- 'E:/Prakash/dssat_outputs/uganda'
scripts<- 'E:/Prakash'

# load helper functions
basefuns <- list.files(file.path(scripts, "funs_ugan"), pattern = ".R$", full.names = TRUE)
sapply(basefuns, source)

## Load input files
dfile <- readRDS(paste0(indir,'/obs/agMeChrips_ugan_1991_2010.RDS'))
dfile <-dfile[,-7]

## load points for simulation
pts1<- readRDS('E:/Prakash/dssat_outputs/uganda/sample_pts1.RDS')
pts2<- readRDS('E:/Prakash/dssat_outputs/uganda/sample_pts2.RDS')
pts<- c(pts1, pts2)
pts<-sort(pts)

plan(multisession, workers = 15)
future.apply::future_lapply(pts, runDSSATSingle_obs_bd, dfile,outdir)
