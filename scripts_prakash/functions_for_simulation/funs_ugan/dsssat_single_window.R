runDSSATSingle_window <- function(n,dfile,outdir,comb,i,indir){
  cat("processing row ", n, "\n")
  # read one file
  # this removes a lot of the files
  x <- dfile[n,] #%>% drop_na()
  
  # make this separate function
  if(nrow(x)>0){
    x <- x %>%
      mutate(Verify = 1:nrow(x) %>%
               map(.f = function(i){
                 x$climate[[1]] %>%
                   complete.cases() %>% sum()}) %>%
               unlist())
    
    xs <- x %>%
      filter(Verify !=0)
  } else {
    return(cat("all data NA for", n, "\n"))
  }
  
  if(nrow(xs)>0){
    # where to save results 
    out_dir <- file.path(outdir, paste0('model_runs/',comb$mod[i],'_ssp_',comb$sc[i],'_',comb$year[i]), xs$Code) 
    dir.create(out_dir,showWarnings=FALSE,recursive=TRUE)
    setwd(out_dir)
    
    ## 1. weather
    # no need for out_dir as we have already in the outdir
    make_wth(data = xs$climate[[1]], out_dir, name_xfile_climate = 'UGAND001', lat = xs$lat, lon = xs$lon, co2 = comb$co2[i])
    
    ## 2. Soil
    write_soil(xs$SOIL.SOL[[1]], out_dir)
    # copy files  
    file.copy(file.path(scripts, "original_files_ug/BNGRO047.CUL"), out_dir)
    file.copy(file.path(scripts, "original_files_ug/BNGRO047.SPE"), out_dir)
    file.copy(file.path(scripts, "original_files_ug/BNGRO047.ECO"), out_dir)
    file.copy(file.path(scripts, "original_files_ug/CCPA9105.BNX"), out_dir)
    
    ## change soil code in experimental file
    all_Prfl<-read_sol(paste0(out_dir,'/SOIL.SOL'))
    file_x<-read_filex(paste0(out_dir,'/CCPA9105.BNX'))
    file_x$FIELDS[1,12]=all_Prfl[1,1]
    file_x$FIELDS[1,3]='UGAND001'
    
    ## change cultivar
    file_x$`CULTIVARS`[1,3] <- 'IB2013'
    file_x$`CULTIVARS`[1,4] <- 'Calima'
    
    ## Constant planting date supplied (Starting from Feb 10 until Oct 8 given by NARS with an interval of 15 days.
    ## since DSSAT takes pdate as.POIXct, i.e. in yyyy-mm-dd. convert doy to that.
    ## the origin is from weather file. -1 is needed coz the first day needs to be counted.
    pl<-seq(41, 272, by = 7)
    file_x$`PLANTING DETAILS`[ ,2]=as.POSIXct(as.Date(pl,origin=x$climate[[1]][1,1]))
    
    ## plant density
    file_x$`PLANTING DETAILS`[1,4]=15
    file_x$`PLANTING DETAILS`[1,5]=15
    
    ## simulation start date
    file_x$`SIMULATION CONTROLS`[1,6] <- as.POSIXct(xs$climate[[1]][1,1])
    
    ## number of years to simulate
    dimn <- dim(xs$climate[[1]])
    
    st <- xs$climate[[1]][1,2] 
    ed <- xs$climate[[1]][dimn[1],2]
    
    yr <- (ed-st)+1
    
    # note
    file_x$`SIMULATION CONTROLS`[1,3] <- yr
    
    # note
    ## Simulation control: Read CO2 from weather file
    file_x$`SIMULATION CONTROLS`[1,19] <- 'W'
    
    ## write experimental file
    write_filex(file_x, 'CCPA9105.BNX')
    
    ## Run
    CSMbatch(crop="BEAN", name = "CCPA9105.BNX", xs, filename = file.path(out_dir, "DSSBatch.v47"))
    
    #dir_dssat <- '//dapadfs/workspace_cluster_12/AVISA/dssat_test1/uganda_input/dssat-csm-original/Data/'
    dir_dssat <- 'C:/DSSAT47'
    
    files_dssat1(dir_dssat, out_dir)    
    system(paste0("./dscsm047 B DSSBatch.v47"), ignore.stdout = TRUE)
    # rm(file_x)
    
    ## make .RDS file from output files
    ## 1. for data frame type of files
    # prepare output dir
    rds_dir <- file.path(outdir, paste0('/new_dssat_runs_',cntry,'_',comb$mod[i],'_ssp_',comb$sc[i],'_',comb$year[i]), xs$Code)
    dir.create(rds_dir, FALSE, TRUE)
    
    ## Read
    ev<- read.csv('evaluate.csv')
    ev<-ev[,c(1,4,7:ncol(ev))]
    ev<-rapply(object = ev, f = round, classes = "numeric", how = "replace", digits = 3)
    
    
    pgr<-read.csv('plantgro.csv')
    pgr<-pgr[ ,c(1,3,6:ncol(pgr))]
    pgr <- rapply(object = pgr, f = round, classes = "numeric", how = "replace", digits = 3) 
    
    we<- read.csv('weather.csv')
    we<-rapply(object = we, f = round, classes = "numeric", how = "replace", digits = 3)
    
    
    lis <- list(ev,pgr,we)
    
    ## make characater vector
    listnames <- c('Evaluate','PlantGro','Weather')
    
    ## Save
    for (j in 1:length(lis)){
      ofilename <- file.path(rds_dir, paste0(listnames[j],'_',cntry,'_',comb$mod[i],'_ssp_',comb$sc[i],'_',comb$year[i]))
      saveRDS(lis[[j]], paste0(ofilename, ".RDS")) 
    }
    
    unlink(out_dir, recursive = TRUE)      
    rm(list=ls())
    gc() 
  }
}  
