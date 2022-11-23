runDSSATSingle_obs <- function(n,dfile,outdir,indir){
  cat("processing row ", n, "\n")
  # read one file
  # this removes a lot of the files
  x <- dfile[n,] #%>% drop_na()
  ## discard rows where no planting dates 
  if (is.na(x$jday_init_avg[1]) & is.na(x$jday_end_avg[1])){
  x<-NA
  }
  
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
    out_dir <- file.path(outdir, paste0('model_runs/', xs$Code)) 
    dir.create(out_dir,showWarnings=FALSE,recursive=TRUE)
    setwd(out_dir)
    
    ## 1. weather
    # no need for out_dir as we have already in the outdir
    make_wth(data = xs$climate[[1]], out_dir, name_xfile_climate = 'UGAND001', lat = xs$lat, lon = xs$lon, co2 = -99)
    
    ## 2. Soil
    write_soil(xs$SOIL.SOL[[1]], out_dir)
    # copy files  
    file.copy(file.path(scripts, "original_files_tz/BNGRO047.CUL"), out_dir)
    file.copy(file.path(scripts, "original_files_tz/BNGRO047.SPE"), out_dir)
    file.copy(file.path(scripts, "original_files_tz/BNGRO047.ECO"), out_dir)
    
    ## change soil code in experimental file
    all_Prfl <- read_sol(file.path(out_dir,'SOIL.SOL'))
    
    # note
    # @what is file_x
    if (is.na(xs$jday_end_avg) == TRUE) {
      
      file.copy(file.path(scripts, "original_files_tz/TANZ9105.BNX"), out_dir)
      file_x <- read_filex(file.path(out_dir,'TANZ9105.BNX'))
      
    } else {
      
      file.copy(file.path(scripts, "original_files_tz/CCPA9105.BNX"), out_dir)
      file_x <- read_filex(file.path(out_dir,'CCPA9105.BNX'))
    }
    
    # note
    file_x$FIELDS[1,12] <- all_Prfl[1,1]
    file_x$FIELDS[1,3] <- 'UGAND001'
    
    ## change cultivar
    file_x$`CULTIVARS`[1,3] <- 'IB2013'
    file_x$`CULTIVARS`[1,4] <- 'Calima'
    
    ## change the planting date according to the given value of the grid.
    ## since DSSAT takes pdate as.POIXct, i.e. in yyyy-mm-dd. convert doy to that.
    ## the origin is from weather file. -1 is needed coz the first day needs to be counted.
    ## If the grid has both first and 2nd season sow date, then 8 dates, else 4 dates.
    ## Date starts from x$jday_init_avg[n] for 1st season and from x$jday_end_avg[n] for 2nd season
    ## Date increase by a week for each 1st and 2nd season for 4 weeks.
    
    pl     <-  xs$jday_init_avg 
    pl_end <-  xs$jday_end_avg
    
    # note
    days <- c(3,10,17,24)
	sow_date_1seas_only <- c(days[1]+pl,days[2]+pl,days[3]+pl,days[4]+pl)
    sow_dates<-c(sow_date_1seas_only,
                days[1]+pl_end,days[2]+pl_end,days[3]+pl_end,days[4]+pl_end)
        
    
    # note
    if (is.na(xs$jday_end_avg) == TRUE){
      
      file_x$`PLANTING DETAILS`[,2][1:4,] <- as.POSIXct(as.Date(sow_date_1seas_only,origin=xs$climate[[1]][1,1]))
      
    } else {
      
      file_x$`PLANTING DETAILS`[,2] <- as.POSIXct(as.Date(sow_dates,origin=xs$climate[[1]][1,1]))
    }
    
    ## plant density
    
    file_x$`PLANTING DETAILS`[1,4:5] <- 15
    
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
    file_x$`SIMULATION CONTROLS`[1,19] <- 'M'
    
    # note
    ## write experimental file
    # note
    ## Run
    if (is.na(xs$jday_end_avg) == TRUE) {
      write_filex(file_x, 'TANZ9105.BNX')
      CSMbatch(crop="BEAN", name = "TANZ9105.BNX", xs, filename = file.path(out_dir,"DSSBatch.v47"))
      
    } else {
      write_filex(file_x, 'CCPA9105.BNX')
      CSMbatch(crop="BEAN", name = "CCPA9105.BNX", xs, filename = file.path(out_dir,"DSSBatch.v47"))
    }
    
    # note
    dir_dssat <- 'C:/DSSAT47'
	files_dssat1(dir_dssat, out_dir)
    
    system(paste0("./dscsm047 B DSSBatch.v47"), ignore.stdout = TRUE)
    # rm(file_x)
    
    ## make .RDS file from output files
    ## 1. for data frame type of files
    # prepare output dir
      rds_dir <- file.path(outdir,paste0('obs_new2/', xs$Code))
                         
    dir.create(rds_dir, FALSE, TRUE)
          
    ## Read
    ## Read
    ev<- read.csv('evaluate.csv')
    ev<-round(ev[,c(1,4,7:ncol(ev))],3)
      
    pgr<-read.csv('plantgro.csv')
    pgr<-pgr[ ,c(1,3,6:ncol(pgr))]
    pgr<-round(pgr,3)
   
    we<- read.csv('weather.csv')
	we<-rapply(object = we, f = round, classes = "numeric", how = "replace", digits = 3)
      
        
  lis <- list(ev,pgr,we)
    
    ## make characater vector
    listnames <- c('Evaluate','PlantGro','Weather')
    
    ## Save
    for (j in 1:length(lis)){
      ofilename <- file.path(rds_dir, paste0(listnames[j],'_',cntry,'_obs'))
      saveRDS(lis[[j]], paste0(ofilename, ".RDS")) 
      }
      unlink(out_dir, recursive = TRUE) 
         rm(list=ls())
         gc() 
  }
}
