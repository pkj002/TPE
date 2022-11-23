runDSSATSingle_obs <- function(n,dfile,outdir,indir){
  cat("processing row ", n, "\n")
  # read one file
  # this removes a lot of the files
  x <- dfile[n,] 
    
	## discard rows where no planting dates 
  if(is.na(x$jday_init_avg[1]) & is.na(x$jday_end_avg[1]) & is.na(x$jday_init_Belg[1])){
  x<-NA
  } 
  
  # make this separate function
  if (nrow(x)>0){
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
    out_dir <- file.path(outdir, paste0('run/', xs$Code)) 
    dir.create(out_dir,showWarnings=FALSE,recursive=TRUE)
    setwd(out_dir)
    
    ## 1. weather
    # no need for out_dir as we have already in the outdir
    make_wth(data = xs$climate[[1]], out_dir, name_xfile_climate = 'UGAND001', lat = xs$lat, lon = xs$lon, co2 = -99)
    
    ## 2. Soil
    write_soil(xs$SOIL.SOL[[1]], out_dir)
    # copy files  
    file.copy(file.path(scripts, "original_files_ethi/BNGRO047.CUL"), out_dir)
    file.copy(file.path(scripts, "original_files_ethi/BNGRO047.SPE"), out_dir)
    file.copy(file.path(scripts, "original_files_ethi/BNGRO047.ECO"), out_dir)
    file.copy(file.path(scripts, "original_files_ethi/CCPA9105.BNX"), out_dir)
    
    ## change the planting date according to the given value of the grid.
        
    # Sowing date
    pl     <-  xs$jday_init_avg 
	pl_end <-  xs$jday_end_avg
	pl_2nd <-  xs$jday_init_Belg

	if (is.na(pl_2nd) == TRUE){
     sow_date=seq(pl,pl_end,7)
	} else {
    sow_date=c(unlist(seq(pl,pl_end,7)),
             unlist(seq(60,69,7)))
	}
    
	lth<-length(sow_date)
	sow_date<-as.POSIXct(as.Date(sow_date,origin=xs$climate[[1]][1,1]))

    ## Adapt experimental file
	 file_x <- read_filex(file.path(out_dir,'CCPA9105.BNX'))
	 
	  ## Modify number of treatments according to number of sowing dates
	 file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`<-
	 file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`[1:lth,]
		 
	 ## Modify number of rows in planting date
	 file_x$`PLANTING DETAILS` <- file_x$`PLANTING DETAILS`[1:lth,]
	     
	 ## change sowing date
	file_x$`PLANTING DETAILS`[,2][1:lth,]<-sow_date

    ## change soil code in experimental file
    all_Prfl <- read_sol(file.path(out_dir,'SOIL.SOL'))
	    
    # note
    file_x$FIELDS[1,12] <- all_Prfl[1,1]
    file_x$FIELDS[1,3] <- 'UGAND001'
    
    ## change cultivar
    file_x$`CULTIVARS`[1,3] <- 'IB2013'
    file_x$`CULTIVARS`[1,4] <- 'Calima'
	
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
    
    ## write experimental file
    write_filex(file_x, 'CCPA9105.BNX')
  
  ## Run
    CSMbatch(crop="BEAN", name = "CCPA9105.BNX", xs, filename = file.path(out_dir,"DSSBatch.v47"), lth=lth)
  # note
    dir_dssat <- 'C:/DSSAT47'
	files_dssat1(dir_dssat, out_dir)
    system(paste0("./dscsm047 B DSSBatch.v47"), ignore.stdout = TRUE)
  
   ## make .RDS file from output files
    ## 1. for data frame type of files
    # prepare output dir
      rds_dir <- file.path(outdir,paste0('obs_new/', xs$Code))
	
    dir.create(rds_dir, FALSE, TRUE)
          
    ## Read
    ev     <- read.csv('evaluate.csv')
    ev<-round(ev[,c(1,4,7:56)],3)
	saveRDS(ev, file.path(rds_dir, paste0('Evaluate_',cntry,'_obs.RDS')))   
      
    pgr<-read.csv('plantgro.csv')
    pgr<-pgr[ ,c(1,3,6:51)]
    pgr<-round(pgr,3)
	saveRDS(pgr, file.path(rds_dir, paste0('PlantGro_',cntry,'_obs.RDS')))
   
    we<- read.csv('weather.csv')
	we<-we[,c(1,3,6,7,9,12,15,16)] 
	we<-rapply(object = we, f = round, classes = "numeric", how = "replace", digits = 3)
    saveRDS(we, file.path(rds_dir, paste0('Weather_',cntry,'_obs.RDS')))     
	
    unlink(out_dir, recursive=TRUE)
       rm(list=ls())
      gc() 
    }
	}
   

