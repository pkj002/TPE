runDSSATSingle_obs_cul_remg <- function(n,dfile,outdir,indir){
  cat("processing row ", n, "\n")
  #print("processing row ", n, "\n")
  # read one file
  # this removes a lot of the files
  x <- dfile[n,] 
    
	## discard rows where no planting dates 
  #if(is.na(x$jday_init_avg[1]) & is.na(x$jday_end_avg[1]) & is.na(x$jday_init_Belg[1])){
  #x<-NA
  #return(cat("all data NA for", n, "\n"))
  #} 
  
  # make this separate function
  if (!is.na(x)){
    x <- x %>%
      mutate(Verify = 1:nrow(x) %>%
               map(.f = function(i){
                 x$climate[[1]] %>%
                   complete.cases() %>% sum()}) %>%
               unlist())
    
    xs <- x %>%
      filter(Verify !=0)
	  }
  #} else {
   # return(cat("all data NA for", n, "\n"))
  #}
  
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
    file.copy(file.path(scripts, "original_files_ethi_cultivars/BNGRO047.CUL"), out_dir)
    file.copy(file.path(scripts, "original_files_ethi_cultivars/BNGRO047.SPE"), out_dir)
    file.copy(file.path(scripts, "original_files_ethi_cultivars/BNGRO047.ECO"), out_dir)
    file.copy(file.path(scripts, "original_files_ethi_cultivars/remg_cul/CCPA9105.BNX"), out_dir)
    
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
	 hh <- file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`
	 hh <- rbind(hh[hh$CU==1,][1:lth,],hh[hh$CU==2,][1:lth,],hh[hh$CU==3,][1:lth,],hh[hh$CU==4,][1:lth,],hh[hh$CU==5,][1:lth,]) 
	 hh$N <- 1:nrow(hh)
	 file_x$`TREATMENTS                        -------------FACTOR LEVELS------------`<- hh
	 rm(hh)
		 
	 ## Modify number of rows in planting date
	 file_x$`PLANTING DETAILS` <- file_x$`PLANTING DETAILS`[1:lth,]
	     
	 ## change sowing date
	file_x$`PLANTING DETAILS`[,2][1:lth,]<-sow_date

    ## change soil code in experimental file
    all_Prfl <- read_sol(file.path(out_dir,'SOIL.SOL'))
	    
    # note
    file_x$FIELDS[1,12] <- all_Prfl[1,1]
    file_x$FIELDS[1,3] <- 'UGAND001'
    
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
    CSMbatch45(crop="BEAN", name = "CCPA9105.BNX", xs, filename = file.path(out_dir,"DSSBatch.v47"), lth=lth)
  # note
    dir_dssat <- 'C:/DSSAT47'
	files_dssat1(dir_dssat, out_dir)
    system(paste0("./dscsm047 B DSSBatch.v47"), ignore.stdout = TRUE)
  
   ## make .RDS file from output files
    ## 1. for data frame type of files
    # prepare output dir
      rds_dir <- file.path(outdir,paste0('obs_cul2/', xs$Code))
	  dir.create(rds_dir, FALSE, TRUE)
          
    ## Read
    ev     <- read.csv('evaluate.csv')
	ev<-ev[,c(1,4,7:ncol(ev))]
    ev<-rapply(object = ev, f = round, classes = "numeric", how = "replace", digits = 3)
    saveRDS(ev, paste0(rds_dir, '/Evaluate_ethiopia_obs_cul_6to10.RDS'))
	
	pgr<-read.csv('plantgro.csv')
   	 pgr<-pgr[ ,c(1,3,6:ncol(pgr))]
    pgr<-round(pgr,3)
	saveRDS(pgr, file.path(rds_dir, paste0('PlantGro_ethiopia_obs_cult_6to10.RDS')))

      unlink(out_dir, recursive = TRUE) 
       rm(list=ls())
         gc()   
    }
	}
   

