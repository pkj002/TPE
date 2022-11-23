runDSSATSingle_obs_cultivar <- function(n,dfile,outdir,indir){
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
    out_dir <- file.path(outdir, paste0('run1/', xs$Code)) 
    dir.create(out_dir,showWarnings=FALSE,recursive=TRUE)
    setwd(out_dir)
    
    ## 1. weather
    # no need for out_dir as we have already in the outdir
    make_wth(data = xs$climate[[1]], out_dir, name_xfile_climate = 'UGAND001', lat = xs$lat, lon = xs$lon, co2 = -99)
    
    ## 2. Soil
    write_soil(xs$SOIL.SOL[[1]], out_dir)
    # copy files  
    file.copy(file.path(scripts, "original_files_ug_cultivars/BNGRO047.CUL"), out_dir)
    file.copy(file.path(scripts, "original_files_ug_cultivars/BNGRO047.SPE"), out_dir)
    file.copy(file.path(scripts, "original_files_ug_cultivars/BNGRO047.ECO"), out_dir)
    file.copy(file.path(scripts, "original_files_ug_cultivars/CCPA9106.BNX"), out_dir)
	 
    ## change soil code in experimental file
    all_Prfl <- read_sol(file.path(out_dir,'SOIL.SOL'))
    file_x <- read_filex(file.path(out_dir,'CCPA9106.BNX'))
      
    file_x$FIELDS[1,12]=all_Prfl[1,1]
    file_x$FIELDS[1,3]='UGAND001'
	
	## change cultivar
    file_x$`CULTIVARS`[1,3] <- eco1
	file_x$`CULTIVARS`[2,3] <- eco2
    file_x$`CULTIVARS`[1,4] <- cul1
	file_x$`CULTIVARS`[2,4] <- cul2
		            
   ## Constant planting date supplied (Starting from Feb 10 until Oct 8 given by NARS with an interval of 15 days.
  ## since DSSAT takes pdate as.POIXct, i.e. in yyyy-mm-dd. convert doy to that.
  ## the origin is from weather file. -1 is needed coz the first day needs to be counted.
  pl<-seq(41, 272, by = 14)
   file_x$`PLANTING DETAILS`[ ,2]=as.POSIXct(as.Date(pl,origin=x$climate[[1]][1,1]))
  
  ## plant density
  #file_x$`PLANTING DETAILS`[1,4]=15
           
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
  write_filex(file_x, 'CCPA9106.BNX')
  
   ## Run
  CSMbatch_34(crop="BEAN", name = "CCPA9106.BNX", xs, filename = file.path(out_dir,"DSSBatch.v47"))
   dir_dssat <- 'C:/DSSAT47'
   files_dssat1(dir_dssat, out_dir)    
   
  system(paste0("./dscsm047 B DSSBatch.v47"), ignore.stdout = TRUE)
    # rm(file_x)
    
    ## make .RDS file from output files
    ## 1. for data frame type of files
    # prepare output dir
	rds_dir <- file.path(outdir,paste0('et/obs_cul3/', xs$Code)) ## change culti12 to number of cultivars                   
    #rds_dir <- file.path(outdir,paste0('obs_cul_test/', xs$Code)) #
	dir.create(rds_dir, FALSE, TRUE)						 
          
    ## Read
  # ev<- read.csv('evaluate.csv')
# ev<-round(ev[,c(1,4,7:ncol(ev))],3)
# saveRDS(ev, paste0(rds_dir, '/Evaluate_uganda_obs_',cul1,'_',cul2,'.RDS'))
# 
# pgr<-read.csv('plantgro.csv')
# pgr<-pgr[ ,c(1,3,6:ncol(pgr))]
# pgr<-round(pgr,3)	
# saveRDS(pgr, paste0(rds_dir, '/PlantGro_uganda_obs_',cul1,'_',cul2,'.RDS'))
	
	et     <- read.csv('et.csv')
    et<-round(et[,c(1,3,6:35)],3)
    saveRDS(et, paste0(rds_dir, '/et_uganda_obs_',cul1,'_',cul2,'.RDS')) 

      unlink(out_dir, recursive = TRUE) 
       rm(list=ls())
         gc()       
  }
}  

