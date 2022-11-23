runDSSATSingle_obs_bd_2lay <- function(n,dfile,outdir){
  cat("processing row ", n, "\n")
  # read one file
  # this removes a lot of the files
  x <- dfile[n,] 
  
  ## discard rows where no planting dates 
  if(is.na(x$jday_init_avg[1]) & is.na(x$jday_end_avg[1]) & is.na(x$jday_init_Belg[1])){
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
      #soil<-c('orig','bd17_srgfO','bd17_srgfP')
      #cultiv <- c('Calima', 'Isabella','A 195', 'BAT 1393', 'WAF 9')
	  
	  soil<-c('orig','bd8','bd17')
      cultiv <- c('Calima','BAT 1393')
      
      for (ff in 1:3){
	       for (gg in 1:2){
			# where to save results 
        out_dir <- file.path(outdir, paste0('run/', xs$Code,'/',soil[ff],'_',cultiv[gg])) 
        dir.create(out_dir,showWarnings=FALSE,recursive=TRUE)
        setwd(out_dir)
      
     ## 1. weather
    # no need for out_dir as we have already in the outdir
    make_wth(data = xs$climate[[1]], out_dir, name_xfile_climate = 'UGAND001', lat = xs$lat, lon = xs$lon, co2 = -99)
    write_soil(xs$SOIL.SOL[[1]], out_dir)
    
    if(ff>1){
      ## change soil water and conductivity according to the change in bulk density
      sol<-read_sol(paste0(out_dir,'/SOIL.SOL'))
      
      rho_b0 = unlist(sol$SBDM)[1:2] 
      
      mx1 <- max(rho_b0 + rho_b0*0.17)
      #mx2 <- max(rho_b0 + rho_b0*0.34)
      
      ## change BD
      if (ff==2 & mx1 < 1.72){  ## 1.72 g cm-3 is the BD threshold used in Zhengchao et al. 2021's equation
        rho_b1 = rho_b0 + rho_b0*0.08
      } else if (ff==3 & mx1 < 1.72){
        rho_b1 = rho_b0 + rho_b0*0.17
      } else {
        rho_b1 = rho_b0
      }
      
      ################################################################################################################
      ## Calculate SDUL, SLLL, SSAT and SSKS for the increase in bulk density
      hhh<-sapply(1:2, soil_water2, ff, sol, rho_b1)
      
      ### Need to replace SDUL, SLLL, SSAT, SSKS with the modified values and
      sol$SSAT[[1]] <- round(c(unlist(hhh[1,]),unlist(sol$SSAT)[4:6]),3)
      sol$SDUL[[1]] <- round(c(unlist(hhh[2,]),unlist(sol$SDUL)[4:6]),3)
      sol$SLLL[[1]] <- round(c(unlist(hhh[3,]),unlist(sol$SLLL)[4:6]),3)
      sol$SSKS[[1]] <- round(c(unlist(hhh[4,]),unlist(sol$SSKS)[4:6]),2)
     
      if(ff==3| ff==5){
        sol$SRGF[[1]]<-sol$SRGF[[1]]*unlist(sol$SBDM)/round(c(rho_b1,unlist(sol$SBDM)[4:6]),2)
      }

      sol$SBDM[[1]] <- round(c(rho_b1,unlist(sol$SBDM)[4:6]),2)
      
       ## then write
      write_sol(sol, append = F, 'SOIL.SOL')
    }
    
    
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
    cid<- c('IB2013', 'IB0011', 'IB0034', 'IB0035', 'IB0036')
   
    ## Isabella
    # file_x$`CULTIVARS`[1,3] <- 'IB0011'
    # file_x$`CULTIVARS`[1,4] <- 'Isabella'
    
    ## A 195
    # file_x$`CULTIVARS`[1,3] <- 'IB0034'
    # file_x$`CULTIVARS`[1,4] <- 'A 195'
    
    ## BAT 1393
    # file_x$`CULTIVARS`[1,3] <- 'IB0035'
    # file_x$`CULTIVARS`[1,4] <- 'BAT 1393'
    
    ## WAF
    # file_x$`CULTIVARS`[1,3] <- 'IB0036'
    # file_x$`CULTIVARS`[1,4] <- 'WAF 9'
	
   
    file_x$`CULTIVARS`[1,3] <- cid[gg]
    file_x$`CULTIVARS`[1,4] <- cultiv[gg]
    
	## plant density
     file_x$`PLANTING DETAILS`[1,4:5] <- 15
	 	
    ## simulation start date
    file_x$`SIMULATION CONTROLS`[1,6] <- as.POSIXct(xs$climate[[1]][1,1])
    
    ## number of years to simulate
    dimn <- dim(xs$climate[[1]])
    
    st <- xs$climate[[1]][1,2] 
    ed <- xs$climate[[1]][dimn[1],2]
    
    yr <- (ed-st)+1  ## to run for 20 years
	    
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
    
    ## save only plantgro, evaluate and carbon files
    # prepare output dir
    cultivar=file_x$`CULTIVARS`[1,4]
    
    rds_dir <- file.path(outdir,paste0('bd_random_2lay/', xs$Code,'/',soil[ff],'_',cultiv[gg]))
    
    dir.create(rds_dir, FALSE, TRUE)
    
   ## Read
       
    ev<- read.csv('evaluate.csv')
    ev<-ev[,c(1,4,7:ncol(ev))]
    ev<-rapply(object = ev, f = round, classes = "numeric", how = "replace", digits = 3)
    saveRDS(ev, file.path(rds_dir, paste0('Evaluate_',cntry,'_',soil[ff],'.RDS')))
    
           unlink(out_dir, recursive=TRUE)
    }
      }
      rm(list=ls())
      gc() 
     
}
}
