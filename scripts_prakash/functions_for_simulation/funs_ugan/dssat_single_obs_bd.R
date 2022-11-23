runDSSATSingle_obs_bd <- function(n,dfile,outdir){
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
    soil<-c('orig','bd8','bd17')
    cultiv <- c('Calima','BAT 1393')
    
    
    for (ff in 1:3){ ##  soil
	      for (gg in 1:2){ ## cultivar
    # where to save results 
    out_dir <- file.path(outdir, paste0('run/', xs$Code,'/',soil[ff],'_',cultiv[gg]))
    dir.create(out_dir,showWarnings=FALSE,recursive=TRUE)
    setwd(out_dir)
    
    ## 1. weather
    # no need for out_dir as we have already in the outdir
    make_wth(data = xs$climate[[1]], out_dir, name_xfile_climate = 'UGAND001', lat = xs$lat, lon = xs$lon, co2 = -99)
    
    ## 2. Soil
    write_soil(xs$SOIL.SOL[[1]], out_dir)
    
    if(ff>1){
      ## change soil water and conductivity according to the change in bulk density
      sol<-read_sol(paste0(out_dir,'/SOIL.SOL'))
      
      rho_b0 = unlist(sol$SBDM)[1:3] 
      
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
      hhh<-sapply(1:3, soil_water, ff, sol, rho_b1)
      
      ### Need to replace SDUL, SLLL, SSAT, SSKS with the modified values and
      sol$SSAT[[1]] <- round(c(unlist(hhh[1,]),unlist(sol$SSAT)[4:6]),3)
      sol$SDUL[[1]] <- round(c(unlist(hhh[2,]),unlist(sol$SDUL)[4:6]),3)
      sol$SLLL[[1]] <- round(c(unlist(hhh[3,]),unlist(sol$SLLL)[4:6]),3)
      sol$SSKS[[1]] <- round(c(unlist(hhh[4,]),unlist(sol$SSKS)[4:6]),2)
      
      #if(ff==3| ff==5){
        #sol$SRGF[[1]]<-sol$SRGF[[1]]*unlist(sol$SBDM)/round(c(rho_b1,unlist(sol$SBDM)[4:6]),2)
      #}
      
      sol$SBDM[[1]] <- round(c(rho_b1,unlist(sol$SBDM)[4:6]),2)
      
      ## then write
      write_sol(sol, append = F, 'SOIL.SOL')
    }
    
    
    # copy files  
    file.copy(file.path(scripts, "original_files_ug/BNGRO047.CUL"), out_dir)
    file.copy(file.path(scripts, "original_files_ug/BNGRO047.SPE"), out_dir)
    file.copy(file.path(scripts, "original_files_ug/BNGRO047.ECO"), out_dir)
    file.copy(file.path(scripts, "original_files_ug/CCPA9105.BNX"), out_dir)
    
    ## change soil code in experimental file
    all_Prfl <- read_sol(file.path(out_dir,'SOIL.SOL'))
    file_x <- read_filex(file.path(out_dir,'CCPA9105.BNX'))
    
    file_x$FIELDS[1,12]=all_Prfl[1,1]
    file_x$FIELDS[1,3]='UGAND001'
    
    ## change cultivar
    cid<- c('IB2013','IB0035')
    
    file_x$`CULTIVARS`[1,3] <- cid[gg]
    file_x$`CULTIVARS`[1,4] <- cultiv[gg]
    
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
    file_x$`SIMULATION CONTROLS`[1,19] <- 'M'
    
    ## write experimental file
    write_filex(file_x, 'CCPA9105.BNX')
    
    ## Run
    CSMbatch(crop="BEAN", name = "CCPA9105.BNX", xs, filename = file.path(out_dir,"DSSBatch.v47"))
    dir_dssat <- 'C:/DSSAT47'
    files_dssat1(dir_dssat, out_dir)    
    system(paste0("./dscsm047 B DSSBatch.v47"), ignore.stdout = TRUE)
    # rm(file_x)
    
    ## make .RDS file from output files
    ## 1. for data frame type of files
    # prepare output dir
    rds_dir <- file.path(outdir,paste0('bd_random_N/', xs$Code,'/',soil[ff],'_',cultiv[gg]))
    dir.create(rds_dir, FALSE, TRUE)						 
    
    ## Read
    et     <- read.csv('et.csv')
    ev<-round(et[,c(1,3,6:35)],3)
    saveRDS(et, file.path(rds_dir, paste0('Et_',cntry,'_',soil[ff],'.RDS'))) 
    
    ev<- read.csv('evaluate.csv')
    ev<-ev[,c(1,4,7:ncol(ev))]
    ev<-rapply(object = ev, f = round, classes = "numeric", how = "replace", digits = 3)
    saveRDS(ev, file.path(rds_dir, paste0('Evaluate_',cntry,'_',soil[ff],'.RDS')))
    
    pgr<-read.csv('plantgro.csv')
    pgr<-pgr[ ,c(1,3,6:ncol(pgr))]
    pgr<-round(pgr,3)
    saveRDS(pgr, file.path(rds_dir, paste0('PlantGro_',cntry,'_',soil[ff],'.RDS')))
    
    file.copy('SOIL.SOL', rds_dir)
    file.copy('OVERVIEW.OUT', rds_dir)
    
    su<-read.csv('summary.csv')
    su<-round(su[,c(1,2,13:85)],3)
    saveRDS(su,file.path(rds_dir, paste0('summary_',cntry,'_',soil[ff],'.RDS')))
    
    unlink(out_dir, recursive=TRUE)
      }
    }
    rm(list=ls())
    gc() 
  }
}
