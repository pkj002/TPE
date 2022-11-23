
filter_text <- function(data, matches, different = F){ 
  
  if(different == F){
    
    return(data[grep(matches, data)])
    
  }
  
  if(different == T){
    
    return(data[-grep(matches, data)])
    
  }
  
}
## copiar los archivos necesarios para correr DSSAT
# library(dplyr)
# dir_dssat <- "dssat-csm-original/Data/"
# out_dir <- 'dssat_runs_ug_bcc/'
# 
# files_dssat(dir_dssat, out_dir)

files_dssat1 <- function(dir_dssat, out_dir){
  
  rutinas <- "*.WDA|*.SDA|MODEL.ERR|DSSATPRO.L47|*.CDE"
  
  files <- "BNGRO047.CUL|BNGRO047.ECO|BNGRO047.SPE"  ## special files
  
  exe_dssat <- file.path(dir_dssat, 'DSCSM047.exe')    ## Executable DSSAT v 4.6
  
  parameters <- file.path(dir_dssat, 'Genotype') %>%
    list.files(full.names= T) %>%
    filter_text(files, different = F)
  
  rutines_files <- dir_dssat%>%
    list.files(full.names= T) %>%
    filter_text(rutinas, different = F)
    
  file.copy(exe_dssat, out_dir)
  file.copy(parameters, out_dir)
  file.copy(rutines_files, out_dir)  
}
