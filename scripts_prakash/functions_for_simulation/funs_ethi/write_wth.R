
make_wth <- function(data, out_dir, name_xfile_climate, lat, long, co2){
  
  # sprintf("%.3d", num_scenarios)
  data <- data %>% 
    mutate(Date = anydate(Date),
           jday = yday(Date),
           jday = sprintf("%.3d", jday),
           year_dssat = str_sub(year, start = 3)) %>% 
    mutate(date_dssat = paste0(year_dssat, jday)) #%>% 
    # dplyr::select(date_dssat)
    
  
  Srad <- data$srad
  Tmax <- data$tmax
  Tmin <- data$tmin
  Prec <- data$prec_bias_correction
  date <- data$date_dssat 
    
  sink(file.path(out_dir, paste0(name_xfile_climate, '.WTH')), append = F)
  ## Agregar las siguientes Lineas
  
  ##cat(paste("*WEATHER DATA :"),paste(coordenadas[1,1]),paste(coordenadas[1,2]))
  cat(paste("*WEATHER DATA :"), paste("USAID"))
  cat("\n")
  cat("\n")
  cat(c("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT CO2"))
  cat("\n")
  cat(sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.1f %5.2f %5.2f %5.2f", "USCI", lat, long, -99,-99, -99.0, 0, 0, co2))
  cat("\n")
  cat(c('@DATE  SRAD  TMAX  TMIN  RAIN'))
  cat("\n")
  cat(cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f", date, Srad, Tmax, Tmin, Prec)), sep = "\n")
  sink()
  
  
}