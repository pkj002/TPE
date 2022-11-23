save_mod_yld_tz <- function(pp,comb,path){

      #out_dir<-paste0(path,'obs_new2/')
  out_dir<-paste0(path,'new_dssat_runs_tanzania_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'/')
  setwd(out_dir)
  
  x<-vector('list', len) 
  yld_tpe<-x
  seas<-list()
  yld_av1<-list()
  df<-list()
  
for (i in 1:len){
    if(!dir.exists(paste0(out_dir,i))) {
      next
    } else if (!file.exists(paste0(out_dir,i,'/Evaluate_tanzania_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))) {
      next
    } else {      
      ## read output files
      #x[[i]]<-readRDS(paste0(out_dir,i,'/Evaluate_',cntry,'_obs.RDS'))
      x[[i]]<-readRDS(paste0(out_dir,i,'/Evaluate_tanzania_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
      hwam<-x[[i]][,c(1,2,11)]; 
      hwam[hwam==-99]=NA
      
      if (max(hwam$TN)==8){
        seas<-'seas2'
        yld_av1<-round(mean(hwam$HWAMS,na.rm=T),1)
       } else {
        seas<-'seas1'
        yld_av1<-round(mean(hwam$HWAMS,na.rm=T),1)
      }
      df<-data.frame(yield=yld_av1,season=seas)
     
       yld_tpe[[i]]<-df
     }
}
    rm(x); rm(hwam) 
  yld <- map_df(yld_tpe, ~as.data.frame(.x), .id="grids")
  #saveRDS(yld_tpe,paste0(path,'tpe/yld_tpe_',cntry,'_obs_new2.RDS'))
  saveRDS(yld,paste0(path,'tpe/yld_tpe/yld_new_dssat_runs_tanzania_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
}
  #q(save='no')
  
