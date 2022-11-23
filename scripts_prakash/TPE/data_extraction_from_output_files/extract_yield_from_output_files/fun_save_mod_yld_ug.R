save_mod_yld_ug <- function(pp,comb,path){
  out_dir<-paste0(path,paste0('new_dssat_runs_uganda_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'/'))
  setwd(out_dir)
  
  x<-vector('list', len) 
  yld_tpe<-x
  
  for (i in 1:len){
    if(!dir.exists(paste0(out_dir,i))) {
      next
    } else if (!file.exists(paste0(out_dir,i,'/Evaluate_',cntry,'_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))) {
      next
    } else {      
      ## read output files
      x[[i]]<-readRDS(paste0(out_dir,i,'/Evaluate_',cntry,'_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
      hwam<-x[[i]][,11]; 
      hwam[hwam==-99]=NA
      yld_tpe[[i]]<-hwam
      #print(i)
    }
  }
  rm(x); rm(hwam) 
  yld <- map_df(yld_tpe, ~as.data.frame(.x), .id="grids")
  saveRDS(yld,paste0(path,'tpe/model_yld_tpe/yld_tpe_',cntry,'_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
}
#q(save='no')
