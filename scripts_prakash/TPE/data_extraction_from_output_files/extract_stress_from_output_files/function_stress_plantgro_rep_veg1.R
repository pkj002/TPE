plntGro_tz <-function(b,comb,path){
  print(paste0('model_',b))
#out_dir<-paste0(path,'obs_new2/')
  out_dir<-paste0(path,'new_dssat_runs_',cntry,'_',comb$mod[b],'_ssp_',comb$sc[b],'_',comb$year[b],'/')
  setwd(out_dir)
  
## model 
x<-vector('list', len) 
f<-vector('list', len) 
#yr<-1991:2010

for (i in 1:len){
    if(!dir.exists(paste0(out_dir,i))) {
    next
  } else if (length(list.files(paste0(out_dir,i)))<3) {
    next
  } else {      
    ## read output files
    #x[[i]]<-readRDS(paste0(out_dir,i,'/PlantGro_',cntry,'_obs.RDS'))[,c(1:4,6:8,21:22)] 
    x[[i]]<-readRDS(paste0(out_dir,i,'/PlantGro_',cntry,'_',comb$mod[b],'_ssp_',comb$sc[b],'_',comb$year[b],'.RDS'))[,c(1:4,6:8,21:22)]
    
    bb1<-x[[i]]
    for (n in 1:nrow(bb1)){
      if(bb1$GSTD[n]==0 & bb1$L.SD[n] <2){
        bb1$GSTD[n]=-3
      } else if (bb1$L.SD[n] >=2 & bb1$L.SD[n] <5 & bb1$GSTD[n]==0){
        bb1$GSTD[n]=-2
      } else if (bb1$L.SD[n] >=5 & bb1$GSTD[n]==0) {
        bb1$GSTD[n]=-1 
      } else {
        bb1$GSTD[n]=bb1$GSTD[n]
      }
    }
    
    trtnum<-max(bb1$TRTNUM) ## number of treatments
    gr<-bb1 %>% group_by(RUN,GSTD) %>% summarize(mean_wspd = round(mean(WSPD, na.rm = TRUE),3)); rm(bb1)
    vv<-gr$mean_wspd;  rm(gr)
    
    if (trtnum==4){
      vv<-array(vv,c(8,80)) ## 8 stages x 20 yr and 4 treatments
    } else {
      vv<-array(vv,c(8,160)) ## 8 stages x 20 yr and 8 treatments
    }
   
    vv<-t(vv)
    colnames(vv)<-c('vg1','vg2','vg3','rp1','rp2','rp3','rp4','rp5')
    f[[i]]<-vv; rm(vv)
  }
}
rm(x)
#saveRDS(f,paste0(path,'tpe/PlantGro/PlantGro_',cntry,'_obs1.RDS'))
saveRDS(f,paste0(path,'tpe/PlantGro/PlantGro_',cntry,'_',comb$mod[b],'_ssp_',comb$sc[b],'_',comb$year[b],'.RDS'))
rm(list=ls())
gc()
}

#q(save='no')

