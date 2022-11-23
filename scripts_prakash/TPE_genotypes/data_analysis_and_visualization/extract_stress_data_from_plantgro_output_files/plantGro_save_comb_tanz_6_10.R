rm(list=ls())
gc()
library(lubridate)
library(dplyr)
library(future.apply)
library(purrr)

## WSPD = water stress -photosynthesis; WSGD: same water stress for expansion, partitioning and development
path<-'E:/Prakash/dssat_out_culti/tanzania/' 
cntry<-'tanzania'

# dimension of domain
lon<-seq(from=29.125,to=40.125,by=0.05)
lat<-seq(from=-11.975, to=-0.975,by=0.05)
len <- length(lon)*length(lat)

out_dir<-paste0(path,'obs_cul2/')
setwd(out_dir)

x<- list()
f <- list()
vv_all <- list()

#for (i in 1:len){
for (i in 38491:len){  
     if(!dir.exists(paste0(out_dir,i))) {
    next
  } else if (length(list.files(paste0(out_dir,i)))<2) {
    next
  } else {      
    ## read output files
    x[[i]]<-readRDS(paste0(out_dir,i,'/PlantGro_tz_obs_cul_6to10.RDS'))[,c(1:4,6:8,21:22)]
    
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
    
    trt<-max(bb1$TRTNUM) ## number of treatments
    gr<-bb1 %>% group_by(RUN,GSTD) %>% summarize(mean_wspd = round(mean(WSPD, na.rm = TRUE),3)); rm(bb1)
   
    ## Following codes are in case some stages are missing in the simulation, need to put NA
    rr <- matrix(rep(c(-3,-2,-1,1,3,5,7,8),20*trt),nrow=20*trt*8)
    rr1 <- matrix(rep(1:(20*trt),each=8),nrow=(20*trt*8))
    rr2 <- data.frame(rr1,rr); rm(rr); rm(rr1)
    colnames(rr2) <- c('RUN','GSTD')
    gr <- merge(rr2, gr, by=c("RUN","GSTD"), all.x = TRUE); rm(rr2)
    ## end of code for missing stage
    
    ## code to split for each cultivar
    vv<-gr$mean_wspd; rm(gr)
    vv<-array(vv,c(8,20*trt)) ## 8 stages x 20 yr and 4 treatments
    vv<-t(vv)
    nn <- nrow(vv)/5
  
    for (j in 1:5){
      vv_all[[j]] <- vv[(((j-1)*nn)+1):(j*nn),]
      colnames(vv_all[[j]])<-c('vg1','vg2','vg3','rp1','rp2','rp3','rp4','rp5')
    }
    
    md1 <- map_df(vv_all, ~as.data.frame(.x), .id="cultivar")
    f[[i]]<- md1; rm(md1)
  }
  print(i)
}

all <- map_df(f, ~as.data.frame(.x), .id="grids")
rm(x); rm(f)
c1 <- all %>% filter(cultivar==1)
c2 <- all %>% filter(cultivar==2)
c3 <- all %>% filter(cultivar==3)
c4 <- all %>% filter(cultivar==4)
c5 <- all %>% filter(cultivar==5)

saveRDS(c1,paste0(path,'PlantGro/PlantGro_c6_38491_end_',cntry,'.RDS'))
saveRDS(c2,paste0(path,'PlantGro/PlantGro_c7_38491_end_',cntry,'.RDS'))
saveRDS(c3,paste0(path,'PlantGro/PlantGro_c8_38491_end_',cntry,'.RDS'))
saveRDS(c4,paste0(path,'PlantGro/PlantGro_c9_38491_end_',cntry,'.RDS'))
saveRDS(c5,paste0(path,'PlantGro/PlantGro_c10_38491_end_',cntry,'.RDS'))

