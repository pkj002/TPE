rm(list=ls())
gc()
library(lubridate)
library(dplyr)
library(future.apply)
library(purrr)

path<-'E:/Prakash/dssat_out_culti/uganda/'
cntry<-'uganda'

# dimension of domain
lon<-seq(from=29.525,to=35.025,by=0.05)
lat<-seq(from=-1.525, to=4.475,by=0.05)
len <- length(lon)*length(lat)

## There are 5 lots of simulations. Each lot contains 2 cultivars.
sim <- 1
#for (sim in 1:5){
  
if (sim==1){
   ## cultivars
  c1 <- 'SUG_73'
  c2 <- 'UYOLE_94'
} else if (sim==2) {
  c1 <- 'SER_119'
  c2 <- 'SELIAN_97'
} else if (sim==3){
  c1 <- 'SCR_26'
  c2 <- 'MCM_1015'
} else if (sim==4){
  c1 <- 'KAT_B9'
  c2 <- 'HTA_4'  
} else if (sim==5){
  c1 <- 'DAB_489'
  c2 <- 'Cal_96'
}

out_dir<-paste0(path,'obs_cul',sim,'/')
setwd(out_dir)


x<-vector('list', len) 
f <- list()
vv_all <- list()

for (i in 3809:len){
  #for (i in 112:113){  
  if(!dir.exists(paste0(out_dir,i))) {
    next
  } else if (length(list.files(paste0(out_dir,i)))<2) {
    next
  } else {      
    ## read output files
    #x[[i]]<-readRDS(paste0(out_dir,i,'/Evaluate_',cntry,'_obs.RDS'))
    x[[i]]<-readRDS(paste0(out_dir,i,'/PlantGro_uganda_obs_',c1,'_',c2,'.RDS'))[,c(1:4,6:8,21:22)] 
    
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
    
    gr<-bb1 %>% group_by(RUN,GSTD) %>% summarize(mean_wspd = round(mean(WSPD, na.rm = TRUE),3))
    trt<-max(bb1$TRTNUM)
    ## Following codes are in case some stages are missing in the simulation, need to put NA
    rr <- matrix(rep(c(-3,-2,-1,1,3,5,7,8),20*trt),nrow=20*trt*8)
    rr1 <- matrix(rep(1:(20*trt),each=8),nrow=(20*trt*8))
    rr2 <- data.frame(rr1,rr); rm(rr); rm(rr1)
    colnames(rr2) <- c('RUN','GSTD')
    gr <- merge(rr2, gr, by=c("RUN","GSTD"), all.x = TRUE); rm(rr2)
    ## end of code for missing stage
    
    ## code to split for each cultivar
    vv<-gr$mean_wspd; rm(gr)
    vv<-array(vv,c(8,20*trt))
    vv<-t(vv)
    nn <- nrow(vv)/2
    
    for (j in 1:2){
      vv_all[[j]] <- vv[(((j-1)*nn)+1):(j*nn),]
      colnames(vv_all[[j]])<-c('vg1','vg2','vg3','rp1','rp2','rp3','rp4','rp5')
    }
    
    md1 <- map_df(vv_all, ~as.data.frame(.x), .id="cultivar")
    f[[i]]<- md1
  }
  print(i)
}

all <- map_df(f, ~as.data.frame(.x), .id="grids")
rm(x); rm(f)
df1 <- all %>% filter(cultivar==1)
df2 <- all %>% filter(cultivar==2)

saveRDS(df1,paste0(path,'PlantGro/PlantGro_1_3808_',c1,'_',cntry,'.RDS'))
saveRDS(df2,paste0(path,'PlantGro/PlantGro_1_3808_',c2,'_',cntry,'.RDS'))

#}


