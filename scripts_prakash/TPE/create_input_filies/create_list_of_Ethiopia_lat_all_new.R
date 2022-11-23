rm(list=ls())
library(dplyr)
library(abind)
library(sf)
library(raster)

dir<-'//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/'
scripts<- '//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/submit_ethi'

country<-'Ethiopia'

## Lat and lon for each model
#Lat and long of all files for different models 

mod1_lon<-as.character(seq(from=32.625,to=47.625,by=0.05))
mod1_lat<-as.character(seq(from=2.625,to=15.125,by=0.05))

mod2_lon<-as.character(seq(from=32.375,to=48.125,by=0.05))
mod2_lat<-as.character(seq(from=2.375,to=15.125,by=0.05))

mod3_lon<-as.character(seq(from=32.625,to=47.625,by=0.05))
mod3_lat<-as.character(seq(from=2.625,to=15.125,by=0.05))

mod4_lon<-as.character(seq(from=32.625,to=47.625,by=0.05))
mod4_lat<-as.character(seq(from=2.625,to=15.125,by=0.05))

mod5_lon<-as.character(seq(from=32.625,to=47.625,by=0.05))
mod5_lat<-as.character(seq(from=2.625,to=15.125,by=0.05))


## 1. Models
mod <- c('BCC-CSM2-MR','EC-Earth3-Veg','GFDL-ESM4','IPSL-CM6A-LR','MRI-ESM2-0')

# dimension of domain
comb <- data.frame(year = c(rep(2030,20),rep(2050,20)), mod = c(rep(mod,each=4),rep(mod,each=4)), sc = rep(c(126,245,370,585),10),
                   period=c(rep('202101-204012',20),rep('204101-206012',20)), mod_numb=c(rep(1:5,each=4),rep(1:5,each=4)),
                   stringsAsFactors = FALSE)

###############################################################
for (k in c(4,12:40)){ ## period
    dd<-readRDS(paste0(dir,'cc_bc/',country,'/all/',comb$year[k],'/all_Ethi/all_var/all_Amon_',comb$mod[k],'_ssp',comb$sc[k],'_r1i1p1f1_Ethi_bc_all_',comb$period[k],'.RDS'))
      
      ## Select domain to cover Ethiopia only
      lon<-seq(from=33.025,to=44.025,by=0.05)
      lat<-seq(from=3.025, to=15.025,by=0.05)
      
      ln1<-which(get(paste0('mod',comb$mod_numb[k],'_lon'))==33.025);    ln2<-which(get(paste0('mod',comb$mod_numb[k],'_lon'))==44.025)
      lt1<-which(get(paste0('mod',comb$mod_numb[k],'_lat'))==3.025);     lt2<-which(get(paste0('mod',comb$mod_numb[k],'_lat'))==15.025)           
      
      dd<-dd[ln1:ln2,lt1:lt2, , ]
      
      ## condition to remove incosistent data (tmx <= tmin)
      #dd[ , , ,2]<-ifelse(round(dd[ , , ,2])<=round(dd[ , , ,3]),dd[ , , ,3]+0.6,dd[ , , ,2])
      
      ## convert radiation equal to 0 to 0.11, since DSSAT stops to run
      #dd[ , , ,4]<-ifelse(dd[ , , ,4]==0,0.11,dd[ , , ,4])
      
      ## precip 4 digits after decimal and other 2 digits.
      dda<-round(dd[,, ,1],digits=4)
      ddb<-round(dd[,, ,2:4],digits=2)
      
      dd<-abind(dda,ddb,along=4)
      rm(dda); rm(ddb)
      
      ## lon and lat repeated to make a mesh grid
      
      dimen<-dim(dd)
      ln2<-rep(lon,each=dimen[2])
      t<-dimen[2]
      
      x<-array(numeric(),dim=c(length(lon),dimen[2],2))
      for (l in 1:length(lon)){
        x[l, , ]<-cbind(lon=ln2[((l-1)*t+1):(l*t)],lat=round(lat[1:dimen[2]],2))
      }
      
      x<-matrix(x, nrow=dim(x)[1]*dim(x)[2],byrow=F)
      x<-as.data.frame(x)
      colnames(x)<-c('lon','lat')
      
      ## Date,m and day
      if (comb$year==2030){
        ymd<-seq(as.Date('2021-01-01'),by='1 days', length.out=dim(dd)[3])
      } else {
        ymd<-seq(as.Date('2041-01-01'),by='1 days', length.out=dim(dd)[3])
      }
      
      y<-substr(ymd[1:dim(dd)[3]],1,4)
      m<-substr(ymd[1:dim(dd)[3]],6,7)
      d<- substr(ymd[1:dim(dd)[3]],9,10)
      
      ## to make '01' to '1' in both month and day
      for (i in 1:length(y)){
        if(as.numeric(m[i]) < 10){
          m[i]=substr(ymd[i],7,7)
        } else {
          m[i]<-substr(ymd[i],6,7)
        }
      }
      
      for (i in 1:length(y)){
        if(as.numeric(d[i]) < 10){
          d[i]=substr(ymd[i],10,10)
        } else {
          d[i]<-substr(ymd[i],9,10)
        }
      }
      
      y<-as.integer(y)
      m<-as.integer(m)
      d<-as.integer(d)
      ################
      dimn<-dim(dd)
      
      dd1<-array(list(), dim=c(dimn[1],dimn[2]))
      for (i in 1:dimn[1]){
        for (j in 1:dimn[2]){
          dd1[[i,j]]<-data.frame(dd[i,j, , ])
          dd1[[i,j]]<-cbind.data.frame(ymd,y,m,d,dd1[[i,j]])
          colnames(dd1[[i,j]])<-c('Date','year','month','day','prec_bias_correction','tmax','tmin','srad')
          dd1[[i,j]]$Date <- as.character(dd1[[i,j]]$Date)
        }
      }
      rm(dd)
      dd2<-array(dd1, dim=c(dimn[1]*dimn[2]))
      rm(dd1)
      
      ### ID for each lat and lon
      code<-as.integer(seq(from=1,to=length(dd2),by=1))
      
      ## Join soil, climate and code
      ug<-readRDS('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/dssat_input/Ethiopia_soils.RDS')
      ug<-ug[1:dim(dd2),]
      
      require(purrr)
      ## prepare code, lon and lat
      plnt<-tibble( Code = code %>% unlist(),lon = x$lon, lat = x$lat)
      
      ## Join by code
      dfile=ug %>%dplyr::mutate(climate = purrr::map(.x = dd2, .f = function(i){return(i)} ),
                                Code = purrr::map(.x = code, .f = function(i){return(i)} ))  %>% 
        dplyr::select(-c(geometry,CELL5M,X,Y,ISO2)) %>%
        tidyr::unnest(Code) %>% 
        full_join(. , plnt)  %>% 
        dplyr::select(c('Code','lat','lon','climate','SOIL.SOL', 'SoilProfil'))
      
      dfile$Code<-as.numeric(dfile$Code)
      
      rm(ug); rm(dd2); rm(code); rm(plnt)
      
      ## Add planting date
      mps <- shapefile(paste0(scripts,'/ETH_adm/ETH_adm2.shp'))
      mat<-raster(ncol=241, nrow=221, xmn=33.025, xmx=44.025, ymn=3.025, ymx=15.025)
      
      ## first season
      ## start
      jday_init_avg<-rasterize(mps, field = "sow_dt1_in", mat)
      pp <- raster::extract(jday_init_avg, dfile[,c("lon", "lat")])
      pp <- data.frame(jday_init_avg = pp, dfile[,c("lon", "lat")])
      
      ## end
      jday_end_avg<-rasterize(mps, field = "sow_dt1_en", mat)
      ff<-raster::extract(jday_end_avg, dfile[,c('lon','lat')])
      ff<-data.frame(jday_end_avg=ff, dfile[,c('lon','lat')])
      
      ## Belg season
      jday_init_Belg<-rasterize(mps, field = "sow_dt2_1", mat)
      gg<-raster::extract(jday_init_Belg, dfile[,c('lon','lat')])
      gg<-data.frame(jday_init_Belg=gg,dfile[,c('lon','lat')])
      
      ## merge by lon and lat
      both<-merge(pp, ff, by = c("lon", "lat"))
      three<- merge(both,gg, by = c("lon", "lat"))
      
      ## join dfile and all sowing dates by lon and lat
      dfile<- dfile %>% full_join(.,three, by = c("lon", "lat"))
      
      ## replace 0 with NA
      dfile$jday_end_avg<-ifelse(dfile$jday_end_avg==0,NA,dfile$jday_end_avg)
      dfile$jday_init_avg<-ifelse(dfile$jday_init_avg==0,NA,dfile$jday_init_avg)
      dfile$jday_init_Belg<-ifelse(dfile$jday_init_Belg==0,NA,dfile$jday_init_Belg)
      
      rm(both); rm(three); rm(ff); rm(pp); rm(gg); rm(jday_end_avg); rm(jday_init_avg); rm(jday_init_Belg)
      
      ## for test
      saveRDS(dfile,paste0(dir,'dssat_input/',country,'/',comb$year[k],'/new_',comb$mod[k],'_ssp',comb$sc[k],'_r1i1p1f1_',comb$period[k],'.RDS'))
      rm(dfile)
    }
#q(save="no")






















