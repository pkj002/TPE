rm(list=ls())
library(ncdf4)
library(abind)
library(dplyr)

dd<-readRDS('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_bc/Tanzania/obs/pr_tmx_tmin_rad_tanz_1991_2010.RDS')
dir<-'//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/'
file<-nc_open(paste0(dir,'observed_baseline/chirps/nc/proc/chirps-v2.0.1991_2010.days_p05.tanz.nc'))
obs_lt<-ncvar_get(file, 'latitude')
obs_ln<-ncvar_get(file, 'longitude') 
ln<-obs_ln[2:312]; lt<-obs_lt[3:318]
ln<-round(ln,3); lt<-round(lt,3)

## condition to remove incosistent data (tmx <= tmin)
#dd[ , , ,2]<-ifelse(round(dd[ , , ,2])<=round(dd[ , , ,3]),dd[ , , ,3]+0.6,dd[ , , ,2])

## convert radiation equal to 0 to 0.11, since DSSAT stops to run
#dd[ , , ,4]<-ifelse(dd[ , , ,4]==0,0.11,dd[ , , ,4])

## Select area enclosed by Uganda only
lon<-seq(from=29.125,to=40.125,by=0.05)
lat<-seq(from=-11.975, to=-0.975,by=0.05)
ln1<-which(as.character(ln)==29.125);    ln2<-which(as.character(ln)==40.125)
lt1<-which(as.character(lt)==-11.975);     lt2<-which(as.character(lt)==-0.975) 

## precip 4 digits after decimal and other 2 digits.
dda<-round(dd[ln1:ln2,lt1:lt2, ,1],digits=4)
ddb<-round(dd[ln1:ln2,lt1:lt2, ,2:4],digits=2)

dd<-abind(dda,ddb,along=4)
rm(dda); rm(ddb)

## lon and lat repeated to make a mesh grid
ln2<-rep(lon,each=length(lat))
t<-length(lat)

x<-array(numeric(),dim=c(length(lon),length(lat),2))
for (l in 1:length(lon)){
  x[l, , ]<-cbind(lon=ln2[((l-1)*t+1):(l*t)],lat=round(lat,2))
}

x<-matrix(x, nrow=dim(x)[1]*dim(x)[2],byrow=F)
x<-as.data.frame(x)
colnames(x)<-c('lon','lat')

## Date,m and day
ymd<-seq(as.Date('1991-01-01'),by='1 days', length.out=dim(dd)[3])

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

dd2<-array(dd1, dim=c(dimn[1]*dimn[2]))
rm(dd1)

### ID for each lat and lon
code<-as.integer(seq(from=1,to=length(dd2),by=1))

## Join soil, climate and code
ug<-readRDS('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/dssat_input/Tanzania_soils.RDS')

require(purrr)

## Planting date 
## first season
jday_init_avg<-readRDS('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/final_sowing_date_tz_NARS_seas1.RDS')
dim_pl<-dim(jday_init_avg)
jday_init_avg<-array(jday_init_avg,c(dim_pl[1]*dim_pl[2]))

## second season
jday_end_avg<-readRDS('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/final_sowing_date_tz_NARS_seas2.RDS')
dim_pl<-dim(jday_end_avg)
jday_end_avg<-array(jday_end_avg,c(dim_pl[1]*dim_pl[2]))

## combine
plnt<-tibble( Code = code %>% unlist(),lon = x$lon, lat = x$lat, jday_init_avg = jday_init_avg,  jday_end_avg = jday_end_avg) 

yyy=ug %>%dplyr::mutate(climate = purrr::map(.x = dd2, .f = function(i){return(i)} ),
                        Code = purrr::map(.x = code, .f = function(i){return(i)} ))  %>% 
  dplyr::select(-c(geometry,CELL5M,X,Y,ISO2)) %>%
  tidyr::unnest(Code) %>% 
  full_join(. , plnt)  %>% 
  select(c('Code','lat','lon','climate','SOIL.SOL', 'jday_init_avg', 'jday_end_avg', 'SoilProfil'))
yyy$Code<-as.numeric(yyy$Code)

saveRDS(yyy,'//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/dssat_input/Tanzania/obs/agMeChrips_tanz_1991_2010.RDS')
q(save='no')

