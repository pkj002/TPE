rm(list=ls())
gc()
library(dplyr)
library(abind)
library(raster)
dir<-'//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/'

country<-'Tanzania'
sc<-c('126','245','370','585')
yr<-c('2030','2050')
period<-c('202101-204012','204101-206012')

## Lat and lon for each model
## 1. Models
mod<-c('BCC-CSM2-MR','EC-Earth3-Veg','GFDL-ESM4','IPSL-CM6A-LR','MRI-ESM2-0')
#Lat and long of all files for different models 

mod1_lon<-as.character(seq(from=27.625,to=41.375,by=0.05))
mod1_lat<-as.character(seq(from=-12.375,to=1.375,by=0.05))

mod2_lon<-as.character(seq(from=27.875,to=41.375,by=0.05))
mod2_lat<-as.character(seq(from=-12.625,to=1.625,by=0.05))

mod3_lon<-as.character(seq(from=27.625,to=41.375,by=0.05))
mod3_lat<-as.character(seq(from=-12.375,to=1.375,by=0.05))

mod4_lon<-as.character(seq(from=27.625, to=40.125, by=0.05))
mod4_lat<-as.character(seq(from=-12.375, to=1.375, by=0.05))

mod5_lon<- as.character(seq(from=27.625,to=41.375,by=0.05))
mod5_lat<-as.character(seq(from=-12.375,to=1.375,by=0.05))
###############################################################

for (k in 1:2){ ## period
for (p in 1:5){ ## model
for (n in 1:4){ ## scenario

dd<-readRDS(paste0(dir,'cc_bc/',country,'/all/',yr[k],'/all_Tanz/all_var/all_Amon_',mod[p],'_ssp',sc[n],'_r1i1p1f1_Tanz_bc_all_',period[k],'.RDS'))

## Select area enclosed by Tanzania only
lon<-seq(from=29.125,to=40.125,by=0.05)
lat<-seq(from=-11.975, to=-0.975,by=0.05)
ln1<-which(get(paste0('mod',p,'_lon'))==29.125);    ln2<-which(get(paste0('mod',p,'_lon'))==40.125)
lt1<-which(get(paste0('mod',p,'_lat'))==-11.975);     lt2<-which(get(paste0('mod',p,'_lat'))==-0.975)    

## condition to remove incosistent data (tmx <= tmin)
#dd[ , , ,2]<-ifelse(round(dd[ , , ,2])<=round(dd[ , , ,3]),dd[ , , ,3]+0.6,dd[ , , ,2])

## convert radiation equal to 0 to 0.11, since DSSAT stops to run
#dd[ , , ,4]<-ifelse(dd[ , , ,4]==0,0.11,dd[ , , ,4])

## precip 4 digits after decimal and other 2 digits.
dda<-round(dd[ln1:ln2,lt1:lt2, ,1],digits=4)
ddb<-round(dd[ln1:ln2,lt1:lt2, ,2:4],digits=2)

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
if (k==1){
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
ug<-readRDS(paste0('//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/dssat_input/',country,'_soils.RDS'))

require(purrr)

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

scripts<- '//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/cc_raw/scripts/submit_tanz'
## Load showing date
mps <- shapefile(paste0(scripts,'/TZA_adm/TZA_adm2.shp'))
mat<-raster(ncol=221, nrow=221, xmn=29.125, xmx=40.125, ymn=-11.975, ymx=-0.975)

## first season
jday_init_avg<-rasterize(mps, field = "sow_NARS1", mat)
pp <- raster::extract(jday_init_avg, dfile[,c("lon", "lat")])
pp <- data.frame(jday_init_avg = pp, dfile[,c("lon", "lat")])

jday_end_avg<-rasterize(mps, field = "sow_NARS2", mat)
ff<-raster::extract(jday_end_avg, dfile[,c('lon','lat')])
ff<-data.frame(jday_end_avg=ff, dfile[,c('lon','lat')])

both<-merge(pp, ff, by = c("lon", "lat"))
dfile<- dfile %>% full_join(.,both, by = c("lon", "lat")) #%>% relocate(SoilProfil, .after = jday_end_avg)
rm(both); rm(ff); rm(pp); rm(jday_end_avg); rm(jday_init_avg)
dfile$jday_end_avg<-ifelse(dfile$jday_end_avg==0,NA,dfile$jday_end_avg) ## convert 0 to NA
saveRDS(dfile,paste0(dir,'dssat_input/',country,'/',yr[k],'/',mod[p],'_ssp',sc[n],'_r1i1p1f1_',period[k],'.RDS'))
}
}
}
q(save="no")


## Run the following script
indir <- '//dapadfs/workspace_cluster_12/AVISA/data_for_dssat_eaf/dssat_input/Tanzania'
#outdir <- '//dapadfs/workspace_cluster_12/AVISA/dssat_outputs/ethiopia'
mod <- c('BCC-CSM2-MR','EC-Earth3-Veg','GFDL-ESM4','IPSL-CM6A-LR','MRI-ESM2-0')

# dimension of domain
comb <- data.frame(year = c(rep(2030,20),rep(2050,20)), mod = c(rep(mod,each=4),rep(mod,each=4)), sc = rep(c(126,245,370,585),10),
                   period=c(rep('202101-204012',20),rep('204101-206012',20)), co2=c(rep(c(440,446,454,456),5),rep(c(469,508,544,569),5)),
                   stringsAsFactors = FALSE)

path<-'//dapadfs/workspace_cluster_12/AVISA/dssat_outputs/tanzania/obs_new2/'
pt<-list.dirs(path = path, full.names = F, recursive = T)
pt<-as.numeric(as.character(pt[2:length(pt)]))
pt<-sort(pt, decreasing=FALSE)
dfile <-dfile[pt,]
df1<-dfile[1:15000, ]
df2<-dfile[15001:30479,]
saveRDS(df1, paste0(indir,'/',comb$year[i],'/n1_',comb$mod[i],'_ssp',comb$sc[i],'_r1i1p1f1_',comb$period[i],'.RDS'))
saveRDS(df2, paste0(indir,'/',comb$year[i],'/n2_',comb$mod[i],'_ssp',comb$sc[i],'_r1i1p1f1_',comb$period[i],'.RDS'))





















