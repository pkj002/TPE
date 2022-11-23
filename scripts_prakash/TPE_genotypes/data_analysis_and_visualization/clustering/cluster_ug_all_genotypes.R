rm(list=ls())
gc()
require(raster);require(ggplot2);require(dplyr);require(maptools)
require(rgdal);require(cluster);require(fastcluster); library(factoextra); require(purrr)
library(parallel)
library(future.apply)

cul <- c('Cal_96','DAB_489','HTA_4','KAT_B9','MCM_1015','SCR_26','SELIAN_97','SER_119','SUG_73','UYOLE_94')
path <- 'E:/Prakash/dssat_out_culti/uganda/PlantGro/'

stress <- list(); stress2 <- list()
for (n in 1:10){
    ## Load data base yield
    stress1<-readRDS(paste0(path,'PlantGro_',cul[n],'_uganda.RDS'))
    stress1 <- data.frame(row1=1:nrow(stress1),stress1)
    stress[[n]]<-stress1[complete.cases(stress1),]
    stress2[[n]] <- stress1
  }
 
stress2 <- map_df(stress2, ~as.data.frame(.x), .id="genotypes") ## all rows even with NAs
stress <- map_df(stress, ~as.data.frame(.x), .id="genotypes")

#Cluster analysis
#3000 initial centroids to form the first clusters
km.res <- kmeans(stress[,5:12], 4000, iter.max = 10, nstart = 15, "Lloyd") #aprox 30 minutes
stress$cluster <- km.res$cluster

#Calc centroids (3000)
centroids <- as.data.frame(km.res$centers)
centroids$cluster <- 1:4000

system.time(daisy.mat <- as.matrix(daisy(centroids[,1:8], metric="euclidean"))) 
a <- as.dist(daisy.mat)

#Adjusting a hierarchical cluster4
clust_hc <- fastcluster::hclust(a, method = "ward.D"); rm(daisy.mat)

# choose the number of clusters, decision of the researcher I will use 6.
memb <- cutree(clust_hc, k= 2)
d_clust <- data.frame(centroids, clust= factor(memb))

####extrapolation for the entire database
c1 <- dplyr::filter(d_clust, clust==1)
c2 <- dplyr::filter(d_clust, clust==2)

d1 <- dplyr::filter(stress,cluster %in% c1$cluster)
d2 <- dplyr::filter(stress,cluster %in% c2$cluster)

d1$clust <- 1
d2$clust <- 2

df <- rbind(d1,d2) ### Final result 

## to merge this with the orig df, which contains NA, first sort and then merge
df <- df[order(df$genotypes,df$row1),]
# gg <- gg[,-c(11:19)] ## check column numbers to remove unwanted columns
gg <- merge(stress2,df, by.x=c("genotypes","row1"), by.y=c("genotypes","row1"), all.x = TRUE) ## stress2 is original with NA
gg <- gg[,-c(4,13:23)] ## remove unwanted columns
colnames(gg) <- colnames(df[-c(4,13)])

saveRDS(gg,paste0(path,'cluster/cluster_uganda_all_genotypes.RDS'))
