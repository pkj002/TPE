rm(list=ls())
gc()
require(raster);require(ggplot2);require(dplyr);require(maptools)
require(rgdal);require(cluster);require(fastcluster); library(factoextra); require(purrr)
library(parallel)
library(future.apply)

cntry<-'tanzania'
path<-'E:/Prakash/dssat_out_culti/tanzania/PlantGro/'
cul <- 1:10

stress <- list(); stress2 <- list()
for (n in 1:10){
  stress1<-readRDS(paste0(path,'PlantGro_c',n,'_tanzania.RDS'))
  stress1 <- stress1 %>% dplyr::select(-cultivar) ## remove column cultivar
  stress1 <- data.frame(row1=1:nrow(stress1),stress1)
  stress[[n]] <-stress1[complete.cases(stress1),]
  stress2[[n]] <- stress1
}

stress2 <- map_df(stress2, ~as.data.frame(.x), .id="genotypes")
stress <- map_df(stress, ~as.data.frame(.x), .id="genotypes")
km.res <- kmeans(stress[,4:11], 4000, iter.max = 10, nstart = 15, "Lloyd") #aprox 30 minutes
stress$cluster <- km.res$cluster

#Calc centroids (3000)
centroids <- as.data.frame(km.res$centers)
centroids$cluster <- 1:4000

system.time(daisy.mat <- as.matrix(daisy(centroids[,1:8], metric="euclidean"))) 
a <- as.dist(daisy.mat)

#The elbow method suggests using 6 clusters
# png(paste0(path,'elbow_',cntry,'.png'), units="in", width=10, height=10,res=300)
# fviz_nbclust(centroids[,1:8], kmeans, method = "wss")
# dev.off()

#Adjusting a hierarchical cluster
clust_hc <- fastcluster::hclust(a, method = "ward.D"); rm(daisy.mat)
# png(paste0(path,'dendro_veg_rep_',cntry,'.png'),units="in", width=10, height=10,res=300)
# plot(clust_hc)
# dev.off()

# choose the number of clusters, decision of the researcher I will use 6.
memb <- cutree(clust_hc, k= 5)
d_clust <- data.frame(centroids, clust= factor(memb))

####extrapolation for the entire database
c1 <- dplyr::filter(d_clust, clust==1)
c2 <- dplyr::filter(d_clust, clust==2)
c3 <- dplyr::filter(d_clust, clust==3)
c4 <- dplyr::filter(d_clust, clust==4)
c5 <- dplyr::filter(d_clust, clust==5)

d1 <- dplyr::filter(stress,cluster %in% c1$cluster)
d2 <- dplyr::filter(stress,cluster %in% c2$cluster)
d3 <- dplyr::filter(stress,cluster %in% c3$cluster)
d4 <- dplyr::filter(stress,cluster %in% c4$cluster)
d5 <- dplyr::filter(stress,cluster %in% c5$cluster)


d1$clust <- 1
d2$clust <- 2
d3$clust <- 3
d4$clust <- 4
d5$clust <- 5

df <- rbind(d1,d2,d3,d4,d5) ### Final result 
## to merge this with the orig df, which contains NA, first sort and then merge
df <- df[order(df$genotypes,df$row1),]
gg <- merge(stress2,df, by.x=c("genotypes","row1"), by.y=c("genotypes","row1"), all.x = TRUE) ## stress2 is original with NA
gg <- gg[,-c(12:21)] ## remove unwanted columns
colnames(gg) <- colnames(df[-12])
saveRDS(gg,paste0(path,'cluster/cluster_tanzania_all_genotypes_all_row.RDS'))

