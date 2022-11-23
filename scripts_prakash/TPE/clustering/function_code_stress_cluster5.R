clust_tz4<-function(pp,comb,path){
## Load data base yield
#stress<-readRDS(paste0(path,'PlantGro_tanzania_',comb$mod[pp],'_ssp_',comb$sc[pp],'_2050.RDS'))
stress<-readRDS(paste0(path,'tmin_plantgro/tmin_PlantGro_1_tanzania_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
stress <- map_df(stress, ~as.data.frame(.x), .id="grids")
#stress<-readRDS(paste0(path,'final_PlantGro_tanzania_obs.RDS'))
stress<-stress[complete.cases(stress),]

#Cluster analysis
#3000 initial centroids to form the first clusters
km.res <- kmeans(stress[,2:9], 4000, iter.max = 10, nstart = 15, "Lloyd") #aprox 30 minutes
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
#saveRDS(df,paste0(path,'cluster/tanz_veg_rep_stress_cluster6.RDS'))
#saveRDS(df,paste0(path,'cluster/tz_clust5_',comb$mod[pp],'_ssp_',comb$sc[pp],))
saveRDS(df,paste0(path,'tmin_plantgro/cluster/tmin_tz_clust5_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))

}
#q(save='no')
