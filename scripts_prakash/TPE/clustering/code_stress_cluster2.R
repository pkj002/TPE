ug_clust2 <- function(pp,comb,path){
  #stress<-readRDS(paste0(path,'tmin_PlantGro_uganda_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
  #stress<-readRDS(paste0(path,'PlantGro_uganda_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
  #stress <- map_df(stress, ~as.data.frame(.x), .id="grids")
  stress<-readRDS(paste0(path,'final/wspd680/wspd_wkly_uganda_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
  #stress<-readRDS(paste0(path,'PlantGro_uganda_new1_obs1_final.RDS'))
  #stress<-readRDS(paste0(path,'final_tmin_PlantGro_uganda_obs2.RDS'))
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
  
  #Adjusting a hierarchical cluster
  clust_hc <- fastcluster::hclust(a, method = "ward.D"); rm(daisy.mat)
  
  # choose the number of clusters, decision of the researcher I will use 6.
  memb <- cutree(clust_hc, k= 2)
  d_clust <- data.frame(centroids, clust= factor(memb))
  
  ####extrapolation for the entire database
  c1 <- dplyr::filter(d_clust, clust==1)
  c2 <- dplyr::filter(d_clust, clust==2)
  #c3 <- dplyr::filter(d_clust, clust==3)
  #c4 <- dplyr::filter(d_clust, clust==4)
  #c5 <- dplyr::filter(d_clust, clust==5)
  #c6 <- dplyr::filter(d_clust, clust==6)
  
  d1 <- dplyr::filter(stress,cluster %in% c1$cluster)
  d2 <- dplyr::filter(stress,cluster %in% c2$cluster)
  #d3 <- dplyr::filter(stress,cluster %in% c3$cluster)
  #d4 <- dplyr::filter(stress,cluster %in% c4$cluster)
  #d5 <- dplyr::filter(stress,cluster %in% c5$cluster)
  #d6 <- dplyr::filter(stress,cluster %in% c6$cluster)
  
  
  d1$clust <- 1
  d2$clust <- 2
  #d3$clust <- 3
  #d4$clust <- 4
  #d5$clust <- 5
  #d6$clust <- 6
  
  df <- rbind(d1,d2) ### Final result 
  saveRDS(df,paste0(path,'final/cluster/cluster_pltgro_yr_sowdt/cluster680/final_ug_clust2_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
  #saveRDS(df,paste0(path,'cluster/clst_tmin_PlantGro_uganda_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
  #saveRDS(df,paste0(path,'cluster/cluster_pltgro_yr_sowdt/final_ug_clust2_',comb$mod[pp],'_ssp_',comb$sc[pp],'_2030.RDS'))
  #saveRDS(df,paste0(path,'cluster/obs_new1_veg_rep_stress_cluster2.RDS'))
  #saveRDS(df,paste0(path,'cluster/final_tmin_PlantGro_cluster4.RDS'))
}

#q(save='no')


