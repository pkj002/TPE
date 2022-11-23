rm(list=ls())
source('E:/Prakash/dssat_out_culti/ethiopia/anova_tpe_all_genotypes_comb_grids.R')
pral <- list(); prp <- list(); kr <- list()
gs <- list(); mts <- list(); ets <- list(); gs1 <- list(); mts1 <- list(); ets1 <- list()
gsyld1 <- list(); mtsyld1 <- list(); etsyld1 <- list(); gsyld <- list(); mtsyld <- list(); etsyld <- list()
#for (rr in 1:3){ ## stress

path <- 'E:/Prakash/dssat_out_culti/ethiopia/PlantGro/cluster/'
cl<- readRDS(paste0(path,'cluster_ethiopia_all_genotypes_all_rows.RDS')) %>% dplyr::select(c(genotypes,row1,grids,clust))
#cl <- cl[complete.cases(cl),]

for (pp in 1:10){ ## cultivar
  for (qq in 1:10){ ## corrid
    bftot <-  md1 %>% filter(genotypes==cul[pp] & corridor==his$MBPA[qq]) 
    bfsf <-  md1 %>% filter(genotypes==cul[pp] & corridor==his$MBPA[qq] & stress == 'SF')
    bfgs <- md1 %>% filter(genotypes==cul[pp] & corridor==his$MBPA[qq] & stress == 'GS')
    bfmts <- md1 %>% filter(genotypes==cul[pp] & corridor==his$MBPA[qq] & stress == 'MTS')
    bfets <- md1 %>% filter(genotypes==cul[pp] & corridor==his$MBPA[qq] & stress == 'ETS')
    
    bfgs50 <- bfgs %>% filter(bfgs$val >= 0.25 & bfgs$val < 0.5)
    bfgs50a <- nrow(bfgs50)/(nrow(bftot) - nrow(bfsf))
    bfgs75 <- bfgs %>% filter(bfgs$val >= 0.5 & bfgs$val < 0.75)
    bfgs75a <- nrow(bfgs75)/(nrow(bftot) - nrow(bfsf))
    bfgs100 <- bfgs %>% filter(bfgs$val >= 0.75 & bfgs$val < 1)
    bfgs100a <- nrow(bfgs100)/(nrow(bftot) - nrow(bfsf))
    
    gs[[qq]] <- data.frame(bfgs50a,bfgs75a,bfgs100a) 
    
    bfmts50 <- bfmts %>% filter(bfmts$val >= 0.25 & bfmts$val < 0.5)
    bfmts50a <- nrow(bfmts50)/(nrow(bftot) - nrow(bfsf))
    bfmts75 <- bfmts %>% filter(bfmts$val >= 0.5 & bfmts$val < 0.75)
    bfmts75a <- nrow(bfmts75)/(nrow(bftot) - nrow(bfsf))
    bfmts100 <- bfmts %>% filter(bfmts$val >= 0.75 & bfmts$val < 1)
    bfmts100a <- nrow(bfmts100)/(nrow(bftot) - nrow(bfsf))
    mts[[qq]] <- data.frame(bfmts50a,bfmts75a,bfmts100a)
    
    bfets50 <- bfets %>% filter(bfets$val >= 0.25 & bfets$val < 0.5)
    bfets50a <- nrow(bfets50)/(nrow(bftot) - nrow(bfsf))
    bfets75 <- bfets %>% filter(bfets$val >= 0.5 & bfets$val < 0.75)
    bfets75a <- nrow(bfets75)/(nrow(bftot) - nrow(bfsf))
    bfets100 <- bfets %>% filter(bfets$val >= 0.75 & bfets$val < 1)
    bfets100a <- nrow(bfets100)/(nrow(bftot) - nrow(bfsf)) 
    ets[[qq]] <- data.frame(bfets50a,bfets75a,bfets100a)
    
    ## yield in corresponding cluster
    clus <- cl %>% filter(genotypes==pp)
    clus <- clus[order(clus$row1),]
    grd1 <- unique(clus$grids)
    
    if (pp<=5){
      yld <- readRDS('E:/Prakash/dssat_out_culti/ethiopia/yld/yld_1to5_ethiopia.RDS')
      yld <- yld[,c(1,pp+1)]
    } else {
      yld <- readRDS('E:/Prakash/dssat_out_culti/ethiopia/yld/yld_6to10_ethiopia.RDS')
      yld <- yld[,c(1,pp-4)]
    }
     
    grd2 <- unique(yld$grids)
    dif <- dplyr::setdiff(grd2,grd1)
    yld <- yld %>% dplyr::filter(!grids %in% dif) ## these grids were not pres in cluster data
    
    ylcl <- data.frame(grids=clus$grids,clust=clus$clust,yld=yld[,2])
    ylcl <- ylcl[complete.cases(ylcl),]
    
    ## yield in stress GS
    gs50 <- bfgs50$grids; gs75 <- bfgs75$grids; gs100 <- bfgs100$grids
    gsyl50 <- ylcl %>% filter(grids %in% gs50 & clust==2)
    gsyl75 <- ylcl %>% filter(grids %in% gs75 & clust==2)
    gsyl100 <- ylcl %>% filter(grids %in% gs100 & clust==2)
    gsyld[[qq]] <- data.frame(mean(gsyl50$yld),mean(gsyl75$yld),mean(gsyl100$yld))
    
    ## yield in MTS
    mts50 <- bfmts50$grids; mts75 <- bfmts75$grids; mts100 <- bfmts100$grids
    mtsyl50 <- ylcl %>% filter(grids %in% mts50 & clust==3)
    mtsyl75 <- ylcl %>% filter(grids %in% mts75 & clust==3)
    mtsyl100 <- ylcl %>% filter(grids %in% mts100 & clust==3)
    mtsyld[[qq]] <- data.frame(mean(mtsyl50$yld),mean(mtsyl75$yld),mean(mtsyl100$yld))
    
    ## yld in ets
    ets50 <- bfets50$grids; ets75 <- bfets75$grids; ets100 <- bfets100$grids
    etsyl50 <- ylcl %>% filter(grids %in% ets50 & clust==4)
    etsyl75 <- ylcl %>% filter(grids %in% ets75 & clust==4)
    etsyl100 <- ylcl %>% filter(grids %in% ets100 & clust==4)
    etsyld[[qq]] <- data.frame(mean(etsyl50$yld),mean(etsyl75$yld),mean(etsyl100$yld))
 }
  ## stress
  gs1[[pp]] <- map_df(gs, ~as.data.frame(.x), .id="corr")
  mts1[[pp]] <- map_df(mts,~as.data.frame(.x),.id='corr')
  ets1[[pp]] <- map_df(ets,~as.data.frame(.x), .id='corr')
  ## yld
  gsyld1[[pp]] <- map_df(gsyld, ~as.data.frame(.x), .id="corr")
  mtsyld1[[pp]] <- map_df(mtsyld,~as.data.frame(.x),.id='corr')
  etsyld1[[pp]] <- map_df(etsyld,~as.data.frame(.x), .id='corr')
}

gs2 <- map_df(gs1, ~as.data.frame(.x), .id="genotypes") %>% mutate_if(is.numeric, ~round(.,2))
mts2 <- map_df(mts1, ~as.data.frame(.x), .id="genotypes") %>% mutate_if(is.numeric, ~round(.,2))
ets2 <- map_df(ets1, ~as.data.frame(.x), .id="genotypes") %>% mutate_if(is.numeric, ~round(.,2))

## yld
gsyld2 <- map_df(gsyld1, ~as.data.frame(.x), .id="genotypes") %>% mutate_if(is.numeric, ~round(.,2))
mtsyld2 <- map_df(mtsyld1, ~as.data.frame(.x), .id="genotypes") %>% mutate_if(is.numeric, ~round(.,2))
etsyld2 <- map_df(etsyld1, ~as.data.frame(.x), .id="genotypes") %>% mutate_if(is.numeric, ~round(.,2))

for (i in 1:10){
  gs2$genotypes[gs2$genotypes==i]=cultivar[i]  
  mts2$genotypes[mts2$genotypes==i]=cultivar[i]  
  ets2$genotypes[ets2$genotypes==i]=cultivar[i]  
  ###
  gsyld2$genotypes[gsyld2$genotypes==i]=cultivar[i]  
  mtsyld2$genotypes[mtsyld2$genotypes==i]=cultivar[i]  
  etsyld2$genotypes[etsyld2$genotypes==i]=cultivar[i] 
}

for (i in 1:10){
  gs2$corr[gs2$corr==i]=his$MBPA[i]  
  mts2$corr[mts2$corr==i]=his$MBPA[i]  
  ets2$corr[ets2$corr==i]=his$MBPA[i]  
  ###
  gsyld2$corr[gsyld2$corr==i]=his$MBPA[i]  
  mtsyld2$corr[mtsyld2$corr==i]=his$MBPA[i]  
  etsyld2$corr[etsyld2$corr==i]=his$MBPA[i]  
}

colnames(gsyld2) <- c('genotypes','corr','g50','g75','g100')
colnames(mtsyld2) <- c('genotypes','corr','m50','m75','m100')
colnames(etsyld2) <- c('genotypes','corr','e50','e75','e100')

comb <- list(); comb_yld <- list(); comb_gen <- list(); comb_gen_yld <- list()
for (pp in 1:10){
  for (i in 1:10){
    gs3 <- gs2 %>% filter(genotypes==cultivar[pp] & corr==his$MBPA[i])
    mts3 <- mts2  %>% filter(genotypes==cultivar[pp] & corr==his$MBPA[i])
    ets3 <- ets2 %>% filter(genotypes==cultivar[pp] & corr==his$MBPA[i])
    comb[[i]] <- data.frame(genotypes=gs3$genotypes, corr= gs3$corr,
                            g1=gs3$bfgs50, g2=gs3$bfgs75,g3=gs3$bfgs100,m1=mts3$bfmts50,m2=mts3$bfmts75,m3=mts3$bfmts100,
                            e1=ets3$bfets50,e2=ets3$bfets75,e3=ets3$bfets100)
   
    ### yield
    gsyld3 <- gsyld2 %>% filter(genotypes==cultivar[pp] & corr==his$MBPA[i])
    mtsyld3 <- mtsyld2  %>% filter(genotypes==cultivar[pp] & corr==his$MBPA[i])
    etsyld3 <- etsyld2 %>% filter(genotypes==cultivar[pp] & corr==his$MBPA[i])
    comb_yld[[i]] <- data.frame(genotypes=gsyld3$genotypes, corr= gsyld3$corr,
                                g1=gsyld3$g50,g2=gsyld3$g75,g3=gsyld3$g100,m1=mtsyld3$m50,m2=mtsyld3$m75,m3=mtsyld3$m100,
                                e1=etsyld3$e50,e2=etsyld3$e75,e3=etsyld3$e100)
  }
  comb_gen[[pp]] <- map_df(comb, ~as.data.frame(.x), .id='corridor')
  comb_gen_yld[[pp]] <- map_df(comb_yld, ~as.data.frame(.x), .id='corridor')
}

comb_gen1 <- map_df(comb_gen, ~as.data.frame(.x), .id='genotypes') %>% select(-corridor)
comb_gen_yld1 <- map_df(comb_gen_yld, ~as.data.frame(.x), .id='genotypes') %>% select(-corridor)

for (i in 1:10){
  comb_gen1$genotypes[comb_gen1$genotypes==i]=cultivar[i] 
  comb_gen_yld1$genotypes[comb_gen_yld1$genotypes==i]=cultivar[i] 
}

comb_gen1 <- comb_gen1[order(comb_gen1$corr),]
comb_gen_yld1 <- comb_gen_yld1[order(comb_gen_yld1$corr),] %>% mutate_if(is.numeric, ~round(.,0))


write.csv(comb_gen1, 'E:/Prakash/dssat_out_culti/ethiopia/stress_spat_time_comb_gen.csv')
write.csv(comb_gen_yld1, 'E:/Prakash/dssat_out_culti/ethiopia/yld_spat_time_comb_gen.csv')
