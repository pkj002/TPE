rm(list=ls())
source('E:/Prakash/dssat_out_culti/uganda/anova_stress_tpe_ug_all_gen_comb_grids.R')
pral <- list(); prp <- list(); gryld <- list(); cryld <- list()

## clustering data
path <- 'E:/Prakash/dssat_out_culti/uganda/PlantGro/cluster/'
cul <- c('Cal_96','DAB_489','HTA_4','KAT_B9','MCM_1015','SCR_26','SELIAN_97','SER_119','SUG_73','UYOLE_94')

cl<- readRDS(paste0(path,'cluster_uganda_all_genotypes.RDS')) %>% dplyr::select(c(genotypes,row1,grids,clust))
# cl <- cl[complete.cases(cl),]
# dd<-cl %>% group_by(clust)

for (pp in 1:10){
  for (qq in 1:10){
    bftot <-  md1 %>% filter(genotypes==cul[pp] & corridor==his$MBPA[qq]) 
    bfsf <-  md1 %>% filter(genotypes==cul[pp] & corridor==his$MBPA[qq] & stress == 'SF') 
    bfsts <- md1 %>% filter(genotypes==cul[pp] & corridor==his$MBPA[qq] & stress == 'STS')
    b50 <- bfsts %>% filter(bfsts$val >= 0.25 & bfsts$val < 0.5) 
    b50a <- nrow(b50)/(nrow(bftot) - nrow(bfsf))
    b75 <- bfsts %>% filter(bfsts$val >= 0.5 & bfsts$val < 0.75)
    b75a <- nrow(b75)/(nrow(bftot) - nrow(bfsf))
    b100 <- bfsts %>% filter(bfsts$val >= 0.75 & bfsts$val < 1)
    b100a <- nrow(b100)/(nrow(bftot) - nrow(bfsf))
    prp[[qq]] <- data.frame(b50a,b75a,b100a) 
    
    ## yield in corresponding cluster
    clus <- cl %>% filter(genotypes==pp)
    clus <- clus[order(clus$row1),]
    grd1 <- unique(clus$grids)
    yld <- readRDS(paste0('E:/Prakash/dssat_out_culti/uganda/yld/yld_yr_trt/yld_',cul[pp],'_ug.RDS'))
    grd2 <- unique(yld$grids)
    dif <- dplyr::setdiff(grd2,grd1)
    yld <- yld %>% dplyr::filter(!grids %in% dif) 
    ylcl <- data.frame(grids=clus$grids,clust=clus$clust,yld=yld$yld)
    ylcl <- ylcl[complete.cases(ylcl),]
    gr50 <- b50$grids; gr75 <- b75$grids; gr100 <- b100$grids
    yl50 <- ylcl %>% filter(grids %in% gr50 & clust==2)
    yl75 <- ylcl %>% filter(grids %in% gr75 & clust==2)
    yl100 <- ylcl %>% filter(grids %in% gr100 & clust==2)
    gryld[[qq]] <- data.frame(mean(yl50$yld),mean(yl75$yld),mean(yl100$yld))
  }
  prp1 <- map_df(prp, ~as.data.frame(.x), .id="corr")
  pral[[pp]] <- prp1
  gryld1 <- map_df(gryld, ~as.data.frame(.x), .id="corr")
  cryld[[pp]] <- gryld1
}

pra <- map_df(pral, ~as.data.frame(.x), .id="genotypes")
clyld <- map_df(cryld, ~as.data.frame(.x), .id="genotypes")

for (i in 1:10){
  pra$genotypes[pra$genotypes==i]=cul[i]  
  clyld$genotypes[clyld$genotypes==i]=cul[i]
}

for (i in 1:10){
  pra$corr[pra$corr==i]=his$MBPA[i] 
  clyld$corr[clyld$corr==i]=his$MBPA[i] 
}


pra <- pra %>%  mutate_if(is.numeric, ~round(., 2))
clyld <- clyld %>%  mutate_if(is.numeric, ~round(., 2))

p5 <- list(); p10 <- list()
## make it vertical
for (pp in 1:10){
  pr1 <- pra  %>% filter(genotypes==cul[pp])
  p5[[pp]] <- t(pr1$b50)
  p10[[pp]] <- t(pr1$b75)
}

p50 <- map_df(p5, ~as.data.frame(.x), .id="genotypes")
p100 <- map_df(p10, ~as.data.frame(.x), .id="genotypes")

for (i in 1:10){
  p50$genotypes[p50$genotypes==i]=cul[i]  
  p100$genotypes[p100$genotypes==i]=cul[i] 
  
}

colnames(p50) <- c('genotypes',paste0(c(his$MBPA[1],his$MBPA[2],his$MBPA[3],his$MBPA[4],his$MBPA[5],his$MBPA[6],
                                        his$MBPA[7],his$MBPA[8],his$MBPA[9],his$MBPA[10]),'_25-50'))

colnames(p100) <- c('genotypes',paste0(c(his$MBPA[1],his$MBPA[2],his$MBPA[3],his$MBPA[4],his$MBPA[5],his$MBPA[6],
                                         his$MBPA[7],his$MBPA[8],his$MBPA[9],his$MBPA[10]),'_50-75'))

all <- data.frame(p50$genotypes,
                  p50$`Eastern Tall Grass_25-50`,p100$`Eastern Tall Grass_50-75`,
                  p50$`Mt Elgon_25-50`,          p100$`Mt Elgon_50-75`,
                  p50$`North Central_25-50`,p100$`North Central_50-75`,
                  p50$`Northern Short Grass_25-50`,p100$`Northern Short Grass_50-75`,
                  p50$`South-western Highlands_25-50`,p100$`South-western Highlands_50-75`,
                  p50$`South-western Tall Grass_25-50`,p100$`South-western Tall Grass_50-75`,
                  p50$`Western Highlands_25-50`,p100$`Western Highlands_50-75`,
                  p50$`Western Short Grass_25-50`,p100$`Western Short Grass_50-75`,
                  p50$`Western Tall Grass_25-50`,p100$`Western Tall Grass_50-75`,
                  p50$`North-western Tall Grass_25-50`,p100$`North-western Tall Grass_50-75`)

write.csv(all, 'E:/Prakash/dssat_out_culti/uganda/ug_stress_extent_prob1.csv')


