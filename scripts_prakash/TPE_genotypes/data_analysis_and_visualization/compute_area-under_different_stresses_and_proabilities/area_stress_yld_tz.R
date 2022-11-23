rm(list=ls())
source('E:/Prakash/dssat_out_culti/tanzania/anova_stress_tpe_tz.R')

as <- list(); mts <- list(); sts <- list(); ets <- list(); 
as1 <- list(); mts1 <- list(); sts1 <- list(); ets1 <- list(); 
asyld1 <- list(); mtsyld1 <- list(); etsyld1 <- list(); stsyld1 <- list()
asyld <- list(); mtsyld <- list(); etsyld <- list(); stsyld <- list()

path <- 'E:/Prakash/dssat_out_culti/tanzania/PlantGro/cluster/'
cl<- readRDS(paste0(path,'cluster_tanzania_all_genotypes_all_row.RDS')) %>% dplyr::select(c(genotypes,row1,grids,clust))

for (pp in 1:10){ ## cultivar
  for (qq in 1:11){ ## corrid
    bftot <-  md1 %>% filter(genotypes==cul[pp] & corridor==his$MBPA[qq]) 
    bfsf <-  md1 %>% filter(genotypes==cul[pp] & corridor==his$MBPA[qq] & stress == 'SF')
    bfas <- md1 %>% filter(genotypes==cul[pp] & corridor==his$MBPA[qq] & stress == 'AS')
    bfmts <- md1 %>% filter(genotypes==cul[pp] & corridor==his$MBPA[qq] & stress == 'MTS')
    bfsts <- md1 %>% filter(genotypes==cul[pp] & corridor==his$MBPA[qq] & stress == 'STS')
    bfets <- md1 %>% filter(genotypes==cul[pp] & corridor==his$MBPA[qq] & stress == 'ETS')
    
    bfas50 <- bfas %>% filter(bfas$val >= 0.25 & bfas$val < 0.5)
    bfas50a <- nrow(bfas50)/(nrow(bftot) - nrow(bfsf))
    bfas75 <- bfas %>% filter(bfas$val >= 0.5 & bfas$val < 0.75)
    bfas75a <- nrow(bfas75)/(nrow(bftot) - nrow(bfsf))
    bfas100 <- bfas %>% filter(bfas$val >= 0.75 & bfas$val < 1)
    bfas100a <- nrow(bfas100)/(nrow(bftot) - nrow(bfsf))
    as[[qq]] <- data.frame(bfas50a,bfas75a,bfas100a) 
    
    bfmts50 <- bfmts %>% filter(bfmts$val >= 0.25 & bfmts$val < 0.5)
    bfmts50a <- nrow( bfmts50)/(nrow(bftot) - nrow(bfsf))
    bfmts75 <- bfmts %>% filter(bfmts$val >= 0.5 & bfmts$val < 0.75)
    bfmts75a <- nrow(bfmts75)/(nrow(bftot) - nrow(bfsf))
    bfmts100 <- bfmts %>% filter(bfmts$val >= 0.75 & bfmts$val < 1)
    bfmts100a <- nrow(bfmts100)/(nrow(bftot) - nrow(bfsf))
    mts[[qq]] <- data.frame(bfmts50a,bfmts75a,bfmts100a)
    
    bfsts50 <- bfsts %>% filter(bfsts$val >= 0.25 & bfsts$val < 0.5)
    bfsts50a <- nrow(bfsts50)/(nrow(bftot) - nrow(bfsf))
    bfsts75 <- bfsts %>% filter(bfsts$val >= 0.5 & bfsts$val < 0.75)
    bfsts75a <- nrow(bfsts75)/(nrow(bftot) - nrow(bfsf))
    bfsts100 <-  bfsts %>% filter(bfsts$val >= 0.75 & bfsts$val < 1)
    bfsts100a <- nrow(bfsts100)/(nrow(bftot) - nrow(bfsf))
    sts[[qq]] <- data.frame(bfsts50a,bfsts75a,bfsts100a)
    
    bfets50 <- bfets %>% filter(bfets$val >= 0.25 & bfets$val < 0.5)
    bfets50a <- nrow(bfets50)/(nrow(bftot) - nrow(bfsf))
    bfets75 <- bfets %>% filter(bfets$val >= 0.5 & bfets$val < 0.75)
    bfets75a <- nrow(bfets75)/(nrow(bftot) - nrow(bfsf))
    bfets100 <- bfets %>% filter(bfets$val >= 0.75 & bfets$val < 1)
    bfets100a <- nrow(bfets100)/(nrow(bftot) - nrow(bfsf)) 
    ets[[qq]] <- data.frame(bfets50a,bfets75a,bfets100a)
    
    ## yld
    clus <- cl %>% filter(genotypes==pp)
    clus <- clus[order(clus$row1),]
    grd1 <- unique(clus$grids)
    
    if (pp<=5){
      yld <- readRDS('E:/Prakash/dssat_out_culti/tanzania/yld/yld_1to5_tanzania.RDS')
      yld <- yld[,c(1,pp+1)]
    } else {
      yld <- readRDS('E:/Prakash/dssat_out_culti/tanzania/yld/yld_6to10_tanzania.RDS')
      yld <- yld[,c(1,pp-4)]
    }
    
    grd2 <- unique(yld$grids)
    dif <- dplyr::setdiff(grd2,grd1)
    yld <- yld %>% dplyr::filter(!grids %in% dif) 
    ylcl <- data.frame(grids=clus$grids,clust=clus$clust,yld=yld[,2])
    ylcl <- ylcl[complete.cases(ylcl),]
    
    ## yield under diff stresses
    # 1. AS
    as50 <- bfas50$grids; as75 <- bfas75$grids; as100 <- bfas100$grids
    asyl50 <- ylcl %>% filter(grids %in% as50 & clust==2)
    asyl75 <- ylcl %>% filter(grids %in% as75 & clust==2)
    asyl100 <- ylcl %>% filter(grids %in% as100 & clust==2)
    asyld[[qq]] <- data.frame(mean(asyl50$yld,na.rm=T),mean(asyl75$yld,na.rm=T),mean(asyl100$yld,na.rm=T))
    
    # 2. MTS
    mts50 <- bfmts50$grids; mts75 <- bfmts75$grids; mts100 <- bfmts100$grids
    mtsyl50 <- ylcl %>% filter(grids %in% mts50 & clust==3)
    mtsyl75 <- ylcl %>% filter(grids %in% mts75 & clust==3)
    mtsyl100 <- ylcl %>% filter(grids %in% mts100 & clust==3)
    mtsyld[[qq]] <- data.frame(mean(mtsyl50$yld,na.rm=T),mean(mtsyl75$yld,na.rm=T),mean(mtsyl100$yld,na.rm=T))
    
    ## STS
    sts50 <- bfsts50$grids; sts75 <- bfsts75$grids; sts100 <- bfsts100$grids
    stsyl50 <- ylcl %>% filter(grids %in% sts50 & clust==4)
    stsyl75 <- ylcl %>% filter(grids %in% sts75 & clust==4)
    stsyl100 <- ylcl %>% filter(grids %in% sts100 & clust==4)
    stsyld[[qq]] <- data.frame(mean(stsyl50$yld,na.rm=T),mean(stsyl75$yld,na.rm=T),mean(stsyl100$yld,na.rm=T))
    
    # ETS
    ets50 <- bfets50$grids; ets75 <- bfets75$grids; ets100 <- bfets100$grids
    etsyl50 <- ylcl %>% filter(grids %in% ets50 & clust==5)
    etsyl75 <- ylcl %>% filter(grids %in% ets75 & clust==5)
    etsyl100 <- ylcl %>% filter(grids %in% ets100 & clust==5)
    etsyld[[qq]] <- data.frame(mean(etsyl50$yld,na.rm=T),mean(etsyl75$yld,na.rm=T),mean(etsyl100$yld,na.rm=T))
    
  }
  as1[[pp]] <- map_df(as, ~as.data.frame(.x), .id="corr")
  mts1[[pp]] <- map_df(mts,~as.data.frame(.x),.id='corr')
  sts1[[pp]] <- map_df(sts,~as.data.frame(.x),.id='corr')
  ets1[[pp]] <- map_df(ets,~as.data.frame(.x), .id='corr')
  
  ## yld
  asyld1[[pp]] <- map_df(asyld, ~as.data.frame(.x), .id="corr")
  mtsyld1[[pp]] <- map_df(mtsyld,~as.data.frame(.x),.id='corr')
  stsyld1[[pp]] <- map_df(stsyld,~as.data.frame(.x),.id='corr')
  etsyld1[[pp]] <- map_df(etsyld,~as.data.frame(.x), .id='corr')
}


as2 <- map_df(as1, ~as.data.frame(.x), .id="genotypes") %>% mutate_if(is.numeric, ~round(.,2))
mts2 <- map_df(mts1, ~as.data.frame(.x), .id="genotypes") %>% mutate_if(is.numeric, ~round(.,2))
sts2 <- map_df(sts1, ~as.data.frame(.x), .id="genotypes") %>% mutate_if(is.numeric, ~round(.,2))
ets2 <- map_df(ets1, ~as.data.frame(.x), .id="genotypes") %>% mutate_if(is.numeric, ~round(.,2))

## yld
asyld2 <- map_df(asyld1, ~as.data.frame(.x), .id="genotypes") %>% mutate_if(is.numeric, ~round(.,0))
mtsyld2 <- map_df(mtsyld1, ~as.data.frame(.x), .id="genotypes") %>% mutate_if(is.numeric, ~round(.,0))
stsyld2 <- map_df(stsyld1, ~as.data.frame(.x), .id="genotypes") %>% mutate_if(is.numeric, ~round(.,0))
etsyld2 <- map_df(etsyld1, ~as.data.frame(.x), .id="genotypes") %>% mutate_if(is.numeric, ~round(.,0))

for (i in 1:10){
  as2$genotypes[as2$genotypes==i]=cultivar[i]  
  mts2$genotypes[mts2$genotypes==i]=cultivar[i]  
  sts2$genotypes[sts2$genotypes==i]=cultivar[i]  
  ets2$genotypes[ets2$genotypes==i]=cultivar[i]  
  
  ## yld
  asyld2$genotypes[asyld2$genotypes==i]=cultivar[i]  
  mtsyld2$genotypes[mtsyld2$genotypes==i]=cultivar[i]  
  stsyld2$genotypes[stsyld2$genotypes==i]=cultivar[i]  
  etsyld2$genotypes[etsyld2$genotypes==i]=cultivar[i]  
}

for (i in 1:11){
  as2$corr[as2$corr==i]=his$MBPA[i]  
  mts2$corr[mts2$corr==i]=his$MBPA[i]  
  sts2$corr[sts2$corr==i]=his$MBPA[i] 
  ets2$corr[ets2$corr==i]=his$MBPA[i]  
  
  ## yld
  asyld2$corr[asyld2$corr==i]=his$MBPA[i]  
  mtsyld2$corr[mtsyld2$corr==i]=his$MBPA[i]  
  stsyld2$corr[stsyld2$corr==i]=his$MBPA[i] 
  etsyld2$corr[etsyld2$corr==i]=his$MBPA[i]  
}

colnames(asyld2) <- c('genotypes','corr','a50','a75','a100')
colnames(mtsyld2) <- c('genotypes','corr','m50','m75','m100')
colnames(stsyld2) <- c('genotypes','corr','s50','s75','s100')
colnames(etsyld2) <- c('genotypes','corr','e50','e75','e100')

comb <- list(); comb_yld <- list(); comb_gen <- list(); comb_gen_yld <- list()
for (pp in 1:10){
  for (i in 1:11){
    as3 <- as2 %>% filter(genotypes==cultivar[pp] & corr==his$MBPA[i])
    mts3 <- mts2  %>% filter(genotypes==cultivar[pp] & corr==his$MBPA[i])
    sts3 <- sts2  %>% filter(genotypes==cultivar[pp] & corr==his$MBPA[i])
    ets3 <- ets2 %>% filter(genotypes==cultivar[pp] & corr==his$MBPA[i])
    comb[[i]] <- data.frame(genotypes=as3$genotypes, corr= as3$corr,
                            a1=as3$bfas50, a2=as3$bfas75,a3=as3$bfas100,
                            m1=mts3$bfmts50,m2=mts3$bfmts75,m3=mts3$bfmts100,
                            s1=sts3$bfsts50,s2=sts3$bfsts75,s3=sts3$bfsts100,
                            e1=ets3$bfets50,e2=ets3$bfets75,e3=ets3$bfets100)
    ### yield
    asyld3 <- asyld2 %>% filter(genotypes==cultivar[pp] & corr==his$MBPA[i])
    mtsyld3 <- mtsyld2  %>% filter(genotypes==cultivar[pp] & corr==his$MBPA[i])
    stsyld3 <- stsyld2  %>% filter(genotypes==cultivar[pp] & corr==his$MBPA[i])
    etsyld3 <- etsyld2 %>% filter(genotypes==cultivar[pp] & corr==his$MBPA[i])
    comb_yld[[i]] <- data.frame(genotypes=asyld3$genotypes, corr= asyld3$corr,
                                a1=asyld3$a50,a2=asyld3$a75,a3=asyld3$a100,
                                m1=mtsyld3$m50,m2=mtsyld3$m75,m3=mtsyld3$m100,
                                s1=stsyld3$s50,s2=stsyld3$s75,s3=stsyld3$s100,
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

comb_gen2 <- comb_gen1[,3:14]
comb_gen2 <- comb_gen2*100
fin_comb2 <- data.frame(genotypes=comb_gen1$genotypes,corr=comb_gen1$corr,
                                comb_gen2)

comb_gen_yld1 <- comb_gen_yld1[order(comb_gen_yld1$corr),] %>% mutate_if(is.numeric, ~round(.,0))
comb_gen_yld1[is.na(comb_gen_yld1)] <- 0

write.csv(fin_comb2, 'E:/Prakash/dssat_out_culti/tanzania/stress_spat_time_comb_gen.csv')
write.csv(comb_gen_yld1, 'E:/Prakash/dssat_out_culti/tanzania/yld_spat_time_comb_gen.csv')

