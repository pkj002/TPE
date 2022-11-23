yld_ana_with_season<-function(pp,comb,path){
  md1<- readRDS(paste0(path,'yld_tpe_new_dssat_runs_ethiopia_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
  #md1<-readRDS(paste0(path,'yld_tpe_ethiopia_obs_new.RDS'))
  df2 <- map_df(md1, ~as.data.frame(.x), .id="grids")
  
  ## First analyze grid points with 2 seasons (main + belg)
  belg<-readRDS('//catalogue/BaseLineDataCluster01/temp/dapa_dssat_outputs/ethiopia/Belg_sow.RDS')
  
  off<-list()
  for (i in 1:length(belg)){
    off[[i]]<- df2 %>% filter(grids==belg[i])
  }
  off_df<-map_df(off, ~as.data.frame(.x), .id = 'grids')
  
  yield<-list()
  yld_av1<-list()
  yld_av2<-list()
  df<-list()
  
  for ( i in 1:length(belg)){
    yield[[i]]<-off_df %>% filter(grids==i) %>% dplyr::select(grids,TN, HWAMS) %>% group_by(grids, TN) %>%
      summarize(mean=mean(HWAMS, na.rm=T))
    
    if (nrow(yield[[i]])==9){
      yld_av1[i]<-round(mean(yield[[i]]$mean[1:7],na.rm=T),1)
      yld_av2[i]<-round(mean(yield[[i]]$mean[8:9],na.rm=T),1)
    } else if(nrow(yield[[i]])==5) {
      yld_av1[i]<-round(mean(yield[[i]]$mean[1:3],na.rm=T),1)
      yld_av2[i]<-round(mean(yield[[i]]$mean[4:5],na.rm=T),1)
    }  else {
      yld_av1[i]<-round(mean(yield[[i]]$mean[1:2],na.rm=T),1)
      yld_av2[i]<-round(mean(yield[[i]]$mean[3:4],na.rm=T),1)
    }
    
    df[[i]]<-cbind(yld_av1[i],yld_av2[i])
    colnames(df[[i]])<-c('yld_seas2_1','yld_seas2_2')
    print(i)
  }
  
  df_final<-map_df(df, ~as.data.frame(.x), .id = 'grids')
  df_final$grids<-belg
  df_final$yld_seas2_1[df_final$yld_seas2_1=='NaN']=NA
  df_final$yld_seas2_2[df_final$yld_seas2_2=='NaN']=NA
  
  df_final1<-df_final %>% complete(grids = full_seq(grids, 1))
  df_final1$yld_seas2_1[df_final1$yld_seas2_1=='NULL']<-NA
  df_final1$yld_seas2_2[df_final1$yld_seas2_2=='NULL']<-NA
  
  top<-data.frame(grids=1:2326, yld_seas2_1=NA, yld_seas2_2=NA)
  bot<-data.frame(grids=35775:len, yld_seas2_1=NA, yld_seas2_2=NA)
  main_fina2<-rbind(top,df_final1,bot)
  
  saveRDS(main_fina2[,c(1,3)],paste0(path,'yld_2seas_ethiopia_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS')) 
  
  ## Now analyze regions with only one season
  ## take out grids with 2 seasons
  
  #main_gr<-readRDS('//dapadfs/workspace_cluster_12/AVISA/dssat_outputs/ethiopia/main_sow_grids.RDS')
   main_gr<-readRDS('//catalogue/BaseLineDataCluster01/temp/dapa_dssat_outputs/ethiopia/main_sow_grids.RDS')
  
  yld_av<-list()
  
  for ( i in 1:len){
    yield[[i]]<-df2 %>% filter(grids==i) %>% dplyr::select(grids,TN, HWAMS) %>% group_by(grids, TN) %>%
      summarize(mean=mean(HWAMS, na.rm=T))
    if (nrow(yield[[i]])==0){
      next
    } else {
      yld_av[i]<-round(mean(yield[[i]]$mean,na.rm=T),1)
    }
    print(i)
  }
  
  main_final<-map_df(yld_av, ~as.data.frame(.x), .id = 'grids')
  colnames(main_final)<-c('grids','yld')
  main_final$grids<-as.numeric(as.character(main_final$grids))
  
  main_final<-main_final %>% complete(grids = full_seq(grids, 1))
  
  top<-data.frame(grids=1:2326,yld=NA)
  bot<-data.frame(grids=52477:len,yld=NA)
  main_fina2<-rbind(top,main_final,bot)
  
  ## change the value of main season from the value of season 2_1. Because the average of main season is the average
  ## of both season. 
  for (n in 1:length(belg)){
    main_fina2[which(main_fina2$grids==belg[n]),2]<-df_final1[which(df_final1$grids==belg[n]),2]
  }
  
  saveRDS(main_fina2,paste0(path,'yld_main_seas_ethiopia_',comb$mod[pp],'_ssp_',comb$sc[pp],'_',comb$year[pp],'.RDS'))
}

#q(save='no')
