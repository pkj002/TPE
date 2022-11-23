
rm(list=ls())
library(dplyr)
library(ggplot2)

path<- '//catalogue/BaseLineDataCluster01/temp/'
ug<-readRDS(paste0(path,'tmin_area_stress_corridor_ug.RDS'))
et<-readRDS(paste0(path,'tmin_area_stress_corridor_ethi.RDS'))
tz<-readRDS(paste0(path,'tmin_area_stress_corridor_tz.RDS'))


ug1<- ug %>% rowwise %>% mutate(Hist_tc=
                                  ifelse(Hist_t<=15,'VL',
                                         ifelse(Hist_t>15 & Hist_t<=18,'L',
                                                ifelse(Hist_t>18 & Hist_t<=20,'M',
                                                       ifelse(Hist_t>20 & Hist_t<=22,'H',
                                                              ifelse(Hist_t>22 & Hist_t<=25,'VH',
                                                                     ifelse(Hist_t>25,'EH')))))),
                                
                                ssp1_30_tc=ifelse(ssp1_30_t<=15,'VL',
                                                  ifelse(ssp1_30_t>15 & ssp1_30_t<=18,'L',
                                                         ifelse(ssp1_30_t>18 & ssp1_30_t<=20,'M',
                                                                ifelse(ssp1_30_t>20 & ssp1_30_t<=22,'H',
                                                                       ifelse(ssp1_30_t>22 & ssp1_30_t<=25,'VH',
                                                                              ifelse(ssp1_30_t>25,'EH')))))),
                                ssp2_30_tc=ifelse(ssp2_30_t<=15,'VL',
                                                  ifelse(ssp2_30_t>15 & ssp2_30_t<=18,'L',
                                                         ifelse(ssp2_30_t>18 & ssp2_30_t<=20,'M',
                                                                ifelse(ssp2_30_t>20 & ssp2_30_t<=22,'H',
                                                                       ifelse(ssp2_30_t>22 & ssp2_30_t<=25,'VH',
                                                                              ifelse(ssp2_30_t>25,'EH')))))),
                                ssp3_30_tc=ifelse(ssp3_30_t<=15,'VL',
                                                  ifelse(ssp3_30_t>15 & ssp3_30_t<=18,'L',
                                                         ifelse(ssp3_30_t>18 & ssp3_30_t<=20,'M',
                                                                ifelse(ssp3_30_t>20 & ssp3_30_t<=22,'H',
                                                                       ifelse(ssp3_30_t>22 & ssp3_30_t<=25,'VH',
                                                                              ifelse(ssp3_30_t>25,'EH')))))),
                                ssp4_30_tc=ifelse(ssp4_30_t<=15,'VL',
                                                  ifelse(ssp4_30_t>15 & ssp4_30_t<=18,'L',
                                                         ifelse(ssp4_30_t>18 & ssp4_30_t<=20,'M',
                                                                ifelse(ssp4_30_t>20 & ssp4_30_t<=22,'H',
                                                                       ifelse(ssp4_30_t>22 & ssp4_30_t<=25,'VH',
                                                                              ifelse(ssp4_30_t>25,'EH')))))),
                                ssp1_50_tc=ifelse(ssp1_50_t<=15,'VL',
                                                  ifelse(ssp1_50_t>15 & ssp1_50_t<=18,'L',
                                                         ifelse(ssp1_50_t>18 & ssp1_50_t<=20,'M',
                                                                ifelse(ssp1_50_t>20 & ssp1_50_t<=22,'H',
                                                                       ifelse(ssp1_50_t>22 & ssp1_50_t<=25,'VH',
                                                                              ifelse(ssp1_50_t>25,'EH')))))),
                                ssp2_50_tc=ifelse(ssp2_50_t<=15,'VL',
                                                  ifelse(ssp2_50_t>15 & ssp2_50_t<=18,'L',
                                                         ifelse(ssp2_50_t>18 & ssp2_50_t<=20,'M',
                                                                ifelse(ssp2_50_t>20 & ssp2_50_t<=22,'H',
                                                                       ifelse(ssp2_50_t>22 & ssp2_50_t<=25,'VH',
                                                                              ifelse(ssp2_50_t>25,'EH')))))),
                                ssp3_50_tc=ifelse(ssp3_50_t<=15,'VL',
                                                  ifelse(ssp3_50_t>15 & ssp3_50_t<=18,'L',
                                                         ifelse(ssp3_50_t>18 & ssp3_50_t<=20,'M',
                                                                ifelse(ssp3_50_t>20 & ssp3_50_t<=22,'H',
                                                                       ifelse(ssp3_50_t>22 & ssp3_50_t<=25,'VH',
                                                                              ifelse(ssp3_50_t>25,'EH')))))),
                                
                                ssp4_50_tc=ifelse(ssp4_50_t<=15,'VL',
                                                  ifelse(ssp4_50_t>15 & ssp4_50_t<=18,'L',
                                                         ifelse(ssp4_50_t>18 & ssp4_50_t<=20,'M',
                                                                ifelse(ssp4_50_t>20 & ssp4_50_t<=22,'H',
                                                                       ifelse(ssp4_50_t>22 & ssp4_50_t<=25,'VH',
                                                                              ifelse(ssp4_50_t>25,'EH')))))))



ug1<- ug1 %>% dplyr::select(country,clus,Production_hub,Hist_a,Hist_tc,ssp1_30_a,ssp1_30_tc,ssp2_30_a,ssp2_30_tc,ssp3_30_a,ssp3_30_tc,
                            ssp4_30_a,ssp4_30_tc,ssp1_50_a,ssp1_50_tc,ssp2_50_a,ssp2_50_tc,ssp3_50_a,ssp3_50_tc,ssp4_50_a,ssp4_50_tc)


et1<- et %>% rowwise %>% mutate(Hist_tc=
                                  ifelse(Hist_t<=15,'VL',
                                         ifelse(Hist_t>15 & Hist_t<=18,'L',
                                                ifelse(Hist_t>18 & Hist_t<=20,'M',
                                                       ifelse(Hist_t>20 & Hist_t<=22,'H',
                                                              ifelse(Hist_t>22 & Hist_t<=25,'VH',
                                                                     ifelse(Hist_t>25,'EH')))))),
                                
                                ssp1_30_tc=ifelse(ssp1_30_t<=15,'VL',
                                                  ifelse(ssp1_30_t>15 & ssp1_30_t<=18,'L',
                                                         ifelse(ssp1_30_t>18 & ssp1_30_t<=20,'M',
                                                                ifelse(ssp1_30_t>20 & ssp1_30_t<=22,'H',
                                                                       ifelse(ssp1_30_t>22 & ssp1_30_t<=25,'VH',
                                                                              ifelse(ssp1_30_t>25,'EH')))))),
                                ssp2_30_tc=ifelse(ssp2_30_t<=15,'VL',
                                                  ifelse(ssp2_30_t>15 & ssp2_30_t<=18,'L',
                                                         ifelse(ssp2_30_t>18 & ssp2_30_t<=20,'M',
                                                                ifelse(ssp2_30_t>20 & ssp2_30_t<=22,'H',
                                                                       ifelse(ssp2_30_t>22 & ssp2_30_t<=25,'VH',
                                                                              ifelse(ssp2_30_t>25,'EH')))))),
                                ssp3_30_tc=ifelse(ssp3_30_t<=15,'VL',
                                                  ifelse(ssp3_30_t>15 & ssp3_30_t<=18,'L',
                                                         ifelse(ssp3_30_t>18 & ssp3_30_t<=20,'M',
                                                                ifelse(ssp3_30_t>20 & ssp3_30_t<=22,'H',
                                                                       ifelse(ssp3_30_t>22 & ssp3_30_t<=25,'VH',
                                                                              ifelse(ssp3_30_t>25,'EH')))))),
                                ssp4_30_tc=ifelse(ssp4_30_t<=15,'VL',
                                                  ifelse(ssp4_30_t>15 & ssp4_30_t<=18,'L',
                                                         ifelse(ssp4_30_t>18 & ssp4_30_t<=20,'M',
                                                                ifelse(ssp4_30_t>20 & ssp4_30_t<=22,'H',
                                                                       ifelse(ssp4_30_t>22 & ssp4_30_t<=25,'VH',
                                                                              ifelse(ssp4_30_t>25,'EH')))))),
                                ssp1_50_tc=ifelse(ssp1_50_t<=15,'VL',
                                                  ifelse(ssp1_50_t>15 & ssp1_50_t<=18,'L',
                                                         ifelse(ssp1_50_t>18 & ssp1_50_t<=20,'M',
                                                                ifelse(ssp1_50_t>20 & ssp1_50_t<=22,'H',
                                                                       ifelse(ssp1_50_t>22 & ssp1_50_t<=25,'VH',
                                                                              ifelse(ssp1_50_t>25,'EH')))))),
                                ssp2_50_tc=ifelse(ssp2_50_t<=15,'VL',
                                                  ifelse(ssp2_50_t>15 & ssp2_50_t<=18,'L',
                                                         ifelse(ssp2_50_t>18 & ssp2_50_t<=20,'M',
                                                                ifelse(ssp2_50_t>20 & ssp2_50_t<=22,'H',
                                                                       ifelse(ssp2_50_t>22 & ssp2_50_t<=25,'VH',
                                                                              ifelse(ssp2_50_t>25,'EH')))))),
                                ssp3_50_tc=ifelse(ssp3_50_t<=15,'VL',
                                                  ifelse(ssp3_50_t>15 & ssp3_50_t<=18,'L',
                                                         ifelse(ssp3_50_t>18 & ssp3_50_t<=20,'M',
                                                                ifelse(ssp3_50_t>20 & ssp3_50_t<=22,'H',
                                                                       ifelse(ssp3_50_t>22 & ssp3_50_t<=25,'VH',
                                                                              ifelse(ssp3_50_t>25,'EH')))))),
                                
                                ssp4_50_tc=ifelse(ssp4_50_t<=15,'VL',
                                                  ifelse(ssp4_50_t>15 & ssp4_50_t<=18,'L',
                                                         ifelse(ssp4_50_t>18 & ssp4_50_t<=20,'M',
                                                                ifelse(ssp4_50_t>20 & ssp4_50_t<=22,'H',
                                                                       ifelse(ssp4_50_t>22 & ssp4_50_t<=25,'VH',
                                                                              ifelse(ssp4_50_t>25,'EH')))))))



et1<- et1 %>% dplyr::select(country,clus,Production_hub,Hist_a,Hist_tc,ssp1_30_a,ssp1_30_tc,ssp2_30_a,ssp2_30_tc,ssp3_30_a,ssp3_30_tc,
                            ssp4_30_a,ssp4_30_tc,ssp1_50_a,ssp1_50_tc,ssp2_50_a,ssp2_50_tc,ssp3_50_a,ssp3_50_tc,ssp4_50_a,ssp4_50_tc)

tz1<- tz %>% rowwise %>% mutate(Hist_tc=
                                  ifelse(Hist_t<=15,'VL',
                                         ifelse(Hist_t>15 & Hist_t<=18,'L',
                                                ifelse(Hist_t>18 & Hist_t<=20,'M',
                                                       ifelse(Hist_t>20 & Hist_t<=22,'H',
                                                              ifelse(Hist_t>22 & Hist_t<=25,'VH',
                                                                     ifelse(Hist_t>25,'EH')))))),
                                
                                ssp1_30_tc=ifelse(ssp1_30_t<=15,'VL',
                                                  ifelse(ssp1_30_t>15 & ssp1_30_t<=18,'L',
                                                         ifelse(ssp1_30_t>18 & ssp1_30_t<=20,'M',
                                                                ifelse(ssp1_30_t>20 & ssp1_30_t<=22,'H',
                                                                       ifelse(ssp1_30_t>22 & ssp1_30_t<=25,'VH',
                                                                              ifelse(ssp1_30_t>25,'EH')))))),
                                ssp2_30_tc=ifelse(ssp2_30_t<=15,'VL',
                                                  ifelse(ssp2_30_t>15 & ssp2_30_t<=18,'L',
                                                         ifelse(ssp2_30_t>18 & ssp2_30_t<=20,'M',
                                                                ifelse(ssp2_30_t>20 & ssp2_30_t<=22,'H',
                                                                       ifelse(ssp2_30_t>22 & ssp2_30_t<=25,'VH',
                                                                              ifelse(ssp2_30_t>25,'EH')))))),
                                ssp3_30_tc=ifelse(ssp3_30_t<=15,'VL',
                                                  ifelse(ssp3_30_t>15 & ssp3_30_t<=18,'L',
                                                         ifelse(ssp3_30_t>18 & ssp3_30_t<=20,'M',
                                                                ifelse(ssp3_30_t>20 & ssp3_30_t<=22,'H',
                                                                       ifelse(ssp3_30_t>22 & ssp3_30_t<=25,'VH',
                                                                              ifelse(ssp3_30_t>25,'EH')))))),
                                ssp4_30_tc=ifelse(ssp4_30_t<=15,'VL',
                                                  ifelse(ssp4_30_t>15 & ssp4_30_t<=18,'L',
                                                         ifelse(ssp4_30_t>18 & ssp4_30_t<=20,'M',
                                                                ifelse(ssp4_30_t>20 & ssp4_30_t<=22,'H',
                                                                       ifelse(ssp4_30_t>22 & ssp4_30_t<=25,'VH',
                                                                              ifelse(ssp4_30_t>25,'EH')))))),
                                ssp1_50_tc=ifelse(ssp1_50_t<=15,'VL',
                                                  ifelse(ssp1_50_t>15 & ssp1_50_t<=18,'L',
                                                         ifelse(ssp1_50_t>18 & ssp1_50_t<=20,'M',
                                                                ifelse(ssp1_50_t>20 & ssp1_50_t<=22,'H',
                                                                       ifelse(ssp1_50_t>22 & ssp1_50_t<=25,'VH',
                                                                              ifelse(ssp1_50_t>25,'EH')))))),
                                ssp2_50_tc=ifelse(ssp2_50_t<=15,'VL',
                                                  ifelse(ssp2_50_t>15 & ssp2_50_t<=18,'L',
                                                         ifelse(ssp2_50_t>18 & ssp2_50_t<=20,'M',
                                                                ifelse(ssp2_50_t>20 & ssp2_50_t<=22,'H',
                                                                       ifelse(ssp2_50_t>22 & ssp2_50_t<=25,'VH',
                                                                              ifelse(ssp2_50_t>25,'EH')))))),
                                ssp3_50_tc=ifelse(ssp3_50_t<=15,'VL',
                                                  ifelse(ssp3_50_t>15 & ssp3_50_t<=18,'L',
                                                         ifelse(ssp3_50_t>18 & ssp3_50_t<=20,'M',
                                                                ifelse(ssp3_50_t>20 & ssp3_50_t<=22,'H',
                                                                       ifelse(ssp3_50_t>22 & ssp3_50_t<=25,'VH',
                                                                              ifelse(ssp3_50_t>25,'EH')))))),
                                
                                ssp4_50_tc=ifelse(ssp4_50_t<=15,'VL',
                                                  ifelse(ssp4_50_t>15 & ssp4_50_t<=18,'L',
                                                         ifelse(ssp4_50_t>18 & ssp4_50_t<=20,'M',
                                                                ifelse(ssp4_50_t>20 & ssp4_50_t<=22,'H',
                                                                       ifelse(ssp4_50_t>22 & ssp4_50_t<=25,'VH',
                                                                              ifelse(ssp4_50_t>25,'EH')))))))



tz1<- tz1 %>% dplyr::select(country,clus,Production_hub,Hist_a,Hist_tc,ssp1_30_a,ssp1_30_tc,ssp2_30_a,ssp2_30_tc,ssp3_30_a,ssp3_30_tc,
                            ssp4_30_a,ssp4_30_tc,ssp1_50_a,ssp1_50_tc,ssp2_50_a,ssp2_50_tc,ssp3_50_a,ssp3_50_tc,ssp4_50_a,ssp4_50_tc)

#country<- 'UGA'
#country<- 'TZA'
country<- 'ETH'
#country<- 'comb'

all<- rbind(ug1,et1,tz1)

if (country=='UGA'){
  ## For Uganda
  all <- all %>% data.frame() %>% filter(country=='UGA') 
} else if(country=='ETH'){
  all <- all %>% data.frame() %>% filter(country=='ETH') 
} else if(country=='TZA'){
  all <- all %>% data.frame() %>% filter(country=='TZA') 
} else {
  all <- all
}


histr=all %>% dplyr::select(Hist_a, Hist_tc) %>% group_by(Hist_tc) %>%
  summarise(tot_Hist_tc=sum(Hist_a))
histr=data.frame(histr,period='historical')
colnames(histr)=c('category','total','period')


s1_30=all %>% dplyr::select(ssp1_30_a, ssp1_30_tc) %>% group_by(ssp1_30_tc) %>%  summarise(tot_ssp1_30=sum(ssp1_30_a))
s2_30=all %>% dplyr::select(ssp2_30_a, ssp2_30_tc) %>% group_by(ssp2_30_tc) %>%  summarise(tot_ssp2_30=sum(ssp2_30_a))
s3_30=all %>% dplyr::select(ssp3_30_a, ssp3_30_tc) %>% group_by(ssp3_30_tc) %>%  summarise(totssp3_30_=sum(ssp3_30_a))
s4_30=all %>% dplyr::select(ssp4_30_a, ssp4_30_tc) %>% group_by(ssp4_30_tc) %>%  summarise(totssp4_30_=sum(ssp4_30_a))

s1_50=all %>% dplyr::select(ssp1_50_a, ssp1_50_tc) %>% group_by(ssp1_50_tc) %>%  summarise(tot_ssp1_50=sum(ssp1_50_a))
s2_50=all %>% dplyr::select(ssp2_50_a, ssp2_50_tc) %>% group_by(ssp2_50_tc) %>%  summarise(tot_ssp2_50=sum(ssp2_50_a))
s3_50=all %>% dplyr::select(ssp3_50_a, ssp3_50_tc) %>% group_by(ssp3_50_tc) %>%  summarise(totssp3_50_=sum(ssp3_50_a))
s4_50=all %>% dplyr::select(ssp4_50_a, ssp4_50_tc) %>% group_by(ssp4_50_tc) %>%  summarise(totssp4_50_=sum(ssp4_50_a))

s4_30=data.frame(s4_30,period='2030 (ssp5-8.5)')
colnames(s4_30)=c('category','total','period')
s4_50=data.frame(s4_50,period='2050 (ssp5-8.5)')
colnames(s4_50)=c('category','total','period')

df2<-rbind(histr,s4_30,s4_50)
df2 <- df2 %>% mutate(Total_Area  = total/1000) %>% dplyr::select(category,Total_Area,period)

if(country=='UGA'){
  no_val <- data.frame(category=c('VL','VH','VH','L'), Total_Area=c(0,0,0,0), period=c('2050 (ssp5-8.5)','historical','2030 (ssp5-8.5)', '2050 (ssp5-8.5)'))
} else if (country=='TZA') {  
  no_val <- data.frame(category=c('VL','VL','H','H'), Total_Area=c(0,0,0,0), 
                       period=c('2030 (ssp5-8.5)', '2050 (ssp5-8.5)', 'historical', '2030 (ssp5-8.5)'))
} else if (country=='ETH'){
  no_val <- data.frame(category=c('M'), Total_Area=c(0), 
                       period='historical')
} else {
  no_val <- data.frame(category=c('VH','VH'), Total_Area=c(0,0), period=c('historical','2030 (ssp5-8.5)'))
}

df2<- rbind(df2,no_val)


df2<-transform(df2,category=factor(category,levels=c('VL','L','M','H', 'VH','EH')),
               period=factor(period,levels=c('historical','2030 (ssp5-8.5)','2050 (ssp5-8.5)')))
df2$Total_Area <- as.numeric(as.vector(df2$Total_Area))

My_Theme = theme(legend.position='top', legend.text = element_text(color = "black", size = 20),
                 legend.title =element_blank(),
                 axis.text.y = element_text(size = 14),
                 axis.title.y = element_text(size = 16))

## For all country
plot<-ggplot(data=df2, aes(x=category, y=Total_Area, fill=period)) +
  geom_bar(stat="identity", position=position_dodge())+
  xlab('') + ylab('Total Area (000 ha)') + scale_fill_manual(values=c('#a1d99b','#9ecae1','#de2d26')) +
  ylim(0,700)+
  #scale_fill_brewer(palette=c('#a1d99b','#9ecae1','#de2d26'))+
  My_Theme
#theme_minimal()

ggsave(plot = plot, 
       filename = paste0(path,'Tmin_havest_area_6_cat_',country,'.png'), 
       width = 10, height = 10, units = 'in', dpi = 300) 

################################################################################################################################################
### the % of SF (all countries together) that has Tmin 18-20ºC and > 20ºC,  for historical and each future scenario
hhh<- all %>% as.data.frame() %>% filter(clus=='SF')
histr_SF=hhh %>% dplyr::select(Hist_a, Hist_tc) %>% group_by(Hist_tc) %>%
  summarise(tot_Hist_tc=sum(Hist_a))

histr_SF=data.frame(histr_SF,period='historical')
colnames(histr_SF)=c('category','total','period')

s1_30_SF=hhh %>% dplyr::select(ssp1_30_a, ssp1_30_tc) %>% group_by(ssp1_30_tc) %>%  summarise(tot_ssp1_30=sum(ssp1_30_a))
s2_30_SF=hhh %>% dplyr::select(ssp2_30_a, ssp2_30_tc) %>% group_by(ssp2_30_tc) %>%  summarise(tot_ssp2_30=sum(ssp2_30_a))
s3_30_SF=hhh %>% dplyr::select(ssp3_30_a, ssp3_30_tc) %>% group_by(ssp3_30_tc) %>%  summarise(totssp3_30_=sum(ssp3_30_a))
s4_30_SF=hhh %>% dplyr::select(ssp4_30_a, ssp4_30_tc) %>% group_by(ssp4_30_tc) %>%  summarise(totssp4_30_=sum(ssp4_30_a))

s1_50_SF=hhh %>% dplyr::select(ssp1_50_a, ssp1_50_tc) %>% group_by(ssp1_50_tc) %>%  summarise(tot_ssp1_50=sum(ssp1_50_a))
s2_50_SF=hhh %>% dplyr::select(ssp2_50_a, ssp2_50_tc) %>% group_by(ssp2_50_tc) %>%  summarise(tot_ssp2_50=sum(ssp2_50_a))
s3_50_SF=hhh %>% dplyr::select(ssp3_50_a, ssp3_50_tc) %>% group_by(ssp3_50_tc) %>%  summarise(totssp3_50_=sum(ssp3_50_a))
s4_50_SF=hhh %>% dplyr::select(ssp4_50_a, ssp4_50_tc) %>% group_by(ssp4_50_tc) %>%  summarise(totssp4_50_=sum(ssp4_50_a))


s1_30_SF=data.frame(s1_30_SF,period='2030_s1')
s2_30_SF=data.frame(s2_30_SF,period='2030_s2')
s3_30_SF=data.frame(s3_30_SF,period='2030_s3')
s4_30_SF=data.frame(s4_30_SF,period='2030_s4')

s1_50_SF=data.frame(s1_50_SF,period='2050_s1')
s2_50_SF=data.frame(s2_50_SF,period='2050_s2')
s3_50_SF=data.frame(s3_50_SF,period='2050_s3')
s4_50_SF=data.frame(s4_50_SF,period='2050_s4')

###################################################################################################
############## #	the % of terminal drought (i.e., sum of MTS+STS+ETS) that has Tmin > 22ºC, for historical and future scenario
hhh<- all %>% as.data.frame() %>% filter(clus %in% c('MTS','STS','ETS'))
histr_SF=hhh %>% dplyr::select(Hist_a, Hist_tc) %>% group_by(Hist_tc) %>%
  summarise(tot_Hist_tc=sum(Hist_a))

s1_30_SF=hhh %>% dplyr::select(ssp1_30_a, ssp1_30_tc) %>% group_by(ssp1_30_tc) %>%  summarise(tot_ssp1_30=sum(ssp1_30_a))
s2_30_SF=hhh %>% dplyr::select(ssp2_30_a, ssp2_30_tc) %>% group_by(ssp2_30_tc) %>%  summarise(tot_ssp2_30=sum(ssp2_30_a))
s3_30_SF=hhh %>% dplyr::select(ssp3_30_a, ssp3_30_tc) %>% group_by(ssp3_30_tc) %>%  summarise(totssp3_30_=sum(ssp3_30_a))
s4_30_SF=hhh %>% dplyr::select(ssp4_30_a, ssp4_30_tc) %>% group_by(ssp4_30_tc) %>%  summarise(totssp4_30_=sum(ssp4_30_a))

s1_50_SF=hhh %>% dplyr::select(ssp1_50_a, ssp1_50_tc) %>% group_by(ssp1_50_tc) %>%  summarise(tot_ssp1_50=sum(ssp1_50_a))
s2_50_SF=hhh %>% dplyr::select(ssp2_50_a, ssp2_50_tc) %>% group_by(ssp2_50_tc) %>%  summarise(tot_ssp2_50=sum(ssp2_50_a))
s3_50_SF=hhh %>% dplyr::select(ssp3_50_a, ssp3_50_tc) %>% group_by(ssp3_50_tc) %>%  summarise(totssp3_50_=sum(ssp3_50_a))
s4_50_SF=hhh %>% dplyr::select(ssp4_50_a, ssp4_50_tc) %>% group_by(ssp4_50_tc) %>%  summarise(totssp4_50_=sum(ssp4_50_a))


s1_30_SF=data.frame(s1_30_SF,period='2030_s1')
s2_30_SF=data.frame(s2_30_SF,period='2030_s2')
s3_30_SF=data.frame(s3_30_SF,period='2030_s3')
s4_30_SF=data.frame(s4_30_SF,period='2030_s4')

s1_50_SF=data.frame(s1_50_SF,period='2050_s1')
s2_50_SF=data.frame(s2_50_SF,period='2050_s2')
s3_50_SF=data.frame(s3_50_SF,period='2050_s3')
s4_50_SF=data.frame(s4_50_SF,period='2050_s4')

s1_30_SF$tot_ssp1_30[3]/sum(s1_30_SF$tot_ssp1_30)
s1_30_SF$tot_ssp1_30[1]/sum(s1_30_SF$tot_ssp1_30)


