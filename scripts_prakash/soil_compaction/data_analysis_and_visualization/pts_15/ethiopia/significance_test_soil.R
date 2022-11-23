############## Available water ##############
aw1<- aw3 %>% filter(depth %in% c('layer_1','layer_2','layer_3'))

test3<-aov(as.numeric(as.character(aw)) ~ depth + stress, data=aw1)
summary(test3)

sts<- c('Stress free','Terminal moderate','Terminal severe','Flowering to Grain filling')

s<-list()
for (i in 1:4){
gg1<- aw1 %>% filter(stress==sts[i], depth=='layer_1')
gg2<- aw1 %>% filter(stress==sts[i], depth=='layer_2')
gg3<- aw1 %>% filter(stress==sts[i], depth=='layer_3')
s[[i]]<- c(mean(gg1$aw), mean(gg2$aw), mean(gg2$aw)) 
}

aww<-t(do.call(rbind,s))

aw<- data.frame(c('0-5cm', '5-15cm', '15-30cm'), aww, sources=c('depth','stress .','depthxstress'))
colnames(aw) <- c('Available water',sts,'Significant?')

################ clay ###
aw1 <- clay4 %>% filter(depth %in% c('layer_1','layer_2','layer_3'))
test3<-aov(as.numeric(as.character(value)) ~ depth + stress, data=clay4)
summary(test3)

c<-list()

for (i in 1:4){
  gg1<- aw1 %>% filter(stress==sts[i], depth=='layer_1')
  gg2<- aw1 %>% filter(stress==sts[i], depth=='layer_2')
  gg3<- aw1 %>% filter(stress==sts[i], depth=='layer_3')
  c[[i]]<- c(mean(gg1$value), mean(gg2$value), mean(gg2$value)) 
}
  
cww<-t(do.call(rbind,c))
cl<- data.frame(c('0-5cm', '5-15cm', '15-30cm'), cww, sources=c('depth ***','stress **','depthxstress'))
colnames(cl) <- c('clay',sts,'Significant?')

## sand aw1 <- sand3 %>% filter(depth %in% c('layer_1','layer_2','layer_3'))
test3<-aov(as.numeric(as.character(value)) ~ depth + stress, data=aw1)
summary(test3)


s <- list()

for (i in 1:4){
  gg1<- aw1 %>% filter(stress==sts[i], depth=='layer_1')
  gg2<- aw1 %>% filter(stress==sts[i], depth=='layer_2')
  gg3<- aw1 %>% filter(stress==sts[i], depth=='layer_3')
  s[[i]]<- c(mean(gg1$value), mean(gg2$value), mean(gg2$value)) 
}

sww<- t(do.call(rbind,s))
sd<- data.frame(c('0-5cm', '5-15cm', '15-30cm'), sww, sources=c('depth .','stress','depthxstress'))
colnames(sd) <- c('clay',sts,'Significant?')

## bulk density
aw1 <- bd3 %>% filter(depth %in% c('layer_1','layer_2','layer_3'))
test3<-aov(as.numeric(as.character(bd)) ~ depth + stress, data=aw1)
summary(test3)

db<-list()
for (i in 1:4){
  gg1<- aw1 %>% filter(stress==sts[i], depth=='layer_1')
  gg2<- aw1 %>% filter(stress==sts[i], depth=='layer_2')
  gg3<- aw1 %>% filter(stress==sts[i], depth=='layer_3')
  db[[i]]<- c(mean(gg1$bd), mean(gg2$bd), mean(gg2$bd)) 
}

sdb<- t(do.call(rbind,db))
bd<- data.frame(c('0-5cm', '5-15cm', '15-30cm'), sdb, sources=c('depth *','stress***','depthxstress'))
colnames(bd) <- c('clay',sts,'Significant?')

write.csv(cl, 'E:/Prakash/dssat_outputs/ethiopia/cl.csv')
write.csv(sd, 'E:/Prakash/dssat_outputs/ethiopia/sd.csv')
write.csv(bd, 'E:/Prakash/dssat_outputs/ethiopia/bd.csv')
write.csv(aw, 'E:/Prakash/dssat_outputs/ethiopia/aw.csv')


