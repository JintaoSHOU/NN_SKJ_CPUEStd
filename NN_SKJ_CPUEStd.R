library(nnet)
library(neuralnet)
library(MASS)
library(NeuralNetTools)
library(tidyverse)
library(ggplot2)
library(parallel)
library(maps)
library(devtools)
library(metR)
library(ggthemes)
library(grDevices) 
library(RColorBrewer)

setwd('D:\\data\\SKJ')
skjdata<-read.csv('SKJ_data.csv',T)
maxs<-apply(skjdata,2,max)
mins<-apply(skjdata,2,min)
skj_scaled<-as.data.frame(scale(skjdata,center = mins , scale =maxs - mins))
data<- skj_scaled[,c('Month','Lon','Lat', 'SST','SSS','O2','PP', 'ONIL','CPUE1','CPUE2','CPUE3','CPUE4')]
summary(data)
Dataset<-na.omit(data)

#Neural network model construction
for(i in 1:100)
{
  set.seed(1000)
  index<-sample(1:nrow(Dataset),round(0.7*nrow(Dataset)))
  train<-Dataset[index,]
  test<-Dataset[-index,]
  i<-names(train)
  output.names <- i[9:13]
  input.names <- i[1:8]
  SKJFormula <- as.formula(paste(paste(output.names, collapse = '+'), '~', paste(input.names, collapse = '+')))
  model1 <- neuralnet(SKJFormula, train, hidden=4, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model2 <- neuralnet(SKJFormula, train, hidden=5, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model3 <- neuralnet(SKJFormula, train, hidden=6, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model4 <- neuralnet(SKJFormula, train, hidden=7, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model5 <- neuralnet(SKJFormula, train, hidden=8, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model6 <- neuralnet(SKJFormula, train, hidden=9, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model7 <- neuralnet(SKJFormula, train, hidden=10, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model8 <- neuralnet(SKJFormula, train, hidden=11, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model9 <- neuralnet(SKJFormula, train, hidden=12, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model10 <- neuralnet(SKJFormula, train, hidden=13, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  predict1=as.data.frame(predict(model1,Dataset,na.rm=T))
  names(predict1)<-c('Pre_1','Pre_2','Pre_3','Pre_4','Pre_5')
  names(predict1)<-make.names(names(predict1))
  predict1<-cbind(predict1,Dataset[,9:13])
  defaultSummary<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict1$Pre_1,pred=predict1$CPUE1)),
                                       defaultSummary(data.frame(obs=predict1$Pre_2,pred=predict1$CPUE2)),
                                       defaultSummary(data.frame(obs=predict1$Pre_3,pred=predict1$CPUE3)),
                                       defaultSummary(data.frame(obs=predict1$Pre_4,pred=predict1$CPUE4)),
                                       defaultSummary(data.frame(obs=predict1$Pre_5,pred=predict1$CPUE5))))
  defaultSummary$type=c('CPUE1','CPUE2','CPUE3','CPUE4','CPUE5')
  defaultSummary$model='model1'
  defaultSummary$iter=10
  predict2=as.data.frame(predict(model2,Dataset,na.rm=T))
  names(predict2)<-c('Pre_1','Pre_2','Pre_3','Pre_4','Pre_5')
  names(predict2)<-make.names(names(predict2))
  predict2<-cbind(predict2,Dataset[,9:13])
  defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict2$Pre_1,pred=predict2$CPUE1)),
                                            defaultSummary(data.frame(obs=predict2$Pre_2,pred=predict2$CPUE2)),
                                            defaultSummary(data.frame(obs=predict2$Pre_3,pred=predict2$CPUE3)),
                                            defaultSummary(data.frame(obs=predict2$Pre_4,pred=predict2$CPUE4)),
                                            defaultSummary(data.frame(obs=predict2$Pre_5,pred=predict2$CPUE5))))
  defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4','CPUE5')
  defaultSummary_temp$model='model2'
  defaultSummary_temp$iter=10
  defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
  predict3=as.data.frame(predict(model3,Dataset,na.rm=T))
  names(predict3)<-c('Pre_1','Pre_2','Pre_3','Pre_4','Pre_5')
  names(predict3)<-make.names(names(predict3))
  predict3<-cbind(predict3,Dataset[,9:13])
  defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict3$Pre_1,pred=predict3$CPUE1)),
                                            defaultSummary(data.frame(obs=predict3$Pre_2,pred=predict3$CPUE2)),
                                            defaultSummary(data.frame(obs=predict3$Pre_3,pred=predict3$CPUE3)),
                                            defaultSummary(data.frame(obs=predict3$Pre_4,pred=predict3$CPUE4)),
                                            defaultSummary(data.frame(obs=predict3$Pre_5,pred=predict3$CPUE5))))
  defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4','CPUE5')
  defaultSummary_temp$model='model3'
  defaultSummary_temp$iter=10
  defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
  predict4=as.data.frame(predict(model4,Dataset,na.rm=T))
  names(predict4)<-c('Pre_1','Pre_2','Pre_3','Pre_4','Pre_5')
  names(predict4)<-make.names(names(predict4))
  predict4<-cbind(predict4,Dataset[,9:13])
  defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict4$Pre_1,pred=predict4$CPUE1)),
                                            defaultSummary(data.frame(obs=predict4$Pre_2,pred=predict4$CPUE2)),
                                            defaultSummary(data.frame(obs=predict4$Pre_3,pred=predict4$CPUE3)),
                                            defaultSummary(data.frame(obs=predict4$Pre_4,pred=predict4$CPUE4)),
                                            defaultSummary(data.frame(obs=predict4$Pre_5,pred=predict4$CPUE5))))
  defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4','CPUE5')
  defaultSummary_temp$model='model4'
  defaultSummary_temp$iter=10
  defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
  predict5=as.data.frame(predict(model5,Dataset,na.rm=T))
  names(predict5)<-c('Pre_1','Pre_2','Pre_3','Pre_4','Pre_5')
  names(predict5)<-make.names(names(predict5))
  predict5<-cbind(predict5,Dataset[,9:13])
  defaultSummary_temp<-as.data.frame(rbind(defaultSummary(data.frame(obs=predict5$Pre_1,pred=predict5$CPUE1)),
                                           defaultSummary(data.frame(obs=predict5$Pre_2,pred=predict5$CPUE2)),
                                           defaultSummary(data.frame(obs=predict5$Pre_3,pred=predict5$CPUE3)),
                                           defaultSummary(data.frame(obs=predict5$Pre_4,pred=predict5$CPUE4)),
                                           defaultSummary(data.frame(obs=predict5$Pre_5,pred=predict5$CPUE5))))
  defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4','CPUE5')
  defaultSummary_temp$model='model5'
  defaultSummary_temp$iter=10
  defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
  predict6=as.data.frame(predict(model6,Dataset,na.rm=T))
  names(predict6)<-c('Pre_1','Pre_2','Pre_3','Pre_4','Pre_5')
  names(predict6)<-make.names(names(predict6))
  predict6<-cbind(predict6,Dataset[,9:13])
  defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict5$Pre_1,pred=predict5$CPUE1)),
                                            defaultSummary(data.frame(obs=predict5$Pre_2,pred=predict5$CPUE2)),
                                            defaultSummary(data.frame(obs=predict5$Pre_3,pred=predict5$CPUE3)),
                                            defaultSummary(data.frame(obs=predict5$Pre_4,pred=predict5$CPUE4)),
                                            defaultSummary(data.frame(obs=predict5$Pre_5,pred=predict5$CPUE5))))
  defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4','CPUE5')
  defaultSummary_temp$model='model6'
  defaultSummary_temp$iter=10
  defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
  predict7=as.data.frame(predict(model7,Dataset,na.rm=T))
  names(predict7)<-c('Pre_1','Pre_2','Pre_3','Pre_4','Pre_5')
  names(predict7)<-make.names(names(predict7))
  predict7<-cbind(predict7,Dataset[,9:13])
  defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict7$CPUE1,pred=predict7$Pre_1)),
                                            defaultSummary(data.frame(obs=predict7$CPUE2,pred=predict7$Pre_2)),
                                            defaultSummary(data.frame(obs=predict7$CPUE3,pred=predict7$Pre_3)),
                                            defaultSummary(data.frame(obs=predict7$CPUE4,pred=predict7$Pre_4)),
                                            defaultSummary(data.frame(obs=predict7$CPUE5,pred=predict7$Pre_5))))
  defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4','CPUE5')
  defaultSummary_temp$model='model7'
  defaultSummary_temp$iter=10
  defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
  predict8=as.data.frame(predict(model8,Dataset,na.rm=T))
  names(predict8)<-c('Pre_1','Pre_2','Pre_3','Pre_4','Pre_5')
  names(predict8)<-make.names(names(predict8))
  predict8<-cbind(predict8,Dataset[,9:13])
  defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict8$CPUE1,pred=predict8$Pre_1)),
                                            defaultSummary(data.frame(obs=predict8$CPUE2,pred=predict8$Pre_2)),
                                            defaultSummary(data.frame(obs=predict8$CPUE3,pred=predict8$Pre_3)),
                                            defaultSummary(data.frame(obs=predict8$CPUE4,pred=predict8$Pre_4)),
                                            defaultSummary(data.frame(obs=predict8$CPUE5,pred=predict8$Pre_5))))
  defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4','CPUE5')
  defaultSummary_temp$model='model8'
  defaultSummary_temp$iter=10
  defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
  predict9=as.data.frame(predict(model9,Dataset,na.rm=T))
  names(predict9)<-c('Pre_1','Pre_2','Pre_3','Pre_4','Pre_5')
  names(predict9)<-make.names(names(predict9))
  predict9<-cbind(predict9,Dataset[,9:13])
  defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict9$CPUE1,pred=predict9$Pre_1)),
                                            defaultSummary(data.frame(obs=predict9$CPUE2,pred=predict9$Pre_2)),
                                            defaultSummary(data.frame(obs=predict9$CPUE3,pred=predict9$Pre_3)),
                                            defaultSummary(data.frame(obs=predict9$CPUE4,pred=predict9$Pre_4)),
                                            defaultSummary(data.frame(obs=predict9$CPUE5,pred=predict9$Pre_5))))
  defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4','CPUE5')
  defaultSummary_temp$model='model9'
  defaultSummary_temp$iter=10
  defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
  predict10=as.data.frame(predict(model10,Dataset,na.rm=T))
  names(predict10)<-c('Pre_1','Pre_2','Pre_3','Pre_4','Pre_5')
  names(predict10)<-make.names(names(predict10))
  predict10<-cbind(predict10,Dataset[,9:13])
  defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict10$CPUE1,pred=predict10$Pre_1)),
                                            defaultSummary(data.frame(obs=predict10$CPUE2,pred=predict10$Pre_2)),
                                            defaultSummary(data.frame(obs=predict10$CPUE3,pred=predict10$Pre_3)),
                                            defaultSummary(data.frame(obs=predict10$CPUE4,pred=predict10$Pre_4)),
                                            defaultSummary(data.frame(obs=predict10$CPUE5,pred=predict10$Pre_5))))
  defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4','CPUE5')
  defaultSummary_temp$model='model10'
  defaultSummary_temp$iter=10
  defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
  skj_net <- neuralnet(SKJFormula, train, hidden=13, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold=0.28)
}
skj_net<-model10 #Optimal model
summary(skj_net)
plotnet(skj_net)
olden(skj_net)
neuralweights(skj_net)
fulldata<-read.csv('TunaFGEnv_20210929.csv',T)
fulldata<-fulldata[,c('Year','Month','Lon','Lat','SST','SSS','O2','PP','ONIL')]
fulldata<-na.omit(fulldata)
fulldata <-fulldata[,c('Month','Lon','Lat','SST','SSS','O2','PP','ONIL')]
maxs<-apply(fulldata,2,max)
mins<-apply(fulldata,2,min)
full_scaled<-as.data.frame(scale(fulldata,center = mins , scale =maxs - mins))
summary(full_scaled)

#Optimal model prediction
predictSKJ<-predict(skj_net,full_scaled)
summary(predictSKJ)
predictSKJ1<-as.data.frame(predictSKJ)
colnames(predictSKJ1)<-c('CPUE1','CPUE2','CPUE3','CPUE4')
predictSKJ1$CPUE1<-(predictSKJ1$CPUE1-min(predictSKJ1$CPUE1))/(max(predictSKJ1$CPUE1)-min(predictSKJ1$CPUE1))
predictSKJ1$CPUE2<-(predictSKJ1$CPUE2-min(predictSKJ1$CPUE2))/(max(predictSKJ1$CPUE2)-min(predictSKJ1$CPUE2))
predictSKJ1$CPUE3<-(predictSKJ1$CPUE3-min(predictSKJ1$CPUE3))/(max(predictSKJ1$CPUE3)-min(predictSKJ1$CPUE3))
predictSKJ1$CPUE4<-(predictSKJ1$CPUE4-min(predictSKJ1$CPUE4))/(max(predictSKJ1$CPUE4)-min(predictSKJ1$CPUE4))
a=sum((skj_net[["weights"]][[1]][[2]][,1])^2)/(sum((skj_net[["weights"]][[1]][[2]][,1])^2)+sum((skj_net[["weights"]][[1]][[2]][,2])^2)+sum((skj_net[["weights"]][[1]][[2]][,3])^2)+sum((skj_net[["weights"]][[1]][[2]][,4])^2))
b=sum((skj_net[["weights"]][[1]][[2]][,2])^2)/(sum((skj_net[["weights"]][[1]][[2]][,1])^2)+sum((skj_net[["weights"]][[1]][[2]][,2])^2)+sum((skj_net[["weights"]][[1]][[2]][,3])^2)+sum((skj_net[["weights"]][[1]][[2]][,4])^2))
c=sum((skj_net[["weights"]][[1]][[2]][,3])^2)/(sum((skj_net[["weights"]][[1]][[2]][,1])^2)+sum((skj_net[["weights"]][[1]][[2]][,2])^2)+sum((skj_net[["weights"]][[1]][[2]][,3])^2)+sum((skj_net[["weights"]][[1]][[2]][,4])^2))
d=sum((skj_net[["weights"]][[1]][[2]][,4])^2)/(sum((skj_net[["weights"]][[1]][[2]][,1])^2)+sum((skj_net[["weights"]][[1]][[2]][,2])^2)+sum((skj_net[["weights"]][[1]][[2]][,3])^2)+sum((skj_net[["weights"]][[1]][[2]][,4])^2))
predictSKJ1$CPUE<predictSKJ1$CPUE1*a+predictSKJ1$CPUE2*b+predictSKJ1$CPUE3*c+predictSKJ1$CPUE4*d
fulldata$CPUE<-predictSKJ1$CPUE
fulldata$CPUE<-(fulldata$CPUE-min(fulldata$CPUE))/(max(fulldata$CPUE)-min(fulldata$CPUE))
summary(fulldata$CPUE)

#plot
colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(10)
newdata<-fulldata
worldMap <- fortify(map_data("world2"), region = "subregion")
sub1 = subset(newdata,newdata$Year>=1995)
ggplot() + 
  geom_contour_fill(data=sub1,aes(x=Lon,y=Lat,z=CPUE),binwidth=0.01)+
  scale_fill_gradientn(colors=colormap)+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='grey80',color='grey60',size=0.5,alpha=1)+
  coord_sf(xlim = c(120, 230),  ylim = c(-40, 40))+
  scale_x_longitude(breaks=seq(120,230,20))+
  scale_y_latitude(breaks=seq(-40,40,20))+ 
  facet_wrap(~Year,labeller = label_both,ncol = 4)+
  theme(strip.text = element_text(face = 'italic',size=8))+
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 9,angle=30,hjust = 1),
        axis.text.y = element_text(size = 9,angle=30,hjust = 1),
        axis.text = element_text(color='black'),
        panel.background = element_blank())+
  theme_bw()
