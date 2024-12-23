library(nnet)
library(neuralnet)
library(caret)
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
library(eoffice)
library(ggpubr)
library(sf)
library(mgcv)
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Arial"))

skjdata<-read.csv('SKJ_data.csv',T)
maxs<-apply(skjdata,2,max)
mins<-apply(skjdata,2,min)
skj_scaled<-as.data.frame(scale(skjdata,center = mins , scale =maxs - mins))
data<- skj_scaled[,c('Month','Lon','Lat', 'SST','SSS','O2','PP', 'ONIL','CPUE1','CPUE2','CPUE3','CPUE4')]
summary(data)
Dataset<-na.omit(data)

set.seed(1234)
for(i in 1:100)
{
  index<-sample(1:nrow(Dataset),round(0.7*nrow(Dataset)))
  train<-Dataset[index,]
  test<-Dataset[-index,]
  i<-names(train)
  output.names <- i[9:12]
  input.names <- i[1:8]
  SKJFormula <- as.formula(paste(paste(output.names, collapse = '+'), '~', paste(input.names, collapse = '+')))
  model1 <- neuralnet(SKJFormula, train, hidden=1, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model2 <- neuralnet(SKJFormula, train, hidden=2, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model3 <- neuralnet(SKJFormula, train, hidden=3, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model4 <- neuralnet(SKJFormula, train, hidden=4, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model5 <- neuralnet(SKJFormula, train, hidden=5, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model6 <- neuralnet(SKJFormula, train, hidden=6, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model7 <- neuralnet(SKJFormula, train, hidden=7, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model8 <- neuralnet(SKJFormula, train, hidden=8, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model9 <- neuralnet(SKJFormula, train, hidden=9, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model10 <- neuralnet(SKJFormula, train, hidden=10, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model11 <- neuralnet(SKJFormula, train, hidden=11, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model12 <- neuralnet(SKJFormula, train, hidden=12, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model13 <- neuralnet(SKJFormula, train, hidden=13, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model14 <- neuralnet(SKJFormula, train, hidden=14, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model15 <- neuralnet(SKJFormula, train, hidden=15, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model16 <- neuralnet(SKJFormula, train, hidden=16, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
  model17 <- neuralnet(SKJFormula, train, hidden=17, act.fct="tanh", linear.output=T, algorithm = "rprop+",lifesign = "full",threshold  = 0.28)
}
predict1=as.data.frame(predict(model1,Dataset,na.rm=T))
names(predict1)<-c('Pre_1','Pre_2','Pre_3','Pre_4')
names(predict1)<-make.names(names(predict1))
predict1<-cbind(predict1,Dataset[,10:13])
defaultSummary<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict1$Pre_1,pred=predict1$CPUE1)),
                                     defaultSummary(data.frame(obs=predict1$Pre_2,pred=predict1$CPUE2)),
                                     defaultSummary(data.frame(obs=predict1$Pre_3,pred=predict1$CPUE3)),
                                     defaultSummary(data.frame(obs=predict1$Pre_4,pred=predict1$CPUE4))))
defaultSummary$type=c('CPUE1','CPUE2','CPUE3','CPUE4')
defaultSummary$model='model1'
defaultSummary$iter=10
predict2=as.data.frame(predict(model2,Dataset,na.rm=T))
names(predict2)<-c('Pre_1','Pre_2','Pre_3','Pre_4')
names(predict2)<-make.names(names(predict2))
predict2<-cbind(predict2,Dataset[,10:13])
defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict2$Pre_1,pred=predict2$CPUE1)),
                                          defaultSummary(data.frame(obs=predict2$Pre_2,pred=predict2$CPUE2)),
                                          defaultSummary(data.frame(obs=predict2$Pre_3,pred=predict2$CPUE3)),
                                          defaultSummary(data.frame(obs=predict2$Pre_4,pred=predict2$CPUE4))))
defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4')
defaultSummary_temp$model='model2'
defaultSummary_temp$iter=10
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
predict3=as.data.frame(predict(model3,Dataset,na.rm=T))
names(predict3)<-c('Pre_1','Pre_2','Pre_3','Pre_4')
names(predict3)<-make.names(names(predict3))
predict3<-cbind(predict3,Dataset[,10:13])
defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict3$Pre_1,pred=predict3$CPUE1)),
                                          defaultSummary(data.frame(obs=predict3$Pre_2,pred=predict3$CPUE2)),
                                          defaultSummary(data.frame(obs=predict3$Pre_3,pred=predict3$CPUE3)),
                                          defaultSummary(data.frame(obs=predict3$Pre_4,pred=predict3$CPUE4))))
defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4')
defaultSummary_temp$model='model3'
defaultSummary_temp$iter=10
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
predict4=as.data.frame(predict(model4,Dataset,na.rm=T))
names(predict4)<-c('Pre_1','Pre_2','Pre_3','Pre_4')
names(predict4)<-make.names(names(predict4))
predict4<-cbind(predict4,Dataset[,10:13])
defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict4$Pre_1,pred=predict4$CPUE1)),
                                          defaultSummary(data.frame(obs=predict4$Pre_2,pred=predict4$CPUE2)),
                                          defaultSummary(data.frame(obs=predict4$Pre_3,pred=predict4$CPUE3)),
                                          defaultSummary(data.frame(obs=predict4$Pre_4,pred=predict4$CPUE4))))
defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4')
defaultSummary_temp$model='model4'
defaultSummary_temp$iter=10
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
predict5=as.data.frame(predict(model5,Dataset,na.rm=T))
names(predict5)<-c('Pre_1','Pre_2','Pre_3','Pre_4')
names(predict5)<-make.names(names(predict5))
predict5<-cbind(predict5,Dataset[,10:13])
defaultSummary_temp<-as.data.frame(rbind(defaultSummary(data.frame(obs=predict5$Pre_1,pred=predict5$CPUE1)),
                                         defaultSummary(data.frame(obs=predict5$Pre_2,pred=predict5$CPUE2)),
                                         defaultSummary(data.frame(obs=predict5$Pre_3,pred=predict5$CPUE3)),
                                         defaultSummary(data.frame(obs=predict5$Pre_4,pred=predict5$CPUE4))))
defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4')
defaultSummary_temp$model='model5'
defaultSummary_temp$iter=10
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
predict6=as.data.frame(predict(model6,Dataset,na.rm=T))
names(predict6)<-c('Pre_1','Pre_2','Pre_3','Pre_4')
names(predict6)<-make.names(names(predict6))
predict6<-cbind(predict6,Dataset[,10:13])
defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict6$Pre_1,pred=predict6$CPUE1)),
                                          defaultSummary(data.frame(obs=predict6$Pre_2,pred=predict6$CPUE2)),
                                          defaultSummary(data.frame(obs=predict6$Pre_3,pred=predict6$CPUE3)),
                                          defaultSummary(data.frame(obs=predict6$Pre_4,pred=predict6$CPUE4))))
defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4')
defaultSummary_temp$model='model6'
defaultSummary_temp$iter=10
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
predict7=as.data.frame(predict(model7,Dataset,na.rm=T))
names(predict7)<-c('Pre_1','Pre_2','Pre_3','Pre_4')
names(predict7)<-make.names(names(predict7))
predict7<-cbind(predict7,Dataset[,10:13])
defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict7$CPUE1,pred=predict7$Pre_1)),
                                          defaultSummary(data.frame(obs=predict7$CPUE2,pred=predict7$Pre_2)),
                                          defaultSummary(data.frame(obs=predict7$CPUE3,pred=predict7$Pre_3)),
                                          defaultSummary(data.frame(obs=predict7$CPUE4,pred=predict7$Pre_4))))
defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4')
defaultSummary_temp$model='model7'
defaultSummary_temp$iter=10
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
predict8=as.data.frame(predict(model8,Dataset,na.rm=T))
names(predict8)<-c('Pre_1','Pre_2','Pre_3','Pre_4')
names(predict8)<-make.names(names(predict8))
predict8<-cbind(predict8,Dataset[,10:13])
defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict8$CPUE1,pred=predict8$Pre_1)),
                                          defaultSummary(data.frame(obs=predict8$CPUE2,pred=predict8$Pre_2)),
                                          defaultSummary(data.frame(obs=predict8$CPUE3,pred=predict8$Pre_3)),
                                          defaultSummary(data.frame(obs=predict8$CPUE4,pred=predict8$Pre_4))))
defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4')
defaultSummary_temp$model='model8'
defaultSummary_temp$iter=10
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
predict9=as.data.frame(predict(model9,Dataset,na.rm=T))
names(predict9)<-c('Pre_1','Pre_2','Pre_3','Pre_4')
names(predict9)<-make.names(names(predict9))
predict9<-cbind(predict9,Dataset[,10:13])
defaultSummary_temp<-as.data.frame(rbind( defaultSummary(data.frame(obs=predict9$CPUE1,pred=predict9$Pre_1)),
                                          defaultSummary(data.frame(obs=predict9$CPUE2,pred=predict9$Pre_2)),
                                          defaultSummary(data.frame(obs=predict9$CPUE3,pred=predict9$Pre_3)),
                                          defaultSummary(data.frame(obs=predict9$CPUE4,pred=predict9$Pre_4))))
defaultSummary_temp$type=c('CPUE1','CPUE2','CPUE3','CPUE4')
defaultSummary_temp$model='model9'
defaultSummary_temp$iter=10
defaultSummary<-rbind(defaultSummary,defaultSummary_temp)
......

####plot MAERMSE
BO<-read.csv('CPUE1MAE.csv',T)
BOX<-gather(BO,Model,Value,model1:model20)
png(filename = "CPUE1MAE.png",width = 1000,height = 1000, res = 300)
ggboxplot(BOX,  "Model","Value",orientation = "horizontal",
          color = "black",
          fill = "Model", palette = c("#00AFBB", "#CAE99B", "#FC4E07","#6FB1E3", "#F67889", "#1274BB","#9564BC", "#CD456E", "#D6CC9D","#86AEEB", "#F1946B", "#4F9F86","#BE0B61","#9564BC", "#CD456E", "#D6CC9D","#86AEEB", "#F1946B", "#4F9F86","#BE0B61"))+
  guides(fill ="none")+ theme(legend.background = element_rect(fill = 'white', colour = 'black'))+
  theme(text =element_text(family ="A"))+xlab("")+ylab("MAE(a)")+
  theme(axis.line = element_line(colour = "black", size = 0.23),
        panel.border = element_rect(colour = "black", fill = NA, size = 1))
dev.off()

library(simputation)
fulldata<-read.csv('TunaFGEnv_20210929.csv',T)
fulldata<- impute_lm(fulldata,PP ~ SST + SSS+Lon+Lat+Year+Month)
fulldata<- impute_lm(fulldata,O2 ~ SST + SSS+Lon+Lat+Year+Month)
fulldata<-fulldata[,c('Year','Month','Lon','Lat','SST','SSS','O2','PP','ONIL')]
fulldata<-na.omit(fulldata)
maxs<-apply(fulldata,2,max)
mins<-apply(fulldata,2,min)
full_scaled<-as.data.frame(scale(fulldata,center = mins , scale =maxs - mins))

#Optimal model prediction
skj_net<-model12 #Optimal model
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
predictSKJ1$CPUE<-predictSKJ1$CPUE1*a+predictSKJ1$CPUE2*b+predictSKJ1$CPUE3*c+predictSKJ1$CPUE4*d
fulldata$CPUE<-predictSKJ1$CPUE
fulldata$CPUE<-(fulldata$CPUE-min(fulldata$CPUE))/(max(fulldata$CPUE)-min(fulldata$CPUE))
summary(fulldata$CPUE)
#plot
colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(10)
newdata<-fulldata
worldMap <- fortify(map_data("world2"), region = "subregion")
sub1 = subset(newdata,newdata$Year==1995)
ggplot() + 
  geom_contour_fill(data=sub1,aes(x=Lon,y=Lat,z=prelv),binwidth=0.01)+
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

##GAM
dff<-read.csv("SKJdata.csv",T)
df<-dff
df$CPUE1 <- log(df$CPUE1 + 1)
df$CPUE2 <- log(df$CPUE2 + 1)
df$CPUE3 <- log(df$CPUE3 + 1)
df$CPUE4 <- log(df$CPUE4 + 1)
gam_model1 <- gam(CPUE1~ s(SST) + s(SSS) + s(O2) + s(PP)+ s(SSTA)+
                    s(Lon, Lat) + Year + Month,
                  data = df,
                  method = "REML")
summary(gam_model1)
AIC(gam_model1)
plot(gam_model1, pages = 1, se = TRUE)
plot(gam_model1, pages=1, scheme=1, unconditional=TRUE) 
predictions1 <- predict(gam_model1, newdata = df, se.fit = TRUE)
df$pred1 <- predictions1$fit
df$se1 <- predictions1$se.fit

gam_model2 <- gam(CPUE2~ s(SST) + s(SSS) + s(O2) + s(PP)+ s(SSTA)+
                    s(Lon, Lat) + Year + Month,
                  data = df,
                  method = "REML")
summary(gam_model2)
AIC(gam_model2)
plot(gam_model2, pages = 1, se = TRUE)
plot(gam_model2, pages=1, scheme=1, unconditional=TRUE) 
predictions2 <- predict(gam_model2, newdata = df, se.fit = TRUE)
df$pred2 <- predictions2$fit
df$se2 <- predictions2$se.fit

gam_model3 <- gam(CPUE3~ s(SST) + s(SSS) + s(O2) + s(PP)+ s(SSTA)+
                    s(Lon, Lat) + Year + Month,
                  data = df,
                  method = "REML")
summary(gam_model3)
AIC(gam_model3)
plot(gam_model3, pages = 1, se = TRUE)
plot(gam_model3, pages=1, scheme=1, unconditional=TRUE) 
predictions3 <- predict(gam_model3, newdata = df, se.fit = TRUE)
df$pred3 <- predictions3$fit
df$se3 <- predictions3$se.fit

gam_model4 <- gam(CPUE4~ s(SST) + s(SSS) + s(O2) + s(PP)+ s(SSTA)+
                    s(Lon, Lat) + Year + Month,
                  data = df,
                  method = "REML")
summary(gam_model4)
AIC(gam_model4)
plot(gam_model4, pages = 1, se = TRUE)
plot(gam_model4, pages=1, scheme=1, unconditional=TRUE) 
predictions4 <- predict(gam_model4, newdata = df, se.fit = TRUE)
df$pred4 <- predictions4$fit
df$se4 <- predictions4$se.fit

df$pre<-df$pred1+df$pred2+df$pred3+df$pred4
df$pre<-(df$pre-min(df$pre))/(max(df$pre)-min(df$pre))

df$CPUE<-df$CPUE1+df$CPUE2+df$CPUE3+df$CPUE4
df$CPUE<-(df$CPUE-min(df$CPUE))/(max(df$CPUE)-min(df$CPUE))
mae_value <- mae(df$CPUE, df$pre)
rmse_value <- rmse(df$CPUE, df$pre)

ggplot(df, aes(x = Lon, y = Lat, color = pre)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Predicted BET_CPUE", x = "Longitude", y = "Latitude")

glm_model1 <- glm(CPUE1~ SST + SSS + O2 + PP+ SSTA+Lon+Lat + Year + Month,
                  data = df)
summary(glm_model1)
AIC(glm_model1)
plot(glm_model1, pages = 1, se = TRUE)
plot(glm_model1, pages=1, scheme=1, unconditional=TRUE) 
predictions11 <- predict(glm_model1, newdata = df, se.fit = TRUE)
df$pred11 <- predictions11$fit
df$se11 <- predictions11$se.fit

output_path <- "E:/SHOUNEW/article/ICES/GAM/"

jpeg(file = paste0(output_path, "residuals_vs_fitted_GLM1.jpg"), width = 1000, height = 1000, 
     units = "px", quality = 100,res = 200)
residuals_vs_fitted <- residuals(glm_model1, type = "pearson")
fitted_values <- fitted(glm_model1)
plot(fitted_values, residuals_vs_fitted,
     xlab = "Fitted Values", ylab = "Pearson Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red") 
dev.off()

jpeg(file = paste0(output_path, "normal_qq_plot_GLM1.jpg"), width = 1000, height = 1000, 
     units = "px", quality = 100,res = 200)
qqnorm(residuals(glm_model1, type = "pearson"), 
       main = "Normal Q-Q Plot of Pearson Residuals")
qqline(residuals(glm_model1, type = "pearson"), col = "red")
dev.off()

if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom")
library(broom)

tidy_model <- tidy(glm_model1, conf.int = TRUE)
jpeg(file = paste0(output_path, "coefficients_with_ci_GLM1.jpg"), width = 1000, height = 1000, 
     units = "px", quality = 100,res = 200)
dotchart(tidy_model$estimate, labels = tidy_model$term, 
         xlim = range(tidy_model$conf.low, tidy_model$conf.high),
         main = "Coefficient Estimates with 95% CI",
         xlab = "Estimate")
segments(tidy_model$conf.low, seq_along(tidy_model$term),
         tidy_model$conf.high, seq_along(tidy_model$term))
abline(v = 0, col = "gray", lty = 2) 
dev.off()


glm_model2 <- glm(CPUE2~ SST + SSS + O2 + PP+ SSTA+Lon+Lat + Year + Month,
                  data = df)
summary(glm_model2)
AIC(glm_model2)
plot(glm_model2, pages = 1, se = TRUE)
plot(glm_model2, pages=1, scheme=1, unconditional=TRUE) 
predictions21 <- predict(glm_model2, newdata = df, se.fit = TRUE)
df$pred21 <- predictions21$fit
df$se21 <- predictions21$se.fit

glm_model3 <- glm(CPUE3~ SST + SSS + O2 + PP+ SSTA+Lon+Lat + Year + Month,data = df)
summary(glm_model3)
AIC(glm_model3)
plot(glm_model3, pages = 1, se = TRUE)
plot(glm_model3, pages=1, scheme=1, unconditional=TRUE) 
predictions31 <- predict(glm_model3, newdata = df, se.fit = TRUE)
df$pred31 <- predictions31$fit
df$se3 <- predictions3$se.fit

glm_model4 <- glm(CPUE4~ SST + SSS + O2 + PP+ SSTA+Lon+Lat + Year + Month,
                  data = df)
summary(glm_model4)
AIC(glm_model4)
plot(glm_model4, pages = 1, se = TRUE)
plot(glm_model4, pages=1, scheme=1, unconditional=TRUE) 
predictions41 <- predict(glm_model4, newdata = df, se.fit = TRUE)
df$pred41 <- predictions41$fit
df$se4 <- predictions4$se.fit

df$preGLM<-df$pred11+df$pred21+df$pred31+df$pred41
df$preGLM<-(df$pre-min(df$preGLM))/(max(df$preGLM)-min(df$preGLM))