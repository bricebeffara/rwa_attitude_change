rm(list=ls())
library ("rprime")
setwd("~/Desktop/analysis_greebles_amelie_2016/filestxt")
myFiles <- list.files()

createDF <- function(myFiles) {
  XPlines <- read_eprime(myFiles)
  XPframes <- FrameList(XPlines)
  
  XPframes <- drop_levels(XPframes, 1)
  XPdf <- to_data_frame(XPframes)
  if ("Slide4.RESP" %in% names(XPdf)){
    XPdf$aware<-XPdf$Slide4.RESP
  } else {
    XPdf$aware<-XPdf$Slide2.RESP
  }
  columns_to_keep <- c("Eprime.Basename","CS", "Slide1.RESP", "aware","codage", "SDOquest.RESP","RWAquest.RESP","Running")
  greebles_df<-NULL
  greebles_df <- XPdf[,columns_to_keep]
  a<-which(!is.na(greebles_df$CS))
  b<-which(!is.na(greebles_df$codage))
  c<-which(!is.na(greebles_df$Slide1.RESP))
  d<-which(!is.na(greebles_df$aware))
  lines_to_keep = c(a,b,c,d)
  greebles_df_short <-greebles_df[lines_to_keep,]
  greebles_df_short$ordre<-greebles_df_short$Running[17]
  SDOscore<-greebles_df$SDOquest.RESP[which(!is.na(greebles_df$SDOquest.RESP))]
  SDOquest<-greebles_df$codage[which(!is.na(greebles_df$SDOquest.RESP))]
  SDO<-data.frame(1:10)
  SDO$score<-SDOscore
  SDO$quest<-SDOquest
  SDO$X1.10<-NULL
  SDO$score<-as.numeric(SDO$score)
  SDO$rec<-ifelse(nchar(SDO$quest)==1,SDO$score,8-SDO$score)
  SDO=sum(SDO$rec)
  RWAscore<-greebles_df$RWAquest.RESP[which(!is.na(greebles_df$RWAquest.RESP))]
  RWAquest<-greebles_df$codage[which(!is.na(greebles_df$RWAquest.RESP))]
  RWA<-data.frame(1:10)
  RWA$score<-RWAscore
  RWA$quest<-RWAquest
  RWA$X1.10<-NULL
  RWA$score<-as.numeric(RWA$score)
  RWA$rec<-ifelse(nchar(RWA$quest)==1,RWA$score,8-RWA$score)
  RWA=sum(RWA$rec)
  df_greeb<-greebles_df_short
  df_greeb$SDO<-SDO
  df_greeb$RWA<-RWA
  k1<-which(!is.na(df_greeb$Slide1.RESP))
  k2<-which(!is.na(df_greeb$aware))
  keep = c(k1,k2)
  df_greeb<-df_greeb[keep,]
  df_greeb$codage<-NULL
  df_greeb$Running<-NULL
  df_greeb$RWAquest.RESP<-NULL
  df_greeb$SDOquest.RESP<-NULL
  ratings<-data.frame(1:16)
  ratings$value<-df_greeb$Slide1.RESP[1:16]
  ratings$X1.16<-NULL
  ratings$CS<-df_greeb$CS[1:16]
  orderDf<-order(ratings$CS)
  ratings<-ratings[orderDf, ]
  df_greeb$CS[1:16]<-ratings$CS
  df_greeb$Slide1.RESP[1:16]<-ratings$value
  df_greeb$aware[1:16]<-df_greeb$aware[33:48]
  df_greeb<-df_greeb[1:16,]
  df_greeb$score<-df_greeb$Slide1.RESP
  df_greeb$Slide1.RESP<-NULL
  df_greeb$awareF<-df_greeb$aware
  df_greeb$aware<-ifelse(df_greeb$aware==100,df_greeb$aware,substr(df_greeb$aware, 1, 2))
  df_greeb$aware<-ifelse(nchar(df_greeb$aware)>1 & grepl("f",df_greeb$aware),substr(df_greeb$aware, 1, 1),df_greeb$aware)
  df_greeb$aware<-as.numeric(df_greeb$aware)
  df_greeb
}
  
all_df <- lapply(myFiles, createDF)
greebles = Reduce(function(...) merge(..., all=T), all_df)
View(greebles)
greebles$cond<-ifelse(grepl("Greebles",greebles$Eprime.Basename)==TRUE,"Greebles","Objects")
greebles$ordR<-ifelse(greebles$ordre=="CE1",1,-1)
greebles$condR<-ifelse(greebles$cond=="Greebles",1,-1)
greebles$fami<-ifelse(grepl("P",greebles$CS)==TRUE|grepl("M",greebles$CS)==TRUE,1,-1)
greebles$famiC<-ifelse(greebles$fami==1,"A","B")
greebles$interact<-greebles$ordR*greebles$fami
greebles$interactC<-ifelse(greebles$interact==1,"PosNeg","NegPos")
head(greebles)
View(greebles)

library(lme4)
library(piecewiseSEM)
greebles$score<-as.numeric(greebles$score)
modelRand<-lmer(score~(1|Eprime.Basename)+(1|CS),data=greebles)
summary(modelRand)

model3<-lmer(score~ordR*condR*SDO*RWA*fami*(1|Eprime.Basename)+(1|CS),data=greebles)
model1<-lmer(score~ordR*condR*fami*(1|Eprime.Basename)+(1|CS),data=greebles)
model2<-lmer(score~interact*SDO*(1|Eprime.Basename)+(1|CS),data=greebles)
model5<-lmer(score~interact*RWA*aware*(1|Eprime.Basename)+(1|CS),data=greebles)
model4<-lmer(score~interact*aware*(1|Eprime.Basename)+(1|CS),data=greebles)
coefs<-data.frame(coef(summary(model5)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

summary(model4)
sem.model.fits(model4)
sem.coefs(model2)
sem.model.fits(model2)
anova(modelRand,model3)
anova(modelRand,model1)
anova(modelRand,model2)
anova(modelRand,model4)
summary(model3)
summary(model1)
summary(model2)



library("ggplot2")
library("grid")

ggplot(greebles,aes(x=SDO,y=score,colour=interactC))+
  geom_point(shape=19,position=position_jitter(width=0,height=0.1),size=1)+
  stat_smooth(method=lm,formula=y~poly(x,1), aes(fill = interactC),size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  xlab("RWA")+
  theme(axis.ticks.length=unit(0.2, "cm"))+
  theme(axis.text=element_text(size=10))+
  theme(axis.title.x=element_text(size=10, vjust=-2.3,face="bold"))+
  theme(axis.title.y=element_text(size=10, vjust=2.3,face="bold"))+
  ylab("Rating")+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme(legend.key.size = unit(0.5, "cm"))+
  theme(legend.text = element_text(size = 10, colour = "black", angle = 0))+
  theme(legend.title = element_blank())+
  scale_colour_manual(values=c("#6699FF","#CC9966"), name="Ordre")+
  scale_fill_manual(values=c("#6699FF","#CC9966"), name="Ordre")
