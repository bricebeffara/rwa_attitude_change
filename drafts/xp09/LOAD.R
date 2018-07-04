###########
#### RWA & spreading of attitudes
###########

# Data preparation --------------------------------------------------------

# Optional generic preliminaries:

#graphics.off() # This closes all of R's graphics windows.
#rm(list=ls())  # Careful! This clears all of R's memory!

## install, load and update necessary packages if required##

if (!require("pacman")) install.packages("pacman")
p_load(dplyr, 
       stringi,
       data.table,
       reshape2,
       yarrr,
       rlist,
       readr,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

# We get the current working directory and it will be used to load the data

curwd <- dirname(rstudioapi::getActiveDocumentContext()$path)

# set the working directory to the folder containing the data

# Select only text files corresponding to collected data
load_df <- read_delim("table_all_data.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE, 
                        skip = 1)

##CODING CONDITION +0.5 = LOAD  -0.5 = CONTROL
load_df$load <- ifelse(grepl("COND1",load_df$ExperimentName),+0.5,-0.5)

##RWA SCORE
load_df$RWARECOD<-ifelse(grepl("R",load_df$codage),8-load_df$RWAquest.RESP,load_df$RWAquest.RESP)
load_dfsum <- aggregate(RWARECOD ~ Subject,load_df,FUN = sum)
load_df <- merge(load_df,load_dfsum,by="Subject")
load_df$RWA<-load_df$RWARECOD.y
load_df$RWAscore <- scale(load_df$RWA, center = TRUE, scale = TRUE)

load_df$response<-load_df$Slide1.RESP
load_df$stim1<-load_df$`CS[Trial]`
load_df$ppt <- as.character(load_df$Subject)

##RECODING OF BLOC 1 valence
load_dfassi <- aggregate(assignment ~ Subject, load_df[which(!is.na(load_df$assignment)),],FUN = unique)
load_df <- merge(load_df,load_dfassi,by="Subject")
load_df$assignment <- load_df$assignment.y
load_df$usvalence <- ifelse ((load_df$assignment == 1 & grepl("M",load_df$CS)) | (load_df$assignment == 2 & grepl("B",load_df$CS)), +0.5, -0.5)
load_df <- load_df[which(!is.na(load_df$Slide1.RESP)),]

col2k <- c("ppt", "usvalence", "load", "RWAscore", "response", "stim1")




##DATA ANALYSIS
library(lme4)
MA <- lmer(response~bloc1val*RWA*cond + (1|Subject) + (1|CS), load_df)
MC <- lmer(response~bloc1val*cond + (1|Subject) + (1|CS), load_df)
MA <- lmer(response~bloc1val*RWA + (1|Subject) + (1|CS), load_df)
MC <- lmer(response~bloc1val + (1|Subject) + (1|CS), load_df)



summary(MA)
anova(MA,MC)
hist(load_df$RWA)

ggplot(load_df[which(load_df$cond==+0.5),],aes(x=RWA,y=response),colour=Bloc1usvalence)+
  geom_point(shape=19,position=position_jitter(width=0,height=0.1),size=1, aes(colour = Bloc1usvalence))+
  stat_smooth(method=lm,formula=y~poly(x,1), aes(fill = Bloc1usvalence, colour=Bloc1usvalence),size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  xlab("RWA")+
  theme(axis.ticks.length=unit(0.2, "cm"))+
  theme(axis.text=element_text(size=10))+
  theme(axis.title.x=element_text(size=10, vjust=-2.3,face="bold"))+
  theme(axis.title.y=element_text(size=10, vjust=2.3,face="bold"))+
  ylab("ratings")+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme(legend.key.size = unit(0.5, "cm"))+
  theme(legend.text = element_text(size = 10, colour = "black", angle = 0))+
  theme(legend.title = element_blank())+
  scale_colour_manual(values=c("#6699FF","#CC9966"), name="Ordre")+
  scale_fill_manual(values=c("#6699FF","#CC9966"), name="Ordre")

ggplot(load_df,aes(x=RWA,y=response),colour=Bloc1usvalence)+
  geom_point(shape=19,position=position_jitter(width=0,height=0.1),size=1, aes(colour = Bloc1usvalence))+
  stat_smooth(method=lm,formula=y~poly(x,1), aes(fill = Bloc1usvalence, colour=Bloc1usvalence),size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  xlab("RWA")+
  theme(axis.ticks.length=unit(0.2, "cm"))+
  theme(axis.text=element_text(size=10))+
  theme(axis.title.x=element_text(size=10, vjust=-2.3,face="bold"))+
  theme(axis.title.y=element_text(size=10, vjust=2.3,face="bold"))+
  ylab("ratings")+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme(legend.key.size = unit(0.5, "cm"))+
  theme(legend.text = element_text(size = 10, colour = "black", angle = 0))+
  theme(legend.title = element_blank())+
  scale_colour_manual(values=c("#6699FF","#CC9966"), name="Ordre")+
  scale_fill_manual(values=c("#6699FF","#CC9966"), name="Ordre")

hist(load_df[which(load_df$cond==-0.5),]$RWA)
