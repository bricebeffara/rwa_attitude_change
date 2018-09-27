# Thu Sep  6 12:27:12 2018 ------------------------------
####################
###EXPERIMENT IAT###
####################


# Data preparation for IAT analyses --------------------------------------------------------

## !! clear out the environment
# rm(list=ls()) 

##install, load and update necessary packages if required##

if (!require("pacman")) install.packages("pacman")
p_load(dplyr, data.table, reshape2, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

##import raw data 

##for one data set

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in an Eprime text file

dt <- data.table(read.csv(file="all_iat.csv", header=TRUE, sep=";"))
df <- select(dt, Subject, Age, Group, assignment, CS.Trial., Ratings, LeftLabel, GreeblesDisplay.RT, GreeblesDisplay2.RT, GreeblesDisplay.RESP, GreeblesDisplay2.RESP, List3, List5, Running.Trial., Slide1.RESP, Trial, CorrectAnswer)


#### IAT dataframe ####

##Keep relevant IAT trials

IAT <- filter(df, Running.Trial. == "IAT")
IAT1 <- filter(IAT, Trial == "3")
IAT2 <- filter(IAT, Trial =="5")
IAT <- rbind(IAT1, IAT2)
IAT <- data.table(IAT)

#RT by trials
RT<-c(IAT1$GreeblesDisplay.RT, IAT2$GreeblesDisplay2.RT)
IAT$RT <- RT

#response by trials

response<-c(IAT1$GreeblesDisplay.RESP, IAT2$GreeblesDisplay2.RESP)
IAT$response <- response
IAT$response <- recode(IAT$response,`2` = "p", `3` = "q")

#correct responses. 1 = correct

IAT$correct<-ifelse(IAT$CorrectAnswer==IAT$response,1,ifelse(IAT$CorrectAnswer==IAT$response,0,NA))
IAT$correct[is.na(IAT$correct)] <- 0

IAT<-data.table(filter(IAT, !is.na(RT)))

##recode stimuli and greebles families
IAT$stim3 <- recode(IAT$List3,`9` = "B1", `10` = "B2", `11` = "B3", `12`="B4", `13`="B5",`14`="B6",`15`="B7",`16`="B8",`25`="M1",`26`="M2",`27`="M3",`28`="M4",`29`="M5",`30`="M6",`31`="M7",`32`="M8" )
IAT$stim5 <- recode(IAT$List5,`9` = "B1", `10` = "B2", `11` = "B3", `12`="B4", `13`="B5",`14`="B6",`15`="B7",`16`="B8",`25`="M1",`26`="M2",`27`="M3",`28`="M4",`29`="M5",`30`="M6",`31`="M7",`32`="M8" )
IAT$stim = IAT$stim3
IAT$stim[!is.na(IAT$stim5)] = IAT$stim5[!is.na(IAT$stim5)]
IAT<-select(IAT, -contains("stim3"),-contains("stim5"))

IAT$family<-IAT$stim
IAT[family %in% c("M1","M2", "M3","M4","M5","M6","M7","M8"), family := "M"]
IAT[family %in% c("B1","B2", "B3","B4","B5","B6","B7","B8"), family := "B"]

#assignment for each participant
assign<-data.table(filter(df, !is.na(assignment)))
assign<-select(assign, Subject, assignment)
assign<-distinct(assign)
assignment<-rep(as.vector(assign$assignment),each=32)
Subject<-rep(as.vector(assign$Subject),each=32)
assign<-data.table(cbind(assignment, Subject))
assign <- assign[order(Subject),]
IAT <- IAT[order(Subject),] 
IAT$assignment<-assign$assignment

#valence of first block. In assignment 1, M strats with positive, B starts with negative. In assignment 2, M starts with negative, B starts with positive.
IAT$usvalence[IAT$family == "M" & IAT$assignment =="1"] <- 0.5
IAT$usvalence[IAT$family == "B" & IAT$assignment =="1"] <- -0.5
IAT$usvalence[IAT$family == "M" & IAT$assignment =="2"] <- -0.5
IAT$usvalence[IAT$family == "B" & IAT$assignment =="2"] <- 0.5

##Code congruent and incongruent trials. 0.5 = congruent, -0.5 = incongruent. 

IAT$cong[IAT$family == "M" & IAT$usvalence =="0.5" & IAT$LeftLabel == "famille A ou j'aime"] <- -0.5
IAT$cong[IAT$family == "M" & IAT$usvalence =="-0.5" & IAT$LeftLabel == "famille A ou j'aime"] <- 0.5
IAT$cong[IAT$family == "B" & IAT$usvalence =="0.5" & IAT$LeftLabel == "famille A ou j'aime"] <- 0.5
IAT$cong[IAT$family == "B" & IAT$usvalence =="-0.5" & IAT$LeftLabel == "famille A ou j'aime"] <- -0.5
IAT$cong[IAT$family == "M" & IAT$usvalence =="0.5" & IAT$LeftLabel == "famille B ou j'aime"] <- 0.5
IAT$cong[IAT$family == "M" & IAT$usvalence =="-0.5" & IAT$LeftLabel == "famille B ou j'aime"] <- -0.5
IAT$cong[IAT$family == "B" & IAT$usvalence =="0.5" & IAT$LeftLabel == "famille B ou j'aime"] <- -0.5
IAT$cong[IAT$family == "B" & IAT$usvalence =="-0.5" & IAT$LeftLabel == "famille B ou j'aime"] <- 0.5

##code for order of evaluative measures: 0.5 = direct measure then indirect measure. -0.5 = indirect measure then direct measure. 

IAT$order[IAT$assignment =="1"] <- 0.5
IAT$order[IAT$assignment =="2"] <- -0.5

## RWA by participant 

RWA<-select(dt, Subject, RWA, RWAquest.RESP)
RWA<-data.table(filter(RWA, !is.na(RWA)))
RWA<-dcast(RWA, Subject ~ RWA, value.var="RWAquest.RESP",fun.aggregate = mean)
RWA<-setnames(RWA, c("Subject","RWA1","RWA2","RWA3","RWA4","RWA5","RWA6","RWA7","RWA8","RWA9","RWA10"))

#reversed items (to be reversed = items 4 and 5)

RWA<- within(RWA,{
  RWA4_r<- 8-RWA4
  RWA5_r<- 8-RWA5
})


#RWA score

RWA <- within(RWA, {
  RWAscore <- RWA1+RWA2+RWA3+RWA4_r+RWA5_r+RWA6+RWA7+RWA8+RWA9+RWA10
})

#Check data & See data

#boxplot(RWA$RWAscore)
#hist(RWA$RWAscore)

quart1 <- quantile(RWA$RWAscore)[2]
quart3 <- quantile(RWA$RWAscore)[4]
iqr <- IQR(RWA$RWAscore)

#which(RWA$RWAscore<quart1-3*iqr)
#which(RWA$RWAscore>quart3+3*iqr)

# Remove highly extreme values of RWA
RWA$RWAscore <-  ifelse (RWA$RWAscore<quart1-3*iqr | RWA$RWAscore>quart3+3*iqr, NA, RWA$RWAscore)
RWA <- RWA[which(!is.na(RWA$RWAscore)),]

# scale RWA score
RWA$RWAscore<-scale(RWA$RWAscore, center = TRUE, scale = TRUE)

##Keep necessary columns

IAT <- full_join(IAT, RWA, by ="Subject")

IAT$RWAscore <- as.numeric( as.character( IAT$RWAscore))

#IAT<-select(IATtot,Subject,Age,RT,correct,stim,family,usvalence,cong,order,RWAscore)

## Remove problematic data recordings (ID overlap)

IAT <- data.table(IAT)
counts <- IAT[, .(rowCount = .N), by = Subject]
IAT <- full_join(IAT, counts, by ="Subject")
idrem <- unique(IAT$Subject[which(IAT$rowCount == 64)])
IAT <- filter(IAT, rowCount == 32) 

'%notin%' = function(x,y) !(x %in% y)
RWA$RWAscore <- as.numeric( as.character( RWA$RWAscore))
RWA <- filter(RWA, Subject %notin% idrem ) 

# indirect measures ------------------------------------------------------------


#### Applying Greenwald et al. recommendations ####

IAT<-filter(IAT, RT<10000)                                          #delete trials longer than 10 seconds
IAT$fastTrials <- ifelse (IAT$RT < 300, 1, 0)                       #Code for trials quicker than 300ms
Subjectcount<-aggregate(fastTrials ~ Subject, IAT, sum, na.rm=TRUE) #sum of trials quicker than 300ms by subject
Subjectcount$subjectcount <- Subjectcount$fastTrials                #sum of trials quicker than 300ms by subject
IAT<-full_join(IAT,Subjectcount, by ="Subject")                     #sum of trials quicker than 300ms by subject
#IAT$Subjectcount[is.na(IAT$subjectcount)] <- 0
IAT<-filter(IAT, subjectcount < 32*10/100)                          #delete subjects who have more than 10% of the 32 trials quicker than 300ms
#MeanRT<-aggregate(RT ~ cong * correct, IAT, mean )                  #Compute mean of correct latencies for each block
#RTSD<-sd(IAT$RT)                                                    #Compute one pooled SD for all trials
#IAT$RT[IAT$cong == "0.5" & IAT$correct =="0"] <- MeanRT[4,3] + 600    #Replace each error latency with block mean(computed in Step 5) + 600 ms
#IAT$RT[IAT$cong == "-0.5" & IAT$correct =="0"] <- MeanRT[3,3] + 600   #Replace each error latency with block mean(computed in Step 5) + 600 ms
#d_IAT<-dcast(IAT,Subject~cong,value.var = "RT",fun.aggregate = mean)#Average the resulting values for each of the blocks
#d_IAT$d<-d_IAT$`-0.5`-d_IAT$`0.5`                                       #Compute difference between incongruent and congruent blocks
#d_IAT$d<-d_IAT$d / RTSD                                             #Divide difference by its associated pooled trials SD


#### Final data frame
#IATfinal<-full_join(d_IAT,RWA, by ="Subject")
#IATfinal<-full_join(IATfinal,IAT, by="Subject")
IATfinal<-full_join(IAT,RWA, by ="Subject")
IATfinal$RWAscore <- IATfinal$RWAscore.x
IATfinal$congruent <- IATfinal$cong
IATfinal$ppt <- IATfinal$Subject
IATfinal<-select(IATfinal, ppt, RT, correct, RWAscore, congruent, order, stim)
#IATfinal<-distinct(IATfinal)
iat_df <- IATfinal
iat_rt <- iat_df[which(iat_df$correct == 1),]


# Data preparation for direct evaluative measure --------------------------

dt <- data.table(read.csv(file="all_iat.csv", header=TRUE, sep=";"))
df <- select(dt, Subject, Age, Group, assignment, CS.Trial., Running.Trial., Slide1.RESP)

#### Direct ratings dataframe ####

##Keep relevant direct ratings trials

diriat_df <- data.table(filter(df, Running.Trial. == "Ratings"))

##recode stimuli and greebles families
diriat_df$family <- diriat_df$CS.Trial.
diriat_df[family %in% c("M1","M2", "M3","M4","M5","M6","M7","M8"), family := "M"]
diriat_df[family %in% c("B1","B2", "B3","B4","B5","B6","B7","B8"), family := "B"]

#assignment for each participant
assign<-data.table(filter(df, !is.na(assignment)))
assign<-select(assign, Subject, assignment)
assign<-distinct(assign)
assignment<-rep(as.vector(assign$assignment),each=16)
Subject<-rep(as.vector(assign$Subject),each=16)
assign<-data.table(cbind(assignment, Subject))
assign <- assign[order(Subject),]
diriat_df <- diriat_df[order(Subject),] 
diriat_df$assignment<-assign$assignment

##code for order of evaluative measures: 0.5 = direct measure then indirect measure. -0.5 = indirect measure then direct measure. 

diriat_df$order[diriat_df$assignment =="1"] <- 0.5
diriat_df$order[diriat_df$assignment =="2"] <- -0.5

#valence of first block. In assignment 1, M starts with positive, B starts with negative. In assignment 2, M starts with negative, B starts with positive.
diriat_df$usvalence[diriat_df$family == "M" & diriat_df$assignment =="1"] <- 0.5
diriat_df$usvalence[diriat_df$family == "B" & diriat_df$assignment =="1"] <- -0.5
diriat_df$usvalence[diriat_df$family == "M" & diriat_df$assignment =="2"] <- -0.5
diriat_df$usvalence[diriat_df$family == "B" & diriat_df$assignment =="2"] <- 0.5

#RWA score for each participant
diriat_df<-full_join(diriat_df,RWA, by ="Subject")
diriat_df$ppt <- diriat_df$Subject
diriat_df$stim1 <- diriat_df$CS.Trial.
diriat_df$response <- diriat_df$Slide1.RESP

# for IDA

xp4_df <- diriat_df[diriat_df$order == 0.5,]
col2k <- c("ppt", "stim1", "RWAscore", "usvalence", "response")
xp4_df <- xp4_df[,col2k]
xp4_df$XP <- "XP04"

setwd("../ida")
write.csv(xp4_df,file="XP04.csv", row.names=F)




