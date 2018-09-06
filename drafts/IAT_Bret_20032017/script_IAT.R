# Thu Sep  6 12:26:46 2018 ------------------------------
####################
###EXPERIMENT IAT###
####################


# Data preparation for IAT analyses --------------------------------------------------------

## clear out the environment
rm(list=ls()) 

##install, load and update necessary packages if required##

if (!require("pacman")) install.packages("pacman")
p_load(dplyr, data.table, reshape2, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

##import raw data 

##for one data set

setwd("/Users/mierop/Google Drive/DATA")

# Read in an Eprime text file

dt <- data.table(read.csv(file="/Users/mierop/Google Drive/DATA/merge.csv", header=TRUE, sep=";"))
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

#valence of second block. In assignment 1, M ends with negative, B ends with positive. In assignment 2, M ends with positive, B ends with negative.
IAT$USvalence[IAT$family == "M" & IAT$assignment =="1"] <- -1
IAT$USvalence[IAT$family == "B" & IAT$assignment =="1"] <- 1
IAT$USvalence[IAT$family == "M" & IAT$assignment =="2"] <- 1
IAT$USvalence[IAT$family == "B" & IAT$assignment =="2"] <- -1

##Code congruent and incongruent trials. 1 = congruent, -1 = incongruent. 

IAT$cong[IAT$family == "M" & IAT$USvalence =="1" & IAT$LeftLabel == "famille A ou j'aime"] <- -1
IAT$cong[IAT$family == "M" & IAT$USvalence =="-1" & IAT$LeftLabel == "famille A ou j'aime"] <- 1
IAT$cong[IAT$family == "B" & IAT$USvalence =="1" & IAT$LeftLabel == "famille A ou j'aime"] <- 1
IAT$cong[IAT$family == "B" & IAT$USvalence =="-1" & IAT$LeftLabel == "famille A ou j'aime"] <- -1
IAT$cong[IAT$family == "M" & IAT$USvalence =="1" & IAT$LeftLabel == "famille B ou j'aime"] <- 1
IAT$cong[IAT$family == "M" & IAT$USvalence =="-1" & IAT$LeftLabel == "famille B ou j'aime"] <- -1
IAT$cong[IAT$family == "B" & IAT$USvalence =="1" & IAT$LeftLabel == "famille B ou j'aime"] <- -1
IAT$cong[IAT$family == "B" & IAT$USvalence =="-1" & IAT$LeftLabel == "famille B ou j'aime"] <- 1

##code for order of evaluative measures: 1 = direct measure then indirect measure. 2 = indirect measure then direct measure. 

IAT$order[IAT$assignment =="1"] <- 1
IAT$order[IAT$assignment =="2"] <- 2

## RWA by participant 

RWAdf<-select(dt, Subject, RWA, RWAquest.RESP)
RWAdf<-data.table(filter(RWAdf, !is.na(RWA)))
RWAdf<-dcast(RWAdf, Subject ~ RWA, value.var="RWAquest.RESP",fun.aggregate = mean)
RWAdf<-setnames(RWAdf, c("Subject","RWA1","RWA2","RWA3","RWA4","RWA5","RWA6","RWA7","RWA8","RWA9","RWA10"))

#reversed items (to be reversed = items 4 and 5)

RWAdf<- within(RWAdf,{
  RWA4_r<- 8-RWA4
  RWA5_r<- 8-RWA5
})


#RWA score

RWAdf <- within(RWAdf, {
  RWAscore <- RWA1+RWA2+RWA3+RWA4_r+RWA5_r+RWA6+RWA7+RWA8+RWA9+RWA10
})


##Keep necessary columns

IAT<-select(IATtot,Subject,Age,RT,correct,stim,family,USvalence,cong,order,RWAscore)



# IAT analyses ------------------------------------------------------------

##install, load and update necessary packages if required##

if (!require("pacman")) install.packages("pacman")
p_load(lme4, stats, moments, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

#### Applying Greenwald et al. recommendations ####

IAT<-filter(IAT, RT<10000)                                          #delete trials longer than 10 seconds
IAT$fastTrials[IAT$RT < 300] <- 1                                   #Code for trials quicker than 300ms
Subjectcount<-aggregate(fastTrials ~ Subject, IAT, sum, na.rm=TRUE) #sum of trials quicker than 300ms by subject
Subjectcount$subjectcount <- Subjectcount$fastTrials                #sum of trials quicker than 300ms by subject
IAT<-full_join(IAT,Subjectcount, by ="Subject")                     #sum of trials quicker than 300ms by subject
IAT$Subjectcount[is.na(IAT$subjectcount)] <- 0
IAT<-filter(IAT, Subjectcount < 32*10/100)                          #delete subjects who have more than 10% of the 32 trials quicker than 300ms
MeanRT<-aggregate(RT ~ cong, IAT, mean )                            #Compute mean of correct latencies for each block
RTSD<-sd(IAT$RT)                                                    #Compute one pooled SD for all trials
IAT$RT[IAT$cong == "1" & IAT$correct =="0"] <- MeanRT[1,2] + 600    #Replace each error latency with block mean(computed in Step 5) + 600 ms
IAT$RT[IAT$cong == "-1" & IAT$correct =="0"] <- MeanRT[2,2] + 600   #Replace each error latency with block mean(computed in Step 5) + 600 ms
d_IAT<-dcast(IAT,Subject~cong,value.var = "RT",fun.aggregate = mean)#Average the resulting values for each of the blocks
d_IAT$d<-d_IAT$`-1`-d_IAT$`1`                                       #Compute difference between congruent and incongruent blocks
d_IAT$d<-d_IAT$d / RTSD                                             #Divide difference by its associated pooled trials SD


#### Final data frame
IATfinal<-full_join(d_IAT,RWAdf, by ="Subject")
IATfinal<-full_join(IATfinal,IAT, by="Subject")
IATfinal<-select(IATfinal, Subject, d, RWAscore.x,order)
IATfinal<-distinct(IATfinal)

#### RWA score distribution

hist(IATfinal$RWAscore.x)
kurtosis(IATfinal$RWAscore.x)
skewness(IATfinal$RWAscore.x)

##creating a normally distributed variable and compare distribution with RWA
x <- rnorm(710000, mean = mean(IATfinal$RWAscore.x), sd=sd(IATfinal$RWAscore.x))
ks.test(IATfinal$RWAscore.x, x)

#### fitting the model with all data

model <-lm(d ~ RWAscore.x, IATfinal)
summary(model)

#### dataframe for order = 1 (direct measure then indirect measure)
IAT_order1 <- filter(IATfinal, order == "1")

#### dataframe for order = 2 (indirect measure then direct measure)
IAT_order2 <- filter(IATfinal, order == "2")

#### fitting the model with restricted data

model1 <-lm(d ~ RWAscore.x, IAT_order1)
summary(model)

model2 <-lm(d ~ RWAscore.x, IAT_order2)
summary(model2)


# Data preparation for direct evaluative measure --------------------------

dt <- data.table(read.csv(file="/Users/mierop/Google Drive/DATA/merge.csv", header=TRUE, sep=";"))
df <- select(dt, Subject, Age, Group, assignment, CS.Trial., Running.Trial., Slide1.RESP)

#### Direct ratings dataframe ####

##Keep relevant direct ratings trials

direct <- data.table(filter(df, Running.Trial. == "Ratings"))

##recode stimuli and greebles families
direct$family <- direct$CS.Trial.
direct[family %in% c("M1","M2", "M3","M4","M5","M6","M7","M8"), family := "M"]
direct[family %in% c("B1","B2", "B3","B4","B5","B6","B7","B8"), family := "B"]

#assignment for each participant
assign<-data.table(filter(df, !is.na(assignment)))
assign<-select(assign, Subject, assignment)
assign<-distinct(assign)
assignment<-rep(as.vector(assign$assignment),each=16)
Subject<-rep(as.vector(assign$Subject),each=16)
assign<-data.table(cbind(assignment, Subject))
assign <- assign[order(Subject),]
direct <- direct[order(Subject),] 
direct$assignment<-assign$assignment

##code for order of evaluative measures: 1 = direct measure then indirect measure. 2 = indirect measure then direct measure. 

direct$order[direct$assignment =="1"] <- 1
direct$order[direct$assignment =="2"] <- 2

#valence of second block. In assignment 1, M ends with negative, B ends with positive. In assignment 2, M ends with positive, B ends with negative.
direct$USvalence[direct$family == "M" & direct$assignment =="1"] <- -1
direct$USvalence[direct$family == "B" & direct$assignment =="1"] <- 1
direct$USvalence[direct$family == "M" & direct$assignment =="2"] <- 1
direct$USvalence[direct$family == "B" & direct$assignment =="2"] <- -1

#RWA score for each participant
directfinal<-full_join(direct,RWAdf, by ="Subject")


# Data analyses for direct ratings ----------------------------------------

##install, load and update necessary packages if required##

if (!require("pacman")) install.packages("pacman")
p_load(lme4, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

## Fitting the model

model <- lmer(Slide1.RESP~USvalence*RWAscore + (1|Subject) + (1|family), directfinal)
summary(model)

#### dataframe for order = 1 (direct measure then indirect measure)
direct_order1 <- filter(directfinal, order == "1")
model1 <- lmer(Slide1.RESP~USvalence*RWAscore + (1|Subject) + (1|family), direct_order1)
summary(model1)

#### dataframe for order = 2 (indirect measure then direct measure)
direct_order2 <- filter(directfinal, order == "2")
model2 <- lmer(Slide1.RESP~USvalence*RWAscore + (1|Subject) + (1|family), direct_order2)
summary(model2)
