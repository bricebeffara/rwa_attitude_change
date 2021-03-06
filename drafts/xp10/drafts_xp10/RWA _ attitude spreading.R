###########
#### RWA & spreading of attitudes
###########

# Data preparation --------------------------------------------------------

# Optional generic preliminaries:

#graphics.off() # This closes all of R's graphics windows.
#rm(list=ls())  # Careful! This clears all of R's memory!

## install, load and update necessary packages if required##

if (!require("pacman")) install.packages("pacman")
p_load(dplyr, data.table, reshape2, yarrr, install = TRUE, update = getOption("pac_update"), character.only = FALSE)

# We get the current working directory and it will be used to load the data

curwd <- dirname(rstudioapi::getActiveDocumentContext()$path)

## set the working directory to the folder containing the data

setwd(paste(curwd,"/data_gen/",sep=""))


files <- list.files()
for(i in 1:length(files)) assign(files[i], read.csv(files[i],header=F,stringsAsFactors=F))

colNum <- length(get(files[1]))

if(colNum < 23){clip = 4} else {clip=3}

d <- data.frame(matrix(nrow=0,ncol=colNum))

for(i in 1:length(files)){
  j <- get(files[i])
  
  j$turkID <- j[2,1]         # get Mturk ID
  j$age <- j[2,2]            # get age
  j$sex <- j[2,3]            # get sex
  j$education <- j[2,4]      # get education
  j$handedness <- j[2,5]     # get handedness
  j$sexOrientation <- j[2,6] # get sexual orientation
  j$ethnicity <- j[2,7]      # get ethnicity
  j$ethnicity <- j[2,8]      # get nationality
  j$other1 <- j[2,9]         # get other 1
  j$other2 <- j[2,10]         # get other 2
  j$other3 <- j[2,11]         # get other 3
  j$other4 <- j[2,12]         # get other 4
  j$other5 <- j[2,13]        # get other 5
  j$browser <- j[2,14]       # get browser
  j$version <- j[2,15]       # get version
  j$screenWidth <- j[2,16]   # get screen width
  j$screenHeight <- j[2,17]  # get screen height
  j$OS <- j[2,18]            # get operating system
  j$OS_lang <- j[2,19]       # get operating system language
  j$calibration <- j[2,20]   # get calibration
  j$ip <- j[2,21]            # get IP address
  j$code <- j[2,22]          # get completion code
  
  j <- j[-c(1:clip),]
  d <- rbind(d,j)
}

colnames(d) <- c(get(files[1])[clip,],colnames(d[,tail(colnames(d),n=21)]))

setwd("../")
write.csv(d,file="TestData.csv", row.names=F)

##Keep relevant columns
df <- data.table(select(d, subjectGroup, condition1, stim1, stim2, response, RT, other2, other3,other4,stimFormat, trialNo))

##Recode families of Greebles
df$family<-df$stim1
df[family %in% c("M1","M2", "M3","M4","M5","M6","M7","M8"), family := "M"]
df[family %in% c("B1","B2", "B3","B4","B5","B6","B7","B8"), family := "B"]
df[family %in% c("O1","O2", "O3","O4","O5","O6","O7","O8"), family := "O"]
df[family %in% c("P1","P2", "P3","P4","P5","P6","P7","P8"), family := "P"]

## Recode US valence: 
# Subject group 1: BP - MO / Bpos - Mneg
# Subject group 2: BP - MO / Bneg - Mpos
# Subject group 3: BO - MP / Bpos - Mneg
# Subject group 4: BO - MP / Bneg - Mpos

df$usvalence[df$subjectGroup == "1" & df$family == "B"] <- 0.5
df$usvalence[df$subjectGroup == "2" & df$family == "B"] <- -0.5
df$usvalence[df$subjectGroup == "3" & df$family == "B"] <- 0.5
df$usvalence[df$subjectGroup == "4" & df$family == "B"] <- -0.5
df$usvalence[df$subjectGroup == "1" & df$family == "M"] <- -0.5
df$usvalence[df$subjectGroup == "2" & df$family == "M"] <- 0.5
df$usvalence[df$subjectGroup == "3" & df$family == "M"] <- -0.5
df$usvalence[df$subjectGroup == "4" & df$family == "M"] <- 0.5

df$usvalence[df$subjectGroup == "1" & df$family == "O"] <- -0.5
df$usvalence[df$subjectGroup == "2" & df$family == "O"] <- 0.5
df$usvalence[df$subjectGroup == "3" & df$family == "O"] <- 0.5
df$usvalence[df$subjectGroup == "4" & df$family == "O"] <- -0.5
df$usvalence[df$subjectGroup == "1" & df$family == "P"] <- 0.5
df$usvalence[df$subjectGroup == "2" & df$family == "P"] <- -0.5
df$usvalence[df$subjectGroup == "3" & df$family == "P"] <- -0.5
df$usvalence[df$subjectGroup == "4" & df$family == "P"] <- 0.5

##Recode for actual pairing vs. spreading condition


df[family %in% c("O","P"), spreading := "1"]
df[family %in% c("B","M"), spreading := "-1"]

###### Create RWA levels dataframe ######

RWA <- filter(df, condition1 == "RWA")

##Long to wide dataframe
RWA <- data.table(dcast(RWA, other2 ~ trialNo, value.var="response"))
colnames(RWA) <- paste("RWA", colnames(RWA), sep = "_")
colnames(RWA)[1] <- "other2"

#Transform variables to numeric format
RWA[, (2:16) := lapply(.SD, as.numeric), .SDcols = (2:16)]

#reversed items. RWA items 2, 4, 6, 10, 12, 14.
RWA<- within(RWA,{
  RWA2R<- 8-RWA_2
  RWA4R<- 8-RWA_4
  RWA6R<- 8-RWA_6
  RWA10R<- 8-RWA_10
  RWA8R<-8-RWA_8
  RWA12R<- 8-RWA_12
  RWA14R<- 8-RWA_14
})

#RWA score
RWA<- within(RWA,{
  RWAscore <- rowSums(RWA[, c("RWA_1","RWA10R","RWA_11","RWA12R","RWA_13","RWA14R","RWA_15","RWA2R","RWA_3","RWA4R", "RWA_5","RWA6R","RWA_7","RWA8R","RWA_9")], na.rm=TRUE)
})

##center RWA score
RWA$RWAscore<-scale(RWA$RWAscore, center = TRUE, scale = TRUE)


###### Create evaluative ratings dataframe ######

ratings <- df[condition1 %in% c("ratings1","ratings2","ratings3","ratings4"), ]


##### create learning accuracy data frame

learning <- filter(df, condition1 =="apprentissage")
learning <- data.table(learning[!is.na(learning$spreading),])

## Code correct response

learning[family %in% "B", acc := "1"]
learning[family %in% "M", acc := "2"]
learning[family %in% "O", acc := "3"]
learning[family %in% "P", acc := "4"]

learning[, learing_acc := as.numeric(response) - as.numeric(acc)]
learning[learing_acc %in% "0", accuracy := 1]
learning[learing_acc %in% c("-3","-2", "-1","1","2","3"), accuracy := 0]

###### Creare memory performance dataframe

mem <- df[condition1 %in% c("memory1", "memory2", "memory3", "memory4"),]

# Correct responses (P = 3 / O = 1)

# Subject group 1: BP - MO 
# Subject group 2: BP - MO 
# Subject group 3: BO - MP 
# Subject group 4: BO - MP 

mem[subjectGroup %in% c("1","2") & family %in% "B", memory := "3"]
mem[subjectGroup %in% c("1","2") & family %in% "M", memory := "1"]
mem[subjectGroup %in% c("3","4") & family %in% "B", memory := "1"]
mem[subjectGroup %in% c("3","4") & family %in% "M", memory := "3"]

# Accuracy (actual vs. correct responses)

mem[, accuracy := as.numeric(memory) - as.numeric(response)]
mem[, accuracy := ifelse(accuracy == "0", 1, 0) ]
mem <- select(mem, other2, stim1, accuracy)

###### Merge RWA with ratings dataframe ######

spreading<-full_join(ratings, RWA)
spreading<-full_join(spreading, mem)

## recoding & transforming for anaylisis

spreading$spreading <- ifelse (spreading$spreading == "1", 0.5, -0.5)
spreading$response <- as.numeric(spreading$response)

## computing new variable for interaction

spreading$interactSU <- spreading$spreading * spreading$usvalence

# Data analyses -----------------------------------------------------------

library(lme4)

model_rep <- lmer(response ~ usvalence*spreading*RWAscore + (1|other2) + (1|stim1), data = spreading)

summary(model_rep)

##Testing estimated coefficients significance
coefs_rep <- data.frame(coef(summary(model_rep)))
# use normal distribution to approximate p-value
coefs_rep$p.z <- 2 * (1 - pnorm(abs(coefs_rep$t.value)))
coefs_rep

## Testing the difference in memory performance

model_acc <- glmer(accuracy ~ RWAscore + (1|other2) + (1|stim1), data = spreading, family = binomial)

summary(model_acc)

##Testing estimated coefficients significance
coefs_acc <- data.frame(coef(summary(model_acc)))


# plot --------------------------------------------------------------------

library(yarrr)

pirateplot(formula = response ~ RWAscore*usvalence*spreading,
           data = spreading,
           theme = 1,
           pal = "black",
           main = "pal = 'black")


library(ggplot2)


spread<- filter(spreading, spreading == "0.5")
nospread<-filter(spreading, spreading == "-0.5")

plot_spread <- spread %>% 
  ggplot(aes(RWAscore, response, colour = as.character(usvalence))) + 
  geom_jitter(height = .08, width = .08, alpha = 1/3, aes(shape = as.character(usvalence))) + 
  theme_classic() +
  geom_smooth(method = lm, se = T) +
  labs(
    x = "RWA",
    y = "CS Ratings")

plot_spread

plot_nospread <- nospread %>% 
  ggplot(aes(RWAscore, response, colour = as.character(usvalence))) + 
  geom_jitter(height = .08, width = .08, alpha = 1/3, aes(shape = as.character(usvalence))) + 
  theme_classic() +
  geom_smooth(method = lm, se = T) +
  labs(
    x = "RWA",
    y = "CS Ratings")

plot_nospread

model_nospread <- lmer(response ~ usvalence*RWAscore + (1|other2) + (1|stim1), data = nospread)
summary(model_nospread)

model_spread <- lmer(response ~ usvalence*RWAscore + (1|other2) + (1|stim1), data = spread)
summary(model_spread)


###### Valence

pos_df<- filter(spreading, usvalence == "0.5")
neg_df<-filter(spreading, usvalence == "-0.5")

plot_pos <- pos_df %>% 
  ggplot(aes(RWAscore, response, colour = as.character(spreading))) + 
  geom_jitter(height = .08, width = .08, alpha = 1/3, aes(shape = as.character(spreading))) + 
  theme_classic() +
  geom_smooth(method = lm, se = T) +
  labs(
    x = "RWA",
    y = "CS Ratings")

plot_pos

plot_neg <- neg_df %>% 
  ggplot(aes(RWAscore, response, colour = as.character(spreading))) + 
  geom_jitter(height = .08, width = .08, alpha = 1/3, aes(shape = as.character(spreading))) + 
  theme_classic() +
  geom_smooth(method = lm, se = T) +
  labs(
    x = "RWA",
    y = "CS Ratings")

plot_neg

model_nospread <- lmer(response ~ usvalence*RWAscore + (1|other2) + (1|stim1), data = nospread)
summary(model_nospread)

model_spread <- lmer(response ~ usvalence*RWAscore + (1|other2) + (1|stim1), data = spread)
summary(model_spread)

### ordinal

library("ordinal")
ord_rep <- clmm(as.factor(response) ~ usvalence*spreading*RWAscore + (1|other2) + (1|stim1), data = spreading, link = "probit", threshold = "equidistant")
ord_rep <- clmm(as.factor(response) ~ usvalence*spreading*RWAscore + (1|other2) + (1|stim1), data = spreading, link = "probit", threshold = "equidistant")

summary(ord_rep)
