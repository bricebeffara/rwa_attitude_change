# Wed Nov 22 17:27:51 2017 ------------------------------
# Clean script data extraction
# Bret et al. 2017
# Right-Wing Authoritarianism Predicts weakened Attitude Change in an Evaluative Counter-conditioning Paradigm
# OSF : https://osf.io/vz78f/
# Preprint : https://psyarxiv.com/3mfa8/
#################################################################################################################

########
# XP 1 #
########

XP1_Greebles <- XP1_Greebles[-which(XP1_Greebles$RWA == 0),]
XP1_Greebles$XP <- "1"
XP1_Greebles$ppt <- XP1_Greebles$Subject
XP1_Greebles$stim <- XP1_Greebles$CS
XP1_Greebles$rwa <- (XP1_Greebles$RWA)
XP1_Greebles$usvalence <-  ifelse(XP1_Greebles$assignment == -1 & grepl("B",XP1_Greebles$CS) | XP1_Greebles$assignment == 1 & grepl("M",XP1_Greebles$CS), -0.5, +0.5) # +0.5 -> bloc1 = negative, bloc2 = positive // -0.5 -> bloc1 = positive, bloc2 = negative
XP1_Greebles$ratings <- XP1_Greebles$CSratings
XP1_Greebles <- XP1_Greebles[c("XP","ppt","stim","rwa","usvalence","ratings")]

# ? outliers ? Check data & See data

#boxplot(XP1_Greebles$rwa)
#hist(XP1_Greebles$rwa)
#unique(XP1_Greebles$usvalence)
#mean(XP1_Greebles$usvalence)

quart1 <- quantile(XP1_Greebles$rwa)[2]
quart3 <- quantile(XP1_Greebles$rwa)[4]
iqr <- IQR(XP1_Greebles$rwa)

#which(XP1_Greebles$rwa<quart1-3*iqr)
#which(XP1_Greebles$rwa>quart3+3*iqr)

XP1_Greebles$rwa <-  ifelse (XP1_Greebles$rwa<quart1-3*iqr | XP1_Greebles$rwa>quart3+3*iqr, NA, XP1_Greebles$rwa)
XP1_Greebles <- XP1_Greebles[which(!is.na(XP1_Greebles$rwa)),]
#XP1_Greebles$rwa <- (XP1_Greebles$rwa-mean(XP1_Greebles$rwa))/sd(XP1_Greebles$rwa)
XP1_Greebles$rwa <- scale (XP1_Greebles$rwa, center = TRUE, scale = TRUE)


########
# XP 2 #
########

XP2_Greebles$XP <- "2"
XP2_Greebles$ppt <- XP2_Greebles$Eprime.Basename
XP2_Greebles$stim <- XP2_Greebles$CS
XP2_Greebles$rwa <- (XP2_Greebles$RWA)
XP2_Greebles$usvalence <- ifelse(XP2_Greebles$interactC == "NegPos", +0.5, -0.5) # # +0.5 -> bloc1 = negative, bloc2 = positive // -0.5 -> bloc1 = positive, bloc2 = negative
XP2_Greebles$ratings <- XP2_Greebles$score
XP2_Greebles <- XP2_Greebles[which(XP2_Greebles$cond=="Greebles"),]
XP2_Greebles <- XP2_Greebles[c("XP","ppt","stim","rwa","usvalence","ratings")]

# ? outliers ? Check data & See data

#boxplot(XP2_Greebles$rwa)
#hist(XP2_Greebles$rwa)
#unique(XP2_Greebles$usvalence)
#mean(XP2_Greebles$usvalence)

quart1 <- quantile(XP2_Greebles$rwa)[2]
quart3 <- quantile(XP2_Greebles$rwa)[4]
iqr <- IQR(XP2_Greebles$rwa)

#which(XP2_Greebles$rwa<quart1-3*iqr)
#which(XP2_Greebles$rwa>quart3+3*iqr)

XP2_Greebles$rwa <-  ifelse (XP2_Greebles$rwa<quart1-3*iqr | XP2_Greebles$rwa>quart3+3*iqr, NA, XP2_Greebles$rwa)
XP2_Greebles <- XP2_Greebles[which(!is.na(XP2_Greebles$rwa)),]
XP2_Greebles$rwa <- scale (XP2_Greebles$rwa, center = TRUE, scale = TRUE)


########
# XP 3 #
########

XP3_Greebles$XP <- "3"
XP3_Greebles$stim <- XP3_Greebles$stim1
XP3_Greebles$rwa <- (XP3_Greebles$RWAsc)/135*70
XP3_Greebles$usvalence <- ifelse(XP3_Greebles$valence == "Pos", +0.5, -0.5) # +0.5 -> bloc1 = negative, bloc2 = positive // -0.5 -> bloc1 = positive, bloc2 = negative
XP3_Greebles$ratings <- XP3_Greebles$responses
#XP3_Greebles <- XP3_Greebles[which(XP3_Greebles$bloc=="2"),]
XP3_Greebles$bloc <- ifelse(XP3_Greebles$bloc == 1, -0.5, +0.5)
XP3_Greebles <- XP3_Greebles[c("XP","ppt","stim","rwa","usvalence", "bloc", "ratings")]

# ? outliers ? Check data & See data

#boxplot(XP3_Greebles$rwa)
#hist(XP3_Greebles$rwa)
#unique(XP3_Greebles$usvalence)
#mean(XP3_Greebles$usvalence)

quart1 <- quantile(XP3_Greebles$rwa)[2]
quart3 <- quantile(XP3_Greebles$rwa)[4]
iqr <- IQR(XP3_Greebles$rwa)

#which(XP3_Greebles$rwa<quart1-3*iqr)
#unique(XP3_Greebles$ppt[which(XP3_Greebles$rwa>quart3+3*iqr)])
#unique(XP3_Greebles$rwa[which(XP3_Greebles$rwa>quart3+3*iqr)])

XP3_Greebles$rwa <-  ifelse (XP3_Greebles$rwa<quart1-3*iqr | XP3_Greebles$rwa>quart3+3*iqr, NA, XP3_Greebles$rwa)
XP3_Greebles <- XP3_Greebles[which(!is.na(XP3_Greebles$rwa)),]
#XP3_Greebles$rwa <- (XP3_Greebles$rwa-mean(XP3_Greebles$rwa))/sd(XP3_Greebles$rwa)
XP3_Greebles$rwa <- scale (XP3_Greebles$rwa, center = TRUE, scale = TRUE)

###################################################################################################
# Data frame for the integrative data analysis on the first three studies (included in the paper) #
###################################################################################################

# Merging data from the three studies

IDA3_Greebles <- rbind(XP1_Greebles,XP2_Greebles,XP3_Greebles)
#View(IDA3_Greebles)

# Check data & See data

#boxplot(IDA3_Greebles$rwa)
#hist(IDA3_Greebles$rwa)
#unique(IDA3_Greebles$usvalence)
#mean(IDA3_Greebles$usvalence)


# ------------------------------- Supplementaty studies -------------------------------

########
# XP 4 #
########

# ? outliers ? Check data & See data

#boxplot(XP4$rwa)
#hist(XP4$rwa)
#unique(XP4$usvalence)
#mean(XP4$usvalence)

quart1 <- quantile(XP4$rwa)[2]
quart3 <- quantile(XP4$rwa)[4]
iqr <- IQR(XP4$rwa)

#which(XP4$rwa<quart1-3*iqr)
#unique(XP4$ppt[which(XP4$rwa>quart3+3*iqr)])
#unique(XP4$rwa[which(XP4$rwa>quart3+3*iqr)])

XP4$rwa <-  ifelse (XP4$rwa<quart1-3*iqr | XP4$rwa>quart3+3*iqr, NA, XP4$rwa)
XP4 <- XP4[which(!is.na(XP4$rwa)),]
XP4$rwa <- (XP4$rwa-mean(XP4$rwa))/sd(XP4$rwa)

#head(XP4)
#unique(XP4$usvalence)
#mean(XP4$usvalence)
#mean(XP4$rwa)

########
# XP 5 #
########

# ? outliers ? Check data & See data

#boxplot(XP5$rwa)
#hist(XP5$rwa)
#unique(XP5$usvalence)
#mean(XP5$usvalence)

quart1 <- quantile(XP5$rwa)[2]
quart3 <- quantile(XP5$rwa)[4]
iqr <- IQR(XP5$rwa)

#which(XP5$rwa<quart1-3*iqr)
#which(XP5$rwa>quart3+3*iqr)
#unique(XP5$ppt[which(XP5$rwa>quart3+3*iqr)])
#unique(XP5$rwa[which(XP5$rwa>quart3+3*iqr)])

XP5$rwa <-  ifelse (XP5$rwa<quart1-3*iqr | XP5$rwa>quart3+3*iqr, NA, XP5$rwa)
XP5 <- XP5[which(!is.na(XP5$rwa)),]
XP5$rwa <- (XP5$rwa-mean(XP5$rwa))/sd(XP5$rwa)

#head(XP5)
#unique(XP5$usvalence)
#mean(XP5$usvalence)
#mean(XP5$rwa)

########
# XP 6 #
########

# ? outliers ? Check data & See data

#boxplot(XP6$rwa)
#hist(XP6$rwa)
#unique(XP6$usvalence)
#mean(XP6$usvalence)

quart1 <- quantile(XP6$rwa)[2]
quart3 <- quantile(XP6$rwa)[4]
iqr <- IQR(XP6$rwa)

#which(XP6$rwa<quart1-3*iqr)
#which(XP6$rwa>quart3+3*iqr)
#unique(XP6$ppt[which(XP6$rwa>quart3+3*iqr)])
#unique(XP6$rwa[which(XP6$rwa>quart3+3*iqr)])

XP6$rwa <-  ifelse (XP6$rwa<quart1-3*iqr | XP6$rwa>quart3+3*iqr, NA, XP6$rwa)
XP6 <- XP6[which(!is.na(XP6$rwa)),]
XP6$rwa <- (XP6$rwa-mean(XP6$rwa))/sd(XP6$rwa)

#head(XP6)
#unique(XP6$usvalence)
#mean(XP6$usvalence)
#mean(XP6$rwa)

########
# XP 7 #
########

# ? outliers ? Check data & See data

#boxplot(XP7$rwa)
#hist(XP7$rwa)
#unique(XP7$usvalence)
#mean(XP7$usvalence)

quart1 <- quantile(XP7$rwa)[2]
quart3 <- quantile(XP7$rwa)[4]
iqr <- IQR(XP7$rwa)

#which(XP7$rwa>quart3+3*iqr)
#which(XP7$rwa<quart1-3*iqr)
#unique(XP7$ppt[which(XP7$rwa>quart3+3*iqr)])
#unique(XP7$rwa[which(XP7$rwa>quart3+3*iqr)])

XP7$rwa <-  ifelse (XP7$rwa<quart1-3*iqr | XP7$rwa>quart3+3*iqr, NA, XP7$rwa)
XP7 <- XP7[which(!is.na(XP7$rwa)),]
XP7$rwa <- (XP7$rwa-mean(XP7$rwa))/sd(XP7$rwa)

#unique(XP7$usvalence)
#mean(XP7$usvalence)
#mean(XP7$rwa)
#head(XP7)

XP7$ratings <- as.numeric(XP7$ratings)

#########################################################################################
# Data frame for the integrative data analysis on all studies (supplementary materials) #
#########################################################################################

IDA7_Greebles <-rbind(XP1_Greebles, XP2_Greebles, XP3_Greebles,
           XP4_Greebles, XP5_Greebles, XP6_Greebles, XP7_Greebles)

# Check data & See data

#boxplot(IDA$rwa)
#hist(IDA$rwa)
#unique(IDA$usvalence)
#mean(IDA$usvalence)
#unique(IDA$XP)
