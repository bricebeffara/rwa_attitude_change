

aggregate(response ~ usvalence, FUN = quantile, data = diriat_df)



aggregate(response ~ order, FUN = quantile, data = diriat_df)



aggregate(response ~ usvalence, FUN = quantile, data = diriat_df)


aggregate(RT ~ congruent, FUN = mean, data = apt_df)

aggregate(RT ~ congruent, FUN = sd, data = apt_df)


length(unique(diriat_df$ppt))

length(unique(diriat_df$stim1))

diriat_df_ordone <- diriat_df[diriat_df$order == -0.5,]
diriat_df_ordtwo <- diriat_df[diriat_df$order == 0.5,]

diriat_df[diriat_df$ppt == 6280,]

length(unique(diriat_df_ordone$ppt))

length(unique(diriat_df_ordtwo$ppt))

boxplot(RT ~ usvalence * order, data = apt_df)

### indirect

#1
length(IAT$RT)
#2272

#2
length(IAT$RT)
#2270

#3
length( unique( IAT$Subject))
#67

#4
length(which(IAT$RT<300))
#67

#5
length( unique( IAT$Subject))
#67

length( unique( IAT$Subject[which(IAT$order==0.5)]))
length( unique( IAT$Subject[which(IAT$order==-0.5)]))

length(iat_df$correct[which(iat_df$correct == 0)])
length(iat_df$correct[which(iat_df$correct == 1)])

(1- length(iat_df$correct[which(iat_df$correct == 0)]) / length(iat_df$correct[which(iat_df$correct == 1)])) *100

length(iat_rt$correct[which(iat_rt$correct == 0)])
length(iat_rt$correct[which(iat_rt$correct == 1)])
