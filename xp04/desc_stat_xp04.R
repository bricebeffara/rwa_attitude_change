

aggregate(response ~ usvalence, FUN = quantile, data = diriat_df)



aggregate(response ~ order, FUN = quantile, data = diriat_df)



aggregate(response ~ usvalence*diriat_df, FUN = quantile, data = diriat_df)


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
