

aggregate(response ~ usvalence, FUN = quantile, data = expli_df)



aggregate(response ~ order, FUN = quantile, data = expli_df)



aggregate(response ~ usvalence*expli_df, FUN = quantile, data = expli_df)


aggregate(RT ~ congruent, FUN = mean, data = apt_df)

aggregate(RT ~ congruent, FUN = sd, data = apt_df)


length(unique(expli_df$ppt))

length(unique(expli_df$stim1))

length(unique(expli_df_ordone$ppt))

length(unique(expli_df_ordtwo$ppt))

boxplot(RT ~ usvalence * order, data = apt_df)
