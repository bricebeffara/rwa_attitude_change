

aggregate(response ~ usvalencedir, FUN = quantile, data = direct_df)

aggregate(ampresp ~ usvalence, FUN = mean, data = indir_df)



aggregate(response ~ order, FUN = quantile, data = direct_df)



aggregate(response ~ usvalence*direct_df, FUN = quantile, data = direct_df)


aggregate(RT ~ congruent, FUN = mean, data = apt_df)

aggregate(RT ~ congruent, FUN = sd, data = apt_df)


length(unique(direct_df$ppt))

length(unique(direct_df$stim1))

length(unique(direct_df_ordone$ppt))

length(unique(direct_df_ordtwo$ppt))

boxplot(RT ~ usvalence * order, data = apt_df)
