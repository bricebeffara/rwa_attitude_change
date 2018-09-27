

aggregate(response ~ usvalence, FUN = quantile, data = doubletime_df)



aggregate(response ~ doubletime_df, FUN = quantile, data = doubletime_df)



aggregate(response ~ usvalence*doubletime_df, FUN = quantile, data = doubletime_df)

length(unique(doubletime_df$ppt))

length(unique(doubletime_df$ppt[which(doubletime_df$condition=="one_time")]))
length(unique(doubletime_df$ppt[which(doubletime_df$condition=="two")]))
length(unique(doubletime_df$ppt[which(doubletime_df$condition=="one")]))
length(unique(doubletime_df$ppt[which(doubletime_df$condition=="time_one")]))

length(unique(doubletime_df_ctone$ppt))

length(unique(doubletime_df_cttwo$ppt))
