

aggregate(response ~ usvalence, FUN = quantile, data = warn_df)



aggregate(response ~ warn, FUN = quantile, data = warn_df)



aggregate(response ~ usvalence*warn, FUN = quantile, data = warn_df)

length(unique(warn_df$ppt))

length(unique(warn_df_nowa$ppt))

length(unique(warn_df_yewa$ppt))
