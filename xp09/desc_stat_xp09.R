

aggregate(response ~ usvalence, FUN = quantile, data = load_df)
aggregate(response ~ usvalence, FUN = mean, data = load_df)



aggregate(response ~ load, FUN = quantile, data = load_df)



aggregate(response ~ usvalence*load, FUN = quantile, data = load_df)

length(unique(load_df$ppt))

length(unique(load_df_nolo$ppt))

length(unique(load_df_yelo$ppt))
