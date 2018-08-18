

aggregate(response ~ usvalence, FUN = quantile, data = base_df)

length(unique(base_df$ppt))

length(unique(base_df$stim1))
