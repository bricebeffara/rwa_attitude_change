

aggregate(response ~ usvalence, FUN = quantile, data = warn_df)



aggregate(response ~ warn, FUN = quantile, data = warn_df)



aggregate(response ~ usvalence*warn, FUN = quantile, data = warn_df)
