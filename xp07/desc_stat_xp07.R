

aggregate(response ~ usvalence, FUN = quantile, data = double_df)



aggregate(response ~ counter2, FUN = quantile, data = double_df)



aggregate(response ~ usvalence*double_df, FUN = quantile, data = double_df)

length(unique(double_df$ppt))

length(unique(double_df_ctone$ppt))

length(unique(double_df_cttwo$ppt))
