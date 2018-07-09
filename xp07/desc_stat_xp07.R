

aggregate(response ~ usvalence, FUN = quantile, data = spreading)



aggregate(response ~ spreading, FUN = quantile, data = spreading)



aggregate(response ~ usvalence*spreading, FUN = quantile, data = spreading)

length(unique(double_df$ppt))

length(unique(double_df_ctone$ppt))

length(unique(double_df_cttwo$ppt))
