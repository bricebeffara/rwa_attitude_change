

aggregate(response ~ usvalence, FUN = quantile, data = spreading)



aggregate(response ~ spreading, FUN = quantile, data = spreading)



aggregate(response ~ usvalence*spreading, FUN = quantile, data = spreading)

length(unique(doubletime_df$ppt))

length(unique(doubletime_df_ctone$ppt))

length(unique(doubletime_df_cttwo$ppt))
