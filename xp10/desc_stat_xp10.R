

aggregate(response ~ usvalence, FUN = quantile, data = spreading)



aggregate(response ~ spreading, FUN = quantile, data = spreading)



aggregate(response ~ usvalence*spreading, FUN = quantile, data = spreading)

length(unique(spreading$ppt))

length(unique(spreading_lvone$ppt))

length(unique(spreading_lvtwo$ppt))
