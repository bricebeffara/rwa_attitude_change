

aggregate(response ~ usvalence, FUN = quantile, data = spreading)



aggregate(response ~ spreading, FUN = quantile, data = spreading)



aggregate(response ~ usvalence*spreading, FUN = quantile, data = spreading)
