library(stringr)

aggregate(response ~ usvalence2, FUN = quantile, data = bloc_df)

aggregate(response ~ usvalence1, FUN = quantile, data = bloc_df)

aggregate(response ~ bloc, FUN = quantile, data = bloc_df)

aggregate(response ~ usvalence2 * bloc, FUN = quantile, data = bloc_df)

length(unique(bloc_df$ppt))

length(unique(bloc_df$stim1))

length(unique(bloc_df$ppt[which(bloc_df$bloc == -0.5)]))

length(unique(bloc_df$ppt[which(bloc_df$bloc == 0.5)]))

length (which(str_sub(bloc_df$aware, start= -1) == 0)) / length (bloc_df$aware)

mean (bloc_df$aware)

sd (bloc_df$aware)

aggregate(aware ~ usvalence, FUN = sd, data = bloc_df)

aggregate(aware ~ bloc, FUN = mean, data = bloc_df)



unique(bloc_df_backup$ppt[which(bloc_df$RWAscore>quart3+3*iqr)])

unique(bloc_df_backup$RWAscore[which(bloc_df$RWAscore>quart3+3*iqr)])

quart1

quart3

iqr
