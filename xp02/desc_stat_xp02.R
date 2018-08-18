library(stringr)

aggregate(response ~ usvalence, FUN = quantile, data = social_df)

aggregate(response ~ social, FUN = quantile, data = social_df)

length(unique(social_df$ppt))

length(unique(social_df$stim1))

length(unique(social_df$ppt[(grep("Greeble", social_df$ppt))]))

length(unique(social_df$ppt[(grep("Objet", social_df$ppt))]))

length (which(str_sub(social_df$aware, start= -1) == 0)) / length (social_df$aware)

mean (social_df$aware)

sd (social_df$aware)

aggregate(aware ~ usvalence, FUN = sd, data = social_df)

aggregate(aware ~ social, FUN = mean, data = social_df)

