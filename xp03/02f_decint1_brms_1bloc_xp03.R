#------------------------------------------------------------------------------------
# Then we run our third step model to decompose the interaction 
# between RWA and usvalence1
#------------------------------------------------------------------------------------

#-------------
### Simple slope of RWA in the !!negative!! valence condition
#-------------

## first step = recode variable usvalence1
## We interpret parameters for a 0 level of others

# Coding !!negative!! valence as 0
bloc_df$usvalence1_neg <- ifelse (bloc_df$usvalence1 == -0.5, 0, 1)

bloc1_resp_neg <- brm(response ~ usvalence1_neg  * RWAscore + (1|ppt) + (1|stim1), #here we change the usvalence1 variable with the recoded one
                             data = bloc_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_bloc1_neg_xp03 <- summary(bloc1_resp_neg)$fixed[,col2keep]
# model_bloc1_neg_xp03 <- round(model_bloc1_neg_xp03, 3)

# arrange output
model_bloc1_neg_xp03 <- tidy_stan(bloc1_resp_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_bloc1_neg_xp03.png", height=480, width=720)
p<-tableGrob(model_bloc1_neg_xp03)
grid.arrange(p)
dev.off()

# test with rope
equi_bloc1_neg_xp03_rwa <- equi_test(bloc1_resp_neg, rope = roperwaN)
equi_bloc1_neg_xp03_rwa <- equi_bloc1_neg_xp03_rwa[c(10,11),]
equi_bloc1_neg_xp03_rwa$ROPE <- roperwaC

equi_bloc1_neg_xp03_0.5 <- equi_test(bloc1_resp_neg, rope = c(-0.5, 0.5))
equi_bloc1_neg_xp03_0.5 <- equi_bloc1_neg_xp03_0.5[c(9),]
equi_bloc1_neg_xp03_0.5$ROPE <- "-0.5, 0.5"

equi_bloc1_neg_xp03 <- rbind(equi_bloc1_neg_xp03_rwa, equi_bloc1_neg_xp03_0.5)
equi_bloc1_neg_xp03[,c(3:5)] <- round(equi_bloc1_neg_xp03[,c(3:5)], 2)
ordrow <- c("3","1", "2")
equi_bloc1_neg_xp03 <- equi_bloc1_neg_xp03[ordrow,]

# export test
png("tables/rope/equi_bloc1_neg_xp03.png", height=480, width=720)
p<-tableGrob(equi_bloc1_neg_xp03)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence condition
#-------------

## first step = recode variable usvalence1
## We interpret parameters for a 0 level of others

# Coding !!positive!! valence as 0
bloc_df$usvalence1_pos <- ifelse (bloc_df$usvalence1 == 0.5, 0, 1)

bloc1_resp_pos <- brm(response ~ usvalence1_pos * RWAscore + (1|ppt) + (1|stim1), #here we change the usvalence1 variable with the recoded one
                             data = bloc_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_bloc1_pos_xp03 <- summary(bloc1_resp_pos)$fixed[,col2keep]
# model_bloc1_pos_xp03 <- round(model_bloc1_pos_xp03, 3)

# arrange output
model_bloc1_pos_xp03 <- tidy_stan(bloc1_resp_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_bloc1_pos_xp03.png", height=480, width=720)
p<-tableGrob(model_bloc1_pos_xp03)
grid.arrange(p)
dev.off()

# test with rope
equi_bloc1_pos_xp03_rwa <- equi_test(bloc1_resp_pos, rope = roperwaN)
equi_bloc1_pos_xp03_rwa <- equi_bloc1_pos_xp03_rwa[c(10,11),]
equi_bloc1_pos_xp03_rwa$ROPE <- roperwaC

equi_bloc1_pos_xp03_0.5 <- equi_test(bloc1_resp_pos, rope = c(-0.5, 0.5))
equi_bloc1_pos_xp03_0.5 <- equi_bloc1_pos_xp03_0.5[c(9),]
equi_bloc1_pos_xp03_0.5$ROPE <- "-0.5, 0.5"

equi_bloc1_pos_xp03 <- rbind(equi_bloc1_pos_xp03_rwa, equi_bloc1_pos_xp03_0.5)
equi_bloc1_pos_xp03[,c(3:5)] <- round(equi_bloc1_pos_xp03[,c(3:5)], 2)
ordrow <- c("3","1", "2")
equi_bloc1_pos_xp03 <- equi_bloc1_pos_xp03[ordrow,]

# export test
png("tables/rope/equi_bloc1_pos_xp03.png", height=480, width=720)
p<-tableGrob(equi_bloc1_pos_xp03)
grid.arrange(p)
dev.off()
