#------------------------------------------------------------------------------------
# Then we run our third step models to decompose the interactions 
# between RWA and usvalence in the level 1 and level 2 conditioning conditions
#------------------------------------------------------------------------------------

#############################
##################### Level 1
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!level 1!! conditioning condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!negative!! valence as 0
spreading$usvalence_neg <- ifelse (spreading$usvalence == -0.5, 0, 1)

spread_resp_lvone_neg <- brm(response ~ usvalence_neg * level_one  * RWAscore + (1|ppt) + (1|stim1), #here we change the spreading variable with the recoded one
                             data = spreading, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_lvone_neg_xp10 <- summary(spread_resp_lvone_neg)$fixed[,col2keep]
# model_lvone_neg_xp10 <- round(model_lvone_neg_xp10, 3)

# arrange output
model_lvone_neg_xp10 <- tidy_stan(spread_resp_lvone_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_lvone_neg_xp10.png", height=480, width=720)
p<-tableGrob(model_lvone_neg_xp10)
grid.arrange(p)
dev.off()

# test with rope
equi_lvone_neg_xp10_rwa <- equi_test(spread_resp_lvone_neg, rope = roperwaN)
equi_lvone_neg_xp10_rwa <- equi_lvone_neg_xp10_rwa[c(11,13:15),]
equi_lvone_neg_xp10_rwa$ROPE <- roperwaC

equi_lvone_neg_xp10_0.5 <- equi_test(spread_resp_lvone_neg, rope = c(-0.5, 0.5))
equi_lvone_neg_xp10_0.5 <- equi_lvone_neg_xp10_0.5[c(9,10,12),]
equi_lvone_neg_xp10_0.5$ROPE <- "-0.5, 0.5"

equi_lvone_neg_xp10 <- rbind(equi_lvone_neg_xp10_rwa, equi_lvone_neg_xp10_0.5)
equi_lvone_neg_xp10[,c(3:5)] <- round(equi_lvone_neg_xp10[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_lvone_neg_xp10 <- equi_lvone_neg_xp10[ordrow,]

# export test
png("tables/rope/equi_lvone_neg_xp10.png", height=480, width=720)
p<-tableGrob(equi_lvone_neg_xp10)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!level 1!! conditioning condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!positive!! valence as 0
spreading$usvalence_pos <- ifelse (spreading$usvalence == 0.5, 0, 1)

spread_resp_lvone_pos <- brm(response ~ usvalence_pos * level_one  * RWAscore + (1|ppt) + (1|stim1), #here we change the spreading variable with the recoded one
                             data = spreading, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_lvone_pos_xp10 <- summary(spread_resp_lvone_pos)$fixed[,col2keep]
# model_lvone_pos_xp10 <- round(model_lvone_pos_xp10, 3)

# arrange output
model_lvone_pos_xp10 <- tidy_stan(spread_resp_lvone_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_lvone_pos_xp10.png", height=480, width=720)
p<-tableGrob(model_lvone_pos_xp10)
grid.arrange(p)
dev.off()

# test with rope
equi_lvone_pos_xp10_rwa <- equi_test(spread_resp_lvone_pos, rope = roperwaN)
equi_lvone_pos_xp10_rwa <- equi_lvone_pos_xp10_rwa[c(11,13:15),]
equi_lvone_pos_xp10_rwa$ROPE <- roperwaC

equi_lvone_pos_xp10_0.5 <- equi_test(spread_resp_lvone_pos, rope = c(-0.5, 0.5))
equi_lvone_pos_xp10_0.5 <- equi_lvone_pos_xp10_0.5[c(9,10,12),]
equi_lvone_pos_xp10_0.5$ROPE <- "-0.5, 0.5"

equi_lvone_pos_xp10 <- rbind(equi_lvone_pos_xp10_rwa, equi_lvone_pos_xp10_0.5)
equi_lvone_pos_xp10[,c(3:5)] <- round(equi_lvone_pos_xp10[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_lvone_pos_xp10 <- equi_lvone_pos_xp10[ordrow,]

# export test
png("tables/rope/equi_lvone_pos_xp10.png", height=480, width=720)
p<-tableGrob(equi_lvone_pos_xp10)
grid.arrange(p)
dev.off()

#############################
##################### Level 2
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!level 2!! conditioning condition
#-------------

spread_resp_lvtwo_neg <- brm(response ~ usvalence_neg * level_two  * RWAscore + (1|ppt) + (1|stim1), #here we change the spreading variable with the recoded one
                             data = spreading, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_lvtwo_neg_xp10 <- summary(spread_resp_lvtwo_neg)$fixed[,col2keep]
# model_lvtwo_neg_xp10 <- round(model_lvtwo_neg_xp10, 3)

# arrange output
model_lvtwo_neg_xp10 <- tidy_stan(spread_resp_lvtwo_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_lvtwo_neg_xp10.png", height=480, width=720)
p<-tableGrob(model_lvtwo_neg_xp10)
grid.arrange(p)
dev.off()

# test with rope
equi_lvtwo_neg_xp10_rwa <- equi_test(spread_resp_lvtwo_neg, rope = roperwaN)
equi_lvtwo_neg_xp10_rwa <- equi_lvtwo_neg_xp10_rwa[c(11,13:15),]
equi_lvtwo_neg_xp10_rwa$ROPE <- roperwaC

equi_lvtwo_neg_xp10_0.5 <- equi_test(spread_resp_lvtwo_neg, rope = c(-0.5, 0.5))
equi_lvtwo_neg_xp10_0.5 <- equi_lvtwo_neg_xp10_0.5[c(9,10,12),]
equi_lvtwo_neg_xp10_0.5$ROPE <- "-0.5, 0.5"

equi_lvtwo_neg_xp10 <- rbind(equi_lvtwo_neg_xp10_rwa, equi_lvtwo_neg_xp10_0.5)
equi_lvtwo_neg_xp10[,c(3:5)] <- round(equi_lvtwo_neg_xp10[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_lvtwo_neg_xp10 <- equi_lvtwo_neg_xp10[ordrow,]

# export test
png("tables/rope/equi_lvtwo_neg_xp10.png", height=480, width=720)
p<-tableGrob(equi_lvtwo_neg_xp10)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!level 2!! conditioning condition
#-------------

spread_resp_lvtwo_pos <- brm(response ~ usvalence_pos * level_two  * RWAscore + (1|ppt) + (1|stim1), #here we change the spreading variable with the recoded one
                             data = spreading, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_lvtwo_pos_xp10 <- summary(spread_resp_lvtwo_pos)$fixed[,col2keep]
# model_lvtwo_pos_xp10 <- round(model_lvtwo_pos_xp10, 3)

# arrange output
model_lvtwo_pos_xp10 <- tidy_stan(spread_resp_lvtwo_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_lvtwo_pos_xp10.png", height=480, width=720)
p<-tableGrob(model_lvtwo_pos_xp10)
grid.arrange(p)
dev.off()

# test with rope
equi_lvtwo_pos_xp10_rwa <- equi_test(spread_resp_lvtwo_pos, rope = roperwaN)
equi_lvtwo_pos_xp10_rwa <- equi_lvtwo_pos_xp10_rwa[c(11,13:15),]
equi_lvtwo_pos_xp10_rwa$ROPE <- roperwaC

equi_lvtwo_pos_xp10_0.5 <- equi_test(spread_resp_lvtwo_pos, rope = c(-0.5, 0.5))
equi_lvtwo_pos_xp10_0.5 <- equi_lvtwo_pos_xp10_0.5[c(9,10,12),]
equi_lvtwo_pos_xp10_0.5$ROPE <- "-0.5, 0.5"

equi_lvtwo_pos_xp10 <- rbind(equi_lvtwo_pos_xp10_rwa, equi_lvtwo_pos_xp10_0.5)
equi_lvtwo_pos_xp10[,c(3:5)] <- round(equi_lvtwo_pos_xp10[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_lvtwo_pos_xp10 <- equi_lvtwo_pos_xp10[ordrow,]

# export test
png("tables/rope/equi_lvtwo_pos_xp10.png", height=480, width=720)
p<-tableGrob(equi_lvtwo_pos_xp10)
grid.arrange(p)
dev.off()
