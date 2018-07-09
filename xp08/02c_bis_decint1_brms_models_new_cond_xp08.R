#############################
#####onetime counter-conditioning + delay
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!1 counter-conditioning + delay!! condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!negative!! valence as 0
double_onetime_df$usvalence_neg <- ifelse (double_onetime_df$usvalence == -0.5, 0, 1)

doubleonetime_resp_ctone_neg <- brm(response ~ usvalence_neg * counter_one  * RWAscore + (1|ppt) + (1|stim1), #here we change the doubleonetime_df variable with the recoded onetime
                                data = double_onetime_df, 
                                family = cumulative (link = "logit", threshold = "flexible"),
                                prior = priors,
                                warmup = 1000, iter = 2000,
                                chains = 4, cores = parallel::detectCores(),
                                control = list(adapt_delta = 0.8, max_treedepth = 10),
                                sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ctonetime_neg_xp08 <- summary(doubleonetime_resp_ctone_neg)$fixed[,col2keep]
# model_ctonetime_neg_xp08 <- round(model_ctonetime_neg_xp08, 3)

# arrange output
model_ctonetime_neg_xp08 <- tidy_stan(doubleonetime_resp_ctone_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_ctonetime_neg_xp08.png", height=480, width=720)
p<-tableGrob(model_ctonetime_neg_xp08)
grid.arrange(p)
dev.off()

# test with rope
equi_ctonetime_neg_xp08_rwa <- equi_test(doubleonetime_resp_ctone_neg, rope = roperwaN)
equi_ctonetime_neg_xp08_rwa <- equi_ctonetime_neg_xp08_rwa[c(11,13:15),]
equi_ctonetime_neg_xp08_rwa$ROPE <- roperwaC

equi_ctonetime_neg_xp08_0.5 <- equi_test(doubleonetime_resp_ctone_neg, rope = c(-0.5, 0.5))
equi_ctonetime_neg_xp08_0.5 <- equi_ctonetime_neg_xp08_0.5[c(9,10,12),]
equi_ctonetime_neg_xp08_0.5$ROPE <- "-0.5, 0.5"

equi_ctonetime_neg_xp08 <- rbind(equi_ctonetime_neg_xp08_rwa, equi_ctonetime_neg_xp08_0.5)
equi_ctonetime_neg_xp08[,c(3:5)] <- round(equi_ctonetime_neg_xp08[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_ctonetime_neg_xp08 <- equi_ctonetime_neg_xp08[ordrow,]

# export test
png("tables/rope/equi_ctonetime_neg_xp08.png", height=480, width=720)
p<-tableGrob(equi_ctonetime_neg_xp08)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!1 counter-conditioning!! conditioning condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!positive!! valence as 0
double_onetime_df$usvalence_pos <- ifelse (double_onetime_df$usvalence == 0.5, 0, 1)

doubleonetime_resp_ctone_pos <- brm(response ~ usvalence_pos * counter_one  * RWAscore + (1|ppt) + (1|stim1), #here we change the doubleonetime_df variable with the recoded onetime
                                data = double_onetime_df, 
                                family = cumulative (link = "logit", threshold = "flexible"),
                                prior = priors,
                                warmup = 1000, iter = 2000,
                                chains = 4, cores = parallel::detectCores(),
                                control = list(adapt_delta = 0.8, max_treedepth = 10),
                                sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ctonetime_pos_xp08 <- summary(doubleonetime_resp_ctone_pos)$fixed[,col2keep]
# model_ctonetime_pos_xp08 <- round(model_ctonetime_pos_xp08, 3)

# arrange output
model_ctonetime_pos_xp08 <- tidy_stan(doubleonetime_resp_ctone_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_ctonetime_pos_xp08.png", height=480, width=720)
p<-tableGrob(model_ctonetime_pos_xp08)
grid.arrange(p)
dev.off()

# test with rope
equi_ctonetime_pos_xp08_rwa <- equi_test(doubleonetime_resp_ctone_pos, rope = roperwaN)
equi_ctonetime_pos_xp08_rwa <- equi_ctonetime_pos_xp08_rwa[c(11,13:15),]
equi_ctonetime_pos_xp08_rwa$ROPE <- roperwaC

equi_ctonetime_pos_xp08_0.5 <- equi_test(doubleonetime_resp_ctone_pos, rope = c(-0.5, 0.5))
equi_ctonetime_pos_xp08_0.5 <- equi_ctonetime_pos_xp08_0.5[c(9,10,12),]
equi_ctonetime_pos_xp08_0.5$ROPE <- "-0.5, 0.5"

equi_ctonetime_pos_xp08 <- rbind(equi_ctonetime_pos_xp08_rwa, equi_ctonetime_pos_xp08_0.5)
equi_ctonetime_pos_xp08[,c(3:5)] <- round(equi_ctonetime_pos_xp08[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_ctonetime_pos_xp08 <- equi_ctonetime_pos_xp08[ordrow,]

# export test
png("tables/rope/equi_ctonetime_pos_xp08.png", height=480, width=720)
p<-tableGrob(equi_ctonetime_pos_xp08)
grid.arrange(p)
dev.off()

#############################
####1 delay + counter-conditionings
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!1 delay + counter-conditionings!! condition
#-------------

double_timeone_df$usvalence_neg <- ifelse (double_timeone_df$usvalence == -0.5, 0, 1)

doubletimeone_resp_ctone_neg <- brm(response ~ usvalence_neg * counter_one  * RWAscore + (1|ppt) + (1|stim1), #here we change the doubletimeone_df variable with the recoded timeone
                                data = double_timeone_df, 
                                family = cumulative (link = "logit", threshold = "flexible"),
                                prior = priors,
                                warmup = 1000, iter = 2000,
                                chains = 4, cores = parallel::detectCores(),
                                control = list(adapt_delta = 0.8, max_treedepth = 10),
                                sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_timeone_neg_xp08 <- summary(doubletimeone_resp_ctone_neg)$fixed[,col2keep]
# model_timeone_neg_xp08 <- round(model_timeone_neg_xp08, 3)

# arrange output
model_timeone_neg_xp08 <- tidy_stan(doubletimeone_resp_ctone_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_timeone_neg_xp08.png", height=480, width=720)
p<-tableGrob(model_timeone_neg_xp08)
grid.arrange(p)
dev.off()

# test with rope
equi_timeone_neg_xp08_rwa <- equi_test(doubletimeone_resp_ctone_neg, rope = roperwaN)
equi_timeone_neg_xp08_rwa <- equi_timeone_neg_xp08_rwa[c(11,13:15),]
equi_timeone_neg_xp08_rwa$ROPE <- roperwaC

equi_timeone_neg_xp08_0.5 <- equi_test(doubletimeone_resp_ctone_neg, rope = c(-0.5, 0.5))
equi_timeone_neg_xp08_0.5 <- equi_timeone_neg_xp08_0.5[c(9,10,12),]
equi_timeone_neg_xp08_0.5$ROPE <- "-0.5, 0.5"

equi_timeone_neg_xp08 <- rbind(equi_timeone_neg_xp08_rwa, equi_timeone_neg_xp08_0.5)
equi_timeone_neg_xp08[,c(3:5)] <- round(equi_timeone_neg_xp08[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_timeone_neg_xp08 <- equi_timeone_neg_xp08[ordrow,]

# export test
png("tables/rope/equi_timeone_neg_xp08.png", height=480, width=720)
p<-tableGrob(equi_timeone_neg_xp08)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!2 counter-conditionings!! condition
#-------------

double_timeone_df$usvalence_pos <- ifelse (double_timeone_df$usvalence == 0.5, 0, 1)

doubletimeone_resp_ctone_pos <- brm(response ~ usvalence_pos * counter_one  * RWAscore + (1|ppt) + (1|stim1), #here we change the doubletimeone_df variable with the recoded timeone
                                data = double_timeone_df, 
                                family = cumulative (link = "logit", threshold = "flexible"),
                                prior = priors,
                                warmup = 1000, iter = 2000,
                                chains = 4, cores = parallel::detectCores(),
                                control = list(adapt_delta = 0.8, max_treedepth = 10),
                                sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_timeone_pos_xp08 <- summary(doubletimeone_resp_ctone_pos)$fixed[,col2keep]
# model_timeone_pos_xp08 <- round(model_timeone_pos_xp08, 3)

# arrange output
model_timeone_pos_xp08 <- tidy_stan(doubletimeone_resp_ctone_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_timeone_pos_xp08.png", height=480, width=720)
p<-tableGrob(model_timeone_pos_xp08)
grid.arrange(p)
dev.off()

# test with rope
equi_timeone_pos_xp08_rwa <- equi_test(doubletimeone_resp_ctone_pos, rope = roperwaN)
equi_timeone_pos_xp08_rwa <- equi_timeone_pos_xp08_rwa[c(11,13:15),]
equi_timeone_pos_xp08_rwa$ROPE <- roperwaC

equi_timeone_pos_xp08_0.5 <- equi_test(doubletimeone_resp_ctone_pos, rope = c(-0.5, 0.5))
equi_timeone_pos_xp08_0.5 <- equi_timeone_pos_xp08_0.5[c(9,10,12),]
equi_timeone_pos_xp08_0.5$ROPE <- "-0.5, 0.5"

equi_timeone_pos_xp08 <- rbind(equi_timeone_pos_xp08_rwa, equi_timeone_pos_xp08_0.5)
equi_timeone_pos_xp08[,c(3:5)] <- round(equi_timeone_pos_xp08[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_timeone_pos_xp08 <- equi_timeone_pos_xp08[ordrow,]

# export test
png("tables/rope/equi_timeone_pos_xp08.png", height=480, width=720)
p<-tableGrob(equi_timeone_pos_xp08)
grid.arrange(p)
dev.off()
