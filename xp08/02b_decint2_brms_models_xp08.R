#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the interaction between RWA and usvalence at each number of counter-conditionings
#------------------------------------------------------------------------------------

#-------------
### RWA * valence in the !!1 counter-conditioning!! condition
#-------------

## first step = recode variable double_df
## We interpret parameters for a 0 level of others

# Coding !!One counter-conditioning!! condition
double_one_df$counter_one <- ifelse (double_one_df$counter2 == -0.5, 0, 1)

doubleone_resp_ctone <- brm(response ~ usvalence * counter_one  * RWAscore + (1|ppt) + (1|stim1), #here we change the doubleone_df variable with the recoded one
                         data = double_one_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ctone_xp08 <- summary(doubleone_resp_ctone)$fixed[,col2keep]
# model_ctone_xp08 <- round(model_ctone_xp08, 3)

# arrange output
model_ctone_xp08 <- tidy_stan(doubleone_resp_ctone,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_ctone_xp08.png", height=480, width=720)
p<-tableGrob(model_ctone_xp08)
grid.arrange(p)
dev.off()

# test with rope
equi_ctone_xp08_rwa <- equi_test(doubleone_resp_ctone, rope = roperwaN)
equi_ctone_xp08_rwa <- equi_ctone_xp08_rwa[c(11,13:15),]
equi_ctone_xp08_rwa$ROPE <- roperwaC

equi_ctone_xp08_0.5 <- equi_test(doubleone_resp_ctone, rope = c(-0.5, 0.5))
equi_ctone_xp08_0.5 <- equi_ctone_xp08_0.5[c(9,10,12),]
equi_ctone_xp08_0.5$ROPE <- "-0.5, 0.5"

equi_ctone_xp08 <- rbind(equi_ctone_xp08_rwa, equi_ctone_xp08_0.5)
equi_ctone_xp08[,c(3:5)] <- round(equi_ctone_xp08[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_ctone_xp08 <- equi_ctone_xp08[ordrow,]

# export test
png("tables/rope/equi_ctone_xp08.png", height=480, width=720)
p<-tableGrob(equi_ctone_xp08)
grid.arrange(p)
dev.off()


#-------------
### RWA * usvalence in the !!2 counter-conditionings!! condition
#-------------

## first step = recode variable double_df
## We interpret parameters for a 0 level of others

# Coding !!Two counter-conditionings!! condition
double_one_df$counter_two <- ifelse (double_one_df$counter2 == 0.5, 0, 1)

doubleone_resp_cttwo <- brm(response ~ usvalence * counter_two  * RWAscore + (1|ppt) + (1|stim1), #here we change the doubleone_df variable with the recoded one
                         data = double_one_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_cttwo_xp08 <- summary(doubleone_resp_cttwo)$fixed[,col2keep]
# model_cttwo_xp08 <- round(model_cttwo_xp08, 3)

# arrange output
model_cttwo_xp08 <- tidy_stan(doubleone_resp_cttwo,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_cttwo_xp08.png", height=480, width=720)
p<-tableGrob(model_cttwo_xp08)
grid.arrange(p)
dev.off()

# test with rope
equi_cttwo_xp08_rwa <- equi_test(doubleone_resp_cttwo, rope = roperwaN)
equi_cttwo_xp08_rwa <- equi_cttwo_xp08_rwa[c(11,13:15),]
equi_cttwo_xp08_rwa$ROPE <- roperwaC

equi_cttwo_xp08_0.5 <- equi_test(doubleone_resp_cttwo, rope = c(-0.5, 0.5))
equi_cttwo_xp08_0.5 <- equi_cttwo_xp08_0.5[c(9,10,12),]
equi_cttwo_xp08_0.5$ROPE <- "-0.5, 0.5"

equi_cttwo_xp08 <- rbind(equi_cttwo_xp08_rwa, equi_cttwo_xp08_0.5)
equi_cttwo_xp08[,c(3:5)] <- round(equi_cttwo_xp08[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_cttwo_xp08 <- equi_cttwo_xp08[ordrow,]

# export test
png("tables/rope/equi_cttwo_xp08.png", height=480, width=720)
p<-tableGrob(equi_cttwo_xp08)
grid.arrange(p)
dev.off()

#-------------
### RWA * valence in the !!1 counter-conditioning + delay !! condition
#-------------

## first step = recode variable double_df
## We interpret parameters for a 0 level of others

# Coding !!onetime counter-conditioning!! condition
double_onetime_df$counter_one <- ifelse (double_onetime_df$counter2 == -0.5, 0, 1)

doubleonetime_resp_ctone <- brm(response ~ usvalence * counter_one  * RWAscore + (1|ppt) + (1|stim1), #here we change the doubleonetime_df variable with the recoded onetime
                            data = double_onetime_df, 
                            family = cumulative (link = "logit", threshold = "flexible"),
                            prior = priors,
                            warmup = 1000, iter = 2000,
                            chains = 4, cores = parallel::detectCores(),
                            control = list(adapt_delta = 0.8, max_treedepth = 10),
                            sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ctonetime_xp08 <- summary(doubleonetime_resp_ctonetime)$fixed[,col2keep]
# model_ctonetime_xp08 <- round(model_ctonetime_xp08, 3)

# arrange output
model_ctonetime_xp08 <- tidy_stan(doubleonetime_resp_ctone,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_ctonetime_xp08.png", height=480, width=720)
p<-tableGrob(model_ctonetime_xp08)
grid.arrange(p)
dev.off()

# test with rope
equi_ctonetime_xp08_rwa <- equi_test(doubleonetime_resp_ctone, rope = roperwaN)
equi_ctonetime_xp08_rwa <- equi_ctonetime_xp08_rwa[c(11,13:15),]
equi_ctonetime_xp08_rwa$ROPE <- roperwaC

equi_ctonetime_xp08_0.5 <- equi_test(doubleonetime_resp_ctone, rope = c(-0.5, 0.5))
equi_ctonetime_xp08_0.5 <- equi_ctonetime_xp08_0.5[c(9,10,12),]
equi_ctonetime_xp08_0.5$ROPE <- "-0.5, 0.5"

equi_ctonetime_xp08 <- rbind(equi_ctonetime_xp08_rwa, equi_ctonetime_xp08_0.5)
equi_ctonetime_xp08[,c(3:5)] <- round(equi_ctonetime_xp08[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_ctonetime_xp08 <- equi_ctonetime_xp08[ordrow,]

# export test
png("tables/rope/equi_ctonetime_xp08.png", height=480, width=720)
p<-tableGrob(equi_ctonetime_xp08)
grid.arrange(p)
dev.off()

#-------------
### RWA * valence in the !! delay + 1 counter-conditioning !! condition
#-------------

## first step = recode variable double_df
## We interpret parameters for a 0 level of others

# Coding !!timeone counter-conditioning!! condition
double_timeone_df$counter_one <- ifelse (double_timeone_df$counter2 == -0.5, 0, 1)

doubletimeone_resp_ctone <- brm(response ~ usvalence * counter_one  * RWAscore + (1|ppt) + (1|stim1), #here we change the doubletimeone_df variable with the recoded timeone
                                data = double_timeone_df, 
                                family = cumulative (link = "logit", threshold = "flexible"),
                                prior = priors,
                                warmup = 1000, iter = 2000,
                                chains = 4, cores = parallel::detectCores(),
                                control = list(adapt_delta = 0.8, max_treedepth = 10),
                                sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_cttimeone_xp08 <- summary(doubletimeone_resp_cttimeone)$fixed[,col2keep]
# model_cttimeone_xp08 <- round(model_cttimeone_xp08, 3)

# arrange output
model_cttimeone_xp08 <- tidy_stan(doubletimeone_resp_ctone,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_cttimeone_xp08.png", height=480, width=720)
p<-tableGrob(model_cttimeone_xp08)
grid.arrange(p)
dev.off()

# test with rope
equi_cttimeone_xp08_rwa <- equi_test(doubletimeone_resp_ctone, rope = roperwaN)
equi_cttimeone_xp08_rwa <- equi_cttimeone_xp08_rwa[c(11,13:15),]
equi_cttimeone_xp08_rwa$ROPE <- roperwaC

equi_cttimeone_xp08_0.5 <- equi_test(doubletimeone_resp_ctone, rope = c(-0.5, 0.5))
equi_cttimeone_xp08_0.5 <- equi_cttimeone_xp08_0.5[c(9,10,12),]
equi_cttimeone_xp08_0.5$ROPE <- "-0.5, 0.5"

equi_cttimeone_xp08 <- rbind(equi_cttimeone_xp08_rwa, equi_cttimeone_xp08_0.5)
equi_cttimeone_xp08[,c(3:5)] <- round(equi_cttimeone_xp08[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_cttimeone_xp08 <- equi_cttimeone_xp08[ordrow,]

# export test
png("tables/rope/equi_cttimeone_xp08.png", height=480, width=720)
p<-tableGrob(equi_cttimeone_xp08)
grid.arrange(p)
dev.off()

# See 02c_decint1_brms_models_xp08.R for other models 