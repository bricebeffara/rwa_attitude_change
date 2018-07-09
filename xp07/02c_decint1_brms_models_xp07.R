#------------------------------------------------------------------------------------
# Then we run our third step models to decompose the interactions 
# between RWA and usvalence in the 1 and 2 counter-conditioning conditions
#------------------------------------------------------------------------------------

#############################
#####One counter-conditioning
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!1 counter-conditioning!! condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!negative!! valence as 0
double_df$usvalence_neg <- ifelse (double_df$usvalence == -0.5, 0, 1)

double_resp_ctone_neg <- brm(response ~ usvalence_neg * counter_one  * RWAscore + (1|ppt) + (1|stim1), #here we change the double_df variable with the recoded one
                             data = double_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ctone_neg_xp07 <- summary(double_resp_ctone_neg)$fixed[,col2keep]
# model_ctone_neg_xp07 <- round(model_ctone_neg_xp07, 3)

# arrange output
model_ctone_neg_xp07 <- tidy_stan(double_resp_ctone_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_ctone_neg_xp07.png", height=480, width=720)
p<-tableGrob(model_ctone_neg_xp07)
grid.arrange(p)
dev.off()

# test with rope
equi_ctone_neg_xp07_rwa <- equi_test(double_resp_ctone_neg, rope = roperwaN)
equi_ctone_neg_xp07_rwa <- equi_ctone_neg_xp07_rwa[c(11,13:15),]
equi_ctone_neg_xp07_rwa$ROPE <- roperwaC

equi_ctone_neg_xp07_0.5 <- equi_test(double_resp_ctone_neg, rope = c(-0.5, 0.5))
equi_ctone_neg_xp07_0.5 <- equi_ctone_neg_xp07_0.5[c(9,10,12),]
equi_ctone_neg_xp07_0.5$ROPE <- "-0.5, 0.5"

equi_ctone_neg_xp07 <- rbind(equi_ctone_neg_xp07_rwa, equi_ctone_neg_xp07_0.5)
equi_ctone_neg_xp07[,c(3:5)] <- round(equi_ctone_neg_xp07[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_ctone_neg_xp07 <- equi_ctone_neg_xp07[ordrow,]

# export test
png("tables/rope/equi_ctone_neg_xp07.png", height=480, width=720)
p<-tableGrob(equi_ctone_neg_xp07)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!1 counter-conditioning!! conditioning condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!positive!! valence as 0
double_df$usvalence_pos <- ifelse (double_df$usvalence == 0.5, 0, 1)

double_resp_ctone_pos <- brm(response ~ usvalence_pos * counter_one  * RWAscore + (1|ppt) + (1|stim1), #here we change the double_df variable with the recoded one
                             data = double_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ctone_pos_xp07 <- summary(double_resp_ctone_pos)$fixed[,col2keep]
# model_ctone_pos_xp07 <- round(model_ctone_pos_xp07, 3)

# arrange output
model_ctone_pos_xp07 <- tidy_stan(double_resp_ctone_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_ctone_pos_xp07.png", height=480, width=720)
p<-tableGrob(model_ctone_pos_xp07)
grid.arrange(p)
dev.off()

# test with rope
equi_ctone_pos_xp07_rwa <- equi_test(double_resp_ctone_pos, rope = roperwaN)
equi_ctone_pos_xp07_rwa <- equi_ctone_pos_xp07_rwa[c(11,13:15),]
equi_ctone_pos_xp07_rwa$ROPE <- roperwaC

equi_ctone_pos_xp07_0.5 <- equi_test(double_resp_ctone_pos, rope = c(-0.5, 0.5))
equi_ctone_pos_xp07_0.5 <- equi_ctone_pos_xp07_0.5[c(9,10,12),]
equi_ctone_pos_xp07_0.5$ROPE <- "-0.5, 0.5"

equi_ctone_pos_xp07 <- rbind(equi_ctone_pos_xp07_rwa, equi_ctone_pos_xp07_0.5)
equi_ctone_pos_xp07[,c(3:5)] <- round(equi_ctone_pos_xp07[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_ctone_pos_xp07 <- equi_ctone_pos_xp07[ordrow,]

# export test
png("tables/rope/equi_ctone_pos_xp07.png", height=480, width=720)
p<-tableGrob(equi_ctone_pos_xp07)
grid.arrange(p)
dev.off()

#############################
####Two counter-conditionings
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!2 counter-conditionings!! condition
#-------------

double_resp_cttwo_neg <- brm(response ~ usvalence_neg * counter_two  * RWAscore + (1|ppt) + (1|stim1), #here we change the double_df variable with the recoded one
                             data = double_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_cttwo_neg_xp07 <- summary(double_resp_cttwo_neg)$fixed[,col2keep]
# model_cttwo_neg_xp07 <- round(model_cttwo_neg_xp07, 3)

# arrange output
model_cttwo_neg_xp07 <- tidy_stan(double_resp_cttwo_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_cttwo_neg_xp07.png", height=480, width=720)
p<-tableGrob(model_cttwo_neg_xp07)
grid.arrange(p)
dev.off()

# test with rope
equi_cttwo_neg_xp07_rwa <- equi_test(double_resp_cttwo_neg, rope = roperwaN)
equi_cttwo_neg_xp07_rwa <- equi_cttwo_neg_xp07_rwa[c(11,13:15),]
equi_cttwo_neg_xp07_rwa$ROPE <- roperwaC

equi_cttwo_neg_xp07_0.5 <- equi_test(double_resp_cttwo_neg, rope = c(-0.5, 0.5))
equi_cttwo_neg_xp07_0.5 <- equi_cttwo_neg_xp07_0.5[c(9,10,12),]
equi_cttwo_neg_xp07_0.5$ROPE <- "-0.5, 0.5"

equi_cttwo_neg_xp07 <- rbind(equi_cttwo_neg_xp07_rwa, equi_cttwo_neg_xp07_0.5)
equi_cttwo_neg_xp07[,c(3:5)] <- round(equi_cttwo_neg_xp07[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_cttwo_neg_xp07 <- equi_cttwo_neg_xp07[ordrow,]

# export test
png("tables/rope/equi_cttwo_neg_xp07.png", height=480, width=720)
p<-tableGrob(equi_cttwo_neg_xp07)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!2 counter-conditionings!! condition
#-------------

double_resp_cttwo_pos <- brm(response ~ usvalence_pos * counter_two  * RWAscore + (1|ppt) + (1|stim1), #here we change the double_df variable with the recoded one
                             data = double_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_cttwo_pos_xp07 <- summary(double_resp_cttwo_pos)$fixed[,col2keep]
# model_cttwo_pos_xp07 <- round(model_cttwo_pos_xp07, 3)

# arrange output
model_cttwo_pos_xp07 <- tidy_stan(double_resp_cttwo_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_cttwo_pos_xp07.png", height=480, width=720)
p<-tableGrob(model_cttwo_pos_xp07)
grid.arrange(p)
dev.off()

# test with rope
equi_cttwo_pos_xp07_rwa <- equi_test(double_resp_cttwo_pos, rope = roperwaN)
equi_cttwo_pos_xp07_rwa <- equi_cttwo_pos_xp07_rwa[c(11,13:15),]
equi_cttwo_pos_xp07_rwa$ROPE <- roperwaC

equi_cttwo_pos_xp07_0.5 <- equi_test(double_resp_cttwo_pos, rope = c(-0.5, 0.5))
equi_cttwo_pos_xp07_0.5 <- equi_cttwo_pos_xp07_0.5[c(9,10,12),]
equi_cttwo_pos_xp07_0.5$ROPE <- "-0.5, 0.5"

equi_cttwo_pos_xp07 <- rbind(equi_cttwo_pos_xp07_rwa, equi_cttwo_pos_xp07_0.5)
equi_cttwo_pos_xp07[,c(3:5)] <- round(equi_cttwo_pos_xp07[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_cttwo_pos_xp07 <- equi_cttwo_pos_xp07[ordrow,]

# export test
png("tables/rope/equi_cttwo_pos_xp07.png", height=480, width=720)
p<-tableGrob(equi_cttwo_pos_xp07)
grid.arrange(p)
dev.off()
