#------------------------------------------------------------------------------------
# Then we run our third step models to decompose the interactions 
# between RWA and usvalence in the ord1 and ord2 conditions
#------------------------------------------------------------------------------------

#############################
##################### ord1
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!order1!! condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!negative!! valence as 0
expli_df$usvalence_neg <- ifelse (expli_df$usvalence == -0.5, 0, 1)

expli_resp_ord1_neg <- brm(response ~ usvalence_neg * order1  * RWAscore + (1|ppt) + (1|stim1), #here we change the order variable with the recoded one
                             data = expli_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ord1_neg_xp05 <- summary(expli_resp_ord1_neg)$fixed[,col2keep]
# model_ord1_neg_xp05 <- round(model_ord1_neg_xp05, 3)

# arrange output
model_ord1_neg_xp05 <- tidy_stan(expli_resp_ord1_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_ord1_neg_xp05.png", height=480, width=720)
p<-tableGrob(model_ord1_neg_xp05)
grid.arrange(p)
dev.off()

# test with rope
equi_ord1_neg_xp05_rwa <- equi_test(expli_resp_ord1_neg, rope = roperwaN)
equi_ord1_neg_xp05_rwa <- equi_ord1_neg_xp05_rwa[c(11,13:15),]
equi_ord1_neg_xp05_rwa$ROPE <- roperwaC

equi_ord1_neg_xp05_0.5 <- equi_test(expli_resp_ord1_neg, rope = c(-0.5, 0.5))
equi_ord1_neg_xp05_0.5 <- equi_ord1_neg_xp05_0.5[c(9,10,12),]
equi_ord1_neg_xp05_0.5$ROPE <- "-0.5, 0.5"

equi_ord1_neg_xp05 <- rbind(equi_ord1_neg_xp05_rwa, equi_ord1_neg_xp05_0.5)
equi_ord1_neg_xp05[,c(3:5)] <- round(equi_ord1_neg_xp05[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_ord1_neg_xp05 <- equi_ord1_neg_xp05[ordrow,]

# export test
png("tables/rope/equi_ord1_neg_xp05.png", height=480, width=720)
p<-tableGrob(equi_ord1_neg_xp05)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!order1!! condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!positive!! valence as 0
expli_df$usvalence_pos <- ifelse (expli_df$usvalence == 0.5, 0, 1)

expli_resp_ord1_pos <- brm(response ~ usvalence_pos * order1  * RWAscore + (1|ppt) + (1|stim1), #here we change the order variable with the recoded one
                             data = expli_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ord1_pos_xp05 <- summary(expli_resp_ord1_pos)$fixed[,col2keep]
# model_ord1_pos_xp05 <- round(model_ord1_pos_xp05, 3)

# arrange output
model_ord1_pos_xp05 <- tidy_stan(expli_resp_ord1_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_ord1_pos_xp05.png", height=480, width=720)
p<-tableGrob(model_ord1_pos_xp05)
grid.arrange(p)
dev.off()

# test with rope
equi_ord1_pos_xp05_rwa <- equi_test(expli_resp_ord1_pos, rope = roperwaN)
equi_ord1_pos_xp05_rwa <- equi_ord1_pos_xp05_rwa[c(11,13:15),]
equi_ord1_pos_xp05_rwa$ROPE <- roperwaC

equi_ord1_pos_xp05_0.5 <- equi_test(expli_resp_ord1_pos, rope = c(-0.5, 0.5))
equi_ord1_pos_xp05_0.5 <- equi_ord1_pos_xp05_0.5[c(9,10,12),]
equi_ord1_pos_xp05_0.5$ROPE <- "-0.5, 0.5"

equi_ord1_pos_xp05 <- rbind(equi_ord1_pos_xp05_rwa, equi_ord1_pos_xp05_0.5)
equi_ord1_pos_xp05[,c(3:5)] <- round(equi_ord1_pos_xp05[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_ord1_pos_xp05 <- equi_ord1_pos_xp05[ordrow,]

# export test
png("tables/rope/equi_ord1_pos_xp05.png", height=480, width=720)
p<-tableGrob(equi_ord1_pos_xp05)
grid.arrange(p)
dev.off()

#############################
##################### order2
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!order2!! condition
#-------------

expli_resp_ord2_neg <- brm(response ~ usvalence_neg * order2  * RWAscore + (1|ppt) + (1|stim1), #here we change the order variable with the recoded one
                             data = expli_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ord2_neg_xp05 <- summary(expli_resp_ord2_neg)$fixed[,col2keep]
# model_ord2_neg_xp05 <- round(model_ord2_neg_xp05, 3)

# arrange output
model_ord2_neg_xp05 <- tidy_stan(expli_resp_ord2_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_ord2_neg_xp05.png", height=480, width=720)
p<-tableGrob(model_ord2_neg_xp05)
grid.arrange(p)
dev.off()

# test with rope
equi_ord2_neg_xp05_rwa <- equi_test(expli_resp_ord2_neg, rope = roperwaN)
equi_ord2_neg_xp05_rwa <- equi_ord2_neg_xp05_rwa[c(11,13:15),]
equi_ord2_neg_xp05_rwa$ROPE <- roperwaC

equi_ord2_neg_xp05_0.5 <- equi_test(expli_resp_ord2_neg, rope = c(-0.5, 0.5))
equi_ord2_neg_xp05_0.5 <- equi_ord2_neg_xp05_0.5[c(9,10,12),]
equi_ord2_neg_xp05_0.5$ROPE <- "-0.5, 0.5"

equi_ord2_neg_xp05 <- rbind(equi_ord2_neg_xp05_rwa, equi_ord2_neg_xp05_0.5)
equi_ord2_neg_xp05[,c(3:5)] <- round(equi_ord2_neg_xp05[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_ord2_neg_xp05 <- equi_ord2_neg_xp05[ordrow,]

# export test
png("tables/rope/equi_ord2_neg_xp05.png", height=480, width=720)
p<-tableGrob(equi_ord2_neg_xp05)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!order2!! condition
#-------------

expli_resp_ord2_pos <- brm(response ~ usvalence_pos * order2  * RWAscore + (1|ppt) + (1|stim1), #here we change the order variable with the recoded one
                             data = expli_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ord2_pos_xp05 <- summary(expli_resp_ord2_pos)$fixed[,col2keep]
# model_ord2_pos_xp05 <- round(model_ord2_pos_xp05, 3)

# arrange output
model_ord2_pos_xp05 <- tidy_stan(expli_resp_ord2_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_ord2_pos_xp05.png", height=480, width=720)
p<-tableGrob(model_ord2_pos_xp05)
grid.arrange(p)
dev.off()

# test with rope
equi_ord2_pos_xp05_rwa <- equi_test(expli_resp_ord2_pos, rope = roperwaN)
equi_ord2_pos_xp05_rwa <- equi_ord2_pos_xp05_rwa[c(11,13:15),]
equi_ord2_pos_xp05_rwa$ROPE <- roperwaC

equi_ord2_pos_xp05_0.5 <- equi_test(expli_resp_ord2_pos, rope = c(-0.5, 0.5))
equi_ord2_pos_xp05_0.5 <- equi_ord2_pos_xp05_0.5[c(9,10,12),]
equi_ord2_pos_xp05_0.5$ROPE <- "-0.5, 0.5"

equi_ord2_pos_xp05 <- rbind(equi_ord2_pos_xp05_rwa, equi_ord2_pos_xp05_0.5)
equi_ord2_pos_xp05[,c(3:5)] <- round(equi_ord2_pos_xp05[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_ord2_pos_xp05 <- equi_ord2_pos_xp05[ordrow,]

# export test
png("tables/rope/equi_ord2_pos_xp05.png", height=480, width=720)
p<-tableGrob(equi_ord2_pos_xp05)
grid.arrange(p)
dev.off()

#############################
##################### w/o order
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence condition
#-------------

expli_resp_neg <- brm(response ~ usvalence_neg * RWAscore + (1|ppt) + (1|stim1),
                           data = expli_df, 
                           family = cumulative (link = "logit", threshold = "flexible"),
                           prior = priors,
                           warmup = 1000, iter = 2000,
                           chains = 4, cores = parallel::detectCores(),
                           control = list(adapt_delta = 0.8, max_treedepth = 10),
                           sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_expli_neg_xp05 <- summary(expli_resp_neg)$fixed[,col2keep]
# model_expli_neg_xp05 <- round(model_expli_neg_xp05, 3)

# arrange output
model_neg_xp05 <- tidy_stan(expli_resp_neg,
                                 typical = "mean",
                                 prob = .95)

# export output
png("tables/brms/model_neg_xp05.png", height=480, width=720)
p<-tableGrob(model_neg_xp05)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_xp05_rwa <- equi_test(expli_resp_neg, rope = roperwaN)
equi_neg_xp05_rwa <- equi_neg_xp05_rwa[c(10:11),]
equi_neg_xp05_rwa$ROPE <- roperwaC

equi_neg_xp05_0.5 <- equi_test(expli_resp_neg, rope = c(-0.5, 0.5))
equi_neg_xp05_0.5 <- equi_neg_xp05_0.5[c(9),]
equi_neg_xp05_0.5$ROPE <- "-0.5, 0.5"

equi_neg_xp05 <- rbind(equi_neg_xp05_rwa, equi_neg_xp05_0.5)
equi_neg_xp05[,c(3:5)] <- round(equi_neg_xp05[,c(3:5)], 2)
ordrow <- c("3","1","2")
equi_neg_xp05 <- equi_neg_xp05[ordrow,]

# export test
png("tables/rope/equi_neg_xp05.png", height=480, width=720)
p<-tableGrob(equi_neg_xp05)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence condition
#-------------

expli_resp_pos <- brm(response ~ usvalence_pos * RWAscore + (1|ppt) + (1|stim1), 
                           data = expli_df, 
                           family = cumulative (link = "logit", threshold = "flexible"),
                           prior = priors,
                           warmup = 1000, iter = 2000,
                           chains = 4, cores = parallel::detectCores(),
                           control = list(adapt_delta = 0.8, max_treedepth = 10),
                           sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_pos_xp05 <- summary(expli_resp_pos)$fixed[,col2keep]
# model_pos_xp05 <- round(model_pos_xp05, 3)

# arrange output
model_pos_xp05 <- tidy_stan(expli_resp_pos,
                            typical = "mean",
                            prob = .95)

# export output
png("tables/brms/model_pos_xp05.png", height=480, width=720)
p<-tableGrob(model_pos_xp05)
grid.arrange(p)
dev.off()

# test with rope
equi_pos_xp05_rwa <- equi_test(expli_resp_pos, rope = roperwaN)
equi_pos_xp05_rwa <- equi_pos_xp05_rwa[c(10:11),]
equi_pos_xp05_rwa$ROPE <- roperwaC

equi_pos_xp05_0.5 <- equi_test(expli_resp_pos, rope = c(-0.5, 0.5))
equi_pos_xp05_0.5 <- equi_pos_xp05_0.5[c(9),]
equi_pos_xp05_0.5$ROPE <- "-0.5, 0.5"

equi_pos_xp05 <- rbind(equi_pos_xp05_rwa, equi_pos_xp05_0.5)
equi_pos_xp05[,c(3:5)] <- round(equi_pos_xp05[,c(3:5)], 2)
ordrow <- c("3","1","2")
equi_pos_xp05 <- equi_pos_xp05[ordrow,]

# export test
png("tables/rope/equi_pos_xp05.png", height=480, width=720)
p<-tableGrob(equi_pos_xp05)
grid.arrange(p)
dev.off()
