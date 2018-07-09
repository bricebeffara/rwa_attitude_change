# Orthogonal contrasts part 1 ----------------------------------------------------

# model
doubletime_oc1_resp <- brm(response ~ usvalence * contrast2 * RWAscore + (1|ppt) + (1|stim1),
                           data = doubletime_df, 
                           family = cumulative (link = "logit", threshold = "flexible"),
                           prior = priors,
                           warmup = 1000, iter = 2000,
                           chains = 4, cores = parallel::detectCores(),
                           control = list(adapt_delta = 0.8, max_treedepth = 10),
                           sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_gen_oc1_xp08 <- summary(doubletime_oc1_resp)$fixed[,col2keep]
# model_gen_oc1_xp08 <- round(model_gen_oc1_xp08, 3)

# arrange output
model_gen_oc1_xp08 <- tidy_stan(doubletime_oc1_resp,
                                typical = "mean",
                                prob = .95)


# export output
png("tables/brms/model_gen_oc1_xp08.png", height=480, width=720)
p<-tableGrob(model_gen_oc1_xp08)
grid.arrange(p)
dev.off()

# test with rope
equi_gen_oc1_xp08_rwa <- equi_test(doubletime_oc1_resp, rope = roperwaN)
equi_gen_oc1_xp08_rwa <- equi_gen_oc1_xp08_rwa[c(11,13:15),]
equi_gen_oc1_xp08_rwa$ROPE <- roperwaC

equi_gen_oc1_xp08_0.5 <- equi_test(doubletime_oc1_resp, rope = c(-0.5, 0.5))
equi_gen_oc1_xp08_0.5 <- equi_gen_oc1_xp08_0.5[c(9,10,12),]
equi_gen_oc1_xp08_0.5$ROPE <- "-0.5, 0.5"

equi_gen_oc1_xp08 <- rbind(equi_gen_oc1_xp08_rwa, equi_gen_oc1_xp08_0.5)
equi_gen_oc1_xp08[,c(3:5)] <- round(equi_gen_oc1_xp08[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_gen_oc1_xp08 <- equi_gen_oc1_xp08[ordrow,]

# export test
png("tables/rope/equi_gen_oc1_xp08.png", height=480, width=720)
p<-tableGrob(equi_gen_oc1_xp08)
grid.arrange(p)
dev.off()

# Orthogonal contrasts part 2 ----------------------------------------------------

# model
doubletime_oc2_resp <- brm(response ~ usvalence * contrast3 * RWAscore + (1|ppt) + (1|stim1),
                           data = doubletime_df, 
                           family = cumulative (link = "logit", threshold = "flexible"),
                           prior = priors,
                           warmup = 1000, iter = 2000,
                           chains = 4, cores = parallel::detectCores(),
                           control = list(adapt_delta = 0.8, max_treedepth = 10),
                           sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_gen_oc2_xp08 <- summary(doubletime_oc2_resp)$fixed[,col2keep]
# model_gen_oc2_xp08 <- round(model_gen_oc2_xp08, 3)

# arrange output
model_gen_oc2_xp08 <- tidy_stan(doubletime_oc2_resp,
                                typical = "mean",
                                prob = .95)


# export output
png("tables/brms/model_gen_oc2_xp08.png", height=480, width=720)
p<-tableGrob(model_gen_oc2_xp08)
grid.arrange(p)
dev.off()

# test with rope
equi_gen_oc2_xp08_rwa <- equi_test(doubletime_oc2_resp, rope = roperwaN)
equi_gen_oc2_xp08_rwa <- equi_gen_oc2_xp08_rwa[c(11,13:15),]
equi_gen_oc2_xp08_rwa$ROPE <- roperwaC

equi_gen_oc2_xp08_0.5 <- equi_test(doubletime_oc2_resp, rope = c(-0.5, 0.5))
equi_gen_oc2_xp08_0.5 <- equi_gen_oc2_xp08_0.5[c(9,10,12),]
equi_gen_oc2_xp08_0.5$ROPE <- "-0.5, 0.5"

equi_gen_oc2_xp08 <- rbind(equi_gen_oc2_xp08_rwa, equi_gen_oc2_xp08_0.5)
equi_gen_oc2_xp08[,c(3:5)] <- round(equi_gen_oc2_xp08[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_gen_oc2_xp08 <- equi_gen_oc2_xp08[ordrow,]

# export test
png("tables/rope/equi_gen_oc2_xp08.png", height=480, width=720)
p<-tableGrob(equi_gen_oc2_xp08)
grid.arrange(p)
dev.off()