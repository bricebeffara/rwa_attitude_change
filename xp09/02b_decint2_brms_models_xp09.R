#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the interaction between RWA and usvalence at each level of load
#------------------------------------------------------------------------------------

#-------------
### RWA * valence in the !!no load!! condition
#-------------

## first step = recode variable load
## We interpret parameters for a 0 level of others

# Coding !!no load!! as 0
load_df$no_load <- ifelse (load_df$load == -0.5, 0, 1)

load_resp_nolo <- brm(response ~ usvalence * no_load  * RWAscore + (1|ppt) + (1|stim1), #here we change the load variable with the recoded one
                         data = load_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_nolo_xp09 <- summary(load_resp_nolo)$fixed[,col2keep]
# model_nolo_xp09 <- round(model_nolo_xp09, 3)

# arrange output
model_nolo_xp09 <- tidy_stan(load_resp_nolo,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_nolo_xp09.png", height=480, width=720)
p<-tableGrob(model_nolo_xp09)
grid.arrange(p)
dev.off()

# test with rope
equi_nolo_xp09_rwa <- equi_test(load_resp_nolo, rope = roperwaN)
equi_nolo_xp09_rwa <- equi_nolo_xp09_rwa[c(11,13:15),]
equi_nolo_xp09_rwa$ROPE <- roperwaC

equi_nolo_xp09_0.5 <- equi_test(load_resp_nolo, rope = c(-0.5, 0.5))
equi_nolo_xp09_0.5 <- equi_nolo_xp09_0.5[c(9,10,12),]
equi_nolo_xp09_0.5$ROPE <- "-0.5, 0.5"

equi_nolo_xp09 <- rbind(equi_nolo_xp09_rwa, equi_nolo_xp09_0.5)
equi_nolo_xp09[,c(3:5)] <- round(equi_nolo_xp09[,c(3:5)], 2)
ordrow <- c(5, 6, 1, 7, 2, 3, 4)
equi_nolo_xp09 <- equi_nolo_xp09[ordrow,]

# export test
png("tables/rope/equi_nolo_xp09.png", height=480, width=720)
p<-tableGrob(equi_nolo_xp09)
grid.arrange(p)
dev.off()


#-------------
### RWA * usvalence in the !!loading!! condition
#-------------

## first step = recode variable load
## We interpret parameters for a 0 level of others

# Coding !!load!! as 0
load_df$ye_load <- ifelse (load_df$load == 0.5, 0, 1)

load_resp_yelo <- brm(response ~ usvalence * ye_load  * RWAscore + (1|ppt) + (1|stim1), #here we change the load variable with the recoded one
                         data = load_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_yelo_xp09 <- summary(load_resp_yelo)$fixed[,col2keep]
# model_yelo_xp09 <- round(model_yelo_xp09, 3)

# arrange output
model_yelo_xp09 <- tidy_stan(load_resp_yelo,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_yelo_xp09.png", height=480, width=720)
p<-tableGrob(model_yelo_xp09)
grid.arrange(p)
dev.off()

# test with rope
equi_yelo_xp09_rwa <- equi_test(load_resp_yelo, rope = roperwaN)
equi_yelo_xp09_rwa <- equi_yelo_xp09_rwa[c(11,13:15),]
equi_yelo_xp09_rwa$ROPE <- roperwaC

equi_yelo_xp09_0.5 <- equi_test(load_resp_yelo, rope = c(-0.5, 0.5))
equi_yelo_xp09_0.5 <- equi_yelo_xp09_0.5[c(9,10,12),]
equi_yelo_xp09_0.5$ROPE <- "-0.5, 0.5"

equi_yelo_xp09 <- rbind(equi_yelo_xp09_rwa, equi_yelo_xp09_0.5)
equi_yelo_xp09[,c(3:5)] <- round(equi_yelo_xp09[,c(3:5)], 2)
ordrow <- c(5, 6, 1, 7, 2, 3, 4)
equi_yelo_xp09 <- equi_yelo_xp09[ordrow,]

# export test
png("tables/rope/equi_yelo_xp09.png", height=480, width=720)
p<-tableGrob(equi_yelo_xp09)
grid.arrange(p)
dev.off()

# See 02c_decint1_brms_models_xp09.R for other models 