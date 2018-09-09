#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the effect of usvalence at each level of order
#------------------------------------------------------------------------------------

#-------------
### usvalence in the !!implicit ratings before explicit ratings!! condition
#-------------

## first step = recode variable expli
## We interpret parameters for a 0 level of others

# Coding !!implicit ratings before explicit ratings!! as 0
diriat_df$order1 <- ifelse (diriat_df$order == -0.5, 0, 1)

diriat_resp_ord1 <- brm(response ~ usvalence * order1 + (1|ppt) + (1|stim1), #here we change the order variable with the recoded one
                         data = diriat_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_order1_xp04 <- summary(diriat_resp_order1)$fixed[,col2keep]
# model_order1_xp04 <- round(model_order1_xp04, 3)

# arrange output
model_ord1_xp04 <- tidy_stan(diriat_resp_ord1,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_ord1_xp04.png", height=480, width=720)
p<-tableGrob(model_ord1_xp04)
grid.arrange(p)
dev.off()

# test with rope

equi_ord1_xp04_0.5 <- equi_test(diriat_resp_ord1, rope = c(-0.5, 0.5))
equi_ord1_xp04_0.5 <- equi_ord1_xp04_0.5[c(9:11),]
equi_ord1_xp04_0.5$ROPE <- "-0.5, 0.5"

equi_ord1_xp04 <- equi_ord1_xp04_0.5
equi_ord1_xp04[,c(3:5)] <- round(equi_ord1_xp04[,c(3:5)], 2)

# export test
png("tables/rope/equi_ord1_xp04.png", height=480, width=720)
p<-tableGrob(equi_ord1_xp04)
grid.arrange(p)
dev.off()


#-------------
### usvalence in the !!explicit ratings before implicit ratings!! condition
#-------------

## first step = recode variable order
## We interpret parameters for a 0 level of others

# Coding !!explicit ratings before implicit ratings!! as 0
diriat_df$order2 <- ifelse (diriat_df$order == 0.5, 0, 1)

diriat_resp_ord2 <- brm(response ~ usvalence * order2 + (1|ppt) + (1|stim1), #here we change the expli variable with the recoded one
                         data = diriat_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ord2_xp04 <- summary(diriat_resp_ord2)$fixed[,col2keep]
# model_ord2_xp04 <- round(model_ord2_xp04, 3)

# arrange output
model_ord2_xp04 <- tidy_stan(diriat_resp_ord2,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_ord2_xp04.png", height=480, width=720)
p<-tableGrob(model_ord2_xp04)
grid.arrange(p)
dev.off()

# test with rope
equi_ord2_xp04_0.5 <- equi_test(diriat_resp_ord2, rope = c(-0.5, 0.5))
equi_ord2_xp04_0.5 <- equi_ord2_xp04_0.5[c(9:11),]
equi_ord2_xp04_0.5$ROPE <- "-0.5, 0.5"

equi_ord2_xp04 <- equi_ord2_xp04_0.5
equi_ord2_xp04[,c(3:5)] <- round(equi_ord2_xp04[,c(3:5)], 2)

# export test
png("tables/rope/equi_ord2_xp04.png", height=480, width=720)
p<-tableGrob(equi_ord2_xp04)
grid.arrange(p)
dev.off()

# export test
png("tables/rope/equi_ord2_xp04.png", height=480, width=720)
p<-tableGrob(equi_ord2_xp04)
grid.arrange(p)
dev.off()