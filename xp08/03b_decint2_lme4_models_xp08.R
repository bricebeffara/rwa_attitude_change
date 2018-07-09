#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the interaction between RWA and usvalence at each level of conditioning
#------------------------------------------------------------------------------------

#-------------
### RWA * valence in the !!1 counter-conditioning!! condition
#-------------

# model
doubleone_resp_ctone_lme4 <- lmer(response ~ usvalence * counter_one  * RWAscore + (1|ppt) + (1|stim1),
                               data = double_one_df)

# Save summary & confint
model_ctone_xp08_lme4 <- round(cbind(summary(doubleone_resp_ctone_lme4)$coefficients,
                                     confint(doubleone_resp_ctone_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_ctone_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_ctone_xp08_lme4)
grid.arrange(p)
dev.off()

#-------------
### RWA * usvalence in the !!2 counter-conditionings!! condition
#-------------

# model
doubleone_resp_cttwo_lme4 <- lmer(response ~ usvalence * counter_two  * RWAscore + (1|ppt) + (1|stim1),
                               data = double_one_df)

# Save summary & confint
model_cttwo_xp08_lme4 <- round(cbind(summary(doubleone_resp_cttwo_lme4)$coefficients,
                                     confint(doubleone_resp_cttwo_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_cttwo_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_cttwo_xp08_lme4)
grid.arrange(p)
dev.off()

#-------------
### RWA * valence in the !!1 counter-conditioning + delay !! condition
#-------------

# model
doubleonetime_resp_ctone_lme4 <- lmer(response ~ usvalence * counter_one  * RWAscore + (1|ppt) + (1|stim1),
                                  data = double_onetime_df)

# Save summary & confint
model_ctonetime_xp08_lme4 <- round(cbind(summary(doubleonetime_resp_ctone_lme4)$coefficients,
                                     confint(doubleonetime_resp_ctone_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_ctonetime_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_ctonetime_xp08_lme4)
grid.arrange(p)
dev.off()

#-------------
### RWA * usvalence in the !!delay + 1 counter-conditioning!! condition
#-------------

# model
doubletimeone_resp_ctone_lme4 <- lmer(response ~ usvalence * counter_one  * RWAscore + (1|ppt) + (1|stim1),
                                  data = double_timeone_df)

# Save summary & confint
model_cttimeone_xp08_lme4 <- round(cbind(summary(doubletimeone_resp_ctone_lme4)$coefficients,
                                     confint(doubletimeone_resp_ctone_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_cttimeone_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_cttimeone_xp08_lme4)
grid.arrange(p)
dev.off()

