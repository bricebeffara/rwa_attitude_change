library(lme4)

modtt <- lmer(response ~ usvalence * spreading  * RWAscore + (1|other2) + (1|stim1), data=spreading)

summary(modtt)

confint(modtt)


plot(marginal_effects(spread_resp, effects = "RWAscore", spaghetti = TRUE, nsamples = 1000))

plot(marginal_effects(spread_resp, effects = "RWAscore",  spaghetti = TRUE, nsamples = 1000))
