# Thu Nov 23 17:13:06 2017 ------------------------------
# Clean script HDI plots
# Bret et al. 2017
# Right-Wing Authoritarianism Predicts weakened Attitude Change in an Evaluative Counter-conditioning Paradigm
# OSF : https://osf.io/vz78f/
# Preprint : https://psyarxiv.com/3mfa8/
#################################################################################################################

# ! This is a generic script !
# ! Replace MoDeLNaMe by the name of the model of which you want to plot parameters !

postPAR <- posterior_samples(MoDeLNaMe)
postPAR$b_Intercept <- (posterior_samples(MoDeLNaMe, "b")$"b_Intercept[1]"+
                         posterior_samples(MoDeLNaMe, "b")$"b_Intercept[2]"+
                         posterior_samples(MoDeLNaMe, "b")$"b_Intercept[3]"+
                         posterior_samples(MoDeLNaMe, "b")$"b_Intercept[4]"+
                         posterior_samples(MoDeLNaMe, "b")$"b_Intercept[5]"+
                         posterior_samples(MoDeLNaMe, "b")$"b_Intercept[6]"+
                         posterior_samples(MoDeLNaMe, "b")$"b_Intercept[7]"+
                         posterior_samples(MoDeLNaMe, "b")$"b_Intercept[8]")/8
old.par <- par(mfrow=c(3, 2))
BEST::plotPost(postPAR$b_Intercept, xlab = expression(Intercept[Averaged]),
               col = as.character(bayesplot::color_scheme_get("gray")[2]), compVal = 0,
               showMode = FALSE, showCurve = FALSE)
BEST::plotPost(posterior_samples(MoDeLNaMe, "b")$"b_usvalence", xlab = expression(beta["Valence Block 1"]),
               col = as.character(bayesplot::color_scheme_get("gray")[2]), compVal = 0,
               showMode = FALSE, showCurve = FALSE)
BEST::plotPost(posterior_samples(MoDeLNaMe, "b")$"b_rwa", xlab = expression(beta[RWA]),
               col = as.character(bayesplot::color_scheme_get("gray")[2]), compVal = 0,
               showMode = FALSE, showCurve = FALSE)
BEST::plotPost(posterior_samples(MoDeLNaMe, "b")$"b_rwa:usvalence", xlab = expression(beta[RWA%*%"Valence Block 1"]),
               col = as.character(bayesplot::color_scheme_get("gray")[2]), compVal = 0,
               showMode = FALSE, showCurve = FALSE)
BEST::plotPost(posterior_samples(MoDeLNaMeP, "b")$"b_rwa", xlab = expression(beta[RWA["Block 1 negative"]]),
               col = as.character(bayesplot::color_scheme_get("gray")[2]), compVal = 0,
               showMode = FALSE, showCurve = FALSE)
BEST::plotPost(posterior_samples(MoDeLNaMeN, "b")$"b_rwa", xlab = expression(beta[RWA["Block 1 positive"]]),
               col = as.character(bayesplot::color_scheme_get("gray")[2]), compVal = 0,
               showMode = FALSE, showCurve = FALSE)

