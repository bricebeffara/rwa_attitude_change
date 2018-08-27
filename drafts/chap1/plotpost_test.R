old.par <- par(mfrow=c(3, 2))

BEST::plotPost(posterior_samples(ec01rwa, "b")$'b_rwa:usvalence',
               credMass = 0.95, compVal = 0,
               ROPE = c(-0.1, 0.1),
               xlab = expression(beta[RWA["positive indirect"]]),
               col = as.character(bayesplot::color_scheme_get("brightblue")[2]),
               showMode = FALSE, showCurve = FALSE)

BEST::plotPost(posterior_samples(ec02rwa, "b")$'b_rwa:usvalence',
               credMass = 0.95, compVal = 0,
               ROPE = c(-0.1, 0.1),
               xlab = expression(beta[RWA["positive indirect"]]),
               col = as.character(bayesplot::color_scheme_get("brightblue")[2]),
               showMode = FALSE, showCurve = FALSE)

BEST::plotPost(posterior_samples(ec03rwa, "b")$'b_rwa:usvalence:bloc',
               credMass = 0.89, compVal = 0,
               ROPE = c(-0.07, 0.07),
               xlab = expression(beta[RWA["positive indirect"]]),
               col = as.character(bayesplot::color_scheme_get("brightblue")[2]),
               showMode = FALSE, showCurve = FALSE)

BEST::plotPost(posterior_samples(ec01rwa, "b")$b_usvalence,
               credMass = 0.95, compVal = 0,
               ROPE = c(-0.2, 0.2),
               xlab = expression(beta[RWA["positive indirect"]]),
               col = as.character(bayesplot::color_scheme_get("brightblue")[2]),
               showMode = FALSE, showCurve = FALSE)


BEST::plotPost(posterior_samples(ec01rwa, "b")$b_RWAscore,
               credMass = 0.95, compVal = 0,
               ROPE = c(-0.2, 0.2),
               xlab = expression(beta[RWA["positive direct"]]),
               col = as.character(bayesplot::color_scheme_get("brightblue")[2]),
               showMode = FALSE, showCurve = FALSE)

BEST::plotPost(posterior_samples(ec01rwa_usneg, "b")$"b_spreading:RWAscore",
               credMass = 0.95, compVal = 0,
               ROPE = c(-0.2, 0.2),
               xlab = expression(beta[RWA["positive direct"]]),
               col = as.character(bayesplot::color_scheme_get("brightblue")[2]),
               showMode = FALSE, showCurve = FALSE)

library(sjstats)
library(sjmisc)

equi_test(ec01rwa, out = "plot", rope = c(-0.2, 0.2))

equi_test(ec01rwa, rope = c(-0.2, 0.2))

tidy_stan(ec01rwa_uspos_ind,
          typical = "mean",
          prob = .95)

summary(ec01rwa_uspos_ind)

