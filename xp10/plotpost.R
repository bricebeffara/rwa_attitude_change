old.par <- par(mfrow=c(3, 2))

BEST::plotPost(posterior_samples(spread_resp, "b")$b_RWAscore,
               credMass = 0.95, compVal = 0,
               ROPE = c(-0.2, 0.2),
               xlab = expression(beta[RWA["positive indirect"]]),
               col = as.character(bayesplot::color_scheme_get("brightblue")[2]),
               showMode = FALSE, showCurve = FALSE)

BEST::plotPost(posterior_samples(spread_resp_uspos_ind, "b")$b_RWAscore,
               credMass = 0.95, compVal = 0,
               ROPE = c(-0.2, 0.2),
               xlab = expression(beta[RWA["positive indirect"]]),
               col = as.character(bayesplot::color_scheme_get("brightblue")[2]),
               showMode = FALSE, showCurve = FALSE)


BEST::plotPost(posterior_samples(spread_resp_uspos_dir, "b")$b_RWAscore,
               credMass = 0.95, compVal = 0,
               ROPE = c(-0.2, 0.2),
               xlab = expression(beta[RWA["positive direct"]]),
               col = as.character(bayesplot::color_scheme_get("brightblue")[2]),
               showMode = FALSE, showCurve = FALSE)

BEST::plotPost(posterior_samples(spread_resp_usneg, "b")$"b_spreading:RWAscore",
               credMass = 0.95, compVal = 0,
               ROPE = c(-0.2, 0.2),
               xlab = expression(beta[RWA["positive direct"]]),
               col = as.character(bayesplot::color_scheme_get("brightblue")[2]),
               showMode = FALSE, showCurve = FALSE)

library(sjstats)
library(sjmisc)

equi_test(spread_resp, out = "plot", rope = c(-0.2, 0.2))

equi_test(spread_resp, rope = c(-0.2, 0.2))

tidy_stan(spread_resp_uspos_ind,
          typical = "mean",
          prob = .95)

summary(spread_resp_uspos_ind)

