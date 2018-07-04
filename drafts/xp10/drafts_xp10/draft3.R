marg_on <- marginal_effects(spread_resp, effects = "RWAscore",
                            conditions = cond_on,
                            spaghetti = TRUE, nsamples = 500)

marg_op <- marginal_effects(spread_resp, effects = "RWAscore",
                            conditions = cond_op,
                            spaghetti = TRUE, nsamples = 500)$RWAscore

plot(marg_op)
plot(marg_sp)

head(marg_op)$RWAscore

plot(marg_on,
     rug = FALSE, mean = TRUE,
     surface_args = list(), spaghetti = TRUE,
     plot = TRUE)

marginal_smooths(marg_on, smooths = NULL, int_conditions = NULL,
                 probs = c(0.025, 0.975), spaghetti = TRUE, resolution = 100,
                 too_far = 0, subset = NULL, nsamples = NULL)

plot(x, ncol = NULL, points = FALSE,
     rug = FALSE, mean = TRUE, jitter_width = 0, stype = c("contour",
                                                           "raster"), line_args = list(), cat_args = list(),
     errorbar_args = list(), surface_args = list(), spaghetti_args = list(),
     point_args = list(), rug_args = list(), theme = NULL, ask = TRUE,
     plot = TRUE, ...)

marg_op %>% # here his where we specify the marginal effects of interest
  ggplot(aes(x = RWAscore, y = estimate__)) +
  aes_string(ymin = "lower__", ymax = "upper__", stat = "identity") +
  labs(subtitle="Direct negative conditioning") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15)

aes_string(ymin = "lower__", ymax = "upper__", fill = gvar)

plot(marg_op, plot = FALSE)[[1]] +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15)

(plot(marg_op, plot = FALSE)[[1]]

plot_info
