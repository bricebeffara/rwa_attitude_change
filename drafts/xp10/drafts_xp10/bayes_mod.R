# Loading packages needed (and installing if necessary) for this part

if (!require("pacman")) install.packages("pacman")
p_load(brms,
       data.table,
       grid,
       ggplot2,
       Rcpp,
       Matrix,
       cowplot,
       magrittr,
       tidybayes,
       ggridges,
       tidyverse,
       ggpomological,
       colorRamps,
       extrafont,
       dplyr,
       showtext,
       sjstats,
       sjPlot,
       devtools,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

# Define priors for all models

priors <- c(
  prior(normal(0, 10), class = Intercept, coef = ""),
  prior(normal(0, 0.5), class = b),
  prior(exponential(2), class = sd) )

spread_resp <- brm(response ~ usvalence * spreading  * RWAscore + (1|other2) + (1|stim1),
               data = spreading, 
               family = cumulative (link = "logit", threshold = "flexible"),
               prior = priors,
               warmup = 1000, iter = 2000,
               chains = 4, cores = parallel::detectCores(),
               control = list(adapt_delta = 0.8, max_treedepth = 10),
               sample_prior = TRUE)


summary(spread_resp)
r2(spread_resp, loo = TRUE)
equi_test(spread_resp, rope = c(-0.3, 0.3))
equi_test(spread_resp, rope = c(-0.28125, 0.28125), out = "plot")

conditions <- data.frame(spreading = c(-0.5, 0.5, -0.5, 0.5),
                         usvalence = c(-0.5, -0.5, 0.5, 0.5),
                         cond__ = c("original_negative", "spreading_negative",
                                    "original_positive", "original_positive"))

conditions <- data.frame(spreading = c(-0.5, 0.5),
                         cond__ = c(spreading = "original", spreading = "spreading"))
int_conditions <- data.frame(spreading = c(-0.5, 0.5))

plot(marginal_effects(spread_resp, effects = "RWAscore", 
                      conditions = conditions,
                      ordinal = TRUE, 
                      method = c("predict"), re_formula = NULL),
     points = TRUE, rug = TRUE, resolution = 1000, theme = theme_gray())

plot(marginal_effects(spread_resp, effects = "usvalence", 
                      ordinal = TRUE, 
                      method = c("predict"), re_formula = NULL),
     points = TRUE, rug = TRUE, resolution = 10000)


source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
source("split_violin_ggplot.R")

cbPalette <- c("#96cac1", "#c2bfd6")
whitepal<- c("#666666","#666666")
#alphalev

data_plot = spreading %>%
  ggplot(aes(x = RWAscore, y = as.character(response),
             fill = as.character(spreading), color = as.character(spreading))) +
  geom_density_ridges2(jittered_points = TRUE, size = 2,
                      alpha = 1, scale = 0.5, point_size = 0.2, point_color = "#666666") +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(-0.5, 0.5)) +
  labs(x = 'RWA', y = 'Rating', color = 'Family', fill = "Family") +
  scale_fill_manual(values=cbPalette) +
  scale_color_manual(values=whitepal, guide = "none") +
  theme_ridges(center = TRUE)
  

data_plot = spreading %>%
  ggplot(aes(x = as.character(response), y = RWAscore,
             fill = as.character(spreading), color = as.character(spreading))) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), size = 2) +
  geom_boxplot(width = .1, outlier.shape = 2, alpha = 1) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(-0.5, 0.5)) +
  labs(x = 'RWA', y = 'Rating', color = 'Family', fill = "Family") +
  scale_fill_manual(values=cbPalette) +
  scale_color_manual(values=whitepal, guide = "none") +
  theme_ridges(center = TRUE) +
  coord_flip()

dat_summa <- aggregate (RWAscore ~ as.character(response) * as.character(spreading), data = spreading, FUN = median)

logist <- function(x){
  y = exp(x) / (1 + exp(x))
}

## plot neg

spreading_neg <- subset(spreading, usvalence==-0.5)

data_plot_neg = spreading_neg %>%
  ggplot(aes(x = as.character(response), y = RWAscore,
             fill = as.character(spreading), color = as.character(spreading))) +
  geom_boxplot(position = position_dodge(width = 1.4), alpha = 1, width = 0.1, outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.75,
                                             dodge.width = 0.75),  size = 1, inherit.aes = TRUE) +
  stat_summary(fun.y = "median", geom = "point", size = 1, color="red",
               position = position_dodge(width = 1.4)) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(-0.5, 0.5)) +
  labs(x = 'Rating', y = 'RWA', color = 'Family', fill = "Family") +
  scale_fill_manual(values=cbPalette) +
  scale_color_manual(values=cbPalette, guide = "none") +
  stat_function(aes(x = as.character(response), y = RWAscore,
                    fill = as.character(spreading), color = as.character(spreading)),
                fun = logist) +
  theme_ridges(center = TRUE) +
  coord_flip()

data_plot_neg

## plot pos

spreading_pos <- subset(spreading, usvalence==0.5)

data_plot_pos = spreading_pos %>%
  ggplot(aes(x = as.character(response), y = RWAscore,
             fill = as.character(spreading), color = as.character(spreading))) +
  geom_boxplot(position = position_dodge(width = 1.4), alpha = 1, width = 0.1, outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.75,
                                             dodge.width = 0.75),  size = 1, inherit.aes = TRUE) +
  stat_summary(fun.y = "median", geom = "point", size = 1, color="red",
               position = position_dodge(width = 1.4)) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(-0.5, 0.5)) +
  labs(x = 'Rating', y = 'RWA', color = 'Family', fill = "Family") +
  scale_fill_manual(values=cbPalette) +
  scale_color_manual(values=cbPalette, guide = "none") +
  stat_function(aes(x = as.character(response), y = RWAscore,
                    fill = as.character(spreading), color = as.character(spreading)),
                fun = logist) +
  theme_ridges(center = TRUE) +
  coord_flip()

data_plot_pos


paint_pomological(data_plot, res = 110)

parameters(spread_resp)

head(add_fitted_samples(spread_resp))

spread_resp %>%
  spread_samples(b_RWAscore) %>%
  mean_qi()


pst <- (posterior_samples(spread_resp, "b"))
pst$b_Intercept <- rowMeans (pst[,1:8])
pst$b_Intercept <- pst$b_Intercept + 5

pst_samp <- pst[sample(nrow(pst), 100), ]

ggplot(spreading, aes(x = RWAscore, y = response)) +
  geom_point(shape = 1) +
  geom_abline(data = pst_samp, alpha = .1, size = .4,
              aes(intercept = b_Intercept, slope = b_RWAscore, color = "blue"))

head(pst)

pst$b_Intercept <- rowMeans (pst[,1:8])

rowMeans (pst[1:8,1:8])

mean(pst)

parameters(spread_resp)

spread_resp %>%
  spread_samples(b_RWAscore, b_spreading) %>%
  head(10)

spreading %>%
  group_by(spreading) %>%
  add_fitted_samples(spread_resp, n = 100) %>%
  ggplot(aes(x = RWAscore, y = response, color = spreading)) +
  geom_line(aes(y = estimate, group = paste(spreading, .iteration)), alpha = 0.25)

spreading %>%
  group_by(spreading) %>%
  add_fitted_samples(spread_resp, n = 100) %>%
  ggplot(aes(x = RWAscore, y = response, color = spreading)) +
  geom_line(aes(y = estimate, group = paste(spreading, .iteration)), alpha = 0.25)

data_frame(spreading = -0.5) %>%
  add_fitted_samples(spread_resp) %>%
  mean_qi(estimate)

data_plot = spreading %>%
  ggplot(aes(x = RWAscore, y = response, color = as.character(spreading))) +
  geom_point()

library(modelr)

fit_plot = spreading %>%
  data_grid(RWAscore = seq_range(RWAscore, n = 101)) %>%
  add_fitted_samples(spread_resp2) %>%
  ggplot(aes(x = RWAscore, y = estimate, color = category)) +
  stat_lineribbon(alpha = .5) +
  scale_fill_brewer(palette = "Greys")

predict(spread_resp, summary = FALSE)
