#------------------------------------------------------------------------------------
# First we determine all the possible combinations of modalities
# between the two categorical independent variables :
# number fo counter-conditionings and usvalence
#------------------------------------------------------------------------------------

# !!1 counter-conditioning!!stimuli with !!negative!! valence
cond_cctneg <- data.frame(counter2 = -0.5, usvalence = -0.5,
                          cond__ = "counter1_negative")

# !!1 counter-conditioning!!stimuli with !!positive!! valence
cond_cctpos <- data.frame(counter2 = -0.5, usvalence = 0.5,
                          cond__ = "counter1_positive")

# !!2 counter-conditionings!!stimuli with !!negative!! valence
cond_tccneg <- data.frame(counter2 = -0.5, usvalence = -0.5,
                          cond__ = "counter2_negative")

# !!2 counter-conditionings!!stimuli with !!positive!! valence
cond_tccpos <- data.frame(counter2 = -0.5, usvalence = 0.5,
                          cond__ = "counter2_positive")

#------------------------------------------------------------------------------------
# We then select the marginal effects of RWA from the model
# for each combination of modalities
#------------------------------------------------------------------------------------

# !!1 counter-conditioning!! with !!negative!! valence
marg_cctneg <- marginal_effects(doubleonetime_resp, effects = "RWAscore", ordinal = TRUE, 
                                conditions = cond_cctneg, method = c("fitted"), # here his where we specify the combination
                                re_formula = NULL)

# !!1 counter-conditioning!! with !!positive!! valence
marg_cctpos <- marginal_effects(doubleonetime_resp, effects = "RWAscore", ordinal = TRUE, 
                                conditions = cond_cctpos, method = c("fitted"), # here his where we specify the combination
                                re_formula = NULL)

# !!2 counter-conditionings!! with !!negative!! valence
marg_tccneg <- marginal_effects(doubletimeone_resp, effects = "RWAscore", ordinal = TRUE, 
                                conditions = cond_tccneg, method = c("fitted"), # here his where we specify the combination
                                re_formula = NULL)

# !!2 counter-conditionings!! with !!positive!! valence
marg_tccpos <- marginal_effects(doubletimeone_resp, effects = "RWAscore", ordinal = TRUE, 
                                conditions = cond_tccpos, method = c("fitted"), # here his where we specify the combination
                                re_formula = NULL)

#------------------------------------------------------------------------------------
# Now we can plot marginal effects for each combination of conditions...
#------------------------------------------------------------------------------------

# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

# !!1 counter-conditioning!! with !!negative!! valence
marg_plot_cctneg = plot(marg_cctneg, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement négatif suivi d'un contre-conditionnement positif et d'un délai") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# !!1 counter-conditioning!! with !!positive!! valence
marg_plot_cctpos = plot(marg_cctpos, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement positif suivi d'un contre-conditionnement négatif et d'un délai") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# !!2 counter-conditionings!!with !!negative!! valence
marg_plot_tccneg = plot(marg_tccneg, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement négatif suivi d'un délai et d'un contre-conditionnement positif") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# !!2 counter-conditionings!! with !!positive!! valence
marg_plot_tccpos = plot(marg_tccpos, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement positif suivi d'un délai et d'un contre-conditionnement négatif") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



