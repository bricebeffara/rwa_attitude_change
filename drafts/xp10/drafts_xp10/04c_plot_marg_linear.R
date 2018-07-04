# File name: plot_marg_xp10.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Thu Jun 07 15:48:36 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to create the marginal plots from brms models
# corresponding to the 10th experiment of Amelie Bret's doctoral work
#
# This R script plots marginal effect corresponding to the association
# of RWA with grebles' ratings in all combinations of the other indepentdent variables :
# usvalence : positive (0.5) vs. negative (-0.5)
# and spreading : direct (0.5) vs. indirect (-0.5) conditioning
#
# This program is believed to be free of errors, but it comes with no guarantee! 
# The user bears all responsibility for interpreting the results.
#
# This preambule is largely inspired by John K. Kruschke's work at https://osf.io/wp2ry/
#
### To run this program, please do the following:
### 1. Install the general-purpose programming language R from  
###      http://www.r-project.org/
###    Install the version of R appropriate for your computer's operating
###    system (Windows, MacOS, or Linux).   
### 2. Install the R editor, RStudio, from
###      http://rstudio.org/
###    This editor is not necessary, but highly recommended.
### 3. After the above actions are accomplished, this program should
###    run as-is in R. You may "source" it to run the whole thing at once,
###    or, preferably, run lines consecutively from the beginning.
################################################################################


# Loading packages needed (and installing if necessary) for this part

if (!require("pacman")) install.packages("pacman")
p_load(ggplot2, # main package for plots
       colorRamps, # to add color palettes
       ggpubr, # to combine plots
       extrafontdb, # to get more available fonts for plots
       hrbrthemes, # for ggplot2 theme
       extrafont,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

#------------------------------------------------------------------------------------
# First we determine all the possible combinations of modalities
# between the two categorical independent variables :
# "spreading" and "usvalence"
#------------------------------------------------------------------------------------

# !!orginally!! conditioned stimuli with !!negative!! valence
cond_on <- data.frame(spreading = -0.5, usvalence = -0.5,
                      cond__ = "original_negative")

# !!orginally!! conditioned stimuli with !!positive!! valence
cond_op <- data.frame(spreading = -0.5, usvalence = 0.5,
                      cond__ = "original_positive")

# !!spread!! conditionned stimuli with !!negative!! valence
cond_sn <- data.frame(spreading = 0.5, usvalence = -0.5,
                      cond__ = "spreading_negative")

# !!spread!! conditionned stimuli with !!positive!! valence
cond_sp <- data.frame(spreading = 0.5, usvalence = 0.5,
                      cond__ = "spreading_positive")

#------------------------------------------------------------------------------------
# We then select the marginal effects of RWA from the model
# for each combination of modalities
#------------------------------------------------------------------------------------

# !!orginally!! conditioned stimuli with !!negative!! valence
marg_on <- marginal_effects(spread_resp, effects = "RWAscore", ordinal = FALSE, 
                            conditions = cond_on, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL, spaghetti = TRUE, nsamples = 200)$RWAscore

# !!orginally!! conditioned stimuli with !!positive!! valence
marg_op <- marginal_effects(spread_resp, effects = "RWAscore", ordinal = FALSE, 
                            conditions = cond_op, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)$RWAscore

# !!spread!! conditionned stimuli with !!negative!! valence
marg_sn <- marginal_effects(spread_resp, effects = "RWAscore", ordinal = FALSE, 
                            conditions = cond_sn, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)$RWAscore

# !!spread!! conditionned stimuli with !!positive!! valence
marg_sp <- marginal_effects(spread_resp, effects = "RWAscore", ordinal = FALSE, 
                            conditions = cond_sp, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)$RWAscore

#------------------------------------------------------------------------------------
# Now we can plot marginal effects for each combination of conditions...
#------------------------------------------------------------------------------------

# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

# !!orginally!! conditioned stimuli with !!negative!! valence
marg_plot_on = marg_on %>% # here his where we specify the marginal effects of interest
  ggplot(aes(x = RWAscore, y = cats__, color = estimate__)) +
  geom_raster(aes(fill = estimate__), interpolate = FALSE) +
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Ratings", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probability",
       subtitle="Direct negative conditioning") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15)

# !!orginally!! conditioned stimuli with !!positive!! valence
marg_plot_op = marg_op %>% # here his where we specify the marginal effects of interest
  ggplot(aes(x = RWAscore, y = cats__, color = estimate__)) +
  geom_raster(aes(fill = estimate__), interpolate = FALSE) +
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Ratings", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probability",
       subtitle="Direct positive conditioning") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15)

# !!spread!! conditionned stimuli with !!negative!! valence
marg_plot_sn = marg_sn %>% # here his where we specify the marginal effects of interest
  ggplot(aes(x = RWAscore, y = cats__, color = estimate__)) +
  geom_raster(aes(fill = estimate__), interpolate = FALSE) +
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Ratings", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probability",
       subtitle="Indirect negative conditioning") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15)

# !!spread!! conditionned stimuli with !!positive!! valence
marg_plot_sp = marg_sp %>% # here his where we specify the marginal effects of interest
  ggplot(aes(x = RWAscore, y = cats__, color = estimate__)) +
  geom_raster(aes(fill = estimate__), interpolate = FALSE) +
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Ratings", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probability",
       subtitle="Indirect positive conditioning") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15)


#------------------------------------------------------------------------------------
# ...and combine plots together
#------------------------------------------------------------------------------------

# Combine plot
marg_all <- ggarrange(marg_plot_on,
                      marg_plot_op,
                      marg_plot_sn,
                      marg_plot_sp,
                      ncol = 2, nrow = 2)

# uncomment to display plot
# marg_all

# save plot
ggsave("plots/marg_all_xp102.pdf", width = 50, height = 30, units = "cm")













