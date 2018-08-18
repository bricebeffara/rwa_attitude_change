# File name: 04a_plot_marg_fitted_xp05.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Thu Aug 16 16:50:50 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to create the marginal plots from brms models
# corresponding to the 05th experiment of Amelie Bret's doctoral work
#
# This R script plots marginal effect corresponding to the association
# of RWA with grebles' ratings in all combinations of the other indepentdent variables :
# usvalence : positive (0.5) vs. negative (-0.5)
# and order : direct first (0.5) vs. indirect first (-0.5)
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
### 3. After the above actions are aordomplished, this program should
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
       ggExtra,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

#------------------------------------------------------------------------------------
# First we determine all the possible combinations of modalities
# between the two categorical independent variables :
# usvalence and order of measurement
#------------------------------------------------------------------------------------

# !!order1!!stimuli with !!negative!! valence
cond_ord1neg <- data.frame(order = -0.5, usvalence = -0.5,
                      cond__ = "order1_negative")

# !!order1!!stimuli with !!positive!! valence
cond_ord1pos <- data.frame(order = -0.5, usvalence = 0.5,
                      cond__ = "order1_positive")

# !!order2!!stimuli with !!negative!! valence
cond_ord2neg <- data.frame(order = 0.5, usvalence = -0.5,
                      cond__ = "order2_negative")

# !!order2!!stimuli with !!positive!! valence
cond_ord2pos <- data.frame(order = 0.5, usvalence = 0.5,
                      cond__ = "order2_positive")

#------------------------------------------------------------------------------------
# We then select the marginal effects of RWA from the model
# for each combination of modalities
#------------------------------------------------------------------------------------

# !!order1!! with !!negative!! valence
marg_ord1neg <- marginal_effects(expli_resp, effects = "RWAscore", ordinal = TRUE, 
                            conditions = cond_ord1neg, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)

# !!order1!! with !!positive!! valence
marg_ord1pos <- marginal_effects(expli_resp, effects = "RWAscore", ordinal = TRUE, 
                            conditions = cond_ord1pos, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)

# !!order2!! with !!negative!! valence
marg_ord2neg <- marginal_effects(expli_resp, effects = "RWAscore", ordinal = TRUE, 
                            conditions = cond_ord2neg, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)

# !!order2!! with !!positive!! valence
marg_ord2pos <- marginal_effects(expli_resp, effects = "RWAscore", ordinal = TRUE, 
                            conditions = cond_ord2pos, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)

#------------------------------------------------------------------------------------
# Now we can plot marginal effects for each combination of conditions...
#------------------------------------------------------------------------------------

# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

# !!order1!! with !!negative!! valence
marg_plot_ord1neg = plot(marg_ord1neg, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement négatif - Mesure indirecte puis directe") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
# !!order1!! with !!positive!! valence
marg_plot_ord1pos = plot(marg_ord1pos, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement positif - Mesure indirecte puis directe") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# !!order2!!with !!negative!! valence
marg_plot_ord2neg = plot(marg_ord2neg, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement négatif - Mesure directe puis indirecte") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())

# !!order2!! with !!positive!! valence
marg_plot_ord2pos = plot(marg_ord2pos, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement positif - Mesure directe puis indirecte") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#------------------------------------------------------------------------------------
# ...and combine plots together
#------------------------------------------------------------------------------------

# Combine plot
marg_all <- ggarrange(marg_plot_ord1neg,
                      marg_plot_ord2neg,
                      marg_plot_ord1pos,
                      marg_plot_ord2pos,
                      ncol = 2, nrow = 2)

# uncomment to display and save plot
# marg_all
# ggsave("plots/marg_xp05_french.jpg", width = 50, height = 30, units = "cm")
# ggsave("plots/marg_xp05_french.pdf", width = 50, height = 30, units = "cm")
# ggsave("plots/marg_xp05_french.tex", width = 50, height = 30, units = "cm")













