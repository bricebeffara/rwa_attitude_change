# File name: plot_marg_xp01.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Sat Aug 04 17:29:44 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to create the marginal plots from brms models
# corresponding to the 10th experiment of Amelie Bret's doctoral work
#
# This R script plots marginal effect corresponding to the association
# of RWA with grebles' ratings in all combinations of the other indepentdent variables :
# usvalence : positive (0.5) vs. negative (-0.5)
# and warn : level 1 (-0.5) vs. level 2 (0.5) conditioning
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
       ggExtra,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

#------------------------------------------------------------------------------------
# First we determine the different modalities of the categorical independent variable :
# "usvalence"
#------------------------------------------------------------------------------------

# !!negative!! valence
cond_neg <- data.frame(usvalence = -0.5,
                          cond__ = "negative")

# !!positive!! valence
cond_pos <- data.frame(usvalence = 0.5,
                          cond__ = "positive")


#------------------------------------------------------------------------------------
# We then select the marginal effects of RWA from the model
# for each combination of modalities
#------------------------------------------------------------------------------------

# !!negative!! valence
marg_neg <- marginal_effects(base_resp, effects = "RWAscore", ordinal = TRUE, 
                                conditions = cond_neg, method = c("fitted"), # here his where we specify the combination
                                re_formula = NULL)

# !!positive!! valence
marg_pos <- marginal_effects(base_resp, effects = "RWAscore", ordinal = TRUE, 
                                conditions = cond_pos, method = c("fitted"), # here his where we specify the combination
                                re_formula = NULL)

#------------------------------------------------------------------------------------
# Now we can plot marginal effects for each modality...
#------------------------------------------------------------------------------------

# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

# !!negative!! valence
marg_plot_neg = plot(marg_neg, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement négatif") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# !!positive!! valence
marg_plot_pos = plot(marg_pos, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement positif") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#------------------------------------------------------------------------------------
# ...and combine plots together
#------------------------------------------------------------------------------------

# Combine plot
marg_all <- ggarrange(marg_plot_neg,
                      marg_plot_pos,
                      ncol = 1, nrow = 2)

# uncomment to display and save plot
# marg_all
# ggsave("plots/marg_xp01_french.jpg", width = 25, height = 30, units = "cm")
# ggsave("plots/marg_xp01_french.pdf", width = 25, height = 30, units = "cm")
# ggsave("plots/marg_xp01_french.tex", width = 25, height = 30, units = "cm")













