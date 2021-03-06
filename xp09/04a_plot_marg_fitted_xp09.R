# File name: plot_marg_xp09.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Thu Jun 07 15:48:36 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to create the marginal plots from brms models
# corresponding to the 9th experiment of Amelie Bret's doctoral work
#
# This R script plots marginal effect corresponding to the association
# of RWA with grebles' ratings in all combinations of the other indepentdent variables :
# usvalence : positive (0.5) vs. negative (-0.5)
# and load : no load (-0.5) vs. load (0.5)
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
# First we determine all the possible combinations of modalities
# between the two categorical independent variables :
# "load" and "usvalence"
#------------------------------------------------------------------------------------

# !!no load!! with !!negative!! valence
cond_nolneg <- data.frame(load = -0.5, usvalence = -0.5,
                      cond__ = "noload_negative")

# !!no load!! with !!positive!! valence
cond_nolpos <- data.frame(load = -0.5, usvalence = 0.5,
                      cond__ = "noload_positive")

# !!load!! conditionned stimuli with !!negative!! valence
cond_yelneg <- data.frame(load = 0.5, usvalence = -0.5,
                      cond__ = "yeload_negative")

# !!load!! conditionned stimuli with !!positive!! valence
cond_yelpos <- data.frame(load = 0.5, usvalence = 0.5,
                      cond__ = "yeload_positive")

#------------------------------------------------------------------------------------
# We then select the marginal effects of RWA from the model
# for each combination of modalities
#------------------------------------------------------------------------------------

# !!noload!! conditioned stimuli with !!negative!! valence
marg_nolneg <- marginal_effects(load_resp, effects = "RWAscore", ordinal = TRUE, 
                            conditions = cond_nolneg, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)

# !!noload!! conditioned stimuli with !!positive!! valence
marg_nolpos <- marginal_effects(load_resp, effects = "RWAscore", ordinal = TRUE, 
                            conditions = cond_nolpos, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)

# !!load!! conditionned stimuli with !!negative!! valence
marg_yelneg <- marginal_effects(load_resp, effects = "RWAscore", ordinal = TRUE, 
                            conditions = cond_yelneg, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)

# !!load!! conditionned stimuli with !!positive!! valence
marg_yelpos <- marginal_effects(load_resp, effects = "RWAscore", ordinal = TRUE, 
                            conditions = cond_yelpos, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)

#------------------------------------------------------------------------------------
# Now we can plot marginal effects for each combination of conditions...
#------------------------------------------------------------------------------------

# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

# !!noload!! conditioned stimuli with !!negative!! valence
marg_plot_nolneg = plot(marg_nolneg, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement négatif sans charge lors du contre-conditionnement positif") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
# !!noload!! conditioned stimuli with !!positive!! valence
marg_plot_nolpos = plot(marg_nolpos, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement positif sans charge lors du contre-conditionnement négatif") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# !!load!! conditionned stimuli with !!negative!! valence
marg_plot_yelneg = plot(marg_yelneg, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement négatif avec charge lors du contre-conditionnement positif") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())

# !!load!! conditionned stimuli with !!positive!! valence
marg_plot_yelpos = plot(marg_yelpos, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement positif avec charge lors du contre-conditionnement négatif") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#------------------------------------------------------------------------------------
# ...and combine plots together
#------------------------------------------------------------------------------------

# Combine plot
marg_all <- ggarrange(marg_plot_nolneg,
                      marg_plot_yelneg,
                      marg_plot_nolpos,
                      marg_plot_yelpos,
                      ncol = 2, nrow = 2)

# uncomment to display and save plot
# marg_all
# ggsave("plots/marg_xp09_french.jpg", width = 50, height = 30, units = "cm")
# ggsave("plots/marg_xp09_french.pdf", width = 50, height = 30, units = "cm")
# ggsave("plots/marg_xp09_french.tex", width = 50, height = 30, units = "cm")













