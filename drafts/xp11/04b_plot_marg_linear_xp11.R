# File name: 04b_plot_marg_linear_xp11.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Tue Jun 26 12:08:58 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to create the marginal plots from brms models
# corresponding to the 10th experiment of Amelie Bret's doctoral work
#
# This R script plots marginal effect corresponding to the association
# of RWA with grebles' ratings in all combinations of the other indepentdent variables :
# usvalence : positive (0.5) vs. negative (-0.5)
# and warn : nor warning (0.5) vs. warning (-0.5)
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
       ggthemes,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

#------------------------------------------------------------------------------------
# First we determine all the possible combinations of modalities
# between the two categorical independent variables :
# "warn" and "usvalence"
#------------------------------------------------------------------------------------

# !!orginally!! conditioned stimuli with !!negative!! valence
cond_n <- data.frame(usvalence = -0.5,
                      cond__ = "negative")

# !!orginally!! conditioned stimuli with !!positive!! valence
cond_p <- data.frame(usvalence = 0.5,
                      cond__ = "positive")

#------------------------------------------------------------------------------------
# We then select the marginal effects of RWA from the model
# for each combination of modalities
#------------------------------------------------------------------------------------

# !!orginally!! conditioned stimuli with !!negative!! valence
marg_n <- marginal_effects(warn_resp, effects = "RWAscore:warn", 
                            conditions = cond_n, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL,
                            spaghetti = TRUE, nsamples = 500)

# !!orginally!! conditioned stimuli with !!positive!! valence
marg_p <- marginal_effects(warn_resp, effects = "RWAscore:warn", 
                            conditions = cond_p, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL,
                            spaghetti = TRUE, nsamples = 500)

#------------------------------------------------------------------------------------
# Now we can plot marginal effects for each combination of conditions...
#------------------------------------------------------------------------------------

# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

# !!orginally!! conditioned stimuli with !!negative!! valence

sawarn <- min((as.numeric(levels(unique(attr(marg_p$`RWAscore:warn`, "spaghetti")$warn)))))

zerwarn <- as.character ( min( abs( ((as.numeric(levels(unique(attr(marg_p$`RWAscore:warn`, "spaghetti")$warn))))))))

attr(marg_n$`RWAscore:warn`, "spaghetti") <- attr(marg_n$`RWAscore:warn`, "spaghetti")[!grepl(zerwarn, attr(marg_n$`RWAscore:warn`, "spaghetti")$sample__),]

attr(marg_p$`RWAscore:warn`, "spaghetti") <- attr(marg_p$`RWAscore:warn`, "spaghetti")[!grepl(zerwarn, attr(marg_p$`RWAscore:warn`, "spaghetti")$sample__),]

attr(marg_n$`RWAscore:warn`, "spaghetti")$warn <- ifelse (attr(marg_n$`RWAscore:warn`, "spaghetti")$warn == sawarn, "Sans", "Avec")

attr(marg_p$`RWAscore:warn`, "spaghetti")$warn <- ifelse (attr(marg_p$`RWAscore:warn`, "spaghetti")$warn == sawarn, "Sans", "Avec")

# !!orginally!! conditioned stimuli with !!positive!! valence
marg_plot_n = plot(marg_n, plot = FALSE, mean = F)[[1]] + # here his where we specify the marginal effects of interest
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  coord_cartesian(ylim=c(1,9)) +
  #scale_colour_hue(labels = c("Level 1", "Level 2")) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(colour="Avertissement",
       subtitle="Valence négative") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15)


# !!orginally!! conditioned stimuli with !!positive!! valence
marg_plot_p = plot(marg_p, plot = FALSE, mean = F)[[1]] + # here his where we specify the marginal effects of interest
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  coord_cartesian(ylim=c(1,9)) +
  #scale_colour_hue(labels = c("Level 1", "Level 2")) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(colour="Avertissement",
       subtitle="Valence positive") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15)

#------------------------------------------------------------------------------------
# ...and combine plots together
#------------------------------------------------------------------------------------

# Combine plot
marg_spag_all <- ggarrange(marg_plot_n,
                      marg_plot_p,
                      ncol = 2, nrow = 1)

# uncomment to display plot
# marg_spag_all

# save plot
ggsave("plots/marg_spag_xp11_french.pdf", width = 50, height = 15, units = "cm")













