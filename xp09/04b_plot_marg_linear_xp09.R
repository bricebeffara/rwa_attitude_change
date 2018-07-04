# File name: plot_marg_xp09.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Wed Jul 04 15:00:51 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to create the marginal plots from brms models
# corresponding to the 9th experiment of Amelie Bret's doctoral work
#
# This R script plots marginal effect corresponding to the association
# of RWA with grebles' ratings in all combinations of the other indepentdent variables :
# usvalence : positive (0.5) vs. negative (-0.5)
# and load_df : no load (- 0.5) vs. load (0.5)
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
# First we determine the condition of the load variable
#------------------------------------------------------------------------------------

# no load
cond_nolo <- data.frame(load = -0.5,
                      cond__ = "Sans")

# load
cond_yelo <- data.frame(load = 0.5,
                      cond__ = "Avec")

#------------------------------------------------------------------------------------
# We then select the marginal effects of RWA*usvalence from the model
# for each condition of load
#------------------------------------------------------------------------------------

# no load
marg_nolo <- marginal_effects(load_resp, effects = "RWAscore:usvalence", 
                            conditions = cond_nolo, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL,
                            spaghetti = TRUE, nsamples = 500)

# !!orginally!! conditioned stimuli with !!positive!! valence
marg_yelo <- marginal_effects(load_resp, effects = "RWAscore:usvalence", 
                            conditions = cond_yelo, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL,
                            spaghetti = TRUE, nsamples = 500)

#------------------------------------------------------------------------------------
# Now we can plot marginal effects of the interaction at each level
#------------------------------------------------------------------------------------

# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

# !!orginally!! conditioned stimuli with !!negative!! valence

attr(marg_nolo$`RWAscore:usvalence`, "spaghetti") <- attr(marg_nolo$`RWAscore:usvalence`, "spaghetti")[grepl(0.5, attr(marg_nolo$`RWAscore:usvalence`, "spaghetti")$sample__),]

attr(marg_yelo$`RWAscore:usvalence`, "spaghetti") <- attr(marg_yelo$`RWAscore:usvalence`, "spaghetti")[grepl(0.5, attr(marg_yelo$`RWAscore:usvalence`, "spaghetti")$sample__),]

attr(marg_nolo$`RWAscore:usvalence`, "spaghetti")$usvalence <- ifelse (attr(marg_nolo$`RWAscore:usvalence`, "spaghetti")$usvalence == -0.5, "Négative", "Positive")

attr(marg_yelo$`RWAscore:usvalence`, "spaghetti")$usvalence <- ifelse (attr(marg_yelo$`RWAscore:usvalence`, "spaghetti")$usvalence == -0.5, "Négative", "Positive")

# !!orginally!! conditioned stimuli with !!positive!! valence
marg_plot_nolo = plot(marg_nolo, plot = FALSE, mean = F)[[1]] + # here his where we specify the marginal effects of interest
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  coord_cartesian(ylim=c(1,9)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(colour="Valence conditionnement",
       subtitle="Contrôle (musique neutre) lors du contre-conditionnement") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15)


# !!orginally!! conditioned stimuli with !!positive!! valence
marg_plot_yelo = plot(marg_yelo, plot = FALSE, mean = F)[[1]] + # here his where we specify the marginal effects of interest
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  coord_cartesian(ylim=c(1,9)) +
  #scale_colour_hue(labels = c("Level 1", "Level 2")) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(colour="Valence conditionnement",
       subtitle="Charge cognitive lors du contre-conditionnement") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15)

#------------------------------------------------------------------------------------
# ...and combine plots together
#------------------------------------------------------------------------------------

# Combine plot
marg_spag_all <- ggarrange(marg_plot_nolo,
                           marg_plot_yelo,
                      ncol = 2, nrow = 1)


# uncomment to display and save plot
# marg_spag_all
# ggsave("plots/marg_spag_xp09_french.jpg", width = 50, height = 15, units = "cm")
# ggsave("plots/marg_spag_xp09_french.pdf", width = 50, height = 15, units = "cm")
# ggsave("plots/marg_spag_xp09_french.tex", width = 50, height = 15, units = "cm")













