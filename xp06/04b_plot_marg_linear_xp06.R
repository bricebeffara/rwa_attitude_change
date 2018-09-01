# File name: plot_marg_xp06.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Sat Sep  1 20:26:07 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to create the marginal plots from brms models
# corresponding to the 06th experiment of Amelie Bret's doctoral work
#
# This R script plots marginal effect corresponding to the association
# of RWA with grebles' ratings in all combinations of the other indepentdent variables :
# usvalencedir : positive (0.5) vs. negative (-0.5)
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
# First we determine the condition of the order variable
#------------------------------------------------------------------------------------


# !!One counter-conditioning!!
cond_ordone <- data.frame(order = -0.5,
                      cond__ = "One")

# !!Two counter-conditionings!!
cond_ordtwo <- data.frame(order = 0.5,
                      cond__ = "Two")

#------------------------------------------------------------------------------------
# We then select the marginal effects of RWA*usvalencedir from the model
# for each condition of order
#------------------------------------------------------------------------------------

# !!One counter-conditioning!!
marg_ordone <- marginal_effects(direct_resp, effects = "RWAscore:usvalencedirdir", 
                            conditions = cond_ordone, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL,
                            spaghetti = TRUE, nsamples = 500)

# !!Two counter-conditionings!!
marg_ordtwo <- marginal_effects(direct_resp, effects = "RWAscore:usvalencedir", 
                            conditions = cond_ordtwo, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL,
                            spaghetti = TRUE, nsamples = 500)

#------------------------------------------------------------------------------------
# Now we can plot marginal effects for each combination of conditions...
#------------------------------------------------------------------------------------

# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

attr(marg_ordone$`RWAscore:usvalencedir`, "spaghetti") <- attr(marg_ordone$`RWAscore:usvalencedir`, "spaghetti")[grepl(0.5, attr(marg_ordone$`RWAscore:usvalencedir`, "spaghetti")$sample__),]

attr(marg_ordtwo$`RWAscore:usvalencedir`, "spaghetti") <- attr(marg_ordtwo$`RWAscore:usvalencedir`, "spaghetti")[grepl(0.5, attr(marg_ordtwo$`RWAscore:usvalencedir`, "spaghetti")$sample__),]

attr(marg_ordone$`RWAscore:usvalencedir`, "spaghetti")$usvalencedir <- ifelse (attr(marg_ordone$`RWAscore:usvalencedir`, "spaghetti")$usvalencedir == -0.5, "Négative", "Positive")

attr(marg_ordtwo$`RWAscore:usvalencedir`, "spaghetti")$usvalencedir <- ifelse (attr(marg_ordtwo$`RWAscore:usvalencedir`, "spaghetti")$usvalencedir == -0.5, "Négative", "Positive")

# !!Order1!!
marg_plot_ordone = plot(marg_ordone, plot = FALSE, mean = F)[[1]] + # here his where we specify the marginal effects of interest
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  coord_cartesian(ylim=c(1,9)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(colour="Valence conditionnement",
       subtitle="Mesure indirecte puis directe") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  guides(color=guide_legend(override.aes=list(fill=NA)))


# !!order2!!
marg_plot_ordtwo = plot(marg_ordtwo, plot = FALSE, mean = F)[[1]] + # here his where we specify the marginal effects of interest
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  coord_cartesian(ylim=c(1,9)) +
  #scale_colour_hue(labels = c("Level 1", "Level 2")) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(colour="Valence conditionnement",
       subtitle="Mesure directe puis indirecte") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  guides(color=guide_legend(override.aes=list(fill=NA)))

#------------------------------------------------------------------------------------
# ...and combine plots together
#------------------------------------------------------------------------------------

# Combine plots
marg_spag_all <- ggarrange(marg_plot_ordone,
                           marg_plot_ordtwo,
                      ncol = 2, nrow = 1)


# uncomment to display and save plot
# marg_spag_all
# ggsave("plots/marg_spag_xp06_french.jpg", width = 50, height = 15, units = "cm")
# ggsave("plots/marg_spag_xp06_french.pdf", width = 50, height = 15, units = "cm")
# ggsave("plots/marg_spag_xp06_french.tex", width = 50, height = 15, units = "cm")













