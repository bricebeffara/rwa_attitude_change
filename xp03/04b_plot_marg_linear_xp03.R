# File name: plot_marg_xp11.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Fri Aug 10 17:16:08 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to create the marginal plots from brms models
# corresponding to the 3rd experiment of Amelie Bret's doctoral work
#
# This R script plots marginal effect corresponding to the association
# of RWA with grebles' ratings in all combinations of the other indepentdent variables :
# usvalence2 : positive (0.5) vs. negative (-0.5)
# and bloc : 1 bloc (-0.5) vs. 2 blocs (0.5)
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
# First we determine the condition of the bloc variable
#------------------------------------------------------------------------------------


# !!orginally!! conditioned stimuli with !!negative!! valence
cond_b1 <- data.frame(bloc = -0.5,
                      cond__ = "1 bloc")

# !!orginally!! conditioned stimuli with !!positive!! valence
cond_b2 <- data.frame(bloc = 0.5,
                      cond__ = "2 blocs")

#------------------------------------------------------------------------------------
# We then select the marginal effects of RWA from the model
# for each combination of modalities
#------------------------------------------------------------------------------------

# !!orginally!! conditioned stimuli with !!negative!! valence
marg_b1 <- marginal_effects(bloc_resp, effects = "RWAscore:usvalence2", 
                            conditions = cond_b1, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL,
                            spaghetti = TRUE, nsamples = 500)

# !!orginally!! conditioned stimuli with !!positive!! valence
marg_b2 <- marginal_effects(bloc_resp, effects = "RWAscore:usvalence2", 
                            conditions = cond_b2, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL,
                            spaghetti = TRUE, nsamples = 500)

#------------------------------------------------------------------------------------
# Now we can plot marginal effects for each combination of conditions...
#------------------------------------------------------------------------------------

# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

# select marginal effects

attr(marg_b1$`RWAscore:usvalence2`, "spaghetti") <- attr(marg_b1$`RWAscore:usvalence2`, "spaghetti")[grepl(0.5, attr(marg_b1$`RWAscore:usvalence2`, "spaghetti")$sample__),]

attr(marg_b2$`RWAscore:usvalence2`, "spaghetti") <- attr(marg_b2$`RWAscore:usvalence2`, "spaghetti")[grepl(0.5, attr(marg_b2$`RWAscore:usvalence2`, "spaghetti")$sample__),]

attr(marg_b1$`RWAscore:usvalence2`, "spaghetti")$usvalence2 <- ifelse (attr(marg_b1$`RWAscore:usvalence2`, "spaghetti")$usvalence2 == -0.5, "Négative", "Positive")

attr(marg_b2$`RWAscore:usvalence2`, "spaghetti")$usvalence2 <- ifelse (attr(marg_b2$`RWAscore:usvalence2`, "spaghetti")$usvalence2 == -0.5, "Négative", "Positive")

# !!1 bloc!!
marg_plot_b1 = plot(marg_b1, plot = FALSE, mean = F)[[1]] + # here his where we specify the marginal effects of interest
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  coord_cartesian(ylim=c(1,9)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(colour="Dernière information",
       subtitle="1 Bloc") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15)


# !!2 blocs!!
marg_plot_b2 = plot(marg_b2, plot = FALSE, mean = F)[[1]] + # here his where we specify the marginal effects of interest
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  coord_cartesian(ylim=c(1,9)) +
  #scale_colour_hue(labels = c("Level 1", "Level 2")) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(colour="Dernière information",
       subtitle="2 Blocs") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15)

#------------------------------------------------------------------------------------
# ...and combine plots together
#------------------------------------------------------------------------------------

# Combine plot
marg_spag_all <- ggarrange(marg_plot_b1,
                           marg_plot_b2,
                      ncol = 2, nrow = 1)


# uncomment to display and save plot
# marg_spag_all
# ggsave("plots/marg_spag_xp11_french.jpg", width = 50, height = 15, units = "cm")
# ggsave("plots/marg_spag_xp11_french.pdf", width = 50, height = 15, units = "cm")
# ggsave("plots/marg_spag_xp11_french.tex", width = 50, height = 15, units = "cm")













