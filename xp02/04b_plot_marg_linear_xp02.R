# File name: plot_marg_xp02.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Mon Aug 06 14:34:33 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to create the marginal plots from brms models
# corresponding to the 2d experiment of Amelie Bret's doctoral work
#
# This R script plots marginal effect corresponding to the association
# of RWA with grebles' ratings in all combinations of the other indepentdent variables :
# usvalence : positive (0.5) vs. negative (-0.5)
# and social : social(0.5) vs. object (-0.5)
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
# We select the marginal effects of RWA from the model
# for each combination of modalities
#------------------------------------------------------------------------------------

marg_social <- marginal_effects(social_resp, effects = "RWAscore:usvalence", 
                             method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL,
                            spaghetti = TRUE, nsamples = 500)


#------------------------------------------------------------------------------------
# Now we can plot marginal effects for each combination of conditions...
#------------------------------------------------------------------------------------

# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

# !!orginally!! conditioned stimuli with !!negative!! valence

attr(marg_social$`RWAscore:usvalence`, "spaghetti") <- attr(marg_social$`RWAscore:usvalence`, "spaghetti")[grepl(0.5, attr(marg_social$`RWAscore:usvalence`, "spaghetti")$sample__),]

attr(marg_social$`RWAscore:usvalence`, "spaghetti")$usvalence <- ifelse (attr(marg_social$`RWAscore:usvalence`, "spaghetti")$usvalence == -0.5, "Négative", "Positive")

#
marg_plot = plot(marg_social, plot = FALSE, mean = F)[[1]] + # here his where we specify the marginal effects of interest
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  coord_cartesian(ylim=c(1,9)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(colour="Valence conditionnement") +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  guides(color=guide_legend(override.aes=list(fill=NA)))

# uncomment to display and save plot
# marg_plot
# ggsave("plots/marg_spag_xp02_french.jpg", width = 25, height = 15, units = "cm")
# ggsave("plots/marg_spag_xp02_french.pdf", width = 25, height = 15, units = "cm")
# ggsave("plots/marg_spag_xp02_french.tex", width = 25, height = 15, units = "cm")













