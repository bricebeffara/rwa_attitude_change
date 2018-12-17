# File name: plot_marg_xp03.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Fri Aug 10 16:54:35 2018 ------------------------------
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
       ggExtra,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

#------------------------------------------------------------------------------------
# First we determine all the possible combinations of modalities
# between the two categorical independent variables :
# "bloc" and "usvalence2"
#------------------------------------------------------------------------------------

# !!1 bloc!! with !!negative!! valence
cond_b1neg <- data.frame(bloc = -0.5, usvalence2 = -0.5,
                         cond__ = "bloc1_negative")

# !!1 bloc!! with !!positive!! valence
cond_b1pos <- data.frame(bloc = -0.5, usvalence2 = 0.5,
                         cond__ = "bloc1_positive")

# !!2 blocs!! with !!negative!! valence
cond_b2neg <- data.frame(bloc = 0.5, usvalence2 = -0.5,
                         cond__ = "bloc2_negative")

# !!2 blocs!! with !!positive!! valence
cond_b2pos <- data.frame(bloc = 0.5, usvalence2 = 0.5,
                         cond__ = "bloc2_positive")

#------------------------------------------------------------------------------------
# We then select the marginal effects of RWA from the model
# for each combination of modalities
#------------------------------------------------------------------------------------

# !!1 bloc!! with !!negative!! valence
marg_b1neg <- marginal_effects(bloc_resp, effects = "RWAscore", ordinal = TRUE, 
                               conditions = cond_b1neg, method = c("fitted"), # here his where we specify the combination
                               re_formula = NULL)

# !!1 bloc!! with !!positive!! valence
marg_b1pos <- marginal_effects(bloc_resp, effects = "RWAscore", ordinal = TRUE, 
                               conditions = cond_b1pos, method = c("fitted"), # here his where we specify the combination
                               re_formula = NULL)

# !!2 blocs!! with !!negative!! valence
marg_b2neg <- marginal_effects(bloc_resp, effects = "RWAscore", ordinal = TRUE, 
                               conditions = cond_b2neg, method = c("fitted"), # here his where we specify the combination
                               re_formula = NULL)

# !!2 blocs!! with !!positive!! valence
marg_b2pos <- marginal_effects(bloc_resp, effects = "RWAscore", ordinal = TRUE, 
                               conditions = cond_b2pos, method = c("fitted"), # here his where we specify the combination
                               re_formula = NULL)

#------------------------------------------------------------------------------------
# Now we can plot marginal effects for each combination of conditions...
#------------------------------------------------------------------------------------

# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

# !!1 bloc!! with !!negative!! valence
marg_plot_b1neg = plot(marg_b1neg, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Evaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probability",
       title="Simple conditioning - Negative LAI") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# !!1 bloc!! with !!positive!! valence
marg_plot_b1pos = plot(marg_b1pos, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Evaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probability",
       title="Simple conditioning - Positive LAI") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# !!2 blocs!! with !!negative!! valence
marg_plot_b2neg = plot(marg_b2neg, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Evaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probability",
       title="Conditioning and counter conditioning - Negative LAI") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# !!2 blocs!! with !!positive!! valence
marg_plot_b2pos = plot(marg_b2pos, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Evaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probability",
       title="Conditioning and counter conditioning - Positive LAI") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#------------------------------------------------------------------------------------
# ...and combine plots together
#------------------------------------------------------------------------------------

# Combine plot
marg_all <- ggarrange(marg_plot_b1neg,
                      marg_plot_b2neg,
                      marg_plot_b1pos,
                      marg_plot_b2pos,
                      ncol = 2, nrow = 2)

marg_b1 <- ggarrange(marg_plot_b1neg,
                     marg_plot_b1pos,
                     ncol = 1, nrow = 2)

marg_b2 <- ggarrange(marg_plot_b2neg,
                     marg_plot_b2pos,
                     ncol = 1, nrow = 2)

# uncomment to display and save plots
# marg_all
# ggsave("plots/marg_xp03_french.jpg", width = 50, height = 30, units = "cm")
# ggsave("plots/marg_xp03_french.pdf", width = 50, height = 30, units = "cm")
# ggsave("plots/marg_xp03_english.pdf", width = 50, height = 30, units = "cm")
# ggsave("plots/marg_xp03_french.tex", width = 50, height = 30, units = "cm")
# marg_b1
# ggsave("plots/marg_b1_xp03_english.pdf", width = 25, height = 30, units = "cm")
# marg_b2
# ggsave("plots/marg_b2_xp03_english.pdf", width = 25, height = 30, units = "cm")













