# File name: plot_marg_xp08.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Mon Jul 09 18:31:43 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to create the marginal plots from brms models
# corresponding to the 08th experiment of Amelie Bret's doctoral work
#
# This R script plots marginal effect corresponding to the association
# of RWA with grebles' ratings in all combinations of the other indepentdent variables :
# usvalence : positive (0.5) vs. negative (-0.5)
# and number of counter conditioning : 1 (- 0.5) vs. 2 (0.5)
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
# number fo counter-conditionings and usvalence
#------------------------------------------------------------------------------------

# !!1 counter-conditioning!!stimuli with !!negative!! valence
cond_cc1neg <- data.frame(counter2 = -0.5, usvalence = -0.5,
                      cond__ = "counter1_negative")

# !!1 counter-conditioning!!stimuli with !!positive!! valence
cond_cc1pos <- data.frame(counter2 = -0.5, usvalence = 0.5,
                      cond__ = "counter1_positive")

# !!2 counter-conditionings!!stimuli with !!negative!! valence
cond_cc2neg <- data.frame(counter2 = 0.5, usvalence = -0.5,
                      cond__ = "counter2_negative")

# !!2 counter-conditionings!!stimuli with !!positive!! valence
cond_cc2pos <- data.frame(counter2 = 0.5, usvalence = 0.5,
                      cond__ = "counter2_positive")

#------------------------------------------------------------------------------------
# We then select the marginal effects of RWA from the model
# for each combination of modalities
#------------------------------------------------------------------------------------

# !!1 counter-conditioning!! with !!negative!! valence
marg_cc1neg <- marginal_effects(doubleone_resp, effects = "RWAscore", ordinal = TRUE, 
                            conditions = cond_cc1neg, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)

# !!1 counter-conditioning!! with !!positive!! valence
marg_cc1pos <- marginal_effects(doubleone_resp, effects = "RWAscore", ordinal = TRUE, 
                            conditions = cond_cc1pos, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)

# !!2 counter-conditionings!! with !!negative!! valence
marg_cc2neg <- marginal_effects(doubleone_resp, effects = "RWAscore", ordinal = TRUE, 
                            conditions = cond_cc2neg, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)

# !!2 counter-conditionings!! with !!positive!! valence
marg_cc2pos <- marginal_effects(doubleone_resp, effects = "RWAscore", ordinal = TRUE, 
                            conditions = cond_cc2pos, method = c("fitted"), # here his where we specify the combination
                            re_formula = NULL)

#------------------------------------------------------------------------------------
# Now we can plot marginal effects for each combination of conditions...
#------------------------------------------------------------------------------------

# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

# !!1 counter-conditioning!! with !!negative!! valence
marg_plot_cc1neg = plot(marg_cc1neg, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement négatif suivit d'un contre-conditionnement positif") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
# !!1 counter-conditioning!! with !!positive!! valence
marg_plot_cc1pos = plot(marg_cc1pos, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement positif suivit d'un contre-conditionnement négatif") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# !!2 counter-conditionings!!with !!negative!! valence
marg_plot_cc2neg = plot(marg_cc2neg, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement négatif suivit de deux contre-conditionnements positifs") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())

# !!2 counter-conditionings!! with !!positive!! valence
marg_plot_cc2pos = plot(marg_cc2pos, plot = FALSE)[[1]] + # here his where we specify the marginal effects of interest
  scale_fill_gradientn(colors = matlab.like(10), na.value = "transparent") +
  scale_y_continuous(name="Évaluations", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), expand = c(0,0)) +
  scale_x_continuous(name="RWA", breaks = scales::pretty_breaks(n = 10), expand=c(0,0)) +
  labs(fill="Probabilité",
       subtitle="Conditionnnement positif suivit de deux contre-conditionnements négatifs") + # here his where we mention the marginal effects of interest
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())















