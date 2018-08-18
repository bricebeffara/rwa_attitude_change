# doubleing packages needed (and installing if necessary) for this part

if (!require("pacman")) install.packages("pacman")
p_load(ggExtra,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

expli_df$Valence <- ifelse (expli_df$usvalence == -0.5, "Négative", "Positive")

# level 1 plot -----------------------------------------------------------

expli_df_ordone <- subset(expli_df, order == -0.5)

data_plot_ordone = expli_df_ordone %>%
  ggplot(aes(x = as.character(response), y = RWAscore,
             fill = Valence, color = Valence)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.05, 
                                             dodge.width = -0.75), alpha = 0.4, size = 1,
             shape = 19, inherit.aes = TRUE) +
  labs(x = 'Évaluations', y = 'RWA', colour="Valence conditionnement", fill ="Valence conditionnement") +
  scale_fill_manual(values=c("#73a5ff", "#50ce76")) +
  scale_color_manual(values= c("#73a5ff", "#50ce76"), guide = "none") +
  coord_cartesian(ylim=c(1,9)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(subtitle="Mesure indirecte puis directe") +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  coord_flip()

data_plot_ordone <- ggMarginal(data_plot_ordone, margins = "x", alpha = 0.6,
                         type = "histogram", size = 4, fill = "gray", colour = "lightgray")
  

# positive plot -----------------------------------------------------------
  
expli_df_ordtwo <- subset(expli_df, order == 0.5)

data_plot_ordtwo = expli_df_ordtwo %>%
  ggplot(aes(x = as.character(response), y = RWAscore,
             fill = Valence, color = Valence)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.05, 
                                             dodge.width = -0.75), alpha = 0.4, size = 1,
             shape = 19, inherit.aes = TRUE) +
  labs(x = 'Évaluations', y = 'RWA', colour="Valence conditionnement", fill ="Valence conditionnement") +
  scale_fill_manual(values=c("#73a5ff", "#50ce76")) +
  scale_color_manual(values= c("#73a5ff", "#50ce76"), guide = "none") +
  coord_cartesian(ylim=c(1,9)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(subtitle="Mesure directe puis indirecte") +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  coord_flip()

data_plot_ordtwo <- ggMarginal(data_plot_ordtwo, margins = "x", alpha = 0.6,
                              type = "histogram", size = 4, fill = "gray", colour = "lightgray")


# Combine plot
data_plot_all <- ggarrange(data_plot_ordone,
                           data_plot_ordtwo,
                           ncol = 2, nrow = 1)

# uncomment to display and save plot
# data_plot_all
# ggsave("plots/data_xp05_french.jpg", width = 50, height = 15, units = "cm")
# ggsave("plots/data_xp05_french.pdf", width = 50, height = 15, units = "cm")
# ggsave("plots/data_xp05_french.tex", width = 50, height = 15, units = "cm")

# Combine with spaghetti
data_spag_all <- ggarrange(marg_plot_ordone,
                           marg_plot_ordtwo,
                           data_plot_ordone,
                           data_plot_ordtwo,
                           ncol = 2, nrow = 2)

# uncomment to display and save plot
# data_spag_all
# ggsave("plots/data_spag_xp05_french.jpg", width = 50, height = 30, units = "cm")
# ggsave("plots/data_spag_xp05_french.pdf", width = 50, height = 30, units = "cm")
# ggsave("plots/data_spag_xp05_french.tex", width = 50, height = 30, units = "cm")

