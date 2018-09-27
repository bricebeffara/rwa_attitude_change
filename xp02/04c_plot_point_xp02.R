# Loading packages needed (and installing if necessary) for this part

if (!require("pacman")) install.packages("pacman")
p_load(ggExtra,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

social_df$Valence <- ifelse (social_df$usvalence == -0.5, "Négative", "Positive")

# data plot -----------------------------------------------------------

data_plot = social_df %>%
  ggplot(aes(x = response, y = RWAscore,
             fill = Valence, color = Valence)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.05, 
                                             dodge.width = -0.75), alpha = 0.4, size = 1,
             shape = 19, inherit.aes = TRUE) +
  labs(x = 'Évaluations', y = 'RWA') +
  scale_fill_manual(values=c("#73a5ff", "#50ce76")) +
  scale_color_manual(values= c("#73a5ff", "#50ce76"), guide = "none") +
  coord_cartesian(ylim=c(1,9)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  scale_x_continuous(limits=c(1, 9), breaks=seq(1, 9, 1)) +
  labs(color="Valence conditionnement",
       fill="Valence conditionnement") +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  coord_flip()

data_plot <- ggMarginal(data_plot, margins = "x", alpha = 0.6,
                              type = "histogram", size = 4, fill = "gray", colour = "lightgray")

data_plot <- ggarrange(data_plot,
                           ncol = 1, nrow = 1)

# uncomment to display and save plot
# data_plot
# ggsave("plots/data_xp02_french.jpg", width = 25, height = 15, units = "cm")
# ggsave("plots/data_xp02_french.pdf", width = 25, height = 15, units = "cm")
# ggsave("plots/data_xp02_french.tex", width = 25, height = 15, units = "cm")

# Combine with spaghetti
data_spag_all <- ggarrange(marg_plot,
                           data_plot,
                           ncol = 1, nrow = 2)

# uncomment to display and save plot
# data_spag_all
# ggsave("plots/data_spag_xp02_french.jpg", width = 25, height = 30, units = "cm")
# ggsave("plots/data_spag_xp02_french.pdf", width = 25, height = 30, units = "cm")
# ggsave("plots/data_spag_xp02_french.tex", width = 25, height = 30, units = "cm")

