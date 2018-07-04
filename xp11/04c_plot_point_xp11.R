# Loading packages needed (and installing if necessary) for this part

if (!require("pacman")) install.packages("pacman")
p_load(ggExtra,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

warn_df$Valence <- ifelse (warn_df$usvalence == -0.5, "Négative", "Positive")

# level 1 plot -----------------------------------------------------------

warn_df_nowa <- subset(warn_df, warn== -0.5)

data_plot_nowa = warn_df_nowa %>%
  ggplot(aes(x = as.character(response), y = RWAscore,
             fill = Valence, color = Valence)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.05, 
                                             dodge.width = -0.75), alpha = 0.4, size = 1,
             shape = 19, inherit.aes = TRUE) +
  labs(x = 'Évaluations', y = 'RWA') +
  scale_fill_manual(values=c("#73a5ff", "#50ce76")) +
  scale_color_manual(values= c("#73a5ff", "#50ce76"), guide = "none") +
  coord_cartesian(ylim=c(1,9)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(subtitle="Sans avertissement de contre-conditionnement",
       color="Valence conditionnement",
       fill="Valence conditionnement") +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  coord_flip()

data_plot_nowa <- ggMarginal(data_plot_nowa, margins = "x", alpha = 0.6,
                              type = "histogram", size = 4, fill = "gray", colour = "lightgray")


# positive plot -----------------------------------------------------------

warn_df_yewa <- subset(warn_df, warn== 0.5)

data_plot_yewa = warn_df_yewa %>%
  ggplot(aes(x = as.character(response), y = RWAscore,
             fill = Valence, color = Valence)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.05, 
                                             dodge.width = -0.75), alpha = 0.4, size = 1,
             shape = 19, inherit.aes = TRUE) +
  labs(x = 'Évaluations', y = 'RWA') +
  scale_fill_manual(values=c("#73a5ff", "#50ce76")) +
  scale_color_manual(values= c("#73a5ff", "#50ce76"), guide = "none") +
  coord_cartesian(ylim=c(1,9)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(subtitle="Avec avertissement de contre-conditionnement",
       color="Valence conditionnement",
       fill="Valence conditionnement") +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  coord_flip()

data_plot_yewa <- ggMarginal(data_plot_yewa, margins = "x", alpha = 0.6,
                              type = "histogram", size = 4, fill = "gray", colour = "lightgray")


# Combine plot
data_plot_all <- ggarrange(data_plot_nowa,
                           data_plot_yewa,
                           ncol = 2, nrow = 1)

# uncomment to display and save plot
# data_plot_all
# ggsave("plots/data_xp11_french.jpg", width = 50, height = 15, units = "cm")
# ggsave("plots/data_xp11_french.pdf", width = 50, height = 15, units = "cm")
# ggsave("plots/data_xp11_french.tex", width = 50, height = 15, units = "cm")

# Combine with spaghetti
data_spag_all <- ggarrange(marg_plot_nowa,
                           marg_plot_yewa,
                           data_plot_nowa,
                           data_plot_yewa,
                           ncol = 2, nrow = 2)

# uncomment to display and save plot
# data_spag_all
# ggsave("plots/data_spag_xp11_french.jpg", width = 50, height = 30, units = "cm")
# ggsave("plots/data_spag_xp11_french.pdf", width = 50, height = 30, units = "cm")
# ggsave("plots/data_spag_xp11_french.tex", width = 50, height = 30, units = "cm")

