# Combine plots
marg_spag_4 <- ggarrange(marg_plot_ctone,
                         marg_plot_cttone,
                         marg_plot_tctone,
                         marg_plot_cttwo,
                                   ncol = 2, nrow = 2)


# uncomment to display and save plot
# marg_spag_4
# ggsave("plots/marg_spag_4_xp08_french.jpg", width = 50, height = 30, units = "cm")
# ggsave("plots/marg_spag_4_xp08_french.pdf", width = 50, height = 30, units = "cm")
# ggsave("plots/marg_spag_4_xp08_french.tex", width = 50, height = 30, units = "cm")