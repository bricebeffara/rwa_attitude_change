#------------------------------------------------------------------------------------
# ...and combine plots together
#------------------------------------------------------------------------------------

# Combine plot
marg_all <- ggarrange(marg_plot_cc1neg,
                      marg_plot_cctneg,
                      marg_plot_tccneg,
                      marg_plot_cc2neg,
                      marg_plot_cc1pos,
                      marg_plot_cctpos,
                      marg_plot_tccpos,
                      marg_plot_cc2pos,
                      ncol = 2, nrow = 4)

# uncomment to display and save plot
# marg_all
# ggsave("plots/marg_xp08_french.jpg", width = 50, height = 60, units = "cm")
# ggsave("plots/marg_xp08_french.pdf", width = 50, height = 60, units = "cm")
# ggsave("plots/marg_xp08_french.tex", width = 50, height = 60, units = "cm")
