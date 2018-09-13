# Tue Aug 22 01:42:17 2017 ------------------------------
# Script data extraction and analysis for RWAscore/evaluative conditionning study A.G. Bret december 2016
# Scipt by B. Beffara & A.G. Bret
#################################################################################################################

##########################################################
##########################################################
# Part 3 = Plot Data
##########################################################
##########################################################

# Loading packages needed (and installing if necessary) for this part (make sure that jags is installed on your computer)
if (!"pacman" %in% installed.packages()[,"Package"] ) install.packages("pacman" )
pacman::p_load(data.table, grid, ggplot2, ggridges, viridis)

############################
####### FOREST PLOTS #######
############################

#### Forest plot for interaction ####

datIDA <- data.frame( value = c(posterior_samples(XP01_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP02_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP03_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP04_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP05_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP06_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP07_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP08_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP09_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP11_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(IDA_resp, "b")$"b_usvalence:RWAscore"
),
type = c(rep("Experiment 1",4000),rep("Experiment 2",4000),
         rep("Experiment 3",4000),rep("Experiment 4",4000),
         rep("Experiment 5",4000),rep("Experiment 6",4000),
         rep("Experiment 7",4000),rep("Experiment 8",4000),
         rep("Experiment 9",4000),rep("Experiment 10",4000),
         rep("Integrative data analysis",4000)
         )
)

setDT(datIDA)[, value.ave := mean(value), by = type]
setDT(datIDA)[, value.low := hdi(value)["lower"], by = type]
setDT(datIDA)[, value.up := hdi(value)["upper"], by = type]

datIDA$type <- factor(datIDA$type,
                       levels = c("Experiment 1", "Experiment 2",
                                  "Experiment 3", "Experiment 4",
                                  "Experiment 5", "Experiment 6",
                                  "Experiment 7", "Experiment 8",
                                  "Experiment 9", "Experiment 10",
                                  "Integrative data analysis"))

datIDA$sep <- ifelse(datIDA$type == "Integrative data analysis", "1", "2")

datIDA$select <- rep(1:4000,4)


ggplot(datIDA, aes(x = value))+
geom_rect(aes(xmin=-0.1, xmax=0.1, ymin=0, ymax=Inf, fill = "gray20")) +
geom_density(color="gray60", fill="gray60")+
facet_grid(type ~ ., switch="y")+
geom_hline(yintercept = 0, colour = "white", size = 0.5)+
labs(x = expression(beta[RWAscore%*%"Valence Block 1"]))+
theme(axis.title.x = element_text(size = rel(1.5), angle = 00))+
geom_segment(data=subset(datIDA, select == 1),
             aes(x = value.low, xend = value.up, y = 0,
                 yend = 0, group = type), colour = "black", size = 0.4)+
geom_point(data=subset(datIDA, select == 1),
           aes(x = value.ave, y = 0, group = type),
           colour = "black", size = 2, shape=19)+
scale_color_manual(values=c("gray20", "white"))+
theme(axis.title.y=element_blank(),
      strip.background = element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.line.y=element_blank(),
      panel.background = element_blank(),
      legend.position="none",
      panel.spacing = unit(0, "lines"),
      strip.text.y = element_text(angle = 180, vjust = 0.2))+
geom_text(x=-1.15, y=1, data=subset(datIDA, select == 1),
          aes(label=paste(round(value.ave,2),"[",
                          round(value.low,2),", ",round(value.up,2),"]")), size = 2.5)+
geom_segment(aes(x = min(value), xend = max(value), y = 10.5,
                 yend = 10.5, colour = sep), size = 0.2)+
geom_vline(xintercept = 0, colour="gray40", linetype="dashed")


library(cowplot)
ggdraw(p1)+draw_plot(p2 + theme(legend.justification = "left"), 0.44, 0.025, 1, 1)
