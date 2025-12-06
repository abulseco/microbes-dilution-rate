# Figure 2 in Meg/Ashley SES Manuscript
# Environmental parameters from chemostats (pH, CO2, O2, Dissolved O2)

# SET UP ENVIRONMENT----
# Load necessary libraries
library("ggplot2"); library(patchwork); library(lme4); library(emmeans)
library(sjstats); library(lmerTest); library(MuMIn)

# Set figure themes
# This is a slightly different theme function from others 
pretty.theme <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=14, color = "black", angle = 0, hjust = 1),
          axis.text.y=element_text(size=14, color = "black"),
          axis.title.x=element_text(size=20, color = "black"),             
          axis.title.y=element_text(size=20, color = "black"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20),
          legend.text = element_text(size=12, face="italic"),          
          legend.title = element_blank(),                              
          legend.position="none")
}

theme.exp1 <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=18, vjust=1, hjust=1, color = "black"),
          axis.text.y=element_text(size=18, color = "black"),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.title = element_text(size=20, vjust=1, hjust=0.5),
          legend.text = element_text(size=12, face="italic"),          
          legend.title = element_blank(), 
          axis.title = element_blank())
}

# Read in the data (dissolved O2 had a different output)
monitor_var <- read.csv("input_files/monitor_param_LONG.csv", header = T)
str(monitor_var)
dissolved_oxy <- read.csv("input_files/dissolved_oxygen.csv", header = T)

##

# Plots----
# pH Plot
ph_plot <- ggplot(monitor_var, group = chemostat_ID, aes(x = time, y = ph)) +
  annotate("rect", fill = "#2B5B6C", alpha = 0.4, 
           xmin = 0, xmax = 15.71,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#E34F33", alpha = 0.4, 
           xmin = 15.71, xmax = 20.79,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FFC87E", alpha = 0.4, 
           xmin = 20.79, xmax = 25,
           ymin = -Inf, ymax = Inf) +
  # geom_vline(xintercept = 15.71, color = "black", linetype = "dashed") +
  # geom_vline(xintercept = 20.79, color = "black", linetype = "dashed") +
  # scale_fill_manual(values = c("#3399CC", "#003399", linetype = "dashed")) +
  # scale_color_manual(values = c("black", "gray45", linetype = "dashed")) +
  # facet_wrap(chemostat_ID~.) +
  geom_line(size = 0.75, aes(linetype = chemostat_ID)) +
  pretty.theme() +
  theme(strip.background = element_blank(), # Overriding some of the original theme
        strip.text.x = element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        legend.position = "none") 
ph_plot

## 

# CO2 Plot
CO2_plot <- ggplot(monitor_var, group = chemostat_ID, aes(x = time, y = carbon_dio_perc)) +
  annotate("rect", fill = "#2B5B6C", alpha = 0.4, 
           xmin = 0, xmax = 15.71,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#E34F33", alpha = 0.4, 
           xmin = 15.71, xmax = 20.79,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FFC87E", alpha = 0.4, 
           xmin = 20.79, xmax = 25,
           ymin = -Inf, ymax = Inf) +
  # geom_vline(xintercept = 15.71, color = "black", linetype = "dashed") +
  # geom_vline(xintercept = 20.79, color = "black", linetype = "dashed") +
  # scale_fill_manual(values = c("#3399CC", "#003399", linetype = "dashed")) +
  # scale_color_manual(values = c("black", "gray45", linetype = "dashed")) +
  # facet_wrap(chemostat_ID~.) +
  geom_line(size = 0.75, aes(linetype = chemostat_ID)) +
  pretty.theme() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")  
CO2_plot

##

# O2 Plot
O2_plot <- ggplot(monitor_var, group = chemostat_ID, aes(x = time, y = oxygen_perc)) +
  annotate("rect", fill = "#2B5B6C", alpha = 0.4, 
           xmin = 0, xmax = 15.71,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#E34F33", alpha = 0.4, 
           xmin = 15.71, xmax = 20.79,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FFC87E", alpha = 0.4, 
           xmin = 20.79, xmax = 25,
           ymin = -Inf, ymax = Inf) +
  # geom_vline(xintercept = 15.71, color = "black", linetype = "dashed") +
  # geom_vline(xintercept = 20.79, color = "black", linetype = "dashed") +
  # scale_fill_manual(values = c("#3399CC", "#003399", linetype = "dashed")) +
  # scale_color_manual(values = c("black", "gray45", linetype = "dashed")) +
  # facet_wrap(chemostat_ID~.) +
  geom_line(size = 0.75, aes(linetype = chemostat_ID)) +
  pretty.theme() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none") 
O2_plot

##

# Dissolved O2 Plot
diss_O2_plot <- ggplot(dissolved_oxy, group = chemostat_ID, aes(x = time, y = diss_o2)) +
  annotate("rect", fill = "#2B5B6C", alpha = 0.4, 
           xmin = 0, xmax = 15.71,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#E34F33", alpha = 0.4, 
           xmin = 15.71, xmax = 20.79,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FFC87E", alpha = 0.4, 
           xmin = 20.79, xmax = 25,
           ymin = -Inf, ymax = Inf) +
  # geom_vline(xintercept = 15.71, color = "black", linetype = "dashed") +
  # geom_vline(xintercept = 20.79, color = "black", linetype = "dashed") +
  # scale_fill_manual(values = c("#3399CC", "#003399", linetype = "dashed")) +
  scale_color_manual(values = c("black", "gray45", linetype = "dashed")) +
  # facet_wrap(chemostat_ID~.) +
  geom_line(size = 0.75, aes(linetype = chemostat_ID)) +
  pretty.theme() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none") 
diss_O2_plot

# Then using patchwork, connect all the plots
# After exporting to PDF, use InkScape to add in timeline along the top
# axes labels along the left of each plot, and legend for MC1 and MC2.

pdf("Fig2.pdf")
CO2_plot + O2_plot + diss_O2_plot + ph_plot + plot_layout(ncol = 1)
dev.off()

##

# Statistics----
monitor_var$chemostat_ID <- factor(monitor_var$chemostat_ID, levels = c("MC1", "MC2"))
monitor_var$dilution_rate <- factor(monitor_var$dilution_rate, levels = c("A", "B", "C"))
summary(monitor_var)
str(monitor_var)

dissolved_oxy$chemostat_ID <- factor(dissolved_oxy$chemostat_ID, levels = c("MC1", "MC2"))
dissolved_oxy$dilution_rate <- factor(dissolved_oxy$dilution_rate, levels = c("A", "B", "C"))
summary(dissolved_oxy)

# CO2 percent ----
CO2_model_GLMM1 <- lmer(carbon_dio_perc ~ dilution_rate * time + (1 | chemostat_ID), data = monitor_var)
CO2_model_GLMM2 <- lmer(carbon_dio_perc ~ dilution_rate + time + (1 | chemostat_ID), data = monitor_var)
CO2_model_GLMM3 <- lmer(carbon_dio_perc ~ time + (1 | chemostat_ID), data = monitor_var)
CO2_model_GLMM4 <- lmer(carbon_dio_perc ~ dilution_rate * time + (1 + time | chemostat_ID), data = monitor_var)
CO2_model_GLMM5 <- lmer(carbon_dio_perc ~ dilution_rate + time + (1 + time | chemostat_ID), data = monitor_var)
CO2_model_GLMM6 <- lmer(carbon_dio_perc ~ time + (1 + time | chemostat_ID), data = monitor_var)
CO2_model_null1 <- lmer(carbon_dio_perc ~ 1 + (1 | chemostat_ID), data = monitor_var)
CO2_model_null2 <- lmer(carbon_dio_perc ~ 1 + (1 + time | chemostat_ID), data = monitor_var)

# Excluding models with singularity issues
AICc(CO2_model_GLMM1, CO2_model_GLMM2, CO2_model_GLMM3, CO2_model_null1)
model.sel(CO2_model_GLMM1, CO2_model_GLMM2, CO2_model_GLMM3, CO2_model_null1)
model.sel(CO2_model_GLMM1, CO2_model_GLMM2, CO2_model_GLMM3, CO2_model_null1, CO2_model_null2)

summary(CO2_model_GLMM1)
anova(CO2_model_GLMM1)
r.squaredGLMM(CO2_model_GLMM1)      
effectsize::eta_squared(CO2_model_GLMM1, partial = TRUE)
emmeans(CO2_model_GLMM1, pairwise~dilution_rate | time)

# Percent Oxygen----
O2_model_GLMM1 <- lmer(oxygen_perc ~ dilution_rate * time + (1 | chemostat_ID), data = monitor_var)
O2_model_GLMM2 <- lmer(oxygen_perc ~ dilution_rate + time + (1 | chemostat_ID), data = monitor_var)
O2_model_GLMM3 <- lmer(oxygen_perc ~ time + (1 | chemostat_ID), data = monitor_var)
O2_model_GLMM4 <- lmer(oxygen_perc ~ dilution_rate * time + (1 + time | chemostat_ID), data = monitor_var)
O2_model_GLMM5 <- lmer(oxygen_perc ~ dilution_rate + time + (1 + time | chemostat_ID), data = monitor_var)
O2_model_GLMM6 <- lmer(oxygen_perc ~ time + (1 + time | chemostat_ID), data = monitor_var)
O2_model_null1 <- lmer(oxygen_perc ~ 1 + (1 | chemostat_ID), data = monitor_var)
O2_model_null2 <- lmer(oxygen_perc ~ 1 + (1 + time | chemostat_ID), data = monitor_var)

# Excluding models with singularity issues
# Models 4, 5, 6, null2
AICc(O2_model_GLMM1, O2_model_GLMM2, O2_model_GLMM3, O2_model_null1)
model.sel(O2_model_GLMM1, O2_model_GLMM2, O2_model_GLMM3, O2_model_null1)

summary(O2_model_GLMM1)
anova(O2_model_GLMM1)
r.squaredGLMM(O2_model_GLMM1)      
effectsize::eta_squared(O2_model_GLMM1, partial = TRUE)
emmeans(O2_model_GLMM1, pairwise~dilution_rate | time)

# Dissolved Oxygen----
dissO2_model_GLMM1 <- lmer(diss_o2 ~ dilution_rate * time + (1 | chemostat_ID), data = dissolved_oxy)
dissO2_model_GLMM2 <- lmer(diss_o2 ~ dilution_rate + time + (1 | chemostat_ID), data = dissolved_oxy)
dissO2_model_GLMM3 <- lmer(diss_o2 ~ time + (1 | chemostat_ID), data = dissolved_oxy)
dissO2_model_GLMM4 <- lmer(diss_o2 ~ dilution_rate * time + (1 + time | chemostat_ID), data = dissolved_oxy)
dissO2_model_GLMM5 <- lmer(diss_o2 ~ dilution_rate + time + (1 + time | chemostat_ID), data = dissolved_oxy)
dissO2_model_GLMM6 <- lmer(diss_o2 ~ time + (1 + time | chemostat_ID), data = dissolved_oxy)
dissO2_model_null1 <- lmer(diss_o2 ~ 1 + (1 | chemostat_ID), data = dissolved_oxy)
dissO2_model_null2 <- lmer(diss_o2 ~ 1 + (1  + time | chemostat_ID), data = dissolved_oxy)

                                                      
# Excluding models with singularity issues
# Models 4, 5, 6, null2
AICc(dissO2_model_GLMM1, dissO2_model_GLMM2, dissO2_model_GLMM3, dissO2_model_null1)
model.sel(dissO2_model_GLMM1, dissO2_model_GLMM2, dissO2_model_GLMM3, dissO2_model_null1)

summary(dissO2_model_GLMM1)
anova(dissO2_model_GLMM1)
r.squaredGLMM(dissO2_model_GLMM1)      
effectsize::eta_squared(dissO2_model_GLMM1, partial = TRUE)
emmeans(dissO2_model_GLMM1, pairwise~dilution_rate | time)

# pH ----
ph_model_GLMM1 <- lmer(ph ~ dilution_rate * time + (1 | chemostat_ID), data = monitor_var)
ph_model_GLMM2 <- lmer(ph ~ dilution_rate + time + (1 | chemostat_ID), data = monitor_var)
ph_model_GLMM3 <- lmer(ph ~ time + (1 | chemostat_ID), data = monitor_var)
ph_model_GLMM4 <- lmer(ph ~ dilution_rate * time + (1 + time | chemostat_ID), data = monitor_var)
ph_model_GLMM5 <- lmer(ph ~ dilution_rate + time + (1 + time | chemostat_ID), data = monitor_var)
ph_model_GLMM6 <- lmer(ph ~ time + (1 + time | chemostat_ID), data = monitor_var)
ph_model_null1 <- lmer(ph ~ 1 + (1 | chemostat_ID), data = monitor_var)
ph_model_null2 <- lmer(ph ~ 1 + (1 + time | chemostat_ID), data = monitor_var)

# Excluding models with singularity issues
# Models 4, 5, 6, null2 
AICc(ph_model_GLMM1, ph_model_GLMM2, ph_model_GLMM3, ph_model_null1)
model.sel(ph_model_GLMM1, ph_model_GLMM2, ph_model_GLMM3, ph_model_null1)

summary(ph_model_GLMM1)
anova(ph_model_GLMM1)
r.squaredGLMM(ph_model_GLMM1)      
effectsize::eta_squared(ph_model_GLMM1, partial = TRUE)
emmeans(ph_model_GLMM1, pairwise~dilution_rate | time)


