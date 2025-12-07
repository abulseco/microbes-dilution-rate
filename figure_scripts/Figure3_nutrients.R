# Figure 3 in Meg/Ashley SES Manuscript
# Environmental parameters from chemostats

# SETUP ENVIRONMENT----
# Load necessary libraries
library("ggplot2"); library(patchwork); library(lme4); library(emmeans)
library(sjstats); library(lmerTest); library(MuMIn)

pretty.theme <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=14, color = "black", angle = 0),
          axis.text.y=element_text(size=14, color = "black"),
          axis.title.x=element_text(size=14, color = "black"),             
          axis.title.y=element_text(size=14, color = "black"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20))
}

pretty.theme.noaxes <- function(){
  theme_bw() +
    theme(axis.text.x=element_text(size=14, color = "black", angle = 0),
          axis.text.y=element_text(size=14, color = "black"),
          axis.title.x=element_blank(),             
          axis.title.y=element_blank(),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
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

## Import data----
nuts_data <- read.csv("input_files/new_nutrients.csv", header = TRUE)

# Plots----
## nitrate----
nitrate_plot <- ggplot(nuts_data, aes(x = days_after, y = nitrate_conc)) +
  geom_point(aes(shape = chemostat_ID, color = dilution_rate), size = 2) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID)) +
  geom_vline(xintercept = c(15.71,20.79), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("#2B5B6C", "#E34F33","#FFC87E")) +
  pretty.theme() +
  labs (x = NULL, y = NULL, shape = "Chemostat ID", color = "Dilution Rate", linetype = NULL) +
  theme(axis.text.x = element_blank())
nitrate_plot

## ammonium----
ammonium_plot <- ggplot(nuts_data, aes(x = days_after, y = ammonium_con)) +
  geom_point(aes(shape = chemostat_ID, color = dilution_rate), size = 2) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID)) +
  geom_vline(xintercept = c(15.71,20.79), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("#2B5B6C", "#E34F33","#FFC87E")) +
  pretty.theme() +
  labs (x = NULL, y = NULL, shape = "Chemostat ID", color = "Dilution Rate", linetype = NULL) +
  theme(axis.text.x = element_blank())
ammonium_plot

## phosphate----
phosphate_plot <- ggplot(nuts_data, aes(x = days_after, y = phos_conc)) +
  geom_point(aes(shape = chemostat_ID, color = dilution_rate), size = 2) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID)) +
  geom_vline(xintercept = c(15.71,20.79), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("#2B5B6C", "#E34F33","#FFC87E")) +
  pretty.theme() +
  labs (x = NULL, y = NULL, shape = "Chemostat ID", color = "Dilution Rate", linetype = NULL) +
  theme(axis.text.x = element_blank())
phosphate_plot

## DOC----
DOC_plot <- ggplot(nuts_data, aes(x = days_after, y = doc_conc_mmol)) +
  geom_point(aes(shape = chemostat_ID, color = dilution_rate), size = 2) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID)) +
  geom_vline(xintercept = c(15.71,20.79), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("#2B5B6C", "#E34F33","#FFC87E")) +
  pretty.theme() +
  labs (x = "Days after start", y = NULL, shape = "Chemostat ID", color = "Dilution Rate", linetype = NULL) 
DOC_plot

## Combine plots----
fig3 <- (nitrate_plot / ammonium_plot / phosphate_plot / DOC_plot) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        legend.justification = "center")
fig3

# Statistics----
test <- anova_test(nuts_data, dv = "nitrate_conc", wid = "chemostat_ID", between = "dilution_rate", within = "days_after")
View(nuts_data)

# Nitrate Concentration ----
no3_model_GLMM1 <- lmer(nitrate_conc ~ dilution_rate * days_after + (1 | chemostat_ID), data = nuts_data)
no3_model_GLMM2 <- lmer(nitrate_conc ~ dilution_rate + days_after + (1 | chemostat_ID), data = nuts_data)
no3_model_GLMM3 <- lmer(nitrate_conc ~ days_after + (1 | chemostat_ID), data = nuts_data)
no3_model_GLMM4 <- lmer(nitrate_conc ~ dilution_rate * days_after + (days_after | chemostat_ID), data = nuts_data)
no3_model_GLMM5 <- lmer(nitrate_conc ~ dilution_rate + days_after + (days_after | chemostat_ID), data = nuts_data)
no3_model_GLMM6 <- lmer(nitrate_conc ~ days_after + (days_after | chemostat_ID), data = nuts_data)
no3_model_null1 <- lmer(nitrate_conc ~ 1 + (1 | chemostat_ID), data = nuts_data)
no3_model_null2 <- lmer(nitrate_conc ~ 1 + (days_after | chemostat_ID), data = nuts_data)

# All models have singularity issues, still electing this over fixed effects model
# delta is <2 so no difference between models 1 and 2
AICc(no3_model_GLMM1, no3_model_GLMM2, no3_model_GLMM3, no3_model_GLMM4, 
     no3_model_GLMM5, no3_model_GLMM6, no3_model_null1, no3_model_null2)

model.sel(no3_model_GLMM1, no3_model_GLMM2, no3_model_GLMM3, no3_model_GLMM4, 
          no3_model_GLMM5, no3_model_GLMM6, no3_model_null1, no3_model_null2)

summary(no3_model_GLMM2)
anova(no3_model_GLMM2)
r.squaredGLMM(no3_model_GLMM2)      
effectsize::eta_squared(no3_model_GLMM2, partial = TRUE)
emmeans(no3_model_GLMM2, pairwise~dilution_rate |  days_after)

# Ammonium Concentration ----
amm_model_GLMM1 <- lmer(ammonium_con ~ dilution_rate * days_after + (1 | chemostat_ID), data = nuts_data)
amm_model_GLMM2 <- lmer(ammonium_con ~ dilution_rate + days_after + (1 | chemostat_ID), data = nuts_data)
amm_model_GLMM3 <- lmer(ammonium_con ~ days_after + (1 | chemostat_ID), data = nuts_data)
amm_model_GLMM4 <- lmer(ammonium_con ~ days_after * dilution_rate + (1 + days_after | chemostat_ID), data = nuts_data)
amm_model_GLMM5 <- lmer(ammonium_con ~ days_after + dilution_rate + (1 + days_after | chemostat_ID), data = nuts_data)
amm_model_GLMM6 <- lmer(ammonium_con ~ days_after + (1 + days_after | chemostat_ID), data = nuts_data)
amm_model_null1 <- lmer(ammonium_con ~ 1 + (1 | chemostat_ID), data = nuts_data)
amm_model_null2 <- lmer(ammonium_con ~ 1 + (dilution_rate | chemostat_ID), data = nuts_data)

# Excluding models with singularity
# GLMMs 4, 5, 6, null1, null2
AICc(amm_model_GLMM1, amm_model_GLMM2, amm_model_GLMM3, amm_model_null1)
model.sel(amm_model_GLMM1, amm_model_GLMM2, amm_model_GLMM3, amm_model_null1)

summary(amm_model_GLMM2)
anova(amm_model_GLMM2)
r.squaredGLMM(amm_model_GLMM2)      
effectsize::eta_squared(amm_model_GLMM2, partial = TRUE)
emmeans(amm_model_GLMM2, pairwise~dilution_rate |  days_after)

# Phosphate Concentration----
phos_model_GLMM1 <- lmer(phos_conc ~ dilution_rate * days_after + (1 | chemostat_ID), data = nuts_data)
phos_model_GLMM2 <- lmer(phos_conc ~ dilution_rate + days_after + (1 | chemostat_ID), data = nuts_data)
phos_model_GLMM3 <- lmer(phos_conc ~ days_after + (1 | chemostat_ID), data = nuts_data)
phos_model_GLMM4 <- lmer(phos_conc ~ dilution_rate * days_after + (1 + days_after | chemostat_ID), data = nuts_data)
phos_model_GLMM5 <- lmer(phos_conc ~ days_after + dilution_rate + (1 + days_after | chemostat_ID), data = nuts_data)
phos_model_GLMM6 <- lmer(phos_conc ~ days_after + (1 + days_after | chemostat_ID), data = nuts_data)
phos_model_null1 <- lmer(phos_conc ~ 1 + (1 | chemostat_ID), data = nuts_data)
phos_model_null2 <- lmer(phos_conc ~ 1 + (1 + days_after | chemostat_ID), data = nuts_data)

# Excluding models with singularity
# GLMMs 4, 5, 6, null2
AICc(phos_model_GLMM1, phos_model_GLMM2, phos_model_GLMM3, phos_model_null1)
model.sel(phos_model_GLMM1, phos_model_GLMM2, phos_model_GLMM3, phos_model_null1)

summary( phos_model_GLMM2)
anova( phos_model_GLMM2)
r.squaredGLMM( phos_model_GLMM2)      
effectsize::eta_squared( phos_model_GLMM2, partial = TRUE)
emmeans( phos_model_GLMM2, pairwise~dilution_rate | days_after)

# DOC Concentration----
doc_model_GLMM1 <- lmer(doc_conc ~ dilution_rate * days_after + (1 | chemostat_ID), data = nuts_data)
doc_model_GLMM2 <- lmer(doc_conc ~ dilution_rate + days_after + (1 | chemostat_ID), data = nuts_data)
doc_model_GLMM3 <- lmer(doc_conc ~ days_after + (1 | chemostat_ID), data = nuts_data)
doc_model_GLMM4 <- lmer(doc_conc ~ dilution_rate * days_after + (1 + days_after| chemostat_ID), data = nuts_data)
doc_model_GLMM5 <- lmer(doc_conc ~ dilution_rate + days_after + (1 + days_after | chemostat_ID), data = nuts_data)
doc_model_GLMM6 <- lmer(doc_conc ~ days_after + (1 + days_after | chemostat_ID), data = nuts_data)
doc_model_null1 <- lmer(doc_conc ~ 1 + (1 | chemostat_ID), data = nuts_data)
doc_model_null2 <- lmer(doc_conc ~ 1 + (1 + days_after | chemostat_ID), data = nuts_data)

# Excluding models with singularity
# 2, 3, 4, 5, 6, null1, null2

AICc(doc_model_GLMM1, doc_model_GLMM2, doc_model_GLMM3, doc_model_null1)
model.sel(doc_model_GLMM1, doc_model_GLMM2, doc_model_GLMM3, doc_model_null1)
# Model 1 is the best fit

summary(doc_model_GLMM1)
anova(doc_model_GLMM1)
r.squaredGLMM(doc_model_GLMM1)      
effectsize::eta_squared(doc_model_GLMM1, partial = TRUE)
emmeans(doc_model_GLMM1, pairwise~dilution_rate)
