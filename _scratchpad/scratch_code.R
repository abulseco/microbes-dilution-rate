# Scratchpad used for prelim code

# figure combo----
# combine fig2 & 3
# test to see if we can combine into one plot cleanly
# and testing combined & greyscale as option 3

pretty.theme <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=12, color = "black", angle = 0),
          axis.text.y=element_text(size=12, color = "black"),
          axis.title.x=element_text(size=12, color = "black"),             
          axis.title.y=element_text(size=12, color = "black"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20))
}

## fig 2----
### CO2----
CO2_plot <- ggplot(monitor_var, group = chemostat_ID, aes(x = time, y = carbon_dio_perc)) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID), linewidth = 1.0) +
  geom_vline(xintercept = c(15.71,20.79), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("#2B5B6C", "#E34F33","#FFC87E")) +
  pretty.theme() +
  labs (x = NULL, y = expression(CO[2] ~ "(%)"), linetype = "Chemostat ID", color = "Dilution Rate") +
  theme(axis.text.x = element_blank())
CO2_plot

# greyscale
CO2_plotg <- ggplot(monitor_var, group = chemostat_ID, aes(x = time, y = carbon_dio_perc)) +
  geom_line(aes(linetype = chemostat_ID), color = "black", linewidth = 1.0) +
  geom_vline(xintercept = c(15.71,20.79), color = "darkgrey", linetype = "dashed") +
  pretty.theme() +
  labs(x = NULL, y = expression(CO[2] ~ "(%)"), linetype = "Chemostat ID", color = "Dilution Rate") +  theme(axis.text.x = element_blank())
CO2_plotg

### O2----
O2_plot <- ggplot(monitor_var, group = chemostat_ID, aes(x = time, y = oxygen_perc)) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID), linewidth = 1.0) +
  geom_vline(xintercept = c(15.71,20.79), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("#2B5B6C", "#E34F33","#FFC87E")) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +  # 1 decimal place only
  pretty.theme() +
  labs(x = NULL, y = expression(O[2] ~ "(%)"), linetype = "Chemostat ID", color = "Dilution Rate") +
  theme(axis.text.x = element_blank())
O2_plot

# greyscale
O2_plotg <- ggplot(monitor_var, group = chemostat_ID, aes(x = time, y = oxygen_perc)) +
  geom_line(aes(linetype = chemostat_ID), color = "black", linewidth = 1.0) +
  geom_vline(xintercept = c(15.71,20.79), color = "darkgrey", linetype = "dashed") +
  pretty.theme() +
  labs(x = NULL, y = expression(O[2] ~ "(%)"), linetype = "Chemostat ID", color = "Dilution Rate") +
  theme(axis.text.x = element_blank())
O2_plotg

### Diss O2----
diss_O2_plot <- ggplot(dissolved_oxy, group = chemostat_ID, aes(x = time, y = diss_o2)) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID), linewidth = 1.0) +
  geom_vline(xintercept = c(15.71,20.79), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("#2B5B6C", "#E34F33","#FFC87E")) +
  pretty.theme() +
  labs (x = NULL, y = expression(O[2] ~ "(mg" ~ L^{-1} * ")"), linetype = "Chemostat ID", color = "Dilution Rate") +
  theme(axis.text.x = element_blank())
diss_O2_plot

# greyscale
diss_O2_plotg <- ggplot(dissolved_oxy, group = chemostat_ID, aes(x = time, y = diss_o2)) +
  geom_line(aes(linetype = chemostat_ID), color = "black", linewidth = 1.0) +
  geom_vline(xintercept = c(15.71,20.79), color = "darkgrey", linetype = "dashed") +
  pretty.theme() +
  labs (x = NULL, y = expression(O[2] ~ "(mg" ~ L^{-1} * ")"), linetype = "Chemostat ID", color = "Dilution Rate") +
  theme(axis.text.x = element_blank())
diss_O2_plotg

### pH----
ph_plot <- ggplot(monitor_var, group = chemostat_ID, aes(x = time, y = ph)) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID), linewidth = 1.0) +
  geom_vline(xintercept = c(15.71,20.79), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("#2B5B6C", "#E34F33","#FFC87E")) +
  pretty.theme() +
  labs (x = NULL, y = "pH", linetype = "Chemostat ID", color = "Dilution Rate") +
  theme(axis.text.x = element_blank())
ph_plot

# greyscale
ph_plotg <- ggplot(monitor_var, group = chemostat_ID, aes(x = time, y = ph)) +
  geom_line(aes(linetype = chemostat_ID), color = "black", linewidth = 1.0) +
  geom_vline(xintercept = c(15.71,20.79), color = "darkgrey", linetype = "dashed") +
  pretty.theme() +
  labs (x = NULL, y = "pH", linetype = "Chemostat ID", color = "Dilution Rate") +
  theme(axis.text.x = element_blank())
ph_plotg

## combine----
fig2 <- (CO2_plot / O2_plot | diss_O2_plot / ph_plot) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        legend.justification = "center")
fig2

fig2g <- (CO2_plotg / O2_plotg | diss_O2_plotg / ph_plotg) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        legend.justification = "center")
fig2g

## fig 3----
### nitrate----
nitrate_plot <- ggplot(nuts_data, aes(x = days_after, y = nitrate_conc)) +
  geom_point(aes(shape = chemostat_ID, color = dilution_rate), size = 2) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID)) +
  geom_vline(xintercept = c(15.71,20.79), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("#2B5B6C", "#E34F33","#FFC87E")) +
  pretty.theme() +
  labs (x = NULL, y = expression(NO[3]^"-" ~ "(" * mu * "mol L"^{-1} * ")"), shape = "Chemostat ID", color = "Dilution Rate", linetype = NULL) +
  theme(axis.text.x = element_blank())
nitrate_plot

# greyscale
nitrate_plotg <- ggplot(nuts_data, aes(x = days_after, y = nitrate_conc)) +
  geom_point(aes(shape = chemostat_ID), color = "black" , size = 2) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID), color = "black") +
  geom_vline(xintercept = c(15.71,20.79), color = "darkgrey", linetype = "dashed") +
  pretty.theme() +
  labs(x = NULL, y = expression(NO[3]^"-" ~ "(" * mu * "mol L"^{-1} * ")"), shape = "Chemostat ID", color = "Dilution Rate", linetype = NULL) +
  theme(axis.text.x = element_blank())
nitrate_plotg

### ammonium----
ammonium_plot <- ggplot(nuts_data, aes(x = days_after, y = ammonium_con)) +
  geom_point(aes(shape = chemostat_ID, color = dilution_rate), size = 2) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID)) +
  geom_vline(xintercept = c(15.71,20.79), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("#2B5B6C", "#E34F33","#FFC87E")) +
  pretty.theme() +
  labs(x = NULL, y = expression(NH[4]^"+" ~ "(" * mu * "mol L"^{-1} * ")"), shape = "Chemostat ID", color = "Dilution Rate", linetype = NULL) +
  theme(axis.text.x = element_blank())
ammonium_plot

# greyscale
ammonium_plotg <- ggplot(nuts_data, aes(x = days_after, y = ammonium_con)) +
  geom_point(aes(shape = chemostat_ID), color = "black" , size = 2) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID), color = "black") +
  geom_vline(xintercept = c(15.71,20.79), color = "darkgrey", linetype = "dashed") +
  pretty.theme() +
  labs(x = NULL, y = expression(NH[4]^"+" ~ "(" * mu * "mol L"^{-1} * ")"), shape = "Chemostat ID", color = "Dilution Rate", linetype = NULL) +
  theme(axis.text.x = element_blank())
ammonium_plotg

### phosphate----
phosphate_plot <- ggplot(nuts_data, aes(x = days_after, y = phos_conc)) +
  geom_point(aes(shape = chemostat_ID, color = dilution_rate), size = 2) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID)) +
  geom_vline(xintercept = c(15.71,20.79), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("#2B5B6C", "#E34F33","#FFC87E")) +
  pretty.theme() +
  labs(x = NULL, y = expression(PO[4]^"3-" ~ "( " * mu * "mol L"^{-1} * " )"), shape = "Chemostat ID", color = "Dilution Rate", linetype = NULL) +
  theme(axis.text.x = element_blank())
phosphate_plot

# greyscale
phosphate_plotg <- ggplot(nuts_data, aes(x = days_after, y = phos_conc)) +
  geom_point(aes(shape = chemostat_ID), color = "black" , size = 2) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID), color = "black") +
  geom_vline(xintercept = c(15.71,20.79), color = "darkgrey", linetype = "dashed") +
  pretty.theme() +
  labs(x = NULL, y = expression(PO[4]^"3-" ~ "( " * mu * "mol L"^{-1} * " )"), shape = "Chemostat ID", color = "Dilution Rate", linetype = NULL) +
  theme(axis.text.x = element_blank())
phosphate_plotg


### DOC----
DOC_plot <- ggplot(nuts_data, aes(x = days_after, y = doc_conc_mmol)) +
  geom_point(aes(shape = chemostat_ID, color = dilution_rate), size = 2) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID)) +
  geom_vline(xintercept = c(15.71,20.79), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("#2B5B6C", "#E34F33","#FFC87E")) +
  pretty.theme() +
  labs(x = "Days after start", y = expression("DOC (mmol L"^"-1" * ")"), shape = "Chemostat ID", color = "Dilution Rate", linetype = NULL) 
DOC_plot

# greyscale
DOC_plotg <- ggplot(nuts_data, aes(x = days_after, y = doc_conc_mmol)) +
  geom_point(aes(shape = chemostat_ID), color = "black", size = 2) +
  geom_line(aes(color = dilution_rate, linetype = chemostat_ID), color = "black") +
  geom_vline(xintercept = c(15.71,20.79), color = "darkgrey", linetype = "dashed") +
  pretty.theme() +
  labs(x = "Days after start", y = expression("DOC (mmol L"^"-1" * ")"), shape = "Chemostat ID", color = "Dilution Rate", linetype = NULL) 
DOC_plotg

## Combine plots----
fig3 <- (nitrate_plot / ammonium_plot | phosphate_plot / DOC_plot) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        legend.justification = "center")
fig3

fig3g <- (nitrate_plotg / ammonium_plotg | phosphate_plotg / DOC_plotg) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        legend.justification = "center")
fig3g

combine_2_3 <- fig2 / fig3
combine_2_3

## combine grey plots----
combine_2_3g <- fig2g / fig3g
combine_2_3g
