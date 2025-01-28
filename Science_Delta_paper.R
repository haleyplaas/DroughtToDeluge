setwd("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\CCRG_PurpleAir")
rm(list = ls())

library(tidyverse);library(dplyr);library(readxl);library(patchwork) 

file2 <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\Stockton\\DroughtDeluge_Bioassay_Data_calcs.xlsx"

chla_data <- read_excel(file2, sheet = "RPlot_chla")
cyano_data <- read_excel(file2, sheet = "RPlot_cyano")
nuts_data <- read_excel(file2, sheet = "Raw_data") 

year.color <- c("2023" = "gray20", "2022" = "gray80")

cyanos <- ggplot(cyano_data) + 
  geom_bar(aes(x = Treatment, y = mean, fill = as.factor(Year)), stat = "identity", position = "dodge", size = 2.5) +
  geom_errorbar(aes(x = Treatment, ymin = mean - std, ymax = mean + std, group = as.factor(Year)), 
                position = position_dodge(width = 0.9), width = 0.25, linewidth = 1) +  # Add error bars
  labs(x = "Nutrient Addition Treatment", y = "Difference in Cyanobacterial Growth (%)", fill = "Year") +
  # coord_flip() +
  theme_minimal() +
  #  scale_y_break(c(100,1000), scales = .5) 
  ylim(c(-250,600)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = 1.0, color = "grey35") +
  theme(panel.grid.major.y = element_line(color = "grey50", linewidth = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),  # Remove major x grid lines
        panel.grid.minor.x = element_blank()) + 
  scale_fill_manual(values = year.color) 

chla <- ggplot(chla_data) + 
  geom_bar(aes(x = Treatment, y = mean, fill = as.factor(Year)), stat = "identity", position = "dodge", size = 2.5) +
  geom_errorbar(aes(x = Treatment, ymin = mean - std, ymax = mean + std, group = as.factor(Year)), 
                position = position_dodge(width = 0.9), width = 0.25, linewidth = 1) +  # Add error bars
  labs(x = "Nutrient Addition Treatment", y = "Difference in Cyanobacterial Growth (%)", fill = "Year") +
  # coord_flip() +
  theme_minimal() +
  #  scale_y_break(c(100,1000), scales = .5) 
   ylim(c(-250,600)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = 1.0, color = "grey35") +
  theme(panel.grid.major.y = element_line(color = "grey50", linewidth = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),  # Remove major x grid lines
        panel.grid.minor.x = element_blank()) + 
  scale_fill_manual(values = year.color) + 
  theme(legend.position = "none")

chla + cyanos

# adding secondary axis for nutrient data -- I don't like this, instead I wanna do a short time series over experiment
ggplot(chla_data) + 
  geom_bar(aes(x = Treatment, y = mean, fill = as.factor(Year)), 
           stat = "identity", 
           position = "dodge", 
           size = 2.5) +
  geom_errorbar(aes(x = Treatment, ymin = mean - std, ymax = mean + std, group = as.factor(Year)),
                position = position_dodge(width = 0.9), 
                width = 0.25, 
                linewidth = 1) +  # Add error bars
  labs(x = "Nutrient Addition Treatment", 
       y = "Difference in Cyanobacterial Growth (%)", 
       fill = "Year") +
  geom_point(data = nuts_data,
             aes(x = Treatment, y = `mean`, color = Nutrient, group = as.factor(Year)), 
             size = 1.5, 
             position = position_dodge(width = 0.9)) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~ (. + 250) * (2000 / 850) - 500, 
                        name = "Change to [Nutrients] During Experiment")) +
  coord_cartesian(ylim = c(-250, 600)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = 1.0, color = "grey35") +
  theme(panel.grid.major.y = element_line(color = "grey50", linewidth = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),  # Remove major x grid lines
        panel.grid.minor.x = element_blank()) + 
  scale_fill_manual(values = year.color) 

# time series of nutrients to show assimilation/regeneration? 
# need separate plots for both 2022 and 2023
# 2022 ---------------------------------------------------------
NH4_2022 <- nuts_data %>% 
  filter(str_detect(Date, "-09-")) %>% 
  filter(Site == "Stockton") %>% 
  filter(Treatment == "NH4" | Treatment == "Control")

# Summarize data to calculate mean and standard deviation for NH4
NH4_2022_summary <- NH4_2022 %>%
  mutate(Treatment = if_else(Experimental_Timepoint == "T0" & Treatment == "Control", "NH4", Treatment)) %>% 
  group_by(Experimental_Timepoint) %>%
  summarize(
    mean_NH4 = mean(NH4, na.rm = TRUE),
    std_NH4 = sd(NH4, na.rm = TRUE),
    mean_NOx = mean(NOx, na.rm = TRUE),
    std_NOx = sd(NOx, na.rm = TRUE),
    mean_PO4 = mean(PO4, na.rm = TRUE),
    std_PO4 = sd(PO4, na.rm = TRUE),
    mean_TDN = mean(TDN, na.rm = TRUE),
    std_TDN = sd(TDN, na.rm = TRUE)) %>%
  mutate(Experimental_Timepoint = as.numeric(sub("T", "", Experimental_Timepoint))) 

# NH4+P
NH4P_2022 <- nuts_data %>% 
  filter(str_detect(Date, "-09-")) %>% 
  filter(Site == "Stockton") %>% 
  filter(Treatment == "NH4+P" | Treatment == "Control")

NH4P_2022_summary <- NH4P_2022 %>%
  mutate(Treatment = if_else(Experimental_Timepoint == "T0" & 
                             Treatment == "Control", "NH4+P", Treatment)) %>% 
  group_by(Experimental_Timepoint) %>%
  summarize(
    mean_NH4 = mean(NH4, na.rm = TRUE),
    std_NH4 = sd(NH4, na.rm = TRUE),
    mean_NOx = mean(NOx, na.rm = TRUE),
    std_NOx = sd(NOx, na.rm = TRUE),
    mean_PO4 = mean(PO4, na.rm = TRUE),
    std_PO4 = sd(PO4, na.rm = TRUE),
    mean_TDN = mean(TDN, na.rm = TRUE),
    std_TDN = sd(TDN, na.rm = TRUE)) %>%
  mutate(Experimental_Timepoint = as.numeric(sub("T", "", Experimental_Timepoint))) 

# NO3
NO3_2022 <- nuts_data %>% 
  filter(str_detect(Date, "-09-")) %>% 
  filter(Site == "Stockton") %>% 
  filter(Treatment == "NO3" | Treatment == "Control")

NO3_2022_summary <- NO3_2022 %>%
  mutate(Treatment = if_else(Experimental_Timepoint == "T0" & 
                               Treatment == "Control", "NO3", Treatment)) %>% 
  group_by(Experimental_Timepoint) %>%
  summarize(
    mean_NH4 = mean(NH4, na.rm = TRUE),
    std_NH4 = sd(NH4, na.rm = TRUE),
    mean_NOx = mean(NOx, na.rm = TRUE),
    std_NOx = sd(NOx, na.rm = TRUE),
    mean_PO4 = mean(PO4, na.rm = TRUE),
    std_PO4 = sd(PO4, na.rm = TRUE),
    mean_TDN = mean(TDN, na.rm = TRUE),
    std_TDN = sd(TDN, na.rm = TRUE)) %>%
  mutate(Experimental_Timepoint = as.numeric(sub("T", "", Experimental_Timepoint))) 

# NO3+P
NO3P_2022 <- nuts_data %>% 
  filter(str_detect(Date, "-09-")) %>% 
  filter(Site == "Stockton") %>% 
  filter(Treatment == "NO3+P" | Treatment == "Control")

NO3P_2022_summary <- NO3P_2022 %>%
  mutate(Treatment = if_else(Experimental_Timepoint == "T0" & 
                               Treatment == "Control", "NO3+P", Treatment)) %>% 
  group_by(Experimental_Timepoint) %>%
  summarize(
    mean_NH4 = mean(NH4, na.rm = TRUE),
    std_NH4 = sd(NH4, na.rm = TRUE),
    mean_NOx = mean(NOx, na.rm = TRUE),
    std_NOx = sd(NOx, na.rm = TRUE),
    mean_PO4 = mean(PO4, na.rm = TRUE),
    std_PO4 = sd(PO4, na.rm = TRUE),
    mean_TDN = mean(TDN, na.rm = TRUE),
    std_TDN = sd(TDN, na.rm = TRUE)) %>%
  mutate(Experimental_Timepoint = as.numeric(sub("T", "", Experimental_Timepoint)))

# P
P_2022 <- nuts_data %>% 
  filter(str_detect(Date, "-09-")) %>% 
  filter(Site == "Stockton") %>% 
  filter(Treatment == "P" | Treatment == "Control")

P_2022_summary <- P_2022 %>%
  mutate(Treatment = if_else(Experimental_Timepoint == "T0" & 
                               Treatment == "Control", "P", Treatment)) %>% 
  group_by(Experimental_Timepoint) %>%
  summarize(
    mean_NH4 = mean(NH4, na.rm = TRUE),
    std_NH4 = sd(NH4, na.rm = TRUE),
    mean_NOx = mean(NOx, na.rm = TRUE),
    std_NOx = sd(NOx, na.rm = TRUE),
    mean_PO4 = mean(PO4, na.rm = TRUE),
    std_PO4 = sd(PO4, na.rm = TRUE),
    mean_TDN = mean(TDN, na.rm = TRUE),
    std_TDN = sd(TDN, na.rm = TRUE)) %>%
  mutate(Experimental_Timepoint = as.numeric(sub("T", "", Experimental_Timepoint)))

library(ggbreak) 

# Plot as a time series
nut.plot <- function(df) {
ggplot(df) + 
  geom_line(aes(x = Experimental_Timepoint, y = mean_NH4), color = "#440154FF", size = 1) +
  #geom_ribbon(aes(x = Experimental_Timepoint, 
                  #ymin = mean_NH4 - std_NH4, 
                  #ymax = mean_NH4 + std_NH4), 
                  #fill = "#440154FF", alpha = 0.2) + 
  geom_line(aes(x = Experimental_Timepoint, y = mean_NOx), color = "#414487FF", size = 1) +
  geom_line(aes(x = Experimental_Timepoint, y = mean_TDN), color ="#7AD151FF", size = 1) +
  geom_line(aes(x = Experimental_Timepoint, y = mean_PO4), color = "#22A884FF", size = 1) +
 # labs(x = "Experimental Timepoint", y = "[Nutrient] over experiment") +
 # scale_y_break(c(350, 700)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
    ylim(c(0,2500)) 
  }

NH4_2022_plot <- nut.plot(NH4_2022_summary)
NH4P_2022_plot <- nut.plot(NH4P_2022_summary)
NO3P_2022_plot <- nut.plot(NO3P_2022_summary)
NO3_2022_plot <- nut.plot(NO3_2022_summary)
P_2022_plot <- nut.plot(P_2022_summary)

NH4_2022_plot 
NH4P_2022_plot
NO3P_2022_plot
NO3_2022_plot 
P_2022_plot

# 2023 ------------------------------------------------------
NH4_2023 <- nuts_data %>% 
  filter(str_detect(Date, "-07-|-08-")) %>% 
  filter(Site == "Stockton") %>% 
  filter(Treatment == "NH4" | Treatment == "Control")

# Summarize data to calculate mean and standard deviation for NH4
NH4_2023_summary <- NH4_2023 %>%
  mutate(Treatment = if_else(Experimental_Timepoint == "T0" & Treatment == "Control", "NH4", Treatment)) %>% 
  group_by(Experimental_Timepoint) %>%
  summarize(
    mean_NH4 = mean(NH4, na.rm = TRUE),
    std_NH4 = sd(NH4, na.rm = TRUE),
    mean_NOx = mean(NOx, na.rm = TRUE),
    std_NOx = sd(NOx, na.rm = TRUE),
    mean_PO4 = mean(PO4, na.rm = TRUE),
    std_PO4 = sd(PO4, na.rm = TRUE),
    mean_TDN = mean(TDN, na.rm = TRUE),
    std_TDN = sd(TDN, na.rm = TRUE)) %>%
  mutate(Experimental_Timepoint = as.numeric(sub("T", "", Experimental_Timepoint))) 

# NH4+P
NH4P_2023 <- nuts_data %>% 
  filter(str_detect(Date, "-07-|-08-")) %>% 
  filter(Site == "Stockton") %>% 
  filter(Treatment == "NH4+P" | Treatment == "Control")

NH4P_2023_summary <- NH4P_2023 %>%
  mutate(Treatment = if_else(Experimental_Timepoint == "T0" & 
                               Treatment == "Control", "NH4+P", Treatment)) %>% 
  group_by(Experimental_Timepoint) %>%
  summarize(
    mean_NH4 = mean(NH4, na.rm = TRUE),
    std_NH4 = sd(NH4, na.rm = TRUE),
    mean_NOx = mean(NOx, na.rm = TRUE),
    std_NOx = sd(NOx, na.rm = TRUE),
    mean_PO4 = mean(PO4, na.rm = TRUE),
    std_PO4 = sd(PO4, na.rm = TRUE),
    mean_TDN = mean(TDN, na.rm = TRUE),
    std_TDN = sd(TDN, na.rm = TRUE)) %>%
  mutate(Experimental_Timepoint = as.numeric(sub("T", "", Experimental_Timepoint))) 

# NO3
NO3_2023 <- nuts_data %>% 
  filter(str_detect(Date, "-07-|-08-")) %>% 
  filter(Site == "Stockton") %>% 
  filter(Treatment == "NO3" | Treatment == "Control")

NO3_2023_summary <- NO3_2023 %>%
  mutate(Treatment = if_else(Experimental_Timepoint == "T0" & 
                               Treatment == "Control", "NO3", Treatment)) %>% 
  group_by(Experimental_Timepoint) %>%
  summarize(
    mean_NH4 = mean(NH4, na.rm = TRUE),
    std_NH4 = sd(NH4, na.rm = TRUE),
    mean_NOx = mean(NOx, na.rm = TRUE),
    std_NOx = sd(NOx, na.rm = TRUE),
    mean_PO4 = mean(PO4, na.rm = TRUE),
    std_PO4 = sd(PO4, na.rm = TRUE),
    mean_TDN = mean(TDN, na.rm = TRUE),
    std_TDN = sd(TDN, na.rm = TRUE)) %>%
  mutate(Experimental_Timepoint = as.numeric(sub("T", "", Experimental_Timepoint))) 

# NO3+P
NO3P_2023 <- nuts_data %>% 
  filter(str_detect(Date, "-07-|-08-")) %>% 
  filter(Site == "Stockton") %>% 
  filter(Treatment == "NO3+P" | Treatment == "Control")

NO3P_2023_summary <- NO3P_2023 %>%
  mutate(Treatment = if_else(Experimental_Timepoint == "T0" & 
                               Treatment == "Control", "NO3+P", Treatment)) %>% 
  group_by(Experimental_Timepoint) %>%
  summarize(
    mean_NH4 = mean(NH4, na.rm = TRUE),
    std_NH4 = sd(NH4, na.rm = TRUE),
    mean_NOx = mean(NOx, na.rm = TRUE),
    std_NOx = sd(NOx, na.rm = TRUE),
    mean_PO4 = mean(PO4, na.rm = TRUE),
    std_PO4 = sd(PO4, na.rm = TRUE),
    mean_TDN = mean(TDN, na.rm = TRUE),
    std_TDN = sd(TDN, na.rm = TRUE)) %>%
  mutate(Experimental_Timepoint = as.numeric(sub("T", "", Experimental_Timepoint)))

# P
P_2023 <- nuts_data %>% 
  filter(str_detect(Date, "-07-|-08-")) %>% 
  filter(Site == "Stockton") %>% 
  filter(Treatment == "P" | Treatment == "Control")

P_2023_summary <- P_2023 %>%
  mutate(Treatment = if_else(Experimental_Timepoint == "T0" & 
                               Treatment == "Control", "P", Treatment)) %>% 
  group_by(Experimental_Timepoint) %>%
  summarize(
    mean_NH4 = mean(NH4, na.rm = TRUE),
    std_NH4 = sd(NH4, na.rm = TRUE),
    mean_NOx = mean(NOx, na.rm = TRUE),
    std_NOx = sd(NOx, na.rm = TRUE),
    mean_PO4 = mean(PO4, na.rm = TRUE),
    std_PO4 = sd(PO4, na.rm = TRUE),
    mean_TDN = mean(TDN, na.rm = TRUE),
    std_TDN = sd(TDN, na.rm = TRUE)) %>%
  mutate(Experimental_Timepoint = as.numeric(sub("T", "", Experimental_Timepoint)))

library(ggbreak) 

# Plot as a time series
nut.plot <- function(df) {
  ggplot(df) + 
    geom_line(aes(x = Experimental_Timepoint, y = mean_NH4), color = "#440154FF", size = 1) +
    #geom_ribbon(aes(x = Experimental_Timepoint, 
    #ymin = mean_NH4 - std_NH4, 
    #ymax = mean_NH4 + std_NH4), 
    #fill = "#440154FF", alpha = 0.2) + 
    geom_line(aes(x = Experimental_Timepoint, y = mean_NOx), color = "#414487FF", size = 1) +
    geom_line(aes(x = Experimental_Timepoint, y = mean_TDN), color ="#7AD151FF", size = 1) +
    geom_line(aes(x = Experimental_Timepoint, y = mean_PO4), color = "#22A884FF", size = 1) +
    # labs(x = "Experimental Timepoint", y = "[Nutrient] over experiment") +
    # scale_y_break(c(350, 700)) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank()) +
  ylim(c(0,2500))  
}

NH4_2023_plot <- nut.plot(NH4_2023_summary)
NH4P_2023_plot <- nut.plot(NH4P_2023_summary)
NO3P_2023_plot <- nut.plot(NO3P_2023_summary)
NO3_2023_plot <- nut.plot(NO3_2023_summary)
P_2023_plot <- nut.plot(P_2023_summary)

NH4_2023_plot 
NH4P_2023_plot
NO3P_2023_plot
NO3_2023_plot 
P_2023_plot


library(cowplot)

save_plot("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Desktop\\new R plots\\NH4_2022_plot.eps", NH4_2022_plot, base_height=3, base_width=6)
save_plot("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Desktop\\new R plots\\NH4_2023_plot.eps", NH4_2023_plot, base_height=3, base_width=6)

save_plot("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Desktop\\new R plots\\NH4P_2022_plot.eps", NH4P_2022_plot, base_height=3, base_width=6)
save_plot("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Desktop\\new R plots\\NH4P_2023_plot.eps", NH4P_2023_plot, base_height=3, base_width=6)

save_plot("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Desktop\\new R plots\\NO3_2022_plot.eps", NO3_2022_plot, base_height=3, base_width=6)
save_plot("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Desktop\\new R plots\\NO3_2023_plot.eps", NO3_2023_plot, base_height=3, base_width=6)

save_plot("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Desktop\\new R plots\\NO3P_2022_plot.eps", NO3P_2022_plot, base_height=3, base_width=6)
save_plot("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Desktop\\new R plots\\NO3P_2023_plot.eps", NO3P_2023_plot, base_height=3, base_width=6)

save_plot("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Desktop\\new R plots\\P_2022_plot.eps", P_2022_plot, base_height=3, base_width=6)
save_plot("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Desktop\\new R plots\\P_2023_plot.eps", P_2023_plot, base_height=3, base_width=6)


# this will look nice -- I think without ribbons, ribbons are too much, but can try once I take the color hex codes from Ellen's figures

# running stats comparing years
cyano_data <- read_excel(file2, sheet = "delta_cyano") %>% 
  dplyr::select(Year, Treatment, diff_percent_change_cyano) %>% 
  group_by(Year, Treatment) %>% 
  mutate(replicate = rep(1:3, length.out = n())) %>%
  pivot_wider(names_from = Year, values_from = diff_percent_change_cyano)

chla_data <- read_excel(file2, sheet = "delta_chla") %>% 
  dplyr::select(Year, Treatment, diff_percent_change_chla) %>% 
  group_by(Year, Treatment) %>% 
  mutate(replicate = rep(1:3, length.out = n())) %>%
  pivot_wider(names_from = Year, values_from = diff_percent_change_chla)

results_cyano <- cyano_data %>%
  group_by(Treatment) %>%
  summarize(t_test = list(t.test(`2022`, `2023`, paired = F))) %>%
  mutate(p_value = map_dbl(t_test, ~ .x$p.value),
         statistic = map_dbl(t_test, ~ .x$statistic))

results_chla <- chla_data %>%
  group_by(Treatment) %>%
  summarize(t_test = list(t.test(`2022`, `2023`, paired = F))) %>%
  mutate(p_value = map_dbl(t_test, ~ .x$p.value),
         statistic = map_dbl(t_test, ~ .x$statistic))

print(results_cyano)
print(results_chla)

# old below ---------------------------
cyano_growth_no_seasonality <- bioassay_data1 %>% 
  dplyr::select(Date, Year, Season, Experimental_Timepoint, Treatment, Cyano_CHLA, Total_CHLA) %>% 
  group_by(Year, Season, Experimental_Timepoint, Treatment) %>% 
  summarize(
    avg_Cyano_CHLA = mean(Cyano_CHLA, na.rm = TRUE), 
    sd_Cyano_CHLA = sd(Cyano_CHLA, na.rm = TRUE),
    avg_Total_CHLA = mean(Total_CHLA, na.rm = TRUE), 
    sd_Total_CHLA = sd(Total_CHLA, na.rm = TRUE)) %>%
  filter(Experimental_Timepoint == "T0" & Season != "Pre-Bloom" | Experimental_Timepoint == "T3" & Season != "Pre-Bloom")

Controls.T0 <- cyano_growth_no_seasonality %>% filter(Experimental_Timepoint == "T0")

Results <- cyano_growth_no_seasonality %>% 
  group_by(Year, Season) %>%
  mutate(percent_growth = case_when( 
    Year == 2022 & Season == "Mid-Bloom" ~ ((avg_Cyano_CHLA/14.9690314)-1)*100,
    Year == 2022 & Season == "Late-Bloom" ~ ((avg_Cyano_CHLA/2.6860398)-1)*100,
    Year == 2023 & Season == "Mid-Bloom" ~ ((avg_Cyano_CHLA/7.3830660)-1)*100)) 

Results.T3 <- Results %>% filter(Experimental_Timepoint == "T3")

Results.chla <- cyano_growth_no_seasonality %>% 
  group_by(Year, Season) %>%
  mutate(percent_growth = case_when( 
    Year == 2022 & Season == "Mid-Bloom" ~ ((avg_Total_CHLA/43.775004)-1)*100,
    Year == 2022 & Season == "Late-Bloom" ~ ((avg_Total_CHLA/7.663333)-1)*100,
    Year == 2023 & Season == "Mid-Bloom" ~ ((avg_Total_CHLA/24.130734)-1)*100)) 

Results.chla.T3 <- Results.chla %>% filter(Experimental_Timepoint == "T3")

Final.values.seasonal <- Results %>% 
  mutate(diff_percent_growth = case_when( 
    Year == 2022 & Season == "Mid-Bloom" ~ percent_growth-341.30042,
    Year == 2022 & Season == "Late-Bloom" ~ percent_growth-452.80595,
    Year == 2023 & Season == "Mid-Bloom" ~ percent_growth-(-51.44235))) %>% 
  filter(Treatment != "Control")

Final.values.seasonal.chla <- Results.chla %>% 
  mutate(diff_percent_growth = case_when( 
    Year == 2022 & Season == "Mid-Bloom" ~ percent_growth-50.90438,
    Year == 2022 & Season == "Late-Bloom" ~ percent_growth-93.76148,
    Year == 2023 & Season == "Mid-Bloom" ~ percent_growth-(-85.14325))) %>% 
  filter(Treatment != "Control")

Final.values.annual <- Final.values.seasonal %>%
  group_by(Year, Treatment) %>%
  summarize(avg_percent_diff = mean(diff_percent_growth, na.rm = TRUE), sd_percent_diff = sd(diff_percent_growth, na.rm = TRUE))

Final.values.annual.chla <- Final.values.seasonal.chla %>% 
  group_by(Year, Treatment) %>%
  summarize(avg_percent_diff = mean(diff_percent_growth, na.rm = TRUE), sd_percent_diff = sd(diff_percent_growth, na.rm = TRUE))

library(ggplot2)
library(ggbreak) 
library(patchwork)

treatment.color <- c("NH4" = "#440154FF",
                     "NH4+P" = "#443A83FF",
                     "NO3" = "#287C8Eff",
                     "NO3+P" = "#35B779FF",
                     "P" = "#75D054FF")

cyanos <- ggplot(Final.values.annual) + 
  geom_bar(aes(x = Treatment, y = avg_percent_diff, fill = as.factor(Year)), stat = "identity", position = "dodge", size = 2.5) +
  labs(x = "Nutrient Addition Treatment", y = "Difference in Cyanobacterial Growth (%)", fill = "Year") +
  # coord_flip() +
  theme_minimal() +
  #  scale_y_break(c(100,1000), scales = .5) 
  ylim(c(-60,250)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = 1.0, color = "grey35") +
  theme(panel.grid.major.y = element_line(color = "grey50", linewidth = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),  # Remove major x grid lines
        panel.grid.minor.x = element_blank()) + 
  scale_fill_manual(values = year.color) + 
  scale_color_manual(values = treatment.color)

chla <- ggplot(Final.values.annual.chla) + 
  geom_bar(aes(x = Treatment, y = avg_percent_diff, fill = as.factor(Year)), stat = "identity", position = "dodge", size = 2.5) +
  labs(x = "Nutrient Addition Treatment", y = "Difference in Total Chlorophyll-a (%)", fill = "Year") +
  # coord_flip() +
  theme_minimal() +
  #  scale_y_break(c(100,1000), scales = .5) 
  ylim(c(-60,250)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed", size = 1.0, color = "grey35") +
  theme(panel.grid.major.y = element_line(color = "grey50", linewidth = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),  # Remove major x grid lines
        panel.grid.minor.x = element_blank()) + 
  scale_fill_manual(values = year.color) + 
  scale_color_manual(values = treatment.color) + 
  theme(legend.position = "none")

library(patchwork)

chla + cyanos



                     