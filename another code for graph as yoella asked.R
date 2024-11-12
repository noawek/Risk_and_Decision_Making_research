# Replace 'your_dataset.csv' with your actual file path
fname <- file.choose()

data <- read.csv(fname, header = TRUE)

# Check the structure of your data
str(data)

# Load necessary library for ANOVA
library(dplyr)
library(tidyverse)
library(ggpubr)
library(gt)
library(broom)

# Reshape the data to long format
data_long <- data %>%
  pivot_longer(cols = c(Stock_LIkelihood, Pension_Likelihood, `Parked.Car_Likelihood`),
               names_to = "Scenario", values_to = "Likelihood") %>%
  pivot_longer(cols = c(Stock_Risk, Pension_Risk, `Parked.Car_Risk`),
               names_to = "Risk_Scenario", values_to = "Risk")

# Perform three-way ANOVA
anova_results <- aov(Likelihood ~ RiskType + Who + Scenario, data = data_long)
summary(anova_results)

# Perform three-way ANOVA
anova_results <- aov(Likelihood ~ RiskType * Who * Scenario, data = data_long)
summary(anova_results)

# Visualize the results
# Mean likelihood by RiskType and Who for each scenario
data_long <- data_long %>%
  mutate(Scenario = recode(Scenario,
                           Stock_LIkelihood = "Stocks",
                           Pension_Likelihood = "Pension",
                           `Parked.Car_Likelihood` = "Parked Car"))

plot_data <- data_long %>%
  group_by(Scenario, RiskType, Who) %>%
  summarise(Mean_Likelihood = mean(Likelihood), .groups = 'drop')

# Define colors for self and others
bar_colors <- c("Self" = "#1f78b4", "Other" = "#bdd7e7")

# Plot the data
ggplot(plot_data, aes(x = RiskType, y = Mean_Likelihood, fill = Who)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  scale_fill_manual(values = bar_colors,
                    labels = c("Self" = "Self", "Other" = "Other")) +
  labs(title = "Perception of Risk Taking Likelihood by Scenario and Risk Type",
       x = "Scenario and Risk Type",
       y = "Mean Likelihood",
       fill = "Who") +
  theme_minimal() +
  facet_grid(~ Scenario, scales = "free", space = "free", switch = "x") +
  theme(strip.placement = "outside",
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 13))
