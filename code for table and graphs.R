# Replace 'your_dataset.csv' with your actual file path
fname <- file.choose()

data <- read.csv(fname, header = TRUE)

# Check the structure of your data
str(data)

# Load necessary library for ANOVA
library(dplyr)

# Load necessary libraries
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

# Define shades of blue for the bars
bar_colors <- c("Active.Self" = "#1f78b4", "Passive.Self" = "#6baed6", "Active.Other" = "#bdd7e7", "Passive.Other" = "#eff3ff")

# Plot the data
ggplot(plot_data, aes(x = Scenario, y = Mean_Likelihood, fill = interaction(RiskType, Who))) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  scale_fill_manual(values = bar_colors,
                    labels = c("Active.Self" = "Self Active", "Passive.Self" = "Self Passive", "Active.Other" = "Other Active", "Passive.Other" = "Other Passive")) +
  labs(title = "Perception of Risk Taking Likelihood by Scenario",
       x = "Scenario",
       y = "Mean Likelihood",
       fill = "Risk Type and Who") +
  theme_minimal()


ggplot(plot_data, aes(x = Scenario, y = Mean_Likelihood, fill = interaction(RiskType, Who))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Perception of Risk Taking Likelihood by Scenario",
       x = "Scenario",
       y = "Mean Likelihood",
       fill = "Risk Type and Who") +
  theme_minimal()



# Tidy the ANOVA results
tidy_anova <- broom::tidy(anova_results)


# Format the p-values as in the ANOVA summary
tidy_anova <- tidy_anova %>%
  mutate(across(c(df, `sumsq`, `meansq`), ~ round(., 1))) %>%
  mutate(across(c(statistic), ~ round(., 3))) %>%
  mutate(across(c(`p.value`), ~ round(., 4))) %>%
  mutate(
    significance = ifelse(p.value < 0.001, "***", ""),
    p.value = ifelse(p.value < 0.001, "<0.001", format(p.value, digits = 3, nsmall = 3))
  ) %>%
  mutate(across(everything(), ~ replace_na(as.character(.), ""))) 
  



# Create a nice table with gt
anova_table <- tidy_anova %>%
  gt() %>%
  tab_header(
    title = md("**ANOVA Results for Perception of Risk Taking**")
  ) %>%
  fmt_number(
    columns = c(p.value),
    decimals = 4
  ) %>%
  cols_label(
    term = "Source",
    df = "df",
    `sumsq` = "Sum sq",
    `meansq` = "Mean sq",
    statistic = "F",
    p.value = "p",
    significance = ""
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      weight = px(1)
    ),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(cell_borders(sides = "all", weight = px(0))),
    locations = cells_body(columns = everything())
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.top.color = "black",
    table_body.border.bottom.color = "black",
    table_body.border.top.color = "black"
  )

# Display the table
anova_table

