# ============================================================================
# DEPENDENCY RATIO ANALYSIS - VISUALIZATION CODE
# Dataset: OGD Open Government Data - Socio-Demographic & Labor Statistics
# ============================================================================

# Install required libraries (run once)
#install.packages("viridis")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(viridis)

# ============================================================================
# STEP 1: LOAD YOUR DATA
# ============================================================================
# Replace this with your actual data loading command
getwd()
setwd("C:/Users/RBI1/Documents/R_Basics")
df <- read.csv("PCA0000_2011_MDDS_population_perSTATE.csv")

# For demonstration, creating sample data structure
# Your actual data should have these columns:
# Name, TRU, Total Population Person, Population in the age group 0-6 Person,
# Non Working Population Person, Total Worker Population Person

# Sample data creation (REPLACE with your actual data)
set.seed(42)
states <- c("Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chhattisgarh",
            "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jharkhand",
            "Karnataka", "Kerala", "Madhya Pradesh", "Maharashtra", "Manipur",
            "Meghalaya", "Mizoram", "Nagaland", "Odisha", "Punjab",
            "Rajasthan", "Sikkim", "Tamil Nadu", "Telangana", "Tripura",
            "Uttar Pradesh", "Uttarakhand", "West Bengal")

# CREATE SAMPLE DATA
df <- expand.grid(
  Name = states,
  TRU = c("Total", "Rural", "Urban")
) %>%
  mutate(
    `Total Population Person` = sample(500000:50000000, n(), replace = TRUE),
    `Population in the age group 0-6 Person` = sample(50000:5000000, n(), replace = TRUE),
    `Non Working Population Person` = sample(100000:20000000, n(), replace = TRUE),
    `Total Worker Population Person` = sample(100000:20000000, n(), replace = TRUE)
  )

# ============================================================================
# STEP 2: CALCULATE DEPENDENCY RATIO
# ============================================================================
df_with_ratio <- df %>%
  mutate(
    # Total Dependency Ratio = (Non-Working Population / Total Worker Population) × 100
    `Dependency Ratio` = (`Non Working Population Person` / `Total Worker Population Person`) * 100,
    
    # Young Dependency Ratio (only 0-6 age group)
    `Young Dependency Ratio` = (`Population in the age group 0-6 Person` / `Total Worker Population Person`) * 100,
    
    # Old Dependency Ratio (proxy: Total Pop - 0-6 - Workers)
    `Other Dependents` = `Non Working Population Person` - `Population in the age group 0-6 Person`
  )

# View first few rows
head(df_with_ratio)
summary(df_with_ratio$`Dependency Ratio`)

# ============================================================================
# VISUALIZATION 1: HORIZONTAL BAR CHART (States ranked by Dependency Ratio)
# ============================================================================

# Calculate average dependency ratio by state (across all TRU)
state_dependency <- df_with_ratio %>%
  group_by(Name) %>%
  summarise(
    `Average Dependency Ratio` = mean(`Dependency Ratio`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(`Average Dependency Ratio`) %>%
  mutate(Name = factor(Name, levels = Name))  # Sort by ratio

# Create horizontal bar chart
plot1 <- ggplot(state_dependency, aes(x = `Average Dependency Ratio`, y = Name)) +
  geom_col(aes(fill = `Average Dependency Ratio`), show.legend = TRUE) +
  scale_fill_viridis_c(
    name = "Dependency Ratio",
    option = "plasma",
    direction = 1,
    breaks = pretty_breaks(5)
  ) +
  labs(
    title = "Dependency Ratio by State",
    subtitle = "Average ratio of non-working to working population × 100",
    x = "Dependency Ratio",
    y = "State",
    caption = "Data Source: OGD Open Government Data Platform (Table PCA)\nHigher values indicate greater economic burden on workers"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, color = "#555555", hjust = 0),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    panel.grid.major.x = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(size = 9, color = "#888888", hjust = 1),
    legend.position = "right",
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot1)

# Save the plot
ggsave("01_horizontal_bar_dependency_ratio.png", plot1, width = 12, height = 10, dpi = 300)

# ============================================================================
# VISUALIZATION 2: FACETED BAR CHARTS (by TRU: Total, Rural, Urban)
# ============================================================================

# Calculate average dependency ratio by state and TRU
state_tru_dependency <- df_with_ratio %>%
  group_by(Name, TRU) %>%
  summarise(
    `Dependency Ratio` = mean(`Dependency Ratio`, na.rm = TRUE),
    .groups = 'drop'
  )

# Order states by average dependency ratio (Total category)
state_order <- state_tru_dependency %>%
  filter(TRU == "Total") %>%
  arrange(`Dependency Ratio`) %>%
  pull(Name)

state_tru_dependency <- state_tru_dependency %>%
  mutate(Name = factor(Name, levels = state_order))

# Create faceted bar chart
plot2 <- ggplot(state_tru_dependency, aes(x = `Dependency Ratio`, y = Name)) +
  geom_col(aes(fill = TRU), position = "dodge") +
  facet_wrap(~TRU, ncol = 3, scales = "free_x") +
  scale_fill_manual(
    values = c("Total" = "#2E86AB", "Rural" = "#A23B72", "Urban" = "#F18F01"),
    guide = "none"
  ) +
  labs(
    title = "Dependency Ratio by State and Area Type",
    subtitle = "Comparison of Total, Rural, and Urban dependency burdens",
    x = "Dependency Ratio",
    y = "State",
    caption = "Data Source: OGD Open Government Data Platform (Table PCA)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, color = "#555555", hjust = 0),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    panel.grid.major.x = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(size = 11, face = "bold", color = "white", 
                              margin = margin(5, 5, 5, 5)),
    strip.background = element_rect(fill = "#333333", color = NA),
    plot.caption = element_text(size = 9, color = "#888888", hjust = 1),
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot2)

# Save the plot
ggsave("02_faceted_bar_tru_dependency.png", plot2, width = 14, height = 10, dpi = 300)

# ============================================================================
# VISUALIZATION 3: HEAT MAP (State × TRU)
# ============================================================================

# Prepare data for heatmap
heatmap_data <- df_with_ratio %>%
  group_by(Name, TRU) %>%
  summarise(
    `Dependency Ratio` = mean(`Dependency Ratio`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(TRU = factor(TRU, levels = c("Total", "Rural", "Urban")))

# Create heatmap
plot3 <- ggplot(heatmap_data, aes(x = TRU, y = Name, fill = `Dependency Ratio`)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = round(`Dependency Ratio`, 1)),
    color = "white",
    size = 3.5,
    fontface = "bold"
  ) +
  scale_fill_viridis_c(
    name = "Dependency\nRatio",
    option = "magma",
    direction = 1,
    breaks = pretty_breaks(6)
  ) +
  labs(
    title = "Dependency Ratio Heat Map: State × Area Type",
    subtitle = "Darker colors indicate higher dependency burden",
    x = "Area Type",
    y = "State",
    caption = "Data Source: OGD Open Government Data Platform (Table PCA)\nValues shown are dependency ratios"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, color = "#555555", hjust = 0),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 9, color = "#888888", hjust = 1),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  coord_fixed(ratio = 0.5)

print(plot3)

# Save the plot
ggsave("03_heatmap_state_tru_dependency.png", plot3, width = 10, height = 12, dpi = 300)

# ============================================================================
# VISUALIZATION 4: STACKED BAR (Dependency Components)
# ============================================================================

# Prepare data for stacked bar - calculate components
stacked_bar_data <- df_with_ratio %>%
  group_by(Name) %>%
  summarise(
    `Young Dependents (0-6)` = mean(`Population in the age group 0-6 Person`, na.rm = TRUE) / mean(`Total Worker Population Person`, na.rm = TRUE) * 100,
    `Other Non-Workers` = mean(`Other Dependents`, na.rm = TRUE) / mean(`Total Worker Population Person`, na.rm = TRUE) * 100,
    .groups = 'drop'
  ) %>%
  mutate(
    `Total Ratio` = `Young Dependents (0-6)` + `Other Non-Workers`,
    Name = factor(Name, levels = (.) %>% arrange(`Total Ratio`) %>% pull(Name))
  ) %>%
  pivot_longer(
    cols = c("Young Dependents (0-6)", "Other Non-Workers"),
    names_to = "Dependency Type",
    values_to = "Ratio"
  )

# Create stacked bar chart
plot4 <- ggplot(stacked_bar_data, aes(x = Ratio, y = Name, fill = `Dependency Type`)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(
    values = c(
      "Young Dependents (0-6)" = "#FF6B6B",
      "Other Non-Workers" = "#4ECDC4"
    ),
    name = "Dependency Type"
  ) +
  labs(
    title = "Dependency Ratio Components by State",
    subtitle = "Breakdown of young (0-6) vs. other non-working dependents",
    x = "Dependency Ratio",
    y = "State",
    caption = "Data Source: OGD Open Government Data Platform (Table PCA)\nShows which dependency source is dominant in each state"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, color = "#555555", hjust = 0),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    panel.grid.major.x = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    plot.caption = element_text(size = 9, color = "#888888", hjust = 1),
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot4)

# Save the plot
ggsave("04_stacked_bar_dependency_components.png", plot4, width = 12, height = 10, dpi = 300)

# ============================================================================
# STEP 3: GENERATE SUMMARY STATISTICS
# ============================================================================

cat("\n========== DEPENDENCY RATIO SUMMARY STATISTICS ==========\n")

# Overall statistics
cat("\nOVERALL DEPENDENCY RATIO STATISTICS:\n")
cat("Mean Dependency Ratio:", round(mean(df_with_ratio$`Dependency Ratio`, na.rm = TRUE), 2), "\n")
cat("Median Dependency Ratio:", round(median(df_with_ratio$`Dependency Ratio`, na.rm = TRUE), 2), "\n")
cat("SD Dependency Ratio:", round(sd(df_with_ratio$`Dependency Ratio`, na.rm = TRUE), 2), "\n")
cat("Min Dependency Ratio:", round(min(df_with_ratio$`Dependency Ratio`, na.rm = TRUE), 2), "\n")
cat("Max Dependency Ratio:", round(max(df_with_ratio$`Dependency Ratio`, na.rm = TRUE), 2), "\n")

# By TRU
cat("\n\nDEPENDENCY RATIO BY AREA TYPE (TRU):\n")
tru_summary <- df_with_ratio %>%
  group_by(TRU) %>%
  summarise(
    `Mean Ratio` = round(mean(`Dependency Ratio`, na.rm = TRUE), 2),
    `Median Ratio` = round(median(`Dependency Ratio`, na.rm = TRUE), 2),
    `SD` = round(sd(`Dependency Ratio`, na.rm = TRUE), 2),
    .groups = 'drop'
  )
print(tru_summary)

# Top 5 highest dependency states
cat("\n\nTOP 5 STATES WITH HIGHEST DEPENDENCY RATIO:\n")
top_states <- df_with_ratio %>%
  group_by(Name) %>%
  summarise(`Average Ratio` = mean(`Dependency Ratio`, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(`Average Ratio`)) %>%
  head(5) %>%
  mutate(`Average Ratio` = round(`Average Ratio`, 2))
print(top_states)

# Top 5 lowest dependency states
cat("\n\nTOP 5 STATES WITH LOWEST DEPENDENCY RATIO:\n")
bottom_states <- df_with_ratio %>%
  group_by(Name) %>%
  summarise(`Average Ratio` = mean(`Dependency Ratio`, na.rm = TRUE), .groups = 'drop') %>%
  arrange(`Average Ratio`) %>%
  head(5) %>%
  mutate(`Average Ratio` = round(`Average Ratio`, 2))
print(bottom_states)

# Export summary table
summary_table <- df_with_ratio %>%
  group_by(Name, TRU) %>%
  summarise(
    `Dependency Ratio` = round(mean(`Dependency Ratio`, na.rm = TRUE), 2),
    `Young Dependency Ratio` = round(mean(`Young Dependency Ratio`, na.rm = TRUE), 2),
    `Workers` = format(round(mean(`Total Worker Population Person`, na.rm = TRUE), 0), big.mark = ","),
    `Non-Workers` = format(round(mean(`Non Working Population Person`, na.rm = TRUE), 0), big.mark = ","),
    .groups = 'drop'
  )

write.csv(summary_table, "dependency_ratio_summary.csv", row.names = FALSE)
cat("\n\nSummary table exported to: dependency_ratio_summary.csv\n")

# ============================================================================
# STEP 4: ADDITIONAL INSIGHTS (Gender Analysis - if needed)
# ============================================================================

# If you have gender-disaggregated data, uncomment below and modify column names:
# plot_gender <- df_with_ratio %>%
#   # Add gender-specific calculations here
#   ggplot(aes(x = Dependency_Ratio, y = Name, fill = Gender)) +
#   geom_col(position = "dodge") +
#   theme_minimal() +
#   labs(title = "Dependency Ratio by State and Gender")
# print(plot_gender)

cat("\n========== ANALYSIS COMPLETE ==========\n")
