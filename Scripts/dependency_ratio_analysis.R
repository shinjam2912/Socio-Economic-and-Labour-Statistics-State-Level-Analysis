# ============================================================================
# DEPENDENCY RATIO ANALYSIS - VISUALIZATION CODE

# ============================================================================

# Install required libraries (run once)
#install.packages("viridis")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(viridis)
library(readr)
# ============================================================================
# STEP 1: LOAD  DATA
# ============================================================================
# Replace this with your actual data loading command
getwd()
setwd("C:/Users/RBI1/Documents/R_Basics")
dfo <- read.csv("population_perSTATE.csv", check.names = FALSE)
colnames(dfo)
View(dfo)
dfo<-dfo[-c(1,2,3),]

# Sample data creation (REPLACE with your actual data)
set.seed(42)
states <- unique(dfo$Name) #36 states 

# data frame mutation
df <- expand.grid(
  Name = states,
  TRU = c("Total", "Rural", "Urban")
) |>
  mutate(
    `Total Population Person` = dfo$`Total Population Person`,
    `Population in the age group 0-6 Person` = dfo$`Population in the age group 0-6 Person`,
    `Non Working Population Person` = dfo$`Non Working Population Person`,
    `Total Worker Population Person` = dfo$`Total Worker Population Person`
  )

# ============================================================================
# STEP 2: CALCULATE DEPENDENCY RATIO
# ============================================================================
df_with_ratio <- dfo %>%
  mutate(
    # Total Dependency Ratio = (Non-Working Population / Total Worker Population) × 100
    `Dependency Ratio` = (`Non Working Population Person` / `Total Worker Population Person`) * 100,
    
    # Young Dependency Ratio (only 0-6 age group)
    `Young Dependency Ratio` = (`Population in the age group 0-6 Person` / `Total Worker Population Person`) * 100,
    
    # Old Dependency Ratio (proxy: Total Pop - 0-6 - Workers)
    `Other Dependents` = ((`Non Working Population Person` - `Population in the age group 0-6 Person`)/ `Total Worker Population Person`)*100
  )


# View first few rows
View(df_with_ratio)
head(df_with_ratio)
summary(df_with_ratio$`Dependency Ratio`)
write_csv(df_with_ratio, "population_with_dependency_ratios.csv")
# ============================================================================
# VISUALIZATION 1: HORIZONTAL BAR CHART (States ranked by Dependency Ratio)
# ============================================================================
View(df_with_ratio)
# Calculate average dependency ratio by state (across all TRU)
state_dependency <- df_with_ratio %>%
  group_by(Name) %>%
  summarise(
    `Average Dependency Ratio` = mean(`Dependency Ratio`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(`Average Dependency Ratio`) %>%
  mutate(Name = factor(Name, levels = Name))  # Sort by ratio
View(state_dependency)

# Create horizontal bar chart
plot1 <- ggplot(state_dependency, aes(x = `Average Dependency Ratio`, y = Name)) +
  geom_col(fill = "steelblue") +
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
national_avg <- mean(state_dependency$`Average Dependency Ratio`, na.rm = TRUE)
plot1 + geom_vline(xintercept = national_avg, linetype = "dashed", color = "red", linewidth = 1)

print(plot1)

# Save the plot
#ggsave("01_horizontal_bar_dependency_ratio.png", plot1, width = 12, height = 10, dpi = 300)

###HEAT MAP 
###for Total Rural Urban-------this has a slight difference from above which calculates the avg( accross TRU) 
# ===== PREPARE DATA FOR HEATMAP =====

heatmap_data <- df_with_ratio %>%
  group_by(Name, TRU) %>%
  summarise(
    `Dependency Ratio` = mean(`Dependency Ratio`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(TRU = factor(TRU, levels = c("Total", "Rural", "Urban")))

# ===== CREATE HEATMAP =====

heatmap_plot <- ggplot(heatmap_data, aes(x = TRU, y = Name, fill = `Dependency Ratio`)) +
  # Heatmap tiles
  geom_tile(color = "white", linewidth = 0.8) +
  # Add values in cells
  geom_text(
    aes(label = round(`Dependency Ratio`, 0)),
    color = "white",
    size = 3.5,
    fontface = "bold"
  ) +
  # Color scale: Orange to Red
  scale_fill_gradient(
    name = "Dependency\nRatio",
    low = "#FFE8D6",      # Very light orange
    high = "#CC0000",     # Dark red
    breaks = c(120, 140, 160, 180),
    limits = c(NA, NA)
  ) +
  # Labels
  labs(
    title = "Dependency Ratio Heatmap: State × Area Type",
    x = "Area Type",
    y = "State",
    caption = "Data Source: OGD Census Data"
  ) +
  # Styling
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.caption = element_text(size = 9, color = "#888888", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  coord_fixed(ratio = 0.35)

print(heatmap_plot)

# Save
#ggsave("05_heatmap_dependency_ratio_state_tru.png", heatmap_plot, width = 10, height = 11, dpi = 300)

#cat("✓ Saved: 05_heatmap_dependency_ratio_state_tru.png\n")

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
#ggsave("02_faceted_bar_tru_dependency.png", plot2, width = 14, height = 10, dpi = 300)

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





# ==============================================
# =====================================================
# Relation: Non-Agricultural Sectors vs Urbanization
# =====================================================

library(dplyr)
library(ggplot2)
library(corrplot)

# 1. Calculate shares of non-agricultural sectors
non_agri_data <- df %>%
  filter(TRU == "Total") %>%
  mutate(
    Main_Household_Industries_Share = (`Main Household Industries Population Person` / 
                                         `Main Working Population Person`) * 100,
    
    Main_Other_Workers_Share = (`Main Other Workers Population Person` / 
                                  `Main Working Population Person`) * 100
  ) %>%
  select(
    Name,
    Main_Household_Industries_Share,
    Main_Other_Workers_Share,
    `Total Population Person`
  )

# 2. Get Urbanization Rate (reuse from earlier code)
urbanization <- df %>%
  filter(TRU == "Total") %>%
  select(Name, Total_Pop = `Total Population Person`) %>%
  left_join(
    df %>% filter(TRU == "Urban") %>% select(Name, Urban_Pop = `Total Population Person`),
    by = "Name"
  ) %>%
  mutate(Urbanization_Rate = (Urban_Pop / Total_Pop) * 100) %>%
  select(Name, Urbanization_Rate)

# 3. Merge everything
final_data <- non_agri_data %>%
  left_join(urbanization, by = "Name") %>%
  drop_na()

# 4. Correlation Analysis
cor_matrix <- cor(final_data %>% select(Main_Household_Industries_Share, 
                                        Main_Other_Workers_Share, 
                                        Urbanization_Rate))
print(round(cor_matrix, 3))

# 5. Scatter Plots

# Plot 1: Household Industries vs Urbanization
ggplot(final_data, aes(x = Main_Household_Industries_Share, y = Urbanization_Rate)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Household Industries vs Urbanization",
       x = "Main Household Industries Share (%)",
       y = "Urbanization Rate (%)") +
  theme_minimal()

# Plot 2: Other Workers (Service + Manufacturing) vs Urbanization
ggplot(final_data, aes(x = Main_Other_Workers_Share, y = Urbanization_Rate)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Other Workers (Service Sector) vs Urbanization",
       x = "Main Other Workers Share (%)",
       y = "Urbanization Rate (%)") +
  theme_minimal()

# =====================================================
# Agricultural Workforce Share vs Urbanization
# =====================================================

library(dplyr)
library(ggplot2)

# 1. Calculate Agri_Workforce_Share
agri_data <- df %>%
  filter(TRU == "Total") %>%
  mutate(
    Agri_Workforce_Share = ((`Main Cultivator Population Person` + 
                               `Main Agricultural Labourers Population Person`) / 
                              `Main Working Population Person`) * 100
  ) %>%
  select(Name, Agri_Workforce_Share)

# 2. Get Urbanization Rate (same as before)
urbanization <- df %>%
  filter(TRU == "Total") %>%
  select(Name, Total_Pop = `Total Population Person`) %>%
  left_join(
    df %>% filter(TRU == "Urban") %>% 
      select(Name, Urban_Pop = `Total Population Person`),
    by = "Name"
  ) %>%
  mutate(Urbanization_Rate = (Urban_Pop / Total_Pop) * 100) %>%
  select(Name, Urbanization_Rate)

# 3. Merge both
agri_urban_data <- agri_data %>%
  left_join(urbanization, by = "Name") %>%
  drop_na()

# 4. Correlation
cor(agri_urban_data$Agri_Workforce_Share, 
    agri_urban_data$Urbanization_Rate, 
    method = "pearson")

# 5. Scatter Plot
ggplot(agri_urban_data, aes(x = Agri_Workforce_Share, y = Urbanization_Rate)) +
  geom_point(color = "darkred", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Agricultural Workforce Share vs Urbanization",
    x = "Agricultural Workforce Share (% of Main Workers)",
    y = "Urbanization Rate (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
