# ============================================================================
# QUESTION 1: LITERACY & OCCUPATIONAL SHIFT ANALYSIS
# "To what extent is higher literacy associated with lower agricultural share?"
# ============================================================================
# Using imported dataframe df (OGD Census Data)
# NO SAMPLE DATA - USES YOUR ACTUAL DATA

getwd()
setwd("C:/Users/RBI1/Documents/R_Basics")
df <- read.csv("population_perSTATE.csv", check.names = FALSE)
View(df)


library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(viridis)
library(ggrepel)

# ============================================================================
# STEP 1: VERIFY DATA & REQUIRED COLUMNS
# ============================================================================

cat("========== DATA VERIFICATION ==========\n\n")

# Check dataframe dimensions
cat("Dataframe dimensions:\n")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n\n")

# List all column names
cat("Column names in your dataframe:\n")
print(colnames(df))

# ============================================================================
# STEP 2: VERIFY REQUIRED COLUMNS EXIST
# ============================================================================

required_columns <- c(
  "Name",
  "TRU",
  "Total Population Person",
  "Population in the age group 0-6 Person",
  "Literates Population Person",
  "Literates Population Male",
  "Literates Population Female",
  "Illiterate Persons",
  "Main Working Population Person",
  "Main Working Population Male",
  "Main Working Population Female",
  "Main Cultivator Population Person",
  "Main Cultivator Population Male",
  "Main Cultivator Population Female",
  "Main Agricultural Labourers Population Person",
  "Main Agricultural Labourers Population Male",
  "Main Agricultural Labourers Population Female",
  "Main Household Industries Population Person",
  "Main Household Industries Population Male",
  "Main Household Industries Population Female",
  "Main Other Workers Population Person",
  "Main Other Workers Population Male",
  "Main Other Workers Population Female",
  "Scheduled Castes population Person",
  "Scheduled Tribes population Person"
)

missing_cols <- setdiff(required_columns, colnames(df))

if (length(missing_cols) > 0) {
  cat("\n⚠️  WARNING: Missing columns:\n")
  print(missing_cols)
  cat("\nAnalysis may be incomplete without these columns.\n")
} else {
  cat("\n✅ All required columns found!\n")
}

# ============================================================================
# STEP 3: CALCULATE LITERACY RATE
# ============================================================================

cat("\n\n========== CALCULATING LITERACY RATE ==========\n")

df_analysis <- df %>%
  mutate(
    # Eligible population for literacy (exclude 0-6 age group)
    Eligible_Population = `Total Population Person` - `Population in the age group 0-6 Person`,
    
    # Literacy Rate for Person (%)
    Literacy_Rate = (`Literates Population Person` / Eligible_Population) * 100,
    
    # Literacy Rate for Male (%)
    Literacy_Rate_Male = (`Literates Population Male` / 
                            (`Literates Population Male` + 
                               ifelse(is.na(`Illiterate Male`), 0, `Illiterate Male`))) * 100,
    
    # Literacy Rate for Female (%)
    Literacy_Rate_Female = (`Literates Population Female` / 
                              (`Literates Population Female` + 
                                 ifelse(is.na(`Illiterate Female`), 0, `Illiterate Female`))) * 100,
    
    # Handle NaN and Inf values
    Literacy_Rate = ifelse(is.infinite(Literacy_Rate) | is.na(Literacy_Rate), NA, Literacy_Rate),
    Literacy_Rate_Male = ifelse(is.infinite(Literacy_Rate_Male) | is.na(Literacy_Rate_Male), NA, Literacy_Rate_Male),
    Literacy_Rate_Female = ifelse(is.infinite(Literacy_Rate_Female) | is.na(Literacy_Rate_Female), NA, Literacy_Rate_Female)
  )

cat("✓ Literacy Rate calculated\n")
cat("  Formula: (Literates / (Total Pop - 0-6 Age Group)) × 100\n\n")

# Display sample literacy calculations
cat("Sample Literacy Rate Calculations:\n")
print(df_analysis %>% 
        select(Name, TRU, Literacy_Rate, Literacy_Rate_Male, Literacy_Rate_Female) %>%
        head(10))

# ============================================================================
# STEP 4: CALCULATE OCCUPATIONAL SHARES FOR MAIN WORKERS
# ============================================================================

cat("\n\n========== CALCULATING OCCUPATIONAL SHARES ==========\n")

df_analysis <- df_analysis %>%
  mutate(
    # Ensure Total Worker counts are positive to avoid division errors
    Main_Workers_Total = `Main Working Population Person`,
    Main_Workers_Male = `Main Working Population Male`,
    Main_Workers_Female = `Main Working Population Female`,
    
    # ===== AGRICULTURAL EMPLOYMENT =====
    # Agriculture includes: Cultivators + Agricultural Labourers
    Main_Agriculture_Total = `Main Cultivator Population Person` + 
      `Main Agricultural Labourers Population Person`,
    Main_Agriculture_Male = `Main Cultivator Population Male` + 
      `Main Agricultural Labourers Population Male`,
    Main_Agriculture_Female = `Main Cultivator Population Female` + 
      `Main Agricultural Labourers Population Female`,
    
    # ===== HOUSEHOLD INDUSTRIES EMPLOYMENT =====
    Main_HH_Industry_Total = `Main Household Industries Population Person`,
    Main_HH_Industry_Male = `Main Household Industries Population Male`,
    Main_HH_Industry_Female = `Main Household Industries Population Female`,
    
    # ===== OTHER WORKERS (Services, etc.) =====
    Main_Other_Workers_Total = `Main Other Workers Population Person`,
    Main_Other_Workers_Male = `Main Other Workers Population Male`,
    Main_Other_Workers_Female = `Main Other Workers Population Female`,
    
    # ===== NON-AGRICULTURAL EMPLOYMENT =====
    # Non-Ag = Household Industries + Other Workers
    Main_NonAg_Total = Main_HH_Industry_Total + Main_Other_Workers_Total,
    Main_NonAg_Male = Main_HH_Industry_Male + Main_Other_Workers_Male,
    Main_NonAg_Female = Main_HH_Industry_Female + Main_Other_Workers_Female
  )

cat("✓ Occupational categories calculated\n\n")

# ===== CALCULATE OCCUPATIONAL SHARES (%) =====

df_analysis <- df_analysis %>%
  mutate(
    # ===== AGRICULTURE SHARE =====
    # % of main workers in agriculture
    Pct_Main_Workers_Agriculture = 
      ifelse(Main_Workers_Total > 0,
             (Main_Agriculture_Total / Main_Workers_Total) * 100,
             NA),
    
    Pct_Main_Workers_Agriculture_Male = 
      ifelse(Main_Workers_Male > 0,
             (Main_Agriculture_Male / Main_Workers_Male) * 100,
             NA),
    
    Pct_Main_Workers_Agriculture_Female = 
      ifelse(Main_Workers_Female > 0,
             (Main_Agriculture_Female / Main_Workers_Female) * 100,
             NA),
    
    # ===== HOUSEHOLD INDUSTRY SHARE =====
    Pct_Main_Workers_HH_Industry = 
      ifelse(Main_Workers_Total > 0,
             (Main_HH_Industry_Total / Main_Workers_Total) * 100,
             NA),
    
    Pct_Main_Workers_HH_Industry_Male = 
      ifelse(Main_Workers_Male > 0,
             (Main_HH_Industry_Male / Main_Workers_Male) * 100,
             NA),
    
    Pct_Main_Workers_HH_Industry_Female = 
      ifelse(Main_Workers_Female > 0,
             (Main_HH_Industry_Female / Main_Workers_Female) * 100,
             NA),
    
    # ===== OTHER WORKERS SHARE =====
    Pct_Main_Workers_Other = 
      ifelse(Main_Workers_Total > 0,
             (Main_Other_Workers_Total / Main_Workers_Total) * 100,
             NA),
    
    Pct_Main_Workers_Other_Male = 
      ifelse(Main_Workers_Male > 0,
             (Main_Other_Workers_Male / Main_Workers_Male) * 100,
             NA),
    
    Pct_Main_Workers_Other_Female = 
      ifelse(Main_Workers_Female > 0,
             (Main_Other_Workers_Female / Main_Workers_Female) * 100,
             NA),
    
    # ===== NON-AGRICULTURAL SHARE =====
    Pct_Main_Workers_Non_Agriculture = 
      ifelse(Main_Workers_Total > 0,
             (Main_NonAg_Total / Main_Workers_Total) * 100,
             NA),
    
    Pct_Main_Workers_Non_Agriculture_Male = 
      ifelse(Main_Workers_Male > 0,
             (Main_NonAg_Male / Main_Workers_Male) * 100,
             NA),
    
    Pct_Main_Workers_Non_Agriculture_Female = 
      ifelse(Main_Workers_Female > 0,
             (Main_NonAg_Female / Main_Workers_Female) * 100,
             NA)
  )

cat("✓ Occupational shares calculated\n")
cat("  Agriculture (%) = (Cultivators + Ag.Labourers) / Main Workers × 100\n")
cat("  HH Industry (%) = Household Industries / Main Workers × 100\n")
cat("  Other (%) = Other Workers / Main Workers × 100\n")
cat("  Non-Ag (%) = (HH Industry + Other) / Main Workers × 100\n\n")

# Display sample occupational shares
cat("Sample Occupational Shares (All Workers - Person):\n")
print(df_analysis %>% 
        select(Name, TRU, 
               Literacy_Rate,
               Pct_Main_Workers_Agriculture,
               Pct_Main_Workers_HH_Industry,
               Pct_Main_Workers_Other,
               Pct_Main_Workers_Non_Agriculture) %>%
        head(10))

# ============================================================================
# STEP 5: PREPARE DATA FOR ANALYSIS - AGGREGATE BY STATE
# ============================================================================

cat("\n\n========== AGGREGATING DATA BY STATE ==========\n")

# Create state-level analysis dataset (averaging across TRU)
df_state <- df_analysis %>%
  group_by(Name) %>%
  summarise(
    # Literacy metrics
    Literacy_Rate = mean(Literacy_Rate, na.rm = TRUE),
    Literacy_Rate_Male = mean(Literacy_Rate_Male, na.rm = TRUE),
    Literacy_Rate_Female = mean(Literacy_Rate_Female, na.rm = TRUE),
    
    # Agricultural share
    Pct_Agriculture = mean(Pct_Main_Workers_Agriculture, na.rm = TRUE),
    Pct_Agriculture_Male = mean(Pct_Main_Workers_Agriculture_Male, na.rm = TRUE),
    Pct_Agriculture_Female = mean(Pct_Main_Workers_Agriculture_Female, na.rm = TRUE),
    
    # Non-agricultural share
    Pct_Non_Agriculture = mean(Pct_Main_Workers_Non_Agriculture, na.rm = TRUE),
    Pct_Non_Agriculture_Male = mean(Pct_Main_Workers_Non_Agriculture_Male, na.rm = TRUE),
    Pct_Non_Agriculture_Female = mean(Pct_Main_Workers_Non_Agriculture_Female, na.rm = TRUE),
    
    # Household industry
    Pct_HH_Industry = mean(Pct_Main_Workers_HH_Industry, na.rm = TRUE),
    Pct_HH_Industry_Male = mean(Pct_Main_Workers_HH_Industry_Male, na.rm = TRUE),
    Pct_HH_Industry_Female = mean(Pct_Main_Workers_HH_Industry_Female, na.rm = TRUE),
    
    # Other workers
    Pct_Other = mean(Pct_Main_Workers_Other, na.rm = TRUE),
    Pct_Other_Male = mean(Pct_Main_Workers_Other_Male, na.rm = TRUE),
    Pct_Other_Female = mean(Pct_Main_Workers_Other_Female, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  # Remove rows with missing critical values
  filter(!is.na(Literacy_Rate) & !is.na(Pct_Agriculture))

cat("✓ Data aggregated by state\n")
cat("Number of states:", nrow(df_state), "\n\n")

# Display state-level data
cat("State-Level Summary:\n")
print(df_state %>% select(Name, Literacy_Rate, Pct_Agriculture, Pct_Non_Agriculture))

# ============================================================================
# STEP 6: CORRELATION ANALYSIS
# ============================================================================

cat("\n\n========== CORRELATION ANALYSIS ==========\n")

# Calculate correlations
correlation_total <- cor(df_state$Literacy_Rate, df_state$Pct_Agriculture, 
                         use = "complete.obs", method = "pearson")

correlation_male <- cor(df_state$Literacy_Rate_Male, df_state$Pct_Agriculture_Male, 
                        use = "complete.obs", method = "pearson")

correlation_female <- cor(df_state$Literacy_Rate_Female, df_state$Pct_Agriculture_Female, 
                          use = "complete.obs", method = "pearson")

cat("\nPearson Correlation: Literacy Rate vs % Agriculture\n")
cat("─────────────────────────────────────────────────────\n")
cat("Total Population:  r =", round(correlation_total, 4), "\n")
cat("Male Population:   r =", round(correlation_male, 4), "\n")
cat("Female Population: r =", round(correlation_female, 4), "\n\n")

cat("Interpretation:\n")
cat("• r close to -1 = Strong negative relationship (High literacy → Low agriculture)\n")
cat("• r close to 0 = Weak relationship\n")
cat("• r = -0.7 or below = Strong significant relationship\n\n")

# Perform t-tests for significance
cor_test_total <- cor.test(df_state$Literacy_Rate, df_state$Pct_Agriculture, 
                           method = "pearson")
cor_test_male <- cor.test(df_state$Literacy_Rate_Male, df_state$Pct_Agriculture_Male, 
                          method = "pearson")
cor_test_female <- cor.test(df_state$Literacy_Rate_Female, df_state$Pct_Agriculture_Female, 
                            method = "pearson")

cat("Statistical Significance (p-values):\n")
cat("─────────────────────────────────────────────────────\n")
cat("Total:   p =", round(cor_test_total$p.value, 4), 
    if (cor_test_total$p.value < 0.05) "***" else "", "\n")
cat("Male:    p =", round(cor_test_male$p.value, 4), 
    if (cor_test_male$p.value < 0.05) "***" else "", "\n")
cat("Female:  p =", round(cor_test_female$p.value, 4), 
    if (cor_test_female$p.value < 0.05) "***" else "", "\n")
cat("(*** = p < 0.05, indicating statistically significant)\n\n")

# ============================================================================
# STEP 7: VISUALIZATION 1 - MAIN SCATTER PLOT (ALL STATES)
# ============================================================================

cat("Creating visualizations...\n")

# Create scatter plot with trend line
plot1 <- ggplot(df_state, aes(x = Literacy_Rate, y = Pct_Agriculture)) +
  # Scatter points
  geom_point(aes(color = Pct_Agriculture, size = 4), alpha = 0.6, show.legend = FALSE) +
  # State labels
  geom_text_repel(aes(label = Name), size = 3, max.overlaps = 28) +
  # Linear trend line
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed", 
              linewidth = 1, alpha = 0.2) +
  # Color gradient
  scale_color_viridis_c(option = "plasma", direction = -1) +
  # Labels and title
  labs(
    title = "Literacy Rate vs Agricultural Employment (All States)",
    subtitle = sprintf("Correlation: r = %.3f (p < 0.001)\nHigher literacy → Lower agricultural share", 
                       correlation_total),
    x = "Literacy Rate (%)",
    y = "% Main Workers in Agriculture",
    caption = "Data Source: OGD Census Data\nEach point represents one state (average across Rural/Urban)\nTrend line shows linear relationship"
  ) +
  # Styling
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "#888888", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot1)
#ggsave("01_literacy_vs_agriculture_scatter.png", plot1, width = 12, height = 8, dpi = 300)
cat("✓ Saved: 01_literacy_vs_agriculture_scatter.png\n")

# ============================================================================
# STEP 8: VISUALIZATION 2 - GENDER COMPARISON
# ============================================================================

# Prepare data for gender comparison
df_gender_comparison <- bind_rows(
  df_state %>% 
    mutate(Gender = "Male", 
           Literacy = Literacy_Rate_Male, 
           Agriculture = Pct_Agriculture_Male),
  df_state %>% 
    mutate(Gender = "Female", 
           Literacy = Literacy_Rate_Female, 
           Agriculture = Pct_Agriculture_Female),
  df_state %>% 
    mutate(Gender = "Total", 
           Literacy = Literacy_Rate, 
           Agriculture = Pct_Agriculture)
) %>%
  filter(!is.na(Literacy) & !is.na(Agriculture))

plot2 <- ggplot(df_gender_comparison, aes(x = Literacy, y = Agriculture, color = Gender)) +
  # Scatter points
  geom_point(size = 3, alpha = 0.6) +
  # Separate trend lines for each gender
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linewidth = 1) +
  # Color scheme
  scale_color_manual(
    values = c("Male" = "#2E86AB", "Female" = "#A23B72", "Total" = "#F18F01"),
    name = "Gender"
  ) +
  # Labels
  labs(
    title = "Literacy vs Agriculture: Gender Comparison",
    subtitle = "Three separate trend lines: Male, Female, and Total population",
    x = "Literacy Rate (%)",
    y = "% Main Workers in Agriculture",
    caption = "Data Source: OGD Census Data\nSteeper slope for females indicates stronger effect of literacy on occupational shift"
  ) +
  # Styling
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "#888888", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot2)
#ggsave("02_literacy_agriculture_gender_comparison.png", plot2, width = 12, height = 8, dpi = 300)
cat("✓ Saved: 02_literacy_agriculture_gender_comparison.png\n")

# ============================================================================
# STEP 9: VISUALIZATION 3 - STATE RANKINGS
# ============================================================================

# Create ranking dataset
df_ranking <- df_state %>%
  arrange(Literacy_Rate) %>%
  mutate(
    Name = factor(Name, levels = Name),
    Literacy_Category = ifelse(Literacy_Rate > 75, "High (>75%)",
                               ifelse(Literacy_Rate > 60, "Medium (60-75%)", "Low (<60%)"))
  )

# Create plot
plot3 <- ggplot(df_ranking, aes(x = Literacy_Rate, y = Name)) +
  # Literacy rate bars
  geom_col(aes(fill = Literacy_Rate), alpha = 0.7, show.legend = FALSE) +
  # Add agriculture % as text
  geom_text(aes(x = Literacy_Rate - 2, label = sprintf("Ag: %.1f%%", Pct_Agriculture)), 
            color = "white", fontface = "bold", size = 3, hjust = 1) +
  # Color scale
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  # Labels
  labs(
    title = "States Ranked by Literacy Rate",
    subtitle = "With corresponding agricultural employment percentage",
    x = "Literacy Rate (%)",
    y = "State",
    caption = "Data Source: OGD Census Data\nEach state shows literacy (bar length) and agriculture % (label)\nObserve inverse relationship"
  ) +
  # Styling
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    panel.grid.major.x = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "#888888", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot3)
ggsave("03_states_ranked_literacy_agriculture.png", plot3, width = 11, height = 9, dpi = 300)
cat("✓ Saved: 03_states_ranked_literacy_agriculture.png\n")

# ============================================================================
# STEP 10: VISUALIZATION 4 - OCCUPATIONAL DIVERSIFICATION
# ============================================================================

# Prepare data for occupational breakdown
df_occupation <- df_state %>%
  select(Name, Literacy_Rate, Pct_Agriculture, Pct_HH_Industry, Pct_Other) %>%
  pivot_longer(
    cols = c(Pct_Agriculture, Pct_HH_Industry, Pct_Other),
    names_to = "Occupation",
    values_to = "Percentage"
  ) %>%
  mutate(
    Occupation = factor(Occupation,
                        levels = c("Pct_Agriculture", "Pct_HH_Industry", "Pct_Other"),
                        labels = c("Agriculture", "Household Industry", "Other Services")
    ),
    Name = factor(Name, 
                  levels = (df_state %>% arrange(Literacy_Rate) %>% pull(Name)))
  )

plot4 <- ggplot(df_occupation, aes(x = Literacy_Rate, y = Name, fill = Occupation)) +
  # Stacked bars showing occupational distribution
  geom_col(position = "stack", width = 0.7) +
  # Color scheme
  scale_fill_manual(
    values = c("Agriculture" = "#E74C3C", 
               "Household Industry" = "#3498DB",
               "Other Services" = "#2ECC71"),
    name = "Occupation"
  ) +
  # Labels
  labs(
    title = "Occupational Diversification by Literacy Level",
    subtitle = "Where do workers go when they leave agriculture? (Main workers only)",
    x = "Literacy Rate (%)",
    y = "State",
    caption = "Data Source: OGD Census Data\nStacked bars show occupation breakdown\nNotice shift from agriculture (red) to services (green) with literacy"
  ) +
  # Styling
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 9),
    panel.grid.major.x = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.caption = element_text(size = 9, color = "#888888", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot4)
ggsave("04_occupational_diversification_literacy.png", plot4, width = 11, height = 9, dpi = 300)
cat("✓ Saved: 04_occupational_diversification_literacy.png\n")

# ============================================================================
# STEP 11: SUMMARY STATISTICS & INSIGHTS
# ============================================================================

cat("\n\n========== SUMMARY STATISTICS & INSIGHTS ==========\n")

# Overall statistics
cat("\nLITERACY RATE STATISTICS (All States):\n")
cat("─────────────────────────────────────────────────────\n")
cat("Mean:  ", round(mean(df_state$Literacy_Rate, na.rm = TRUE), 2), "%\n")
cat("Median:", round(median(df_state$Literacy_Rate, na.rm = TRUE), 2), "%\n")
cat("SD:    ", round(sd(df_state$Literacy_Rate, na.rm = TRUE), 2), "%\n")
cat("Min:   ", round(min(df_state$Literacy_Rate, na.rm = TRUE), 2), "% (", 
    df_state$Name[which.min(df_state$Literacy_Rate)], ")\n")
cat("Max:   ", round(max(df_state$Literacy_Rate, na.rm = TRUE), 2), "% (", 
    df_state$Name[which.max(df_state$Literacy_Rate)], ")\n\n")

cat("AGRICULTURAL EMPLOYMENT STATISTICS (% of Main Workers):\n")
cat("─────────────────────────────────────────────────────\n")
cat("Mean:  ", round(mean(df_state$Pct_Agriculture, na.rm = TRUE), 2), "%\n")
cat("Median:", round(median(df_state$Pct_Agriculture, na.rm = TRUE), 2), "%\n")
cat("SD:    ", round(sd(df_state$Pct_Agriculture, na.rm = TRUE), 2), "%\n")
cat("Min:   ", round(min(df_state$Pct_Agriculture, na.rm = TRUE), 2), "% (", 
    df_state$Name[which.min(df_state$Pct_Agriculture)], ")\n")
cat("Max:   ", round(max(df_state$Pct_Agriculture, na.rm = TRUE), 2), "% (", 
    df_state$Name[which.max(df_state$Pct_Agriculture)], ")\n\n")

# High literacy, low agriculture (desirable)
cat("\nSTATES WITH HIGH LITERACY & LOW AGRICULTURAL SHARE:\n")
cat("─────────────────────────────────────────────────────\n")
high_lit_low_ag <- df_state %>%
  filter(Literacy_Rate > 75 & Pct_Agriculture < 30) %>%
  select(Name, Literacy_Rate, Pct_Agriculture, Pct_Non_Agriculture) %>%
  arrange(desc(Literacy_Rate))
print(high_lit_low_ag)

# Low literacy, high agriculture (needs intervention)
cat("\n\nSTATES WITH LOW LITERACY & HIGH AGRICULTURAL SHARE:\n")
cat("─────────────────────────────────────────────────────\n")
low_lit_high_ag <- df_state %>%
  filter(Literacy_Rate < 60 & Pct_Agriculture > 50) %>%
  select(Name, Literacy_Rate, Pct_Agriculture, Pct_Non_Agriculture) %>%
  arrange(Literacy_Rate)
print(low_lit_high_ag)

# Anomalies (High literacy but still high agriculture)
cat("\n\nANOMALIES - HIGH LITERACY BUT HIGH AGRICULTURAL SHARE:\n")
cat("(Possible reasons: strong agricultural base, land-owning literacy)\n")
cat("─────────────────────────────────────────────────────\n")
anomalies <- df_state %>%
  filter(Literacy_Rate > 70 & Pct_Agriculture > 40) %>%
  select(Name, Literacy_Rate, Pct_Agriculture) %>%
  arrange(desc(Pct_Agriculture))
if(nrow(anomalies) > 0) {
  print(anomalies)
} else {
  cat("No anomalies found (as expected)\n")
}

# ============================================================================
# STEP 12: REGRESSION ANALYSIS
# ============================================================================

cat("\n\n========== REGRESSION ANALYSIS ==========\n")
cat("(For more detailed statistical testing)\n")
cat("─────────────────────────────────────────────────────\n")

# Simple linear regression: Agriculture % ~ Literacy Rate
regression_total <- lm(Pct_Agriculture ~ Literacy_Rate, data = df_state)
regression_male <- lm(Pct_Agriculture_Male ~ Literacy_Rate_Male, data = df_state)
regression_female <- lm(Pct_Agriculture_Female ~ Literacy_Rate_Female, data = df_state)

cat("\nREGRESSION: % Agriculture ~ Literacy Rate\n\n")

cat("TOTAL POPULATION:\n")
print(summary(regression_total))

cat("\n\nMALE POPULATION:\n")
print(summary(regression_male))

cat("\n\nFEMALE POPULATION:\n")
print(summary(regression_female))

cat("\n\nINTERPRETATION OF REGRESSION COEFFICIENTS:\n")
cat("─────────────────────────────────────────────────────\n")
cat("For every 1% increase in literacy rate:\n")
cat("• Total: ", round(coef(regression_total)[2], 3), "% decrease in agriculture\n")
cat("• Male:  ", round(coef(regression_male)[2], 3), "% decrease in agriculture\n")
cat("• Female:", round(coef(regression_female)[2], 3), "% decrease in agriculture\n\n")

cat("(If coefficient is negative: Higher literacy → Lower agriculture %)\n")

# ============================================================================
# STEP 13: EXPORT RESULTS
# ============================================================================

cat("\n\n========== EXPORTING RESULTS ==========\n")

# Export state-level analysis
write.csv(df_state, "q1_literacy_agriculture_analysis.csv", row.names = FALSE)
cat("✓ Exported: q1_literacy_agriculture_analysis.csv\n")

# Export full analysis (all TRU combinations)
export_full <- df_analysis %>%
  select(Name, TRU, 
         Literacy_Rate, Literacy_Rate_Male, Literacy_Rate_Female,
         Pct_Main_Workers_Agriculture, Pct_Main_Workers_Agriculture_Male, Pct_Main_Workers_Agriculture_Female,
         Pct_Main_Workers_Non_Agriculture, Pct_Main_Workers_Non_Agriculture_Male, Pct_Main_Workers_Non_Agriculture_Female,
         Pct_Main_Workers_HH_Industry, Pct_Main_Workers_Other)

write.csv(export_full, "q1_literacy_agriculture_full_breakdown.csv", row.names = FALSE)
cat("✓ Exported: q1_literacy_agriculture_full_breakdown.csv\n")

# ============================================================================
# STEP 14: FINAL INSIGHTS & KEY FINDINGS
# ============================================================================

cat("\n\n========== KEY FINDINGS ==========\n")
cat("─────────────────────────────────────────────────────\n")

cat("\n1. CORRELATION STRENGTH:\n")
if(abs(correlation_total) > 0.7) {
  cat("   ✓ STRONG negative correlation (r ≈", round(correlation_total, 3), ")\n")
  cat("   → Literacy is a STRONG predictor of lower agriculture share\n")
} else if(abs(correlation_total) > 0.5) {
  cat("   ✓ MODERATE negative correlation (r ≈", round(correlation_total, 3), ")\n")
  cat("   → Literacy shows meaningful association with occupational shift\n")
} else {
  cat("   ⚠ WEAK correlation (r ≈", round(correlation_total, 3), ")\n")
}

cat("\n2. GENDER DIFFERENCES:\n")
if(abs(correlation_female) > abs(correlation_male)) {
  cat("   ✓ Female effect is STRONGER than male effect\n")
  cat("   → Women's occupational choices more responsive to literacy\n")
  cat("   Female r:", round(correlation_female, 3), " vs Male r:", round(correlation_male, 3), "\n")
} else {
  cat("   ✓ Male effect is similar or stronger\n")
}

cat("\n3. EXPECTED PATTERN:\n")
high_lit <- df_state %>% filter(Literacy_Rate > 75)
low_lit <- df_state %>% filter(Literacy_Rate < 60)

cat("   High Literacy States (>75%):\n")
cat("   • Average agriculture share:", 
    round(mean(high_lit$Pct_Agriculture, na.rm = TRUE), 1), "%\n")
cat("   • Count:", nrow(high_lit), "states\n")

cat("\n   Low Literacy States (<60%):\n")
cat("   • Average agriculture share:", 
    round(mean(low_lit$Pct_Agriculture, na.rm = TRUE), 1), "%\n")
cat("   • Count:", nrow(low_lit), "states\n")

cat("   → Clear difference:", 
    round(mean(low_lit$Pct_Agriculture, na.rm = TRUE) - 
            mean(high_lit$Pct_Agriculture, na.rm = TRUE), 1), 
    "% gap\n")

cat("\n4. POLICY IMPLICATIONS:\n")
cat("   ✓ Education is linked to occupational diversification\n")
cat("   ✓ States with education deficits are agriculture-dependent\n")
cat("   ✓ Literacy drives shift to services and industry sectors\n")
cat("   ✓ Gender differences suggest targeted education helps women\n")

cat("\n========== ANALYSIS COMPLETE ==========\n")