# ============================================================================
# WORKFORCE PARTICIPATION RATE (WFPR) ANALYSIS
# Using your imported dataframe df (OGD Census Data)
# ============================================================================

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(viridis)

# ============================================================================
# STEP 1: CHECK YOUR DATA
# ============================================================================

# Display basic info about your imported dataframe
cat("========== DATA OVERVIEW ==========\n")
cat("Number of rows:", nrow(df), "\n")
cat("Number of columns:", ncol(df), "\n\n")

# Display column names
cat("Column names in your dataframe:\n")
print(colnames(df))

# Display first few rows
cat("\n\nFirst few rows of your data:\n")
print(head(df))

# Data structure
cat("\n\nData structure:\n")
str(df)

# ============================================================================
# STEP 2: VERIFY REQUIRED COLUMNS EXIST
# ============================================================================

# Check if all required columns are present
required_columns <- c(
  "Name",
  "TRU",
  "Total Population Person",
  "Total Worker Population Person",
  "Main Working Population Person",
  "Marginal Worker Population 3_6 Person",
  "Marginal Worker Population 0_3 Person",
  "Non Working Population Person"
)

missing_cols <- setdiff(required_columns, colnames(df))

if (length(missing_cols) > 0) {
  cat("\n⚠️  WARNING: Missing columns:\n")
  print(missing_cols)
  cat("\nAvailable columns are:\n")
  print(colnames(df))
} else {
  cat("\n✓ All required columns found!\n")
}

# ============================================================================
# STEP 3: CALCULATE WFPR METRICS
# ============================================================================

# Create new dataframe with WFPR calculations
df_wfpr <- df %>%
  mutate(
    # 1. STRICT PRINCIPAL STATUS (Main workers only)
    # This represents workers with stable employment (≥6 months)
    Principal_Status_Workers = `Main Working Population Person`,
    
    # 2. UPSS PROXY (Main + Marginal 3-6 months)
    # This includes both stable and semi-employed workers
    UPSS_Proxy_Workers = `Main Working Population Person` + `Marginal Worker Population 3_6 Person`,
    
    # 3. MARGINAL 0-3 MONTHS (Very marginal/underemployed)
    # Workers with minimal employment (less than 3 months)
    Marginal_0_3_Workers = `Marginal Worker Population 0_3 Person`,
    
    # 4. TOTAL WORKFORCE (All workers)
    Total_Workers = `Total Worker Population Person`,
    
    # ===== PARTICIPATION RATES =====
    
    # Principal Status Participation Rate (%)
    Principal_Status_Rate = (Principal_Status_Workers / `Total Population Person`) * 100,
    
    # UPSS Proxy Participation Rate (%)
    UPSS_Proxy_Rate = (UPSS_Proxy_Workers / `Total Population Person`) * 100,
    
    # Marginal 0-3 Rate (%)
    Marginal_0_3_Rate = (Marginal_0_3_Workers / `Total Population Person`) * 100,
    
    # Total Worker Participation Rate (%)
    Total_Worker_Rate = (Total_Workers / `Total Population Person`) * 100,
    
    # ===== EMPLOYMENT QUALITY METRICS =====
    
    # % of workers with stable employment (Main/Total)
    Main_Worker_Percentage = (Principal_Status_Workers / Total_Workers) * 100,
    
    # % of workers with semi-employment (Marginal 3-6/Total)
    Marginal_3_6_Percentage = (`Marginal Worker Population 3_6 Person` / Total_Workers) * 100,
    
    # % of workers with very marginal employment (Marginal 0-3/Total)
    Marginal_0_3_Percentage = (Marginal_0_3_Workers / Total_Workers) * 100,
    
    # Labor Distress Index (High marginal 0-3 relative to main workers)
    # Ratio of marginal (0-3) to main workers
    Labor_Distress_Index = (Marginal_0_3_Workers / Principal_Status_Workers) * 100,
    
    # Unemployment proxy (Non-working to total workers ratio)
    Unemployment_Proxy = (`Non Working Population Person` / Total_Workers) * 100,
    
    # Quality of Employment Index
    # (Main - Marginal 0-3) / Main * 100
    # Positive = more main workers; Negative = more marginal workers
    Employment_Quality_Index = ((Principal_Status_Workers - Marginal_0_3_Workers) / 
                                  Principal_Status_Workers) * 100
  ) %>%
  # Handle NA and Inf values (in case of division by zero)
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.), NA, .)))

# Display calculated metrics
cat("\n========== WFPR CALCULATIONS COMPLETE ==========\n")
cat("New columns created:\n")
cat("- Principal_Status_Workers\n")
cat("- UPSS_Proxy_Workers\n")
cat("- Marginal_0_3_Workers\n")
cat("- Principal_Status_Rate (% of population)\n")
cat("- UPSS_Proxy_Rate (% of population)\n")
cat("- Marginal_0_3_Rate (% of population)\n")
cat("- Main_Worker_Percentage (% of total workers)\n")
cat("- Labor_Distress_Index\n")
cat("- Employment_Quality_Index\n\n")

# Show sample calculations
cat("Sample calculations (first few rows):\n")
print(df_wfpr %>% 
        select(Name, TRU, 
               Principal_Status_Rate, UPSS_Proxy_Rate, Marginal_0_3_Rate,
               Labor_Distress_Index, Employment_Quality_Index) %>%
        head(10))

# ============================================================================
# VISUALIZATION 1: Principal Status vs UPSS Proxy (Horizontal Bar)
# ============================================================================

# Aggregate by state
state_wfpr_1 <- df_wfpr %>%
  group_by(Name) %>%
  summarise(
    Principal_Status_Rate = mean(Principal_Status_Rate, na.rm = TRUE),
    UPSS_Proxy_Rate = mean(UPSS_Proxy_Rate, na.rm = TRUE),
    Marginal_0_3_Rate = mean(Marginal_0_3_Rate, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(
    cols = c("Principal_Status_Rate", "UPSS_Proxy_Rate", "Marginal_0_3_Rate"),
    names_to = "Employment_Type",
    values_to = "Rate"
  ) %>%
  mutate(
    Employment_Type = factor(Employment_Type, 
                             levels = c("Principal_Status_Rate", "UPSS_Proxy_Rate", "Marginal_0_3_Rate"),
                             labels = c("Principal Status\n(Main Only)", 
                                        "UPSS Proxy\n(Main + 3-6mo)", 
                                        "Marginal 0-3\n(Very Marginal)"))
  ) %>%
  # Order states by Principal Status Rate
  mutate(Name = factor(Name, 
                       levels = (df_wfpr %>% group_by(Name) %>% 
                                   summarise(m = mean(Principal_Status_Rate, na.rm = TRUE)) %>% 
                                   arrange(m) %>% pull(Name))))

# Create the plot
plot1 <- ggplot(state_wfpr_1, aes(x = Rate, y = Name, fill = Employment_Type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(
      "Principal Status\n(Main Only)" = "#2E86AB",
      "UPSS Proxy\n(Main + 3-6mo)" = "#A23B72",
      "Marginal 0-3\n(Very Marginal)" = "#F18F01"
    ),
    name = "Employment Status"
  ) +
  labs(
    title = "Workforce Participation Rates by State",
    subtitle = "Comparing Principal Status vs UPSS Proxy vs Marginal Workers",
    x = "Participation Rate (% of population)",
    y = "State",
    caption = "Data Source: OGD Census Data\nPrincipal Status = Main Workers (≥6 months)\nUPSS Proxy = Main + Marginal 3-6 months\nMarginal 0-3 = Workers employed <3 months"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 9),
    panel.grid.major.x = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.caption = element_text(size = 8, color = "#888888", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot1)
ggsave("01_wfpr_principal_vs_upss.png", plot1, width = 13, height = 10, dpi = 300)

# ============================================================================
# VISUALIZATION 2: Labor Distress Index (Main vs Marginal 0-3)
# ============================================================================

# Labor Distress: High = more marginal workers relative to main
state_distress <- df_wfpr %>%
  group_by(Name) %>%
  summarise(
    Labor_Distress_Index = mean(Labor_Distress_Index, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Labor_Distress_Index)) %>%
  mutate(Name = factor(Name, levels = Name))

plot2 <- ggplot(state_distress, aes(x = Labor_Distress_Index, y = Name)) +
  geom_col(aes(fill = Labor_Distress_Index)) +
  scale_fill_viridis_c(
    name = "Distress Index\n(Marginal 0-3 /\nMain Workers × 100)",
    option = "plasma",
    direction = 1
  ) +
  labs(
    title = "Labor Distress Index by State",
    subtitle = "Higher index = More marginal workers relative to main workers",
    x = "Labor Distress Index (Marginal 0-3 / Main Workers × 100)",
    y = "State",
    caption = "Data Source: OGD Census Data\nIndex > 50 indicates significant labor precarity\nHigher values = greater employment instability"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.x = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.caption = element_text(size = 8, color = "#888888", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot2)
ggsave("02_labor_distress_index.png", plot2, width = 12, height = 10, dpi = 300)

# ============================================================================
# VISUALIZATION 3: Employment Quality Index
# ============================================================================

# Quality index: Shows gap between main and marginal workers
state_quality <- df_wfpr %>%
  group_by(Name) %>%
  summarise(
    Employment_Quality_Index = mean(Employment_Quality_Index, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(Employment_Quality_Index) %>%
  mutate(
    Name = factor(Name, levels = Name),
    Quality_Level = ifelse(Employment_Quality_Index > 80, "Excellent",
                           ifelse(Employment_Quality_Index > 60, "Good",
                                  ifelse(Employment_Quality_Index > 40, "Fair", "Poor")))
  )

plot3 <- ggplot(state_quality, aes(x = Employment_Quality_Index, y = Name)) +
  geom_col(aes(fill = Quality_Level), show.legend = TRUE) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "red", linewidth = 1, alpha = 0.6) +
  scale_fill_manual(
    values = c("Excellent" = "#27AE60", "Good" = "#3498DB", 
               "Fair" = "#F39C12", "Poor" = "#E74C3C"),
    name = "Quality Level"
  ) +
  labs(
    title = "Employment Quality Index by State",
    subtitle = "Gap between main workers and marginal (0-3) workers",
    x = "Employment Quality Index",
    y = "State",
    caption = "Data Source: OGD Census Data\nHigher values indicate better employment quality\nRed line = Quality threshold (50)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.x = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    legend.position = "right",
    plot.caption = element_text(size = 8, color = "#888888", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot3)
ggsave("03_employment_quality_index.png", plot3, width = 12, height = 10, dpi = 300)

# ============================================================================
# VISUALIZATION 4: Employment Composition (Stacked) by State & TRU
# ============================================================================

# Composition by state and TRU
composition_data <- df_wfpr %>%
  group_by(Name, TRU) %>%
  summarise(
    Main = mean(Main_Worker_Percentage, na.rm = TRUE),
    `Marginal 3-6` = mean(Marginal_3_6_Percentage, na.rm = TRUE),
    `Marginal 0-3` = mean(Marginal_0_3_Percentage, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(
    cols = c("Main", "Marginal 3-6", "Marginal 0-3"),
    names_to = "Worker_Type",
    values_to = "Percentage"
  ) %>%
  mutate(
    Worker_Type = factor(Worker_Type, 
                         levels = c("Main", "Marginal 3-6", "Marginal 0-3")),
    Name = factor(Name, 
                  levels = (df_wfpr %>% group_by(Name) %>% 
                              summarise(m = mean(Main_Worker_Percentage, na.rm = TRUE)) %>% 
                              arrange(m) %>% pull(Name)))
  )

# Faceted by TRU
plot4 <- ggplot(composition_data, aes(x = Percentage, y = Name, fill = Worker_Type)) +
  geom_col(position = "fill") +
  facet_wrap(~TRU, ncol = 3) +
  scale_fill_manual(
    values = c("Main" = "#2E86AB", "Marginal 3-6" = "#A23B72", "Marginal 0-3" = "#F18F01"),
    name = "Worker Type"
  ) +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title = "Employment Composition by State and Area Type",
    subtitle = "Percentage breakdown: Main vs Marginal (3-6) vs Marginal (0-3) workers",
    x = "Percentage of Total Workers",
    y = "State",
    caption = "Data Source: OGD Census Data\nShows proportion of stable vs precarious employment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold", color = "white"),
    strip.background = element_rect(fill = "#333333", color = NA),
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.caption = element_text(size = 8, color = "#888888", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot4)
ggsave("04_employment_composition_stacked.png", plot4, width = 13, height = 10, dpi = 300)

# ============================================================================
# VISUALIZATION 5: Scatter Plot - Distress vs Quality
# ============================================================================

scatter_data <- df_wfpr %>%
  group_by(Name) %>%
  summarise(
    Labor_Distress = mean(Labor_Distress_Index, na.rm = TRUE),
    Employment_Quality = mean(Employment_Quality_Index, na.rm = TRUE),
    Main_Workers = mean(Principal_Status_Workers, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Classify by quadrant
  mutate(
    Quadrant = ifelse(Labor_Distress > median(Labor_Distress, na.rm = TRUE),
                      ifelse(Employment_Quality > median(Employment_Quality, na.rm = TRUE),
                             "High Distress, Good Quality",
                             "High Distress, Poor Quality"),
                      ifelse(Employment_Quality > median(Employment_Quality, na.rm = TRUE),
                             "Low Distress, Good Quality",
                             "Low Distress, Poor Quality"))
  )

plot5 <- ggplot(scatter_data, aes(x = Labor_Distress, y = Employment_Quality)) +
  geom_point(aes(size = Main_Workers, color = Quadrant), alpha = 0.6) +
  geom_hline(yintercept = median(scatter_data$Employment_Quality, na.rm = TRUE), 
             linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_vline(xintercept = median(scatter_data$Labor_Distress, na.rm = TRUE), 
             linetype = "dashed", color = "gray", alpha = 0.5) +
  ggrepel::geom_text_repel(aes(label = Name), size = 3, max.overlaps = 20) +
  scale_color_manual(
    values = c(
      "High Distress, Good Quality" = "#F39C12",
      "High Distress, Poor Quality" = "#E74C3C",
      "Low Distress, Good Quality" = "#27AE60",
      "Low Distress, Poor Quality" = "#95A5A6"
    ),
    name = "Quadrant"
  ) +
  labs(
    title = "Labor Market Distress vs Employment Quality",
    subtitle = "State positioning: Distress Index vs Quality Index",
    x = "Labor Distress Index (Higher = More Marginal Workers)",
    y = "Employment Quality Index (Higher = Better Quality)",
    size = "Main Workers",
    caption = "Data Source: OGD Census Data\nBubble size = Number of main workers\nIdeal position: Top-Left (Low distress, High quality)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0),
    axis.title = element_text(size = 10, face = "bold"),
    legend.position = "right",
    plot.caption = element_text(size = 8, color = "#888888", hjust = 0),
    panel.grid = element_line(color = "#e0e0e0", linewidth = 0.2),
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot5)
ggsave("05_distress_vs_quality_scatter.png", plot5, width = 12, height = 9, dpi = 300)

# ============================================================================
# SUMMARY STATISTICS & ANALYSIS
# ============================================================================

cat("\n\n========== WORKFORCE PARTICIPATION RATE SUMMARY STATISTICS ==========\n")

# Overall statistics
cat("\nOVERALL WFPR STATISTICS (All States & Areas):\n")
cat("Principal Status Rate (%):\n")
cat("  Mean:", round(mean(df_wfpr$Principal_Status_Rate, na.rm = TRUE), 2), "\n")
cat("  Median:", round(median(df_wfpr$Principal_Status_Rate, na.rm = TRUE), 2), "\n")
cat("  SD:", round(sd(df_wfpr$Principal_Status_Rate, na.rm = TRUE), 2), "\n\n")

cat("UPSS Proxy Rate (%):\n")
cat("  Mean:", round(mean(df_wfpr$UPSS_Proxy_Rate, na.rm = TRUE), 2), "\n")
cat("  Median:", round(median(df_wfpr$UPSS_Proxy_Rate, na.rm = TRUE), 2), "\n")
cat("  SD:", round(sd(df_wfpr$UPSS_Proxy_Rate, na.rm = TRUE), 2), "\n\n")

cat("Marginal 0-3 Rate (%):\n")
cat("  Mean:", round(mean(df_wfpr$Marginal_0_3_Rate, na.rm = TRUE), 2), "\n")
cat("  Median:", round(median(df_wfpr$Marginal_0_3_Rate, na.rm = TRUE), 2), "\n")
cat("  SD:", round(sd(df_wfpr$Marginal_0_3_Rate, na.rm = TRUE), 2), "\n\n")

cat("Labor Distress Index:\n")
cat("  Mean:", round(mean(df_wfpr$Labor_Distress_Index, na.rm = TRUE), 2), "\n")
cat("  Median:", round(median(df_wfpr$Labor_Distress_Index, na.rm = TRUE), 2), "\n\n")

# By TRU
cat("\nWFPR BY AREA TYPE (TRU):\n")
tru_stats <- df_wfpr %>%
  group_by(TRU) %>%
  summarise(
    Principal_Rate = round(mean(Principal_Status_Rate, na.rm = TRUE), 2),
    UPSS_Rate = round(mean(UPSS_Proxy_Rate, na.rm = TRUE), 2),
    Marginal_0_3_Rate = round(mean(Marginal_0_3_Rate, na.rm = TRUE), 2),
    Distress_Index = round(mean(Labor_Distress_Index, na.rm = TRUE), 2),
    Quality_Index = round(mean(Employment_Quality_Index, na.rm = TRUE), 2),
    .groups = 'drop'
  )
print(tru_stats)

# Top 5 states with HIGHEST distress (most precarity)
cat("\n\nTOP 5 STATES WITH HIGHEST LABOR DISTRESS:\n")
top_distress <- df_wfpr %>%
  group_by(Name) %>%
  summarise(
    Distress_Index = round(mean(Labor_Distress_Index, na.rm = TRUE), 2),
    Main_Workers_Pct = round(mean(Main_Worker_Percentage, na.rm = TRUE), 2),
    Marginal_0_3_Pct = round(mean(Marginal_0_3_Percentage, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(Distress_Index)) %>%
  head(5)
print(top_distress)

# Top 5 states with LOWEST distress (most stability)
cat("\n\nTOP 5 STATES WITH LOWEST LABOR DISTRESS:\n")
low_distress <- df_wfpr %>%
  group_by(Name) %>%
  summarise(
    Distress_Index = round(mean(Labor_Distress_Index, na.rm = TRUE), 2),
    Main_Workers_Pct = round(mean(Main_Worker_Percentage, na.rm = TRUE), 2),
    Marginal_0_3_Pct = round(mean(Marginal_0_3_Percentage, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(Distress_Index) %>%
  head(5)
print(low_distress)

# Top 5 states with BEST employment quality
cat("\n\nTOP 5 STATES WITH BEST EMPLOYMENT QUALITY:\n")
good_quality <- df_wfpr %>%
  group_by(Name) %>%
  summarise(
    Quality_Index = round(mean(Employment_Quality_Index, na.rm = TRUE), 2),
    Main_Workers_Pct = round(mean(Main_Worker_Percentage, na.rm = TRUE), 2),
    Distress_Index = round(mean(Labor_Distress_Index, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(Quality_Index)) %>%
  head(5)
print(good_quality)

# ============================================================================
# EXPORT DETAILED RESULTS
# ============================================================================

# Create comprehensive summary table
summary_table <- df_wfpr %>%
  group_by(Name, TRU) %>%
  summarise(
    Principal_Status_Rate = round(mean(Principal_Status_Rate, na.rm = TRUE), 2),
    UPSS_Proxy_Rate = round(mean(UPSS_Proxy_Rate, na.rm = TRUE), 2),
    Marginal_0_3_Rate = round(mean(Marginal_0_3_Rate, na.rm = TRUE), 2),
    Total_Worker_Rate = round(mean(Total_Worker_Rate, na.rm = TRUE), 2),
    Main_Worker_Percentage = round(mean(Main_Worker_Percentage, na.rm = TRUE), 2),
    Marginal_3_6_Percentage = round(mean(Marginal_3_6_Percentage, na.rm = TRUE), 2),
    Marginal_0_3_Percentage = round(mean(Marginal_0_3_Percentage, na.rm = TRUE), 2),
    Labor_Distress_Index = round(mean(Labor_Distress_Index, na.rm = TRUE), 2),
    Employment_Quality_Index = round(mean(Employment_Quality_Index, na.rm = TRUE), 2),
    Unemployment_Proxy = round(mean(Unemployment_Proxy, na.rm = TRUE), 2),
    .groups = 'drop'
  )

# Export to CSV
write.csv(summary_table, "wfpr_summary_analysis.csv", row.names = FALSE)
cat("\n\n✓ Summary table exported to: wfpr_summary_analysis.csv\n")

# Export full calculated dataframe
write.csv(df_wfpr, "wfpr_full_calculations.csv", row.names = FALSE)
cat("✓ Full calculations exported to: wfpr_full_calculations.csv\n")

cat("\n========== ANALYSIS COMPLETE ==========\n")

# ============================================================================
# KEY INSIGHTS & INTERPRETATION GUIDE
# ============================================================================

cat("\n\n========== KEY INSIGHTS & INTERPRETATION ==========\n\n")

cat("1. LABOR DISTRESS INDEX (Marginal 0-3 / Main Workers × 100):\n")
cat("   - Index > 100 = More marginal workers than main workers (HIGH DISTRESS)\n")
cat("   - Index 50-100 = Significant marginal workforce (MODERATE DISTRESS)\n")
cat("   - Index < 50 = Mostly stable employment (LOW DISTRESS)\n\n")

cat("2. EMPLOYMENT QUALITY INDEX:\n")
cat("   - Higher values = Greater gap between main and marginal workers\n")
cat("   - Index > 80 = EXCELLENT employment quality\n")
cat("   - Index 60-80 = GOOD employment quality\n")
cat("   - Index < 40 = POOR employment quality\n\n")

cat("3. PARTICIPATION RATES:\n")
cat("   - Principal Status Rate = Stable, long-term employment\n")
cat("   - UPSS Proxy Rate = Includes semi-employed (worked 3-6 months)\n")
cat("   - Marginal 0-3 Rate = Very short-term, precarious employment\n\n")

cat("4. RESEARCH FINDINGS:\n")
cat("   - States with high distress index need labor support policies\n")
cat("   - States with low quality index need employment stabilization\n")
cat("   - Rural areas typically show higher marginal employment\n\n")