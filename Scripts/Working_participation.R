# ============================================================================
# WORKFORCE PARTICIPATION RATE (WFPR) ANALYSIS
# Using your imported dataframe df (OGD Census Data)
# ============================================================================

getwd()
setwd("C:/Users/RBI1/Documents/R_Basics")
df <- read.csv("population_perSTATE.csv", check.names = FALSE)
df<-df[-c(1,2,3),] ##remove India Data from states
#now dataset is cleaned
View(df)

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

str(df)


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
    Labor_Distress_Index = ( `Marginal Worker Population 0_3 Person`/ `Main Working Population Person`) * 100,
    
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

View(df_wfpr)

# Show sample calculations
cat("Sample calculations (first few rows):\n")
print(df_wfpr %>% 
        select(Name, TRU, 
               Principal_Status_Rate, UPSS_Proxy_Rate, Marginal_0_3_Rate,
               Labor_Distress_Index, Employment_Quality_Index) %>%
        head(10))



# ============================================================================
# HYPOTHESIS TESTING: Distress Index vs Literacy Rate
# Statistical Validation of Labor Distress Metric
# ============================================================================
# STEP 1: SETUP & HYPOTHESIS STATEMENT
# ============================================================================

#"'States with higher Distress Index should have lower literacy

##"NULL HYPOTHESIS (H0):
##ρ = 0 (No correlation between Distress Index and Literacy Rate)

##ALTERNATIVE HYPOTHESIS (H1):
##ρ < 0 (Negative correlation: Higher distress → Lower literacy)
##SIGNIFICANCE LEVEL (α): α = 0.05 (95% confidence level)
##"EXPECTED OUTCOME:
##"Correlation coefficient < -0.6 (Strong negative)
##"P-value < 0.05 (Statistically significant)

# ============================================================================
# STEP 2: PREPARE DATA
# ============================================================================


# Aggregate data by state (average across TRU)
hypothesis_data <- df_wfpr %>%
  filter(TRU=="Total") %>%
  group_by(Name) %>%
  summarise(
    
    Distress_Index = mean(
      ((`Marginal Worker Population 0_3 Person` + `Marginal Worker Population 3_6 Person`) / 
         `Main Working Population Person`) * 100, 
      na.rm = TRUE
    ),
    
    # Literacy Rate
    Literacy_Rate = (`Literates Population Person` / 
                       (`Total Population Person` - `Population in the age group 0-6 Person`)) * 100,
    .groups = 'drop'
  ) %>%
  # Remove rows with missing values
  filter(!is.na(Distress_Index) & !is.na(Literacy_Rate)) %>%
  # Add observation count
  mutate(Observation_Number = row_number())

cat("Data prepared:\n")
cat("Number of states:", nrow(hypothesis_data), "\n")
cat("Variables:", ncol(hypothesis_data) - 1, "\n\n")

# Display the data
cat("State-Level Data:\n")
print(hypothesis_data %>% select(Name, Literacy_Rate, Distress_Index))
# ============================================================================
# STEP 3: DESCRIPTIVE STATISTICS
# ============================================================================

cat("\n\n========== DESCRIPTIVE STATISTICS ==========\n\n")

cat("LITERACY RATE STATISTICS:\n")
cat("─────────────────────────────────────────────────────\n")
cat("Mean:    ", round(mean(hypothesis_data$Literacy_Rate, na.rm = TRUE), 2), "%\n")
cat("Median:  ", round(median(hypothesis_data$Literacy_Rate, na.rm = TRUE), 2), "%\n")
cat("SD:      ", round(sd(hypothesis_data$Literacy_Rate, na.rm = TRUE), 2), "%\n")
cat("Min:     ", round(min(hypothesis_data$Literacy_Rate, na.rm = TRUE), 2), "%\n")
cat("Max:     ", round(max(hypothesis_data$Literacy_Rate, na.rm = TRUE), 2), "%\n\n")

cat("DISTRESS INDEX STATISTICS:\n")
cat("─────────────────────────────────────────────────────\n")
cat("Mean:    ", round(mean(hypothesis_data$Distress_Index, na.rm = TRUE), 2), "\n")
cat("Median:  ", round(median(hypothesis_data$Distress_Index, na.rm = TRUE), 2), "\n")
cat("SD:      ", round(sd(hypothesis_data$Distress_Index, na.rm = TRUE), 2), "\n")
cat("Min:     ", round(min(hypothesis_data$Distress_Index, na.rm = TRUE), 2), "\n")
cat("Max:     ", round(max(hypothesis_data$Distress_Index, na.rm = TRUE), 2), "\n\n")

# ============================================================================
# STEP 4: PEARSON CORRELATION TEST
# ============================================================================

cat("\n========== PEARSON CORRELATION TEST ==========\n")
cat("(Testing for linear relationship)\n\n")

# Perform correlation test
correlation_test <- cor.test(
  hypothesis_data$Literacy_Rate,
  hypothesis_data$Distress_Index,
  method = "pearson",
  alternative = "less"  # One-tailed: we expect negative correlation
)

# Extract results
r_coefficient <- correlation_test$estimate
p_value <- correlation_test$p.value
conf_int_lower <- correlation_test$conf.int[1]
conf_int_upper <- correlation_test$conf.int[2]
t_statistic <- correlation_test$statistic
df_test <- correlation_test$parameter

cat("CORRELATION COEFFICIENT (r):\n")
cat("─────────────────────────────────────────────────────\n")
cat("r =", round(r_coefficient, 4), "\n\n")

cat("INTERPRETATION OF r:\n")
if(abs(r_coefficient) > 0.7) {
  cat("✓ STRONG correlation\n")
} else if(abs(r_coefficient) > 0.5) {
  cat("✓ MODERATE correlation\n")
} else if(abs(r_coefficient) > 0.3) {
  cat("✓ WEAK correlation\n")
} else {
  cat("✓ VERY WEAK correlation\n")
}
cat(round(abs(r_coefficient)^2 * 100, 2), "% of variance explained\n\n")

cat("P-VALUE (Significance):\n")
cat("─────────────────────────────────────────────────────\n")
cat("p-value =", round(p_value, 4), "\n")

if(p_value < 0.001) {
  cat("Significance: *** (p < 0.001) - HIGHLY SIGNIFICANT\n")
} else if(p_value < 0.01) {
  cat("Significance: ** (p < 0.01) - VERY SIGNIFICANT\n")
} else if(p_value < 0.05) {
  cat("Significance: * (p < 0.05) - SIGNIFICANT\n")
} else {
  cat("Significance: NS (Not Significant, p ≥ 0.05)\n")
}
cat("\n")

cat("95% CONFIDENCE INTERVAL FOR r:\n")
cat("─────────────────────────────────────────────────────\n")
cat("[", round(conf_int_lower, 4), ", ", round(conf_int_upper, 4), "]\n")
cat("(If this interval doesn't include 0, correlation is significant)\n\n")

cat("TEST STATISTICS:\n")
cat("─────────────────────────────────────────────────────\n")
cat("t-statistic =", round(t_statistic, 4), "\n")
cat("Degrees of freedom =", df_test, "\n\n")

# ============================================================================
# STEP 5: HYPOTHESIS DECISION
# ============================================================================

cat("\n========== HYPOTHESIS TEST DECISION ==========\n\n")

cat("CRITERIA FOR VALIDATION:\n")
cat("─────────────────────────────────────────────────────\n")
cat("✓ Expected correlation: r < -0.6\n")
cat("✓ Expected p-value: p < 0.05\n\n")

cat("ACTUAL RESULTS:\n")
cat("─────────────────────────────────────────────────────\n")
cat("Observed correlation: r =", round(r_coefficient, 4), "\n")
cat("Observed p-value: p =", round(p_value, 4), "\n\n")

cat("DECISION:\n")
cat("─────────────────────────────────────────────────────\n")

# Check correlation strength
if(r_coefficient < -0.6) {
  decision_corr <- "✓ STRONG NEGATIVE"
  strong_enough <- TRUE
} else if(r_coefficient < -0.4) {
  decision_corr <- "⚠ MODERATE NEGATIVE"
  strong_enough <- FALSE
} else if(r_coefficient < 0) {
  decision_corr <- "⚠ WEAK NEGATIVE"
  strong_enough <- FALSE
} else {
  decision_corr <- "✗ NOT NEGATIVE"
  strong_enough <- FALSE
}

# Check significance
if(p_value < 0.05) {
  decision_sig <- "✓ SIGNIFICANT"
  significant <- TRUE
} else {
  decision_sig <- "✗ NOT SIGNIFICANT"
  significant <- FALSE
}

cat("Correlation strength:", decision_corr, "\n")
cat("Statistical significance:", decision_sig, "\n\n")

# Final verdict
if(strong_enough & significant) {
  cat("CONCLUSION: ✓✓✓ HYPOTHESIS VALIDATED\n")
  cat("─────────────────────────────────────────────────────\n")
  cat("The metric is CONFIRMED to be valid.\n")
  cat("Higher Distress Index → Lower Literacy\n")
  cat("Relationship is STRONG and STATISTICALLY SIGNIFICANT\n")
  validation_result <- "VALIDATED"
} else if(significant) {
  cat("CONCLUSION: ✓ HYPOTHESIS PARTIALLY VALIDATED\n")
  cat("─────────────────────────────────────────────────────\n")
  cat("Relationship is statistically significant (p < 0.05)\n")
  cat("But correlation strength is", abs(r_coefficient), "not < -0.6\n")
  cat("Still shows meaningful relationship, just weaker than expected\n")
  validation_result <- "PARTIALLY VALIDATED"
} else {
  cat("CONCLUSION: ✗ HYPOTHESIS NOT VALIDATED\n")
  cat("─────────────────────────────────────────────────────\n")
  cat("Relationship is NOT statistically significant\n")
  cat("Cannot confirm the hypothesized relationship\n")
  validation_result <- "NOT VALIDATED"
}

cat("\n")

# ============================================================================
# STEP 6: VISUALIZATION 1 - SCATTER PLOT WITH CONFIDENCE INTERVAL
# ============================================================================

cat("Creating visualizations...\n\n")

# Calculate linear regression for confidence band
lm_model <- lm(Distress_Index ~ Literacy_Rate, data = hypothesis_data)

# Create prediction data for confidence interval
literacy_range <- seq(min(hypothesis_data$Literacy_Rate, na.rm = TRUE),
                      max(hypothesis_data$Literacy_Rate, na.rm = TRUE),
                      length.out = 100)

prediction_data <- data.frame(Literacy_Rate = literacy_range)

# Get predictions with confidence interval
predictions <- predict(lm_model, 
                       newdata = prediction_data,
                       interval = "confidence",
                       level = 0.95)

prediction_data <- cbind(prediction_data, predictions)

# Create scatter plot with confidence band
plot1 <- ggplot(hypothesis_data, aes(x = Literacy_Rate, y = Distress_Index)) +
  # Confidence interval band (light blue)
  geom_ribbon(
    data = prediction_data,
    aes(x = Literacy_Rate, ymin = lwr, ymax = upr, y = NULL),
    fill = "#3498DB",
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  # Regression line
  geom_smooth(method = "lm", se = FALSE, color = "#E74C3C", 
              linetype = "solid", linewidth = 1.2) +
  # Scatter points
  geom_point(aes(color = Distress_Index, size = 3), alpha = 0.7, show.legend = FALSE) +
  # State labels
  geom_text(aes(label = Name), size = 2.5, hjust = 0.5, vjust = -0.8, alpha = 0.7) +
  # Color scale
  scale_color_viridis_c(option = "plasma", direction = -1) +
  # Labels
  labs(
    title = "Hypothesis Test: Distress Index vs Literacy Rate",
    subtitle = sprintf("Pearson r = %.3f (p = %.4f) | 95%% Confidence Interval (Shaded)\n%s",
                       r_coefficient, p_value, validation_result),
    x = "Literacy Rate (%)",
    y = "Labor Distress Index",
    caption = "Data Source: OGD Census Data\nShaded area = 95% Confidence band for regression line\nRed line = Linear regression fit"
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

# ============================================================================
# STEP 7: VISUALIZATION 2 - RESIDUALS PLOT (Checking Assumptions)
# ============================================================================

# Add fitted values and residuals
hypothesis_data$fitted <- fitted(lm_model)
hypothesis_data$residuals <- residuals(lm_model)

plot2 <- ggplot(hypothesis_data, aes(x = fitted, y = residuals)) +
  # Points
  geom_point(aes(color = Literacy_Rate), size = 3, alpha = 0.7) +
  # Reference line at y=0
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  # Smooth line through residuals
  geom_smooth(method = "loess", se = FALSE, color = "#2ECC71", 
              linetype = "dashed", linewidth = 0.8) +
  # Color scale
  scale_color_viridis_c(option = "plasma", name = "Literacy Rate") +
  # Labels
  labs(
    title = "Residuals Plot: Checking Model Assumptions",
    subtitle = "Points should be randomly distributed around the red line\nIf smooth curve is flat → Linear assumption is valid",
    x = "Fitted Values",
    y = "Residuals",
    caption = "Checks: Linearity & Homoscedasticity\nGreen curve shows trend in residuals"
  ) +
  # Styling
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0),
    axis.title = element_text(size = 11, face = "bold"),
    panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "#888888", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot2)

# ============================================================================
# STEP 8: VISUALIZATION 3 - CORRELATION STRENGTH SUMMARY
# ============================================================================

# Create a summary visualization of correlation strength
summary_data <- data.frame(
  Metric = c("Observed\nCorrelation", "Expected\nThreshold"),
  Value = c(r_coefficient, -0.6),
  Type = c("Observed", "Expected")
)

plot3 <- ggplot(summary_data, aes(x = Metric, y = Value, fill = Type)) +
  # Bars
  geom_col(position = "dodge", width = 0.6, alpha = 0.8) +
  # Value labels
  geom_text(aes(label = round(Value, 3)), vjust = -1, size = 4, fontface = "bold") +
  # Reference line at -0.6
  geom_hline(yintercept = -0.6, linetype = "dashed", color = "red", 
             linewidth = 1.2, alpha = 0.7) +
  # Y-axis range
  ylim(-0.8, 0.2) +
  # Colors
  scale_fill_manual(
    values = c("Observed" = "#3498DB", "Expected" = "#E74C3C"),
    name = "Type"
  ) +
  # Labels
  labs(
    title = "Correlation Strength: Observed vs Expected",
    subtitle = "Red dashed line = Expected threshold (-0.6)",
    y = "Correlation Coefficient (r)",
    x = "",
    caption = "For strong validation, observed should be ≤ -0.6"
  ) +
  # Styling
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#555555", hjust = 0),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "right",
    panel.grid.major.y = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(size = 9, color = "#888888", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  )

print(plot3)

# ============================================================================
# STEP 9: REGRESSION SUMMARY & EQUATION
# ============================================================================

cat("LINEAR REGRESSION MODEL:\n")
cat("Distress_Index ~ Literacy_Rate\n\n")

summary_lm <- summary(lm_model)
print(summary_lm)

cat("\n\nREGRESSION EQUATION:\n")
intercept <- coef(lm_model)[1]
slope <- coef(lm_model)[2]

cat("Distress Index =", round(intercept, 4), "+", round(slope, 4), "× Literacy Rate\n\n")

cat("INTERPRETATION:\n")
cat("For every 1% increase in literacy rate,\n")
cat("Distress Index DECREASES by", round(abs(slope), 4), "units\n\n")

cat("R-SQUARED (Goodness of Fit):\n")
cat("─────────────────────────────────────────────────────\n")
r_squared <- summary_lm$r.squared
cat("R² =", round(r_squared, 4), "\n")
cat("This means", round(r_squared * 100, 2), "% of variation in Distress Index\n")
cat("is explained by Literacy Rate\n\n")








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
#ggsave("01_wfpr_principal_vs_upss.png", plot1, width = 13, height = 10, dpi = 300)

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
#ggsave("02_labor_distress_index.png", plot2, width = 12, height = 10, dpi = 300)









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
#ggsave("03_employment_quality_index.png", plot3, width = 12, height = 10, dpi = 300)

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
#ggsave("04_employment_composition_stacked.png", plot4, width = 13, height = 10, dpi = 300)

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
#ggsave("05_distress_vs_quality_scatter.png", plot5, width = 12, height = 9, dpi = 300)

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

