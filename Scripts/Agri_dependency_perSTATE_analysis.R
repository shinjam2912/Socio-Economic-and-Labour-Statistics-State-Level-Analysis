# ============================================================================
# AGRICULTURAL DEPENDENCY PER STATE ANALYSIS
# ============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# ----------------------------------------------------------------------------
# 1. LOAD DATA (exactly as you have)
# ----------------------------------------------------------------------------
getwd()
setwd("C:/Users/RBI1/Documents/R_Basics")
df <- read.csv("population_perSTATE.csv", check.names = FALSE)
df <- df[-c(1,2,3), ]          # remove India-level rows

# ----------------------------------------------------------------------------
# 2. CALCULATE AGRICULTURAL DEPENDENCY METRICS
# ----------------------------------------------------------------------------
df_agri <- df %>%
  filter(TRU == "Total") %>%                     # Focus on state-level total first
  mutate(
    # Direct involvement in agriculture (main workers only = stable income)
    Agri_Workers_Person = `Main Cultivator Population Person` + 
      `Main Agricultural Labourers Population Person`,
    
    # Proportion of TOTAL POPULATION directly involved in agriculture
    Agri_Population_Share = (Agri_Workers_Person / `Total Population Person`) * 100,
    
    # Standard workforce share (most common metric in literature)
    Main_Workers_Person = `Main Working Population Person`,
    Agri_Workforce_Share = (Agri_Workers_Person / Main_Workers_Person) * 100,
    
    # Effective Literacy Rate (7+ years) - for "why" analysis
    Literacy_Rate = (`Literates Population Person` / 
                       (`Total Population Person` - `Population in the age group 0-6 Person`)) * 100,
    
    # Gender-specific agricultural dependency (very insightful)
    Agri_Workers_Male = `Main Cultivator Population Male` + 
      `Main Agricultural Labourers Population Male`,
    Agri_Workers_Female = `Main Cultivator Population Female` + 
      `Main Agricultural Labourers Population Female`,
    
    Agri_Population_Share_Male = (Agri_Workers_Male / `Total Population Male`) * 100,
    Agri_Population_Share_Female = (Agri_Workers_Female / `Total Population Female`) * 100
  ) %>%
  select(Name, TRU, 
         Agri_Population_Share, Agri_Workforce_Share,
         Literacy_Rate,
         Agri_Population_Share_Male, Agri_Population_Share_Female,
         Agri_Workers_Person, `Total Population Person`,
         `Main Working Population Person`) %>%
  arrange(desc(Agri_Workforce_Share))

# ----------------------------------------------------------------------------
# 3. SUMMARY TABLE (Top & Bottom States)
# ----------------------------------------------------------------------------
cat("=== AGRICULTURAL DEPENDENCY PER STATE (Ranked) ===\n")
print(df_agri %>% 
        select(Name, Agri_Population_Share, Agri_Workforce_Share, Literacy_Rate) %>%
        head(10))   # Top 10 most agriculture-dependent

cat("\nBottom 5 (least dependent):\n")
print(df_agri %>% 
        select(Name, Agri_Population_Share, Agri_Workforce_Share, Literacy_Rate) %>%
        tail(5))

# ----------------------------------------------------------------------------
# 4. VISUALISATION 1: Bar chart of Agricultural Workforce Share
# ----------------------------------------------------------------------------
ggplot(df_agri, aes(x = reorder(Name, Agri_Workforce_Share), 
                    y = Agri_Workforce_Share)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.85) +
  coord_flip() +
  labs(title = "Agricultural Dependency by State (2011 Census)",
       subtitle = "% of Main Workers engaged in Agriculture (Cultivators + Agri Labourers)",
       x = "State / UT", 
       y = "Agricultural Workforce Share (%)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 9))

# ----------------------------------------------------------------------------
# 5. VISUALISATION 2: Scatterplot - Literacy vs Agricultural Dependency
# ----------------------------------------------------------------------------
ggplot(df_agri, aes(x = Literacy_Rate, y = Agri_Workforce_Share)) +
  geom_point(size = 3, color = "darkred") +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  geom_text(aes(label = Name), size = 3, vjust = -1, check_overlap = TRUE) +
  labs(title = "Does Higher Literacy Reduce Agricultural Dependency?",
       subtitle = "Negative relationship expected",
       x = "Effective Literacy Rate (%)",
       y = "Agricultural Workforce Share (%)") +
  theme_minimal()

# ----------------------------------------------------------------------------
# 6. CORRELATION (the "WHY" part)
# ----------------------------------------------------------------------------
cor_test <- cor.test(df_agri$Literacy_Rate, df_agri$Agri_Workforce_Share, 
                     method = "pearson", alternative = "less")

cat("\n=== CORRELATION: Literacy vs Agricultural Dependency ===\n")
cat("Correlation coefficient (r) =", round(cor_test$estimate, 4), "\n")
cat("p-value =", round(cor_test$p.value, 4), "\n")
cat(ifelse(cor_test$p.value < 0.05, "→ Statistically significant negative relationship", 
           "→ Not significant"))

# ----------------------------------------------------------------------------
# 7. Optional: Gender Gap in Agricultural Dependency
# ----------------------------------------------------------------------------
gender_gap <- df_agri %>%
  mutate(Gender_Gap_Agri = Agri_Population_Share_Male - Agri_Population_Share_Female)

cat("\nStates with largest gender gap in agricultural population share:\n")
print(gender_gap %>% 
        select(Name, Gender_Gap_Agri) %>%
        arrange(desc(Gender_Gap_Agri)) %>% 
        head(8))

