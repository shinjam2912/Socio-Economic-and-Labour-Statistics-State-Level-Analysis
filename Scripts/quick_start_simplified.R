# QUICK START - Simplified Dependency Ratio Analysis
# Use this if you want to test quickly with sample data

library(tidyverse)
library(ggplot2)

# ============================================================================
# CREATE SAMPLE DATA (Replace with your actual data)
# ============================================================================
set.seed(123)

# Sample states
states <- c("Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", 
            "Chhattisgarh", "Goa", "Gujarat", "Haryana", "Himachal Pradesh", 
            "Jharkhand", "Karnataka", "Kerala", "Madhya Pradesh", "Maharashtra", 
            "Manipur", "Meghalaya", "Mizoram", "Nagaland", "Odisha", "Punjab",
            "Rajasthan", "Sikkim", "Tamil Nadu", "Telangana", "Tripura", 
            "Uttar Pradesh", "Uttarakhand", "West Bengal")

# Create data
data <- expand.grid(
  State = states,
  TRU = c("Total", "Rural", "Urban")
) %>%
  mutate(
    Total_Workers = sample(1000000:20000000, n(), replace = TRUE),
    Non_Workers = sample(500000:15000000, n(), replace = TRUE),
    Young_0_6 = sample(100000:2000000, n(), replace = TRUE)
  )

# ============================================================================
# CALCULATE DEPENDENCY RATIO
# ============================================================================
data <- data %>%
  mutate(
    Dependency_Ratio = (Non_Workers / Total_Workers) * 100,
    Young_Ratio = (Young_0_6 / Total_Workers) * 100,
    Other_Ratio = Dependency_Ratio - Young_Ratio
  )

# ============================================================================
# PLOT 1: Horizontal Bar Chart
# ============================================================================
p1_data <- data %>%
  group_by(State) %>%
  summarise(Avg_Ratio = mean(Dependency_Ratio)) %>%
  arrange(Avg_Ratio) %>%
  mutate(State = factor(State, levels = State))

p1 <- ggplot(p1_data, aes(x = Avg_Ratio, y = State)) +
  geom_col(aes(fill = Avg_Ratio)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Dependency Ratio by State (Ranked)",
       x = "Dependency Ratio", y = "State",
       caption = "Higher = More dependents per 100 workers") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

print(p1)

# ============================================================================
# PLOT 2: Faceted by TRU
# ============================================================================
p2_data <- data %>%
  group_by(State, TRU) %>%
  summarise(Ratio = mean(Dependency_Ratio)) %>%
  ungroup() %>%
  mutate(State = factor(State, 
    levels = (data %>% filter(TRU == "Total") %>% 
      group_by(State) %>% summarise(r = mean(Dependency_Ratio)) %>% 
      arrange(r) %>% pull(State))))

p2 <- ggplot(p2_data, aes(x = Ratio, y = State, fill = TRU)) +
  geom_col(position = "dodge") +
  facet_wrap(~TRU, ncol = 3) +
  scale_fill_manual(values = c("Total" = "#2E86AB", "Rural" = "#A23B72", "Urban" = "#F18F01")) +
  labs(title = "Dependency Ratio: Total vs Rural vs Urban",
       x = "Dependency Ratio", y = "State") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7), strip.text = element_text(face = "bold"))

print(p2)

# ============================================================================
# PLOT 3: Heat Map
# ============================================================================
p3_data <- data %>%
  group_by(State, TRU) %>%
  summarise(Ratio = mean(Dependency_Ratio)) %>%
  ungroup() %>%
  mutate(TRU = factor(TRU, levels = c("Total", "Rural", "Urban")))

p3 <- ggplot(p3_data, aes(x = TRU, y = State, fill = Ratio)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = round(Ratio, 1)), color = "white", size = 2.5, fontface = "bold") +
  scale_fill_viridis_c(option = "magma") +
  labs(title = "Dependency Ratio Heat Map: State × Area Type",
       x = "Area Type", y = "State") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7), axis.text.x = element_text(size = 9, face = "bold"))

print(p3)

# ============================================================================
# PLOT 4: Stacked Bar (Components)
# ============================================================================
p4_data <- data %>%
  group_by(State) %>%
  summarise(
    Young = mean(Young_Ratio),
    Other = mean(Other_Ratio)
  ) %>%
  arrange(Young + Other) %>%
  mutate(State = factor(State, levels = State)) %>%
  pivot_longer(cols = c(Young, Other), names_to = "Type", values_to = "Ratio")

p4 <- ggplot(p4_data, aes(x = Ratio, y = State, fill = Type)) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = c("Young" = "#FF6B6B", "Other" = "#4ECDC4"),
    labels = c("Young" = "Young (0-6)", "Other" = "Other Non-Workers")
  ) +
  labs(title = "Dependency Components: Young vs Other Non-Workers",
       x = "Dependency Ratio", y = "State", fill = "Type") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

print(p4)

# ============================================================================
# QUICK STATISTICS
# ============================================================================
cat("\n===== DEPENDENCY RATIO SUMMARY =====\n")
cat("Overall Mean:", round(mean(data$Dependency_Ratio), 2), "\n")
cat("Overall Median:", round(median(data$Dependency_Ratio), 2), "\n")
cat("Overall SD:", round(sd(data$Dependency_Ratio), 2), "\n\n")

cat("By Area Type:\n")
data %>%
  group_by(TRU) %>%
  summarise(Mean = round(mean(Dependency_Ratio), 2),
            Median = round(median(Dependency_Ratio), 2)) %>%
  print()

cat("\n\nTop 5 States (Highest Dependency):\n")
data %>%
  group_by(State) %>%
  summarise(Avg = round(mean(Dependency_Ratio), 2)) %>%
  arrange(desc(Avg)) %>%
  head(5) %>%
  print()

cat("\n\nBottom 5 States (Lowest Dependency):\n")
data %>%
  group_by(State) %>%
  summarise(Avg = round(mean(Dependency_Ratio), 2)) %>%
  arrange(Avg) %>%
  head(5) %>%
  print()
