# Socio-Demographic & Labor Statistics Analysis
## OGD Census Data Visualization & Analysis

This repository contains R code and visualizations for analyzing **Dependency Ratios** and **Workforce Participation Rates (WFPR)** using Indian Census data from the Open Government Data (OGD) Platform.

---

## 📊 Overview

Two comprehensive analyses exploring:
1. **Dependency Ratio Analysis** - Understanding the economic burden on working populations
2. **Workforce Participation Rate Analysis** - Measuring employment stability and quality across states

---

## 🎯 Research Questions

### Analysis 1: Dependency Ratio
**"How many non-working individuals depend on every 100 working individuals?"**

This helps policymakers understand:
- Social security needs
- Labor force adequacy
- Population support capacity

### Analysis 2: Workforce Participation Rate
**"How does employment stability differ across states when comparing stable vs precarious jobs?"**

This helps identify:
- Labor market health
- Employment quality
- Regional disparities in job stability

---


## 📈 Analysis 1: Dependency Ratio Analysis

### What is a Dependency Ratio?

A **dependency ratio** tells us how many people are NOT working for every 100 people WHO ARE working.

**Simple Example:**
- If a state has 100 workers
- And 70 non-workers (children, elderly, unemployed)
- The dependency ratio = 70
- Meaning: 70 dependents per 100 workers

---

### Variables Calculated

#### 1. **Total Dependency Ratio**
- **What it is:** Compares ALL non-working people to workers
- **Why it matters:** Shows total economic burden
- **Formula:** (Non-Working Population ÷ Total Workers) × 100
- **Interpretation:**
  - Ratio < 50 = Low burden (workers can support easily)
  - Ratio 50-100 = Moderate burden
  - Ratio > 100 = High burden (more dependents than workers!)

#### 2. **Young Dependency Ratio**
- **What it is:** Focuses only on children aged 0-6 years
- **Why it matters:** Shows need for childcare & education
- **Formula:** (Children 0-6 Population ÷ Total Workers) × 100
- **Interpretation:**
  - High ratio = Need for schools, childcare facilities
  - Low ratio = Aging population (fewer young children)

#### 3. **Old/Other Dependency Ratio** (Calculated)
- **What it is:** Dependents BEYOND the 0-6 age group
- **Why it matters:** Includes elderly, disabled, other non-workers
- **How we calculate:** Total Dependency - Young Dependency
- **Interpretation:**
  - Shows aging population burden
  - Indicates need for eldercare services

---

### Visualizations (4 Charts)

#### **Chart 1: Horizontal Bar Chart (States Ranked)**
Shows which states have the highest dependency burden.
- **X-axis:** Dependency Ratio value
- **Y-axis:** State names (sorted by burden)
- **Color:** Darker = Higher burden
- **Use:** Identify states needing policy support

#### **Chart 2: Faceted Bar Charts (Total/Rural/Urban)**
Shows how burden differs in city vs village areas.
- **3 separate charts:** One each for Total, Rural, Urban
- **Use:** Understand geographic differences within states
- **Key insight:** Rural areas often have higher child dependency

#### **Chart 3: Heat Map (State × Area Type)**
Quick visual pattern recognition across all combinations.
- **Rows:** States
- **Columns:** Total/Rural/Urban
- **Color intensity:** Higher burden = Darker color
- **Use:** Spot problem areas at a glance

#### **Chart 4: Stacked Bar Chart (Components)**
Shows what drives the dependency - young vs other.
- **Red portion:** Children (0-6 years)
- **Teal portion:** Other non-workers
- **Use:** Decide what policies are needed
  - High red = Need schools & childcare
  - High teal = Need eldercare & healthcare

---

### Key Insights from Dependency Analysis

| Finding | What It Means | Policy Response |
|---------|--------------|-----------------|
| High young dependency | Many children, fewer workers | Invest in schools, childcare |
| High old dependency | Aging population | Invest in healthcare, pensions |
| Rural > Urban | Villages have more burden | Rural development programs |
| State variation | Some states manage better | Learn from low-ratio states |

---

## 📊 Analysis 2: Workforce Participation Rate (WFPR) Analysis

### What is Workforce Participation?

**Workforce Participation** measures how many people in a population have jobs, and **how stable** those jobs are.

**Key Concept:** Not all jobs are equal!
- Some jobs are stable (year-round)
- Some jobs are temporary (few months)
- Some jobs are seasonal (very temporary)

---

### Employment Categories (Used in this Analysis)

#### 1. **Main Workers (Principal Status)**
- **What:** People who worked for 6+ months continuously
- **Stability:** VERY STABLE (most of the year)
- **Examples:** Government employees, permanent factory workers, established business owners
- **Why important:** Shows core stable employment
- **Formula:** Count of Main Workers ÷ Total Population × 100

#### 2. **Marginal Workers 3-6 Months**
- **What:** People who worked 3-6 months in the past year
- **Stability:** SEMI-STABLE (half the year)
- **Examples:** Agricultural workers during harvest, seasonal laborers
- **Why important:** Shows semi-employment, not quite stable
- **Formula:** (Marginal 3-6 Workers ÷ Total Population) × 100

#### 3. **Marginal Workers 0-3 Months**
- **What:** People who worked less than 3 months in the past year
- **Stability:** VERY UNSTABLE (barely any work)
- **Examples:** Day laborers, occasional gig workers, unemployed part of year
- **Why important:** Shows precarity and unemployment
- **Formula:** (Marginal 0-3 Workers ÷ Total Population) × 100

#### 4. **Non-Workers**
- **What:** People with zero work in the past year
- **Stability:** NO EMPLOYMENT
- **Examples:** Students, homemakers, unemployed, elderly not working
- **Why important:** Shows complete unemployment

---

### Variables Calculated (WFPR Analysis)

#### 1. **Principal Status Rate**
- **What it measures:** % of population with stable jobs (6+ months)
- **Formula:** (Main Workers ÷ Total Population) × 100
- **Example:** 25% = 25 out of 100 people have stable year-round jobs
- **Interpretation:**
  - > 30% = Good employment
  - 20-30% = Moderate employment
  - < 20% = Low stable employment

#### 2. **UPSS Proxy Rate**
- **What it measures:** % with either stable OR semi-stable jobs
- **Includes:** Main workers + Marginal 3-6 workers
- **Formula:** (Main + Marginal 3-6 Workers ÷ Total Population) × 100
- **Why it matters:** More realistic view of employed population
- **Example:** 32% have some regular employment pattern

#### 3. **Marginal 0-3 Rate**
- **What it measures:** % with very unstable/precarious jobs
- **Formula:** (Marginal 0-3 Workers ÷ Total Population) × 100
- **Interpretation:**
  - < 10% = Low precarity (good)
  - 10-20% = Moderate precarity
  - > 20% = High precarity (concern)

#### 4. **Main Worker Percentage**
- **What it measures:** % of total workers with stable jobs
- **Formula:** (Main Workers ÷ Total Workers) × 100
- **Example:** If 60% of all workers are "main workers" = 60% have stable jobs
- **Interpretation:**
  - > 60% = Mostly stable workforce (healthy)
  - 40-60% = Mixed workforce
  - < 40% = Mostly precarious (unhealthy)

#### 5. **Labor Distress Index** ⭐ (Key Metric)
- **What it measures:** How much more precarious workers vs stable workers
- **Formula:** (Marginal 0-3 Workers ÷ Main Workers) × 100
- **Example:**
  - If 100 stable workers, 50 precarious workers
  - Index = (50÷100)×100 = 50
- **Interpretation:**
  - < 25 = Low distress (HEALTHY)
  - 25-50 = Moderate distress
  - 50-100 = High distress (CONCERN)
  - > 100 = More precarious than stable workers! (CRITICAL)

#### 6. **Employment Quality Index** ⭐ (Key Metric)
- **What it measures:** Gap between stable and precarious employment
- **Formula:** (Main - Marginal 0-3) ÷ Main × 100
- **Example:**
  - 100 main workers, 20 precarious workers
  - Index = (100-20)÷100×100 = 80
- **Interpretation:**
  - > 80 = Excellent quality (80%+ are stable)
  - 60-80 = Good quality
  - 40-60 = Fair quality (mixed)
  - < 40 = Poor quality (too many precarious)

#### 7. **Unemployment Proxy**
- **What it measures:** Non-workers relative to total workers
- **Formula:** (Non-Working Population ÷ Total Workers) × 100
- **Why it matters:** Shows unemployment rate (proxy)

---

### Visualizations (5 Charts)

#### **Chart 1: Principal vs UPSS vs Marginal Rates**
Compares three different ways of measuring employment.
- **Blue bars:** Principal Status (stable only)
- **Pink bars:** UPSS Proxy (stable + semi-stable)
- **Orange bars:** Marginal 0-3 (very unstable)
- **Use:** See how many people fall in each employment category
- **Key insight:** Gap between UPSS and Principal = semi-employed people

#### **Chart 2: Labor Distress Index**
Shows which states have the most precarious employment.
- **X-axis:** Distress Index value
- **Y-axis:** States (ranked by distress)
- **Color:** Darker = More distress
- **Use:** Identify states needing labor support
- **Key insight:** High index = Many unstable jobs = Policy needed

#### **Chart 3: Employment Quality Index**
Shows overall job quality by state.
- **Color-coded:** 
  - Green = Excellent quality
  - Blue = Good quality
  - Yellow = Fair quality
  - Red = Poor quality
- **Red line:** Quality threshold (50)
- **Use:** Assess employment stability

#### **Chart 4: Employment Composition (100% Stacked)**
Shows proportion of Main vs Marginal workers by state and area.
- **Faceted by:** Total, Rural, Urban
- **Color breakdown:**
  - Blue = Main workers (stable)
  - Pink = Marginal 3-6 (semi-stable)
  - Orange = Marginal 0-3 (unstable)
- **Use:** Understand employment type mix
- **Key insight:** Rural often has more orange (precarious)

#### **Chart 5: Distress vs Quality (Scatter Plot)**
Shows relationship between labor distress and employment quality.
- **X-axis:** Labor Distress Index
- **Y-axis:** Employment Quality Index
- **Quadrants:**
  - Top-Left = IDEAL (Low distress, High quality)
  - Top-Right = PARADOX (High distress, but still good quality)
  - Bottom-Left = GOOD (Low distress, adequate quality)
  - Bottom-Right = CRITICAL (High distress, poor quality)
- **Use:** Identify states by priority for intervention

---

### Key Insights from WFPR Analysis

| Finding | Meaning | Policy Priority |
|---------|---------|-----------------|
| High distress index | Many unstable jobs | Job creation/stabilization |
| Low quality index | Few stable jobs | Employment program investment |
| High marginal 0-3 | Many seasonal/gig workers | Labor protections, income support |
| Rural > Urban marginal | Village jobs less stable | Rural employment programs |
| State variation | Different labor challenges | State-specific solutions |

---

## 🔗 Relationship Between the Two Analyses

### How They Work Together:

**Dependency Ratio** answers:
- "How many people need support?"

**Workforce Participation** answers:
- "How stable is the support system (jobs)?"

### Combined Insight:
A state with:
- ✅ Low dependency ratio + High employment quality = HEALTHY
- ⚠️ High dependency ratio + Low employment quality = CRITICAL
- 🔴 High dependency ratio + High labor distress = URGENT NEED

---

## 💻 How to Use This Analysis

### For Policy Makers:
1. Look at **Dependency Ratio** to understand burden
2. Check **Labor Distress Index** to see job stability
3. Use **visualizations** to present to stakeholders

### For Researchers:
1. Run **both analyses** on your data
2. Compare **states** using the metrics
3. Export **CSV files** for further analysis

### For Students:
1. Understand the **calculations** (provided above)
2. Learn **data visualization** techniques
3. See **real census data** analysis

---

## 📊 Data Requirements

**Essential Columns:**
```
Name                                    (State name)
TRU                                     (Total/Rural/Urban indicator)
Total Population Person                 (Total population)
Total Worker Population Person          (All workers)
Main Working Population Person          (Stable jobs, 6+ months)
Marginal Worker Population 3_6 Person   (Semi-employed, 3-6 months)
Marginal Worker Population 0_3 Person   (Very unstable, <3 months)
Non Working Population Person           (Non-workers)
Population in the age group 0-6 Person  (For dependency ratio only)
```

---

## 🚀 Quick Start

### Step 1: Load Your Data
```r
setwd("C:/Users/YourName/Documents/Your_Dataset_Folder")

df <- read.csv("your_census_data.csv", 
               fileEncoding = "UTF-16LE")  # Adjust encoding if needed

head(df)  # Check if loaded correctly
```

### Step 2: Run Dependency Ratio Analysis
```r
source("dependency_ratio_analysis.R")
```

### Step 3: Run Workforce Participation Analysis
```r
source("wfpr_analysis_with_df.R")
```

### Step 4: Check Output Files
```
01_horizontal_bar_dependency_ratio.png
02_faceted_bar_tru_dependency.png
03_heatmap_state_tru_dependency.png
04_stacked_bar_dependency_components.png

01_wfpr_principal_vs_upss.png
02_labor_distress_index.png
03_employment_quality_index.png
04_employment_composition_stacked.png
05_distress_vs_quality_scatter.png

dependency_ratio_summary.csv
wfpr_summary_analysis.csv
```

---

## 📚 Understanding the Output

### CSV Files Explained

**dependency_ratio_summary.csv:**
- State-wise dependency ratios
- Breakdowns by Total/Rural/Urban
- Ready for Excel pivot tables

**wfpr_summary_analysis.csv:**
- Principal Status, UPSS, and Marginal rates
- Labor Distress and Employment Quality indices
- Unemployment proxy by state

### PNG Files Explained

Each visualization tells a story:
- **Use charts for presentations** to stakeholders
- **Export to reports** for policy recommendations
- **Print for posters** in offices/institutions

---

## 🔍 Interpretation Examples

### Example 1: Reading Dependency Ratio Chart
```
State: Bihar
Dependency Ratio: 85

What it means:
- For every 100 workers in Bihar
- 85 people depend on them (children, elderly, unemployed)
- This is HIGH (above 50)
- Policy needed: Education, healthcare, job creation
```

### Example 2: Reading Labor Distress Index
```
State: Punjab
Distress Index: 18

What it means:
- For every 100 stable workers
- Only 18 people have precarious jobs
- This is LOW distress (below 50) = GOOD
- Most people have stable employment
```

### Example 3: Reading Employment Quality Index
```
State: Odisha
Quality Index: 42

What it means:
- Gap between stable and unstable workers is only 42%
- This indicates POOR quality
- Many unstable jobs relative to stable jobs
- Policy needed: Job stabilization programs
```

---

## 📖 Learn More

### Files in This Repository:
- `README_DEPENDENCY_ANALYSIS.md` - Detailed dependency guide
- `WFPR_INTEGRATION_GUIDE.md` - Detailed WFPR guide
- `DATA_LOADING_GUIDE.md` - How to load different data formats

### References:
- **Data Source:** Open Government Data (OGD) Platform
- **Census:** 2011 Indian Census, Table PCA
- **NSSO:** UPSS definition and methodology

---

## ✅ Checklist Before Running

- [ ] R and RStudio installed
- [ ] Required packages installed (`tidyverse`, `ggplot2`, `dplyr`, `scales`, `viridis`)
- [ ] CSV file in correct location
- [ ] File encoding correct (usually UTF-16LE)
- [ ] Column names match exactly
- [ ] Working directory set to data folder

---

## 🆘 Troubleshooting

### Issue: "Column not found"
**Solution:** Check column names match exactly
```r
colnames(df)  # List all columns
```

### Issue: File encoding errors
**Solution:** Try different encoding
```r
df <- read.csv("file.csv", fileEncoding = "UTF-16LE")
# Try: "UTF-16", "windows-1252", "latin1"
```

### Issue: Plots don't show
**Solution:** Ensure ggplot2 is loaded
```r
library(ggplot2)
```


