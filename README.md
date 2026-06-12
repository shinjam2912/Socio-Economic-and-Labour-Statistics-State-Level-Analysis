# Socio-Demographic and Labor Market Analysis of Indian States
### 2011 Primary Census Abstract (PCA) Dataset

---

## What Is It About

This project analyzes India's labor market and socio-demographic structure using data from the **2011 Census of India** (Primary Census Abstract / PCA table). The data covers **35 states and union territories** and includes population, literacy, worker categories, age groups, and more.

The goal was to understand:
- How literacy affects the *quality* (not just quantity) of employment
- Which states have high labor distress and why
- How dependent states are on agriculture, and how that links to literacy and gender
- What drives urbanization across Indian states

---

## Dataset

| Detail | Info |
|---|---|
| Source | [OGD Open Government Data Platform](https://data.gov.in) |
| Census Year | 2011 |
| Table | Primary Census Abstract (PCA) |
| Coverage | 35 States and Union Territories |
| Variables | 49 columns |
| Scope | State/UT level, broken down by Total / Rural / Urban (TRU) and Gender |

Key columns include: Total Population, Literates/Illiterates, Main Workers, Marginal Workers (0–3 months, 3–6 months), Cultivators, Agricultural Labourers, Household Industry Workers, Other Workers, Non-Working Population, Age Group 0–6, Scheduled Castes/Tribes population.

---

## Research Questions

**RQ1 — Literacy and Employment Quality**
> Are literate people more likely to be employed for 6+ months (stable jobs)?

**RQ2 — Unemployment and Labor Market Status**
> What is the labor distress / unemployment status of each state, measured using NSSO's UPSS and CWS classification methods?

**RQ3 — Agricultural Dependency**
> How agriculturally dependent is each state? How do literacy and gender link to agricultural employment?

Two additional topics were also explored:
- **Dependency Ratio** — the economic burden on the working population
- **Urbanization Drivers** — what sector of work drives urbanization across states

---

## Data Cleaning 

The raw dataset needed the following cleaning steps before analysis:

1. **Missing values** — all blank cells were replaced with `NA`
2. **Outlier imputation** — 4 erroneous values were replaced with column medians
3. **Removed national aggregates** — the first 3 rows (All-India totals) were dropped so all analysis is at state/UT level only
4. **Derived new columns** — the following columns were computed and added:
   - Dependency Ratio
   - UPSS Proxy Rate
   - Unemployment Proxy
   - Urbanization Rate
   - Labor Distress Index
   - Literacy Rate

---

## Analyses and Key Findings

### 1. Agricultural Dependency per State (RQ3)

- States like **Bihar, Chhattisgarh, and Madhya Pradesh** have the highest share of workers in agriculture (60–70%)
- Urban UTs like **Chandigarh, Delhi, and Lakshadweep** have near-zero agricultural engagement
- Every state with low agricultural dependency has a literacy rate **above 86%**
- **Hypothesis tested:** Does higher literacy reduce agricultural dependency?
  - Result: **Strongly validated** — correlation r = −0.68, p ≈ 0
- **Gender gap finding:** Bihar (gap = 16.42) and UP (gap = 16.57) show high male-dominated agricultural work. UP's gap is even *higher* than Bihar's despite better literacy, showing that social/cultural factors matter beyond education alone

---

### 2. Literacy Rate vs Working Status (RQ1)

| Correlation | r value | Meaning |
|---|---|---|
| Literacy vs Total Workers | −0.006 | Near zero — literacy doesn't affect *how many* people work |
| Literacy vs Main Workers (stable) | +0.304 | Positive — literate states have more stable 6+ month jobs |
| Literacy vs Marginal Workers (unstable) | −0.440 | Negative — illiterate states have more precarious work |

**Conclusion:** Literacy doesn't create more jobs — it improves the *quality and stability* of jobs. Investing in literacy is an anti-precarity measure.

---

### 3. Dependency Ratio (Additional Topic)

The dependency ratio measures how many non-workers are supported by every 100 workers.

> Formula used: `Non-Working Population / Total Worker Population × 100`

- **National average: 155** — meaning 155 non-workers depend on every 100 workers nationwide
- Almost half of all states are above this average
- **Highest dependency:** Daman & Diu (207), Odisha (201), Jharkhand (178)
- **Lowest dependency:** Uttarakhand (98.7), Punjab (123), Haryana (129)
- High dependency is driven by: large young populations (0–6), high unemployment, migration of workers out of states

---

### 4. Workforce Participation and Labor Distress Index (RQ2)

A **Labor Distress Index** was constructed as a proxy for employment precarity:

> Formula: `(Marginal Workers 0–3 months + Marginal Workers 3–6 months) / Main Workers × 100`

This was validated with strong correlations:
- Distress vs Principal Status Rate: **r = −0.70** (as distress rises, stable jobs fall)
- Distress vs Marginal 0–3 Rate: **r = +0.90** (as distress rises, precarious work rises)

**Most distressed states:**
| Rank | State | Distress Index |
|---|---|---|
| 1 | Lakshadweep | 23.66 |
| 2 | Himachal Pradesh | 17.0 |
| 3 | Jharkhand | 16.22 |

> Notable: Lakshadweep has a **91.85% literacy rate** but very high distress — showing education alone is not enough without matching job creation.

**Healthiest labor markets:**
| Rank | State | Distress Index |
|---|---|---|
| 1 | Chandigarh | 1.07 |
| 2 | Daman & Diu | 1.25 |

**Rural vs Urban distress:** Rural distress (9.19) is **3× higher** than urban distress (3.08).

---

### 5. Urbanization and Sector Dynamics 

| Sector | Correlation with Urbanization |
|---|---|
| Service / Other Workers | **+0.816** (strong positive) |
| Agricultural Workers | **−0.814** (strong negative) |
| Household Industries | −0.299 (weak negative) |

**Conclusion:** The service sector is the primary driver of urbanization. States that want to reduce agricultural dependency must build service sector capacity — not just move away from agriculture.

---

## Overall Conclusion

India's labor market challenges are deeply interconnected:

**Low literacy → High agricultural dependency → Seasonal/precarious work → High distress → High dependency ratios**

Breaking this cycle requires simultaneous investment in education, industrial development, and gender-inclusive employment policies.

---

## Tools and Language

- **Language:** R
- **Libraries used:** ggplot2, dplyr, tidyr, corrplot (see `packages.R` for full list)
- **Analysis type:** Descriptive statistics, correlation analysis, Pearson hypothesis tests, proxy metric construction

---

## Repository Structure

```
├── dataset/          # Raw and cleaned Census 2011 PCA data
├── scripts/          # R scripts for cleaning and each analysis
├── report/           # Full project report (PDF)
├── README.md         # This file
└── packages.R        # R packages required to run the scripts
```

---

## Data Source

Office of the Registrar General & Census Commissioner, India.
*Primary Census Abstract (PCA) — Census of India 2011.*
Available at: [https://data.gov.in](https://data.gov.in)

---

## Authors

| Shinjan Majumdar | BSDCC2519 |
| Rachapudi Chinmay | BSDBG2514 |
| Mahima Yadav | BSDDH2512 |
| Guravana Lakshmi Narayana Naidu | BSDBG2508 |
| Abhinav | BSDDH2502 |

