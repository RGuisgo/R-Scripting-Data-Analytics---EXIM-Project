# EXIM Bank – Foreign Corporate Rating Model Revision (R)

## 📌 Overview
This project revises the foreign corporate risk rating model for the Hungarian EXIM Bank.  
Risk rating models are used to estimate the likelihood of default and are vital tools in risk management, underwriting, capital allocation, and portfolio management.  
They provide a structured, objective method for classifying borrowers or securities based on creditworthiness and default risk.

- **Data Source:** Orbis Database  
- **Coverage:** African companies (subset of global dataset)  
- **Period:** 2013–2021  
- **Focus:** Excludes companies in Banking, Insurance, Financial Services, Public Administration, Education, and Health/Social Services sectors to ensure consistency in financial ratios  

---

## 🥅 Objective
- Revise and enhance the foreign corporate risk rating model  
- Analyze financial indicators to assess default risk  
- Assign objective risk scores for portfolio management and loan decision-making  

---

## 🛠️ Data Processing
1. Collected financial and firm-level data from the **Orbis Database**, including revenue, employment, fixed capital, ownership structure, and industry.  
2. Focused on **world regions**, with this project limited to Africa due to storage constraints.  
3. Merged datasets for all indicators into a single dataframe.  
4. Split attribute columns into `date` and `variable` for analysis.  
5. Excluded sectors with atypical financial ratios (e.g., Banking, Public Administration).  
6. Reshaped data into long and wide formats and removed duplicates.  

---

## 🔍 Methods
- **Descriptive Statistics:** Summarized dataset characteristics using central tendency, variability, and frequency measures.  
- **Boxplots & Histograms:** Assessed distribution, skewness, and dispersion across indicators.  
- **Scoring:**  
  - Assigned each company a score from **1 to 7** (septile) for each indicator.  
  - Septiles divide an ordered sample into seven equal parts, allowing for standardized comparison of companies across indicators.  

---

## 🚀 Outcome
- Provides a revised, data-driven corporate rating model for EXIM Bank  
- Supports loan decision-making and portfolio risk management  
- Demonstrates proficiency in R, data cleaning, descriptive analytics, and structured scoring systems  

---

## 🛠️ Tools
- **Language:** R  
- **Packages:** tidyverse, dplyr, ggplot2  

---

## ▶️ How to Run
1. Open the main R script in RStudio.  
2. Execute sequentially to reproduce data cleaning, descriptive analysis, and scoring.  
3. Output includes scored datasets and visualizations of distributions and boxplots.  
