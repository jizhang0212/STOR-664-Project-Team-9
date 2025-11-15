# STOR 664 Project – Ames Housing Analysis - Team 9

## Team Members
- **Joe Zhou** (@mzhouUNC)  
- **Ji Zhang** (@jizhang0212)  
- **Tiger (Jinghao) Teng** (@TigerTeng1024)

## Overview
This repository contains our group project for **STOR 664 (Fall 2025)**.  
Our goal is to identify and evaluate the key determinants of **housing prices in Ames, Iowa**. Using the Ames Housing Dataset, we analyze how factors such as home size, overall quality, age, neighborhood, and various structural or amenity features contribute to the sale price of a home.

Part 1 focuses on exploratory data analysis, literature review, data concerns, and the development of an analysis plan.  
Part 2 will extend this work by fitting multiple predictive models—including linear regression, lasso regression, and nonlinear methods—to explain and predict SalePrice and log(SalePrice).

## Repository Structure

| Folder | Purpose | Key Files |
|--------|---------|-----------|
| `/data/raw` | Original unmodified datasets | `train.csv`, `test.csv` |
| `/data/processed` | Cleaned datasets used for modeling | (generated in Part 2) |
| `/src` | Code for data exploration, cleaning, and modeling | (generated in Part 2) |
| `/results/figures` | Plots and visualizations | `corr_heatmap.png` |
| `/results/tables` | Model output summaries | regression tables, metrics |
| `/reports` | Written deliverables | Part1 Report, Part2 Final Report |
| Root folder | Documentation | `README.md` |

## Getting Started

### 1. Clone the repository
```bash
git clone https://github.com/mzhouUNC/stor-664-project-team9
cd stor-664-project-team9/
```

### 2. Install dependencies
Python example:
```bash
pip install -r requirements.txt
```

R example:
```r
renv::restore()
```

### 3. Run analysis scripts
```bash
python src/eda.py
```
