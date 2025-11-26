# RFM Analysis & Customer Purchase Prediction

## Project Overview
This project utilizes multivariate statistical analysis and machine learning to analyze retail customer behavior. The study is divided into two main parts:
1.  **Customer Segmentation**: Using **RFM (Recency, Frequency, Monetary)** analysis to classify customers into distinct value groups.
2.  **Purchase Prediction**: Building predictive models to classify customers based on their likelihood of accepting a marketing campaign offer.

## Author
* **Name**: WANG CHAO AN
* **Department**: Department of Economics
* **Course**: STAT 5008 Multivariate Statistical Analysis

## Repository Structure
```text
├── datasets/
│   ├── OnlineRetail.csv       # Transactional data
│   └── Superstore.csv         # Store dataset
├── Q1plot/                    # Visualization outputs (Scree plots, Box plots)
├── RFM_analysis.R             # R script for Clustering & RFM calculation
├── prediction.R               # R script for Predictive Modeling
├── RFM_preprocess.ipynb       # Data cleaning and preprocessing
└── README.md
```

## Tools & Libraries
* **R**: stats, MASS, class, e1071, randomForest, adabag
* **Python**: pandas, numpy (for preprocessing)
