# JMIR Deep Demo App

A Shiny application for reproducing the workbook-style `CNN` and `ANN` demo pipelines, comparing them with common prediction algorithms, and exploring ROC-AUC, feature forests, importance filtering, validation summaries, and prediction-mode probability curves.

## Overview

This app is designed around the project workbooks and demo CSV used in the JMIR deep-learning-style workflow. It supports:

- bundled `CNN` and `ANN` demo runs
- uploaded CSV prediction data
- ROC-AUC summaries and workbook-style ROC tables
- feature extraction and forest plots
- backward elimination to a final top-10 variable set
- algorithm comparison tables and comparison forest plots
- prediction mode with a category probability curve (PCC)
- QSubgrouptest export text blocks for external forest plotting

## Project Files

- `app.R`: main Shiny application
- `README.md`: project README shown both on GitHub and in the app README tab
- `Ch05PrembinaryCNNverticle.xlsm`: bundled CNN demo workbook
- `Ch032dimensionANNverticleCNN.xlsm`: bundled ANN demo workbook
- `Ch032dimensionANNverticle_data2_demo.csv`: bundled ANN demo CSV used by the demo CSV button and sample upload testing

## Algorithm Groups

### Machine Learning Methods

The app compares the following methods as machine-learning or predictive modeling methods:

- `CNN`
- `ANN`
- `Logistic Regression`
- `Linear Regression Score`
- `KNN`
- `Naive Bayes`
- `LDA`
- `QDA`
- `Decision Tree`
- `Neural Net`
- `Random Forest`
- `SVM`

### Neural-Network-Based Methods

These methods are neural-network-based in the current app:

- `ANN`
- `Neural Net`

### Deep Learning

The current app does **not** implement a modern deep-learning framework such as TensorFlow or PyTorch.

Important interpretation notes:

- `CNN` in this project refers to the workbook-specific pipeline name, not a modern convolutional image network.
- `ANN` is a workbook-style or surrogate shallow neural-network path, not a deep multi-layer architecture.
- `Neural Net` uses an R neural-network implementation and should be described as neural-network-based machine learning rather than deep learning.

## Data Expectations

### ANN-Style Data

For ANN-style input in the app:

- row 1 contains variable names
- column 1 contains the outcome or status
- columns `2..n` contain predictor variables

### CNN-Style Data

For CNN-style input in the app:

- the first status column is transformed into two indicator columns
- the transformed data then follow the workbook-style expansion path before model fitting

## CNN vs ANN in This App

### CNN Pipeline

`CNN` here is the workbook transformation pipeline.

Steps:

1. Read the source demo or workbook data.
2. Recode the status column into paired indicator columns such as `1 0` and `0 1`.
3. Expand features through the workbook-style `dataabc` process.
4. Fit the selected prediction algorithm on the expanded feature matrix.

### ANN Pipeline

`ANN` here is the direct tabular prediction path.

Steps:

1. Keep the original status column as column 1.
2. Use columns `2..n` as predictors.
3. Run the ANN-style prediction path on the tabular matrix.
4. For the demo CSV and uploaded sample CSV consistency path, the app uses the same `Neural Net probability path` for validation.

## Main App Sections

### Predictions

Displays the main prediction output table for the currently loaded result.

### ROC-AUC Summary

Shows a compact summary table of ROC-AUC information.

### AUC

The AUC section includes:

- AUC summary
- confusion table
- ROC points table
- workbook-style ROC sheet layout
- ROC curve

### Feature Extraction

The feature-extraction section summarizes:

- stepwise feature screening
- grouped positive and negative variable scoring
- extracted feature tables
- grouped-score box plot

### Forest

The forest section shows the retained variable forest plot and forest table.

### Importance

The importance section shows:

- backward elimination by ROC-AUC
- final top-10 variables
- importance forest plot
- importance summary tables

### Comparison

The comparison section shows:

- Top 5 methods by test ROC-AUC
- full algorithm comparison table
- algorithm comparison forest plot
- QSubgrouptest comparison export blocks

Comparison is shown for:

- `Original` variables
- `Top 10` variables

### Prediction Mode

Prediction mode allows manual value entry using the currently selected variable set and model path. It also shows:

- the displayed fitting algorithm
- class probabilities
- PCC curve
- composite score interpretation text

### Validation

Validation includes:

- full-data AUC
- training/test split AUC
- 5-fold cross-validation summary
- validation confusion table
- validation prediction table

## Running the App

From R:

```r
shiny::runApp("F:/jmirdeep")
```

Or if your working directory is the project folder:

```r
shiny::runApp()
```

## Package Requirements

Required packages:

- `shiny`
- `bslib`
- `DT`
- `readxl`
- `openxlsx`

Optional algorithm packages:

- `nnet`
- `MASS`
- `class`
- `rpart`
- `randomForest` or `ranger`
- `e1071`
- `pROC`

If an optional package is missing, the related model may be unavailable or may fall back to a simpler path.

## Current Behavior Notes

- The ANN demo CSV path is relative so the app can be deployed more safely.
- Uploaded sample CSV and demo sample CSV are aligned to the same ANN validation path.
- Comparison rows are deduplicated so algorithms are not repeated unnecessarily in the comparison table and forest.
- The app is workbook-oriented and aims to preserve the original project logic while exposing results in Shiny.

## Known UI Notes

- `README` is currently exposed through the in-app README tab using `includeMarkdown("README.md")`.
- If the running app still shows older behavior after code changes, fully stop and restart the Shiny session.

## External Forest Export

The app provides exportable text blocks for use with:

- [QSubgrouptest.asp](https://www.raschonline.com/kpiall/QSubgrouptest.asp)

These blocks are intended for external forest-plot rendering workflows.
