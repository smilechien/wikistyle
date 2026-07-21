# JMIR Deep Demo App

Shiny app for comparing workbook-style CNN and ANN prediction pipelines together with standard prediction algorithms on bundled demo files or uploaded `.xlsm` workbooks.

## Files

- `app.R`: main Shiny application
- `Ch05PrembinaryCNNverticle.xlsm`: bundled CNN demo workbook
- `Ch032dimensionANNverticle.xlsm`: bundled ANN demo workbook

## What The App Does

- Runs the bundled CNN and ANN demos
- Runs uploaded workbook data in original or normalized mode
- Compares independent algorithms by ROC-AUC
- Builds feature forest plots
- Builds importance-based top-10 variable selection
- Shows prediction mode with a category probability curve (PCC)
- Exports forest-style code blocks for QSubgrouptest-style plotting

## Model Modes

### CNN

CNN in this app is the workbook pipeline, not a modern image library CNN.

Steps:

1. Read the source workbook
2. Recode the first status column into two columns such as `1 0` and `0 1`
3. Expand the data through the workbook-style `dataabc` process
4. Run the selected prediction algorithm on the expanded feature matrix

### ANN

ANN in this app is a direct tabular neural-network workflow.

Steps:

1. Keep the first column as the original status column
2. Use columns `2..n` as predictor variables
3. Prefer the workbook `trainning` sheet as the ANN engine
4. Fall back to the R neural-net engine only if workbook ANN weights are not usable

## Supported Algorithm Comparison

The comparison section can include:

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

Comparison is shown as:

- `Original Variables`
- `Top 10 Variables`

Top 5 ranking is based on test ROC-AUC.

## Running The App

Open R in the project folder and run:

```r
shiny::runApp("app.R")
```

Or inside R:

```r
source("app.R")
```

## R Package Requirements

Core packages used by the app include:

- `shiny`
- `bslib`
- `DT`
- `openxlsx`

Optional packages improve algorithm coverage:

- `nnet`
- `MASS`
- `class`
- `rpart`
- `randomForest` or `ranger`
- `e1071`

If an optional package is missing, the related algorithm may show as unavailable.

## Using The App

### Bundled Demo Buttons

- `Load bundled demo button(CNN)`: opens the bundled CNN workbook
- `Load bundled demo button(ANN)`: opens the bundled ANN workbook
- `Run selected algorithm on demo CSV`: runs the appended ANN demo CSV and also builds the combined comparison view

### Uploaded Prediction

There are two uploaded-data run modes:

- `Prediction model (Original data)`: runs directly from workbook `data2`-style input
- `Prediction model (Normalization data)`: z-score normalizes predictor columns first, then runs the model

### AUC Tab

The `AUC` tab shows:

- AUC summary
- confusion table
- ROC points table
- ROC curve

### Forest And Importance Tabs

- `Forest`: feature-level forest summary
- `Importance`: backward elimination by test ROC-AUC until 10 variables remain

### Comparison Tab

Shows independent algorithm comparison, not a pipeline-paired table.

It includes:

- table of testing and training metrics
- Top 5 methods by test ROC-AUC
- split forest-style ROC-AUC comparison for `Original` and `Top 10`

### Prediction Mode / PCC

Prediction mode uses the importance-selected variables when available.

The PCC panel shows:

- the current prediction-mode algorithm
- the category probability curve
- current score and class probabilities

## Workbook Expectations

For uploaded ANN-style data:

- first column = outcome/status
- first row = variable names
- remaining columns = predictors

For uploaded CNN-style data:

- the first status column is transformed into two columns before workbook-style expansion

## Notes

- ANN demo CSV now uses the 15-column sample file, so the model feature count is `14`
- Forest export text areas are included for QSubgrouptest-style external plotting
- The app keeps the workbook-oriented behavior as closely as possible while presenting results in Shiny
