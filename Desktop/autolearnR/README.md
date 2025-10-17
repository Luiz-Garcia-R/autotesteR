# autolearnR

<!-- badges: start -->
<!-- badges: end -->

autolearnR is an R package designed to provide an automated first-pass evaluation of your dataset to suggest the most appropriate machine learning or clustering method.
It inspects data structure, balance, normality, and feature characteristics, returning a clear recommendation for the main algorithm and alternatives.

*Note:* The current version (0.0.1) includes only the pre.learn() function. Later versions will add full model training and evaluation workflows.

## Installation

You can install the development version directly from GitHub:

``` r
# Install devtools if you haven't already
install.packages("devtools")

# Install autolearnR from GitHub
devtools::install_github("Luiz-Garcia-R/autolearnR")
```

## Input Data Format (Very Important)

To use pre.learn(), you need two input data frames:

  1. raw_data – features or predictors
    - Must contain a first column called Sample
    - Other columns must contain numeric or categorical data

Example of raw_data:

| Sample | Var1 | Var2 | Var3 |
| ------ | ---- | ---- | ---- |
| S1     | 5.1  | 10.2 | A    |
| S2     | 4.8  | 9.5  | B    |
| S3     | 5.5  | 10.0 | A    |
| S4     | 5.0  | 10.3 | B    |


  2. metadata – describes your samples and experimental groups
    Must contain at least two columns:
    - Sample: matches exactly the first column of raw_data
    - Output: target variable for supervised learning

Example of metadata:

| Sample | Group   | Output  |
| ------ | ------- | ------- |
| S1     | Control | Control |
| S2     | Control | Control |
| S3     | Case    | Case    |
| S4     | Case    | Case    |


## Main Function

pre.learn()

The pre.learn() function provides:
  - Automatic detection of task type: classification, regression, or clustering
  - Checks for class balance, normality of numeric variables, presence of categorical features
  - Column-level diagnostics: counts of missing values (NAs) and zero-variance features
  - Heuristic recommendations for the most suitable main and alternative algorithms

Example

``` r
library(autolearnR)

# Run pre-learn analysis
result <- pre.learn(raw_data, metadata)
```

Output:
  - Suggested main algorithm (e.g., Random Forest, Logistic Regression, k-means)
  - Suggested alternative algorithms
  - Reasoning behind suggestions
  - Diagnostic summary of your data

``` r
result$suggested_main          # Main algorithm
result$suggested_alternatives  # Alternative algorithms
result$diagnostic              # Balance, normality, NAs, zero-variance columns
```

## Contact

For questions, suggestions, or contributions, open an issue or pull request on GitHub.

Thank you for using autolearnR!
