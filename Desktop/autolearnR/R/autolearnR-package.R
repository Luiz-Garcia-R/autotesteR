#' autolearnR: Quick Reference and Main Function for Automated Learning Suggestions
#'
#' autolearnR provides an automated first-pass evaluation of datasets to guide
#' users in selecting the most suitable machine learning or clustering approach.
#' Currently, it includes the `pre.learn()` function to inspect your data and
#' provide a heuristic recommendation for main and alternative algorithms.
#'
#' ## Main Workflow
#'
#' The recommended workflow guides users from raw data and metadata inspection
#' to algorithm suggestions:
#'  - pre.learn() – Automatically evaluate data structure, balance, normality,
#'    missing values, and zero-variance features to suggest the best algorithm.
#'
#' Main Function Overview
#' | Function     | Description |
#' |--------------|-------------|
#' | `pre.learn()`| Automatic first-pass data evaluation and algorithm suggestion |
#'
#' Contact and Contributions
#' For suggestions, bug reports, or contributions, see the
#' [GitHub repository](https://github.com/Luiz-Garcia-R/autolearnR).
#'
#' Example Workflow
#'
#' A small synthetic dataset to demonstrate `pre.learn()` usage.
#'
#' @examples
#'
#' # Example raw data
#' raw_data <- data.frame(
#'   Sample = paste0("S", 1:6),
#'   Var1 = c(5.1, 4.8, 5.5, 5.0, 5.2, 4.9),
#'   Var2 = c(10.2, 9.5, 10.0, 10.3, 9.8, 10.1),
#'   Var3 = c("A","B","A","B","A","B")
#' )
#'
#' # Example metadata
#' metadata <- data.frame(
#'   Sample = raw_data$Sample,
#'   Output = c("Control","Control","Control","Case","Case","Case")
#' )
#'
#' # Run pre-learn analysis
#' pre.learn(raw_data, metadata)
#'
#' @name autolearnR
#'
"_PACKAGE"
