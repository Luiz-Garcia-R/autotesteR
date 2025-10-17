#' Preliminary Data Assessment and Model Suggestion
#'
#' This function performs basic checks on raw input data and metadata to
#' automatically suggest suitable machine learning approaches (classification,
#' regression, or clustering). It evaluates sample balance, normality, missing values,
#' and zero-variance predictors, and provides a primary suggestion with alternatives.
#'
#' @param raw_data A data frame where the first column may be "Sample" and the
#' remaining columns are features.
#' @param metadata A data frame containing at least one column named "Sample" to
#' match `raw_data`, and optionally a column "Output" defining the prediction target.
#' @param objective Optional. Specify the task type explicitly:
#' `"classification"`, `"regression"`, or `"clustering"`.
#'
#' @return A list (invisibly) with the following elements:
#' \itemize{
#'   \item \code{task}: Detected or specified task type.
#'   \item \code{diagnostic}: List containing balance, normality, number of samples,
#'         missing value counts, and zero-variance flags.
#'   \item \code{suggested_main}: Main suggested method.
#'   \item \code{suggested_alternatives}: Alternative methods.
#'   \item \code{reason}: Rationale for the suggestion.
#' }
#'
#' @examples
#' raw_data <- data.frame(
#'   Sample = paste0("S", 1:12),
#'   Var1 = rnorm(12, 5, 2),
#'   Var2 = rnorm(12, 10, 3),
#'   Var3 = sample(c("A", "B"), 12, replace = TRUE)
#' )
#' metadata <- data.frame(
#'   Sample = paste0("S", 1:12),
#'   Group = rep(c("Control", "Case"), each = 6),
#'   Output = rep(c("Control", "Case"), each = 6)
#' )
#' pre.learn(raw_data, metadata)
#'
#' @export

pre.learn <- function(raw_data, metadata, objective = NULL) {
  # ------------------------
  # 1. Pre-processing
  # ------------------------
  if (colnames(raw_data)[1] == "Sample") {
    rownames(raw_data) <- raw_data[, 1]
    raw_data <- raw_data[, -1, drop = FALSE]
  }

  if (!all(metadata$Sample %in% rownames(raw_data))) {
    stop("Samples in metadata do not match raw_data.")
  }

  # ------------------------
  # 2. Define task
  # ------------------------
  output_var <- "Output"
  task <- NULL
  if (!is.null(objective)) {
    task <- match.arg(objective, c("classification", "regression", "clustering"))
  } else {
    if (output_var %in% colnames(metadata)) {
      y <- metadata[[output_var]]
      if (is.factor(y) || is.character(y)) {
        task <- "classification"
      } else if (is.numeric(y)) {
        task <- "regression"
      }
    } else {
      task <- "clustering"
    }
  }

  # ------------------------
  # 3. Automatic checks
  # ------------------------
  balance <- "Not applicable"
  normal <- NA
  cats <- any(sapply(raw_data, is.factor) | sapply(raw_data, is.character))
  nsize <- nrow(raw_data)

  # Missing values
  na_count <- sapply(raw_data, function(x) sum(is.na(x)))

  # Zero variance
  var_zero <- sapply(raw_data, function(x) {
    if (is.numeric(x)) var(x, na.rm = TRUE) < 1e-8 else FALSE
  })

  # ------------------------
  # Balance and normality
  # ------------------------
  if (task == "classification") {
    tbl <- table(metadata[[output_var]])
    ratio <- min(tbl) / max(tbl)
    balance <- ifelse(ratio < 0.5, "unbalanced", "balanced")
  }

  if (task %in% c("classification", "regression")) {
    num_vars <- raw_data[, sapply(raw_data, is.numeric), drop = FALSE]
    if (ncol(num_vars) > 0) {
      pvals <- apply(num_vars, 2, function(x) {
        if (length(x) >= 3 && length(x) <= 5000) shapiro.test(x)$p.value else 1
      })
      normal <- all(pvals > 0.05)
    } else {
      normal <- NA
    }
  }

  # ------------------------
  # Heuristics for suggestion
  # ------------------------
  main <- NULL
  alts <- NULL
  reason <- NULL
  if (task == "classification") {
    if (balance == "unbalanced") {
      main <- "Random Forest"; alts <- c("XGBoost", "SVM"); reason <- "robust to unbalanced classes"
    } else if (!is.na(normal) && normal && !cats) {
      main <- "Logistic Regression"; alts <- c("Random Forest", "SVM"); reason <- "balanced classes and normally distributed numeric variables"
    } else if (!is.na(normal) && !normal && !cats) {
      main <- "SVM"; alts <- c("Random Forest", "XGBoost"); reason <- "non-normal numeric data without categorical variables"
    } else {
      main <- "XGBoost"; alts <- c("Random Forest", "Logistic Regression"); reason <- "heterogeneous data (categorical + numeric)"
    }
  } else if (task == "regression") {
    if (!is.na(normal) && normal && !cats) {
      main <- "Linear Regression"; alts <- c("Elastic Net", "Random Forest"); reason <- "normally distributed numeric variables"
    } else if (!is.na(normal) && !normal && !cats) {
      main <- "Elastic Net"; alts <- c("Random Forest", "XGBoost"); reason <- "non-normal numeric data and possible multicollinearity"
    } else {
      main <- "Random Forest"; alts <- c("Elastic Net", "XGBoost"); reason <- "mixed data types or non-normality"
    }
  } else if (task == "clustering") {
    if (cats) {
      main <- "Gower + Hierarchical"; alts <- c("DBSCAN", "GMM"); reason <- "mixed data types (categorical + numeric)"
    } else if (nsize < 200) {
      main <- "Hierarchical Clustering"; alts <- c("k-means", "GMM"); reason <- "small sample size, interpretability"
    } else {
      main <- "k-means"; alts <- c("Hierarchical Clustering", "DBSCAN"); reason <- "larger samples with only numeric variables"
    }
  }

  # ------------------------
  # 4. Friendly output
  # ------------------------
  message("\nEvaluation results")
  message("==================================")
  message(reason)

  if (task == "classification") {
    message("Class balance: ", balance,
            paste0(" (", round(min(tbl)/sum(tbl)*100), "% vs ", round(max(tbl)/sum(tbl)*100), "%)"))
  } else {
    message("Class balance: Not applicable")
  }

  message("Normality: ", ifelse(is.na(normal), "Not applicable", ifelse(normal, "Yes", "No")))
  message("Samples: ", nsize)
  message("Task detected: ", task)

  # Diagnostics per column
  df_diag <- data.frame(
    Column = names(raw_data),
    NAs = na_count,
    ZeroVariance = var_zero,
    stringsAsFactors = FALSE
  )
  message("\nColumn diagnostics:")
  print(df_diag, row.names = FALSE)

  message("==================================")
  message("Suggested method: ", main)
  message("Alternative methods: ", paste(alts, collapse = " and "))


  # ------------------------
  # 5. Return invisibly
  # ------------------------
  invisible(list(
    task = task,
    diagnostic = list(
      balance = balance,
      normal = normal,
      n_samples = nsize,
      na_count = na_count,
      zero_variance = var_zero
    ),
    suggested_main = main,
    suggested_alternatives = alts,
    reason = reason
  ))
}
