library(shiny)
library(readxl)
library(DT)
library(bslib)
library(openxlsx)

demo_workbook_cnn <- "Ch05PrembinaryCNNverticle.xlsm"
demo_workbook_ann <- "Ch032dimensionANNverticle.xlsm"
demo_ann_csv_path <- "C:/Users/user/Downloads/Ch032dimensionANNverticle_data2_demo.csv"

read_sheet_matrix <- function(path, sheet) {
  sheet_dims <- list(
    data = c(9, 266),
    data2 = c(21893, 952),
    dataabc = c(1358, 961),
    CNN2 = c(21893, 1015),
    window = c(37, 34)
  )

  dims <- sheet_dims[[sheet]]
  if (is.null(dims)) {
    stop(sprintf("Unsupported sheet: %s", sheet))
  }

  data <- openxlsx::readWorkbook(
    xlsxFile = path,
    sheet = sheet,
    colNames = FALSE,
    rowNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE,
    rows = seq_len(dims[1]),
    cols = seq_len(dims[2])
  )

  as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
}

cell_value <- function(df, row, col) {
  df[[col]][row]
}

trim_matrix <- function(df) {
  if (!nrow(df) || !ncol(df)) {
    return(df)
  }

  non_empty <- !(is.na(df) | df == "")
  keep_rows <- which(rowSums(non_empty) > 0)
  keep_cols <- which(colSums(non_empty) > 0)

  if (!length(keep_rows) || !length(keep_cols)) {
    return(df[0, 0, drop = FALSE])
  }

  df[seq_len(max(keep_rows)), seq_len(max(keep_cols)), drop = FALSE]
}

extract_demo_data_block <- function(data_sheet) {
  block <- data_sheet[4:9, 10:266, drop = FALSE]
  rownames(block) <- c("pattern", "status_1", "status_2", "status_3", "status_4", "status")
  block
}

build_data2 <- function(data_sheet, pattern_count = 64) {
  feature_count <- 12
  output <- matrix(NA, nrow = pattern_count + 1, ncol = 2 + feature_count)

  for (jk in seq_len(pattern_count)) {
    mcol <- 11 + (jk - 1) * 4
    status <- suppressWarnings(as.numeric(cell_value(data_sheet, 9, 3 + mcol)))

    if (isTRUE(status == 1)) {
      output[jk + 1, 1] <- 1
      output[jk + 1, 2] <- 0
    } else {
      output[jk + 1, 1] <- 0
      output[jk + 1, 2] <- 1
    }

    k <- 3
    for (jm in 0:2) {
      for (j in 5:8) {
        output[jk + 1, k] <- cell_value(data_sheet, j, mcol + jm)
        k <- k + 1
      }
    }
  }

  as.data.frame(output, stringsAsFactors = FALSE, check.names = FALSE)
}

build_dataabc <- function(data2_sheet, cnn2_sheet, window_sheet) {
  category <- as.integer(cell_value(cnn2_sheet, 1, 11))
  if (is.na(category) || category < 1) {
    stop("`CNN2!K1` must contain a positive integer category count.")
  }

  row2 <- unlist(data2_sheet[2, , drop = TRUE], use.names = FALSE)
  mitem <- max(which(!(is.na(row2) | row2 == "")))
  lastrow <- max(which(rowSums(!(is.na(data2_sheet) | data2_sheet == "")) > 0))

  output <- as.data.frame(
    matrix(NA, nrow = lastrow, ncol = category + 144),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  window_mask <- as.matrix(window_sheet[2:37, 1:16, drop = FALSE])

  for (j in 2:lastrow) {
    source_row <- unlist(data2_sheet[j, , drop = TRUE], use.names = FALSE)

    arr <- rep(NA, 36)
    arr_index <- 1
    for (jmm2 in seq_len(16)) {
      for (jmm in (category + 1):mitem) {
        arr[arr_index] <- source_row[jmm]
        arr_index <- arr_index + 1
        if (arr_index > 36) {
          break
        }
      }
      if (arr_index > 36) {
        break
      }
    }

    if (category > 0) {
      output[j, 1:category] <- as.list(source_row[seq_len(category)])
    }

    arr2 <- rep(NA, 144)
    out_index <- 1
    for (jk in seq_len(16)) {
      for (j2 in 2:37) {
        if (isTRUE(window_mask[j2 - 1, jk] == 1)) {
          arr2[out_index] <- arr[j2 - 1]
          out_index <- out_index + 1
          if (out_index > 144) {
            break
          }
        }
      }
      if (out_index > 144) {
        break
      }
    }

    output[j, (category + 1):(category + 144)] <- as.list(arr2)
  }

  output
}

extract_patterns <- function(data_sheet, pattern_count = 64) {
  patterns <- vector("list", pattern_count)
  labels <- vector("list", pattern_count)

  for (jk in seq_len(pattern_count)) {
    mcol <- 11 + (jk - 1) * 4
    patterns[[jk]] <- as.matrix(data_sheet[5:8, mcol:(mcol + 2), drop = FALSE])
    labels[[jk]] <- if (isTRUE(suppressWarnings(as.numeric(cell_value(data_sheet, 9, 3 + mcol))) == 1)) {
      "[1, 0]"
    } else {
      "[0, 1]"
    }
  }

  list(patterns = patterns, labels = unlist(labels, use.names = FALSE))
}

pattern_to_block <- function(pattern) {
  matrix_values <- apply(pattern, c(1, 2), function(value) suppressWarnings(as.numeric(value)))
  rows <- apply(matrix_values, 1, function(row) {
    paste(ifelse(row == 1, "#", "."), collapse = " ")
  })
  paste(rows, collapse = "\n")
}

build_pattern_gallery <- function(patterns, labels) {
  data.frame(
    sample_id = seq_along(patterns),
    label = labels,
    pattern_text = vapply(patterns, pattern_to_block, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

convert_status_to_cnn_data2 <- function(data2_sheet) {
  trimmed <- trim_matrix(data2_sheet)
  if (nrow(trimmed) < 2 || ncol(trimmed) < 2) {
    return(trimmed)
  }

  header <- as.character(unlist(trimmed[1, , drop = TRUE], use.names = FALSE))
  body <- trimmed[-1, , drop = FALSE]
  numeric_body <- safe_numeric_matrix(body)

  # Already one-hot like [1,0] / [0,1]
  if (ncol(numeric_body) >= 2) {
    first_two <- numeric_body[, 1:2, drop = FALSE]
    valid_pairs <- stats::complete.cases(first_two) &
      ((first_two[[1]] == 1 & first_two[[2]] == 0) | (first_two[[1]] == 0 & first_two[[2]] == 1))
    if (all(valid_pairs)) {
      return(trimmed)
    }
  }

  status <- suppressWarnings(as.numeric(numeric_body[[1]]))
  if (any(is.na(status)) || any(!status %in% c(0, 1))) {
    return(trimmed)
  }

  out <- data.frame(
    V1 = c(ifelse(is.na(header[1]) || header[1] == "", "status_1", paste0(header[1], "_1")), ifelse(status == 1, 1, 0)),
    V2 = c(ifelse(is.na(header[1]) || header[1] == "", "status_0", paste0(header[1], "_0")), ifelse(status == 1, 0, 1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  if (ncol(trimmed) >= 2) {
    remainder <- trimmed[, 2:ncol(trimmed), drop = FALSE]
    out <- cbind(out, remainder, stringsAsFactors = FALSE)
  }

  rownames(out) <- NULL
  out
}

compare_frames <- function(left, right) {
  left_trimmed <- trim_matrix(left)
  right_trimmed <- trim_matrix(right)

  same_dims <- identical(dim(left_trimmed), dim(right_trimmed))
  if (!same_dims) {
    return(list(match = FALSE, message = sprintf(
      "dimension mismatch: generated %s vs workbook %s",
      paste(dim(left_trimmed), collapse = "x"),
      paste(dim(right_trimmed), collapse = "x")
    )))
  }

  same_values <- identical(
    lapply(left_trimmed, as.character),
    lapply(right_trimmed, as.character)
  )

  list(
    match = same_values,
    message = if (same_values) "matched workbook output" else "values differ from workbook output"
  )
}

safe_numeric_matrix <- function(df) {
  as.data.frame(lapply(df, function(col) suppressWarnings(as.numeric(col))), check.names = FALSE)
}

sigmoid <- function(x) {
  ifelse(x >= 0, 1 / (1 + exp(-x)), exp(x) / (1 + exp(x)))
}

algorithm_choices <- c(
  "Logistic Regression" = "logistic_regression",
  "Linear Regression Score" = "linear_regression",
  "KNN" = "knn",
  "Naive Bayes" = "naive_bayes",
  "LDA" = "lda",
  "QDA" = "qda",
  "Decision Tree" = "decision_tree",
  "Neural Net" = "neural_net",
  "Random Forest" = "random_forest",
  "SVM" = "svm"
)

model_configuration_choices <- c(
  "CNN" = "cnn",
  "ANN" = "ann",
  unname(algorithm_choices)
)
names(model_configuration_choices) <- c("CNN", "ANN", names(algorithm_choices))

algorithm_label <- function(algorithm) {
  if (identical(algorithm, "ann")) {
    return("ANN")
  }
  if (identical(algorithm, "cnn")) {
    return("CNN")
  }
  matched <- names(algorithm_choices)[match(algorithm, algorithm_choices)]
  ifelse(is.na(matched), algorithm, matched)
}

resolve_model_configuration <- function(config_value) {
  if (identical(config_value, "cnn")) {
    return(list(mode = "CNN", algorithm = "logistic_regression", label = "CNN"))
  }
  if (identical(config_value, "ann")) {
    return(list(mode = "ANN", algorithm = "ann_workbook", label = "ANN"))
  }
  list(mode = "ANN", algorithm = config_value, label = algorithm_label(config_value))
}

make_probability_prediction <- function(score = NULL, prob = NULL, class = NULL) {
  if (is.null(prob) && !is.null(score)) {
    prob <- sigmoid(score)
  }
  if (is.null(score) && !is.null(prob)) {
    clipped <- pmin(pmax(prob, 1e-8), 1 - 1e-8)
    score <- qlogis(clipped)
  }
  if (is.null(class) && !is.null(prob)) {
    class <- ifelse(prob >= 0.5, 1, 0)
  }
  list(score = as.numeric(score), prob = as.numeric(prob), class = as.numeric(class))
}

fit_naive_bayes_model <- function(features, target) {
  x <- as.matrix(features)
  classes <- sort(unique(target))
  stats_list <- lapply(classes, function(cls) {
    subset <- x[target == cls, , drop = FALSE]
    list(
      class = cls,
      prior = nrow(subset) / nrow(x),
      mean = colMeans(subset),
      sd = apply(subset, 2, stats::sd)
    )
  })
  for (i in seq_along(stats_list)) {
    stats_list[[i]]$sd[is.na(stats_list[[i]]$sd) | stats_list[[i]]$sd == 0] <- 1e-6
  }
  list(algorithm = "naive_bayes", stats = stats_list, feature_names = names(features))
}

predict_naive_bayes_model <- function(model, features) {
  x <- as.matrix(features[, model$feature_names, drop = FALSE])
  log_post <- sapply(model$stats, function(cls_stats) {
    mu <- cls_stats$mean
    sigma <- cls_stats$sd
    rowSums(-0.5 * log(2 * pi * sigma^2) - ((x - matrix(mu, nrow(x), length(mu), byrow = TRUE))^2) / (2 * matrix(sigma^2, nrow(x), length(sigma), byrow = TRUE))) + log(cls_stats$prior)
  })
  if (is.vector(log_post)) {
    log_post <- matrix(log_post, ncol = length(model$stats))
  }
  max_log <- apply(log_post, 1, max)
  exp_shifted <- exp(log_post - max_log)
  prob_one <- exp_shifted[, ncol(exp_shifted)] / rowSums(exp_shifted)
  make_probability_prediction(prob = prob_one)
}

fit_prediction_model <- function(features, target, algorithm = "logistic_regression") {
  train_frame <- data.frame(target = target, features, check.names = FALSE)

  switch(
    algorithm,
    logistic_regression = {
      model <- suppressWarnings(glm(target ~ ., data = train_frame, family = binomial()))
      list(algorithm = algorithm, model = model, feature_names = names(features))
    },
    linear_regression = {
      model <- lm(target ~ ., data = train_frame)
      list(algorithm = algorithm, model = model, feature_names = names(features))
    },
    knn = {
      if (!requireNamespace("class", quietly = TRUE)) {
        stop("KNN requires the `class` package.")
      }
      k_value <- max(1, min(9, 2 * floor(sqrt(nrow(features)) / 2) + 1))
      list(
        algorithm = algorithm,
        train_x = as.matrix(features),
        train_y = factor(target, levels = c(0, 1)),
        feature_names = names(features),
        k = k_value
      )
    },
    naive_bayes = {
      fit_naive_bayes_model(features, target)
    },
    lda = {
      if (!requireNamespace("MASS", quietly = TRUE)) {
        stop("LDA requires the `MASS` package.")
      }
      model <- MASS::lda(x = as.matrix(features), grouping = factor(target, levels = c(0, 1)))
      list(algorithm = algorithm, model = model, feature_names = names(features))
    },
    qda = {
      if (!requireNamespace("MASS", quietly = TRUE)) {
        stop("QDA requires the `MASS` package.")
      }
      model <- MASS::qda(x = as.matrix(features), grouping = factor(target, levels = c(0, 1)))
      list(algorithm = algorithm, model = model, feature_names = names(features))
    },
    ann_workbook = {
      fit_prediction_model(features, target, algorithm = "neural_net")
    },
    decision_tree = {
      if (!requireNamespace("rpart", quietly = TRUE)) {
        stop("Decision Tree requires the `rpart` package.")
      }
      model <- rpart::rpart(factor(target) ~ ., data = data.frame(features, target = factor(target)), method = "class")
      list(algorithm = algorithm, model = model, feature_names = names(features))
    },
    neural_net = {
      if (!requireNamespace("nnet", quietly = TRUE)) {
        stop("Neural Net requires the `nnet` package.")
      }
      model <- nnet::nnet(
        x = as.matrix(features),
        y = nnet::class.ind(factor(target, levels = c(0, 1))),
        size = max(2, min(6, ncol(features))),
        rang = 0.1,
        decay = 5e-4,
        maxit = 500,
        trace = FALSE,
        softmax = TRUE
      )
      list(algorithm = algorithm, model = model, feature_names = names(features))
    },
    random_forest = {
      if (requireNamespace("randomForest", quietly = TRUE)) {
        model <- randomForest::randomForest(x = features, y = factor(target, levels = c(0, 1)))
        list(algorithm = algorithm, model = model, feature_names = names(features), engine = "randomForest")
      } else if (requireNamespace("ranger", quietly = TRUE)) {
        model <- ranger::ranger(
          dependent.variable.name = "target",
          data = data.frame(target = factor(target, levels = c(0, 1)), features),
          probability = TRUE
        )
        list(algorithm = algorithm, model = model, feature_names = names(features), engine = "ranger")
      } else {
        stop("Random Forest requires `randomForest` or `ranger`.")
      }
    },
    svm = {
      if (!requireNamespace("e1071", quietly = TRUE)) {
        stop("SVM requires the `e1071` package.")
      }
      model <- e1071::svm(
        x = features,
        y = factor(target, levels = c(0, 1)),
        kernel = "radial",
        probability = TRUE
      )
      list(algorithm = algorithm, model = model, feature_names = names(features))
    },
    stop(sprintf("Unsupported algorithm: %s", algorithm))
  )
}

predict_model <- function(model_object, features) {
  features <- data.frame(features[, model_object$feature_names, drop = FALSE], check.names = FALSE)

  switch(
    model_object$algorithm,
    logistic_regression = {
      score <- as.numeric(stats::predict(model_object$model, newdata = features, type = "link"))
      make_probability_prediction(score = score)
    },
    linear_regression = {
      score <- as.numeric(stats::predict(model_object$model, newdata = features))
      make_probability_prediction(score = score)
    },
    knn = {
      pred <- class::knn(
        train = model_object$train_x,
        test = as.matrix(features),
        cl = model_object$train_y,
        k = model_object$k,
        prob = TRUE
      )
      raw_prob <- attr(pred, "prob")
      pred_num <- as.numeric(as.character(pred))
      prob_one <- ifelse(pred_num == 1, raw_prob, 1 - raw_prob)
      make_probability_prediction(prob = prob_one, class = pred_num)
    },
    naive_bayes = {
      predict_naive_bayes_model(model_object, features)
    },
    lda = {
      pred <- stats::predict(model_object$model, newdata = as.matrix(features))
      prob_one <- pred$posterior[, "1"]
      make_probability_prediction(score = pred$x[, 1], prob = prob_one)
    },
    qda = {
      pred <- stats::predict(model_object$model, newdata = as.matrix(features))
      prob_one <- pred$posterior[, "1"]
      make_probability_prediction(prob = prob_one)
    },
    ann_workbook = {
      predict_ann_workbook_model(model_object, features)
    },
    decision_tree = {
      prob <- stats::predict(model_object$model, newdata = features, type = "prob")
      prob_one <- prob[, "1"]
      make_probability_prediction(prob = prob_one)
    },
    neural_net = {
      prob <- stats::predict(model_object$model, newdata = as.matrix(features), type = "raw")
      if (is.matrix(prob)) {
        prob_one <- prob[, ncol(prob)]
      } else {
        prob_one <- prob
      }
      make_probability_prediction(prob = prob_one)
    },
    random_forest = {
      if (identical(model_object$engine, "randomForest")) {
        prob <- stats::predict(model_object$model, newdata = features, type = "prob")[, "1"]
      } else {
        prob <- stats::predict(model_object$model, data = features)$predictions[, "1"]
      }
      make_probability_prediction(prob = prob)
    },
    svm = {
      pred <- stats::predict(model_object$model, newdata = features, probability = TRUE)
      prob <- attr(pred, "probabilities")
      prob_one <- prob[, "1"]
      make_probability_prediction(prob = prob_one)
    },
    stop(sprintf("Unsupported algorithm: %s", model_object$algorithm))
  )
}

compute_auc <- function(actual, score) {
  pos <- score[actual == 1]
  neg <- score[actual == 0]
  if (!length(pos) || !length(neg)) {
    return(NA_real_)
  }

  wins <- 0
  ties <- 0
  for (p in pos) {
    for (n in neg) {
      if (p > n) {
        wins <- wins + 1
      } else if (p == n) {
        ties <- ties + 1
      }
    }
  }

  (wins + 0.5 * ties) / (length(pos) * length(neg))
}

fit_binary_glm <- function(features, target) {
  fit_prediction_model(features, target, algorithm = "logistic_regression")
}

predict_binary_glm <- function(model, features) {
  predict_model(model, features)
}

build_basic_confusion_matrix <- function(actual, predicted) {
  tp <- sum(predicted == 1 & actual == 1)
  fp <- sum(predicted == 1 & actual == 0)
  tn <- sum(predicted == 0 & actual == 0)
  fn <- sum(predicted == 0 & actual == 1)

  data.frame(
    predicted = c("Positive", "Negative"),
    actual_positive = c(tp, fn),
    actual_negative = c(fp, tn),
    check.names = FALSE
  )
}

build_validation_results <- function(features, target, algorithm = "logistic_regression", seed = 20260720L, test_fraction = 0.25) {
  set.seed(seed)
  n <- nrow(features)
  test_size <- max(2, floor(n * test_fraction))
  test_index <- sort(sample(seq_len(n), size = test_size))
  train_index <- setdiff(seq_len(n), test_index)

  train_features <- features[train_index, , drop = FALSE]
  test_features <- features[test_index, , drop = FALSE]
  train_target <- target[train_index]
  test_target <- target[test_index]

  model <- fit_prediction_model(train_features, train_target, algorithm = algorithm)
  train_pred <- predict_model(model, train_features)
  test_pred <- predict_model(model, test_features)

  fold_id <- rep(1:5, length.out = n)
  fold_id <- sample(fold_id, n)
  cv_rows <- vector("list", 5)

  for (fold in 1:5) {
    fold_test <- which(fold_id == fold)
    fold_train <- setdiff(seq_len(n), fold_test)
    fold_model <- fit_prediction_model(features[fold_train, , drop = FALSE], target[fold_train], algorithm = algorithm)
    fold_pred <- predict_model(fold_model, features[fold_test, , drop = FALSE])
    cv_rows[[fold]] <- data.frame(
      fold = fold,
      n_train = length(fold_train),
      n_test = length(fold_test),
      auc = round(compute_auc(target[fold_test], fold_pred$prob), 6),
      accuracy = round(mean(fold_pred$class == target[fold_test]), 6),
      check.names = FALSE
    )
  }

  cv_table <- do.call(rbind, cv_rows)
  cv_auc <- mean(cv_table$auc, na.rm = TRUE)
  cv_accuracy <- mean(cv_table$accuracy, na.rm = TRUE)

  summary_table <- data.frame(
    metric = c("Training AUC", "Test AUC", "5-fold CV AUC", "Training accuracy", "Test accuracy", "5-fold CV accuracy", "Train size", "Test size"),
    value = c(
      round(compute_auc(train_target, train_pred$prob), 6),
      round(compute_auc(test_target, test_pred$prob), 6),
      round(cv_auc, 6),
      round(mean(train_pred$class == train_target), 6),
      round(mean(test_pred$class == test_target), 6),
      round(cv_accuracy, 6),
      length(train_index),
      length(test_index)
    ),
    check.names = FALSE
  )

  test_predictions <- data.frame(
    sample_id = test_index,
    actual = ifelse(test_target == 1, "Positive", "Negative"),
    actual_class = test_target,
    score = round(test_pred$score, 6),
    prob_positive = round(test_pred$prob, 6),
    predicted = ifelse(test_pred$class == 1, "Positive", "Negative"),
    predicted_class = test_pred$class,
    correct = ifelse(test_pred$class == test_target, "Correct", "Incorrect"),
    check.names = FALSE
  )

  list(
    algorithm = algorithm,
    split_summary = summary_table,
    cv_table = cv_table,
    confusion_matrix = build_basic_confusion_matrix(test_target, test_pred$class),
    test_predictions = test_predictions,
    model = model,
    train_columns = names(train_features)
  )
}

build_forest_data <- function(features, target) {
  x <- safe_numeric_matrix(features)
  x[is.na(x)] <- 0
  feature_names <- names(x)

  rows <- lapply(seq_along(feature_names), function(i) {
    values <- as.numeric(x[[i]])
    group1 <- values[target == 1]
    group0 <- values[target == 0]
    n1 <- length(group1)
    n0 <- length(group0)

    mean1 <- mean(group1)
    mean0 <- mean(group0)
    sd1 <- stats::sd(group1)
    sd0 <- stats::sd(group0)
    if (is.na(sd1)) sd1 <- 0
    if (is.na(sd0)) sd0 <- 0

    pooled_sd_num <- ((n1 - 1) * sd1^2) + ((n0 - 1) * sd0^2)
    pooled_sd_den <- n1 + n0 - 2
    pooled_sd <- if (pooled_sd_den > 0) sqrt(pooled_sd_num / pooled_sd_den) else 0
    if (is.na(pooled_sd) || pooled_sd == 0) pooled_sd <- 1e-8

    d <- (mean1 - mean0) / pooled_sd
    j <- if ((n1 + n0 - 2) > 0) 1 - (3 / (4 * (n1 + n0) - 9)) else 1
    smd <- j * d
      se <- sqrt((n1 + n0) / (n1 * n0) + (smd^2 / (2 * (n1 + n0 - 2))))
      ci_low <- smd - 1.96 * se
      ci_high <- smd + 1.96 * se
      significant <- ifelse(ci_low > 0 | ci_high < 0, "Yes", "No")
      z_value <- if (is.na(se) || se == 0) NA_real_ else smd / se
      p_value <- if (is.na(z_value)) NA_real_ else 2 * stats::pnorm(-abs(z_value))

      data.frame(
        feature = feature_names[i],
        mean_status1 = round(mean1, 6),
        mean_status0 = round(mean0, 6),
        SMD = round(smd, 6),
        SE = round(se, 6),
        CI_low = round(ci_low, 6),
        CI_high = round(ci_high, 6),
        estimate_label = sprintf("%.2f (%.2f, %.2f)", smd, ci_low, ci_high),
        z_value = round(z_value, 2),
        p_value = ifelse(is.na(p_value), NA_character_, ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))),
        significant = significant,
        abs_SMD = round(abs(smd), 6),
        check.names = FALSE
      )
  })

  forest_table <- do.call(rbind, rows)
  forest_table <- forest_table[order(-forest_table$abs_SMD), , drop = FALSE]
  rownames(forest_table) <- NULL

  summary_table <- data.frame(
    metric = c("Features tested", "Significant features", "Largest |SMD|"),
    value = c(
      nrow(forest_table),
      sum(forest_table$significant == "Yes"),
      if (nrow(forest_table)) max(forest_table$abs_SMD) else NA_real_
    ),
    check.names = FALSE
  )

  list(table = forest_table, summary = summary_table)
}

safe_glm_fit <- function(formula, data) {
  tryCatch(
    suppressWarnings(stats::glm(formula, data = data, family = binomial())),
    error = function(e) NULL
  )
}

extract_glm_feature_rows <- function(model, step_label, group_label = NA_character_) {
  if (is.null(model)) {
    return(data.frame())
  }

  coef_table <- summary(model)$coefficients
  coef_names <- rownames(coef_table)
  keep <- coef_names != "(Intercept)"
  if (!any(keep)) {
    return(data.frame())
  }

  kept_names <- coef_names[keep]
  coef_subset <- coef_table[kept_names, , drop = FALSE]

  conf_int <- suppressWarnings(tryCatch(stats::confint.default(model), error = function(e) NULL))
  if (is.null(conf_int)) {
    conf_int <- matrix(NA_real_, nrow = nrow(coef_table), ncol = 2, dimnames = list(coef_names, c("2.5 %", "97.5 %")))
  }
  conf_subset <- conf_int[kept_names, , drop = FALSE]

  out <- data.frame(
    step = step_label,
    group = group_label,
    feature = kept_names,
    estimate = unname(coef_subset[, "Estimate"]),
    std_error = unname(coef_subset[, "Std. Error"]),
    z_value = unname(coef_subset[, "z value"]),
    p_value = unname(coef_subset[, "Pr(>|z|)"]),
    ci_low = unname(conf_subset[, 1]),
    ci_high = unname(conf_subset[, 2]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out$direction <- ifelse(out$estimate >= 0, "Positive", "Negative")
  out$crosses_zero <- ifelse(is.na(out$ci_low) | is.na(out$ci_high), NA, out$ci_low <= 0 & out$ci_high >= 0)
  out$significant <- ifelse(!is.na(out$p_value) & out$p_value < 0.05, "Yes", "No")
  out$estimate_label <- sprintf("%.3f (%.3f, %.3f)", out$estimate, out$ci_low, out$ci_high)
  out$p_value <- signif(out$p_value, 4)
  out
}

empty_advanced_analysis <- function(message) {
  empty <- data.frame(note = message, check.names = FALSE)
  list(
    feature_extraction = list(
      step1 = empty,
      step2 = empty,
      step3 = empty,
      step3_scores = empty,
      step3_metrics = empty,
      final_forest = empty,
      summary = empty
    ),
    importance = list(
      summary = empty,
      elimination = empty,
      final_table = empty,
      forest = empty,
      final_features = character(),
      screened_features = character(),
      screening = empty
    )
  )
}

build_feature_extraction_results <- function(features, target) {
  x <- safe_numeric_matrix(features)
  keep_cols <- vapply(x, function(col) {
    valid <- !is.na(col)
    sum(valid) >= 10 && length(unique(col[valid])) > 1
  }, logical(1))
  x <- x[, keep_cols, drop = FALSE]

  if (!ncol(x)) {
    empty <- data.frame(note = "No analyzable variables were available for feature extraction.", check.names = FALSE)
    return(list(
      step1 = empty,
      step2 = empty,
      step3 = empty,
      step3_scores = empty,
      step3_metrics = empty,
      final_forest = empty,
      summary = empty
    ))
  }

  data_all <- data.frame(target = target, x, check.names = FALSE)
  step1_rows <- lapply(names(x), function(feature_name) {
    fit <- safe_glm_fit(stats::as.formula(paste0("target ~ `", feature_name, "`")), data_all)
    rows <- extract_glm_feature_rows(fit, step_label = "Step 1", group_label = "Single-variable")
    if (!nrow(rows)) {
      return(NULL)
    }
    rows
  })
  step1 <- do.call(rbind, step1_rows)

  if (is.null(step1) || !nrow(step1)) {
    step1 <- data.frame(note = "Single-variable logistic regression did not return usable coefficients.", check.names = FALSE)
    empty <- data.frame(note = "No variables passed Step 1.", check.names = FALSE)
    return(list(step1 = step1, step2 = empty, step3 = empty, step3_scores = empty, step3_metrics = empty, final_forest = empty, summary = empty))
  }

  step1 <- step1[order(step1$p_value, -abs(step1$estimate)), , drop = FALSE]
  step1$selected_step1 <- ifelse(step1$significant == "Yes" & !isTRUE(step1$crosses_zero), "Yes", "No")
  step1_selected <- step1[step1$selected_step1 == "Yes", , drop = FALSE]

  positive_features <- step1_selected$feature[step1_selected$estimate > 0]
  negative_features <- step1_selected$feature[step1_selected$estimate < 0]

  fit_group_model <- function(feature_names, label) {
    if (!length(feature_names)) {
      return(data.frame())
    }
    formula_text <- paste(sprintf("`%s`", feature_names), collapse = " + ")
    fit <- safe_glm_fit(stats::as.formula(paste("target ~", formula_text)), data_all[, c("target", feature_names), drop = FALSE])
    rows <- extract_glm_feature_rows(fit, step_label = "Step 2", group_label = label)
    if (!nrow(rows)) {
      return(data.frame())
    }
    rows$kept_step2 <- ifelse(rows$p_value < 0.05, "Yes", "No")
    rows
  }

  step2 <- rbind(
    fit_group_model(positive_features, "Positive predictors"),
    fit_group_model(negative_features, "Negative predictors")
  )
  if (!nrow(step2)) {
    step2 <- data.frame(note = "No Step 1 variables remained to fit positive/negative multivariable models.", check.names = FALSE)
  }

  final_positive <- if ("kept_step2" %in% names(step2)) step2$feature[step2$group == "Positive predictors" & step2$kept_step2 == "Yes"] else character()
  final_negative <- if ("kept_step2" %in% names(step2)) step2$feature[step2$group == "Negative predictors" & step2$kept_step2 == "Yes"] else character()

  if (length(final_positive) || length(final_negative)) {
    step3_frame <- data.frame(
      positive_sum = if (length(final_positive)) rowSums(x[, final_positive, drop = FALSE], na.rm = TRUE) else 0,
      negative_sum = if (length(final_negative)) rowSums(x[, final_negative, drop = FALSE], na.rm = TRUE) else 0,
      target = target,
      check.names = FALSE
    )
    step3_scores <- data.frame(
      sample_id = seq_len(nrow(step3_frame)),
      status = step3_frame$target,
      positive_sum = round(step3_frame$positive_sum, 6),
      negative_sum = round(step3_frame$negative_sum, 6),
      check.names = FALSE
    )
    step3_model <- safe_glm_fit(target ~ positive_sum + negative_sum, step3_frame)
    step3 <- extract_glm_feature_rows(step3_model, step_label = "Step 3", group_label = "Grouped sums")
    step3_pred <- if (!is.null(step3_model)) stats::predict(step3_model, newdata = step3_frame, type = "response") else rep(NA_real_, nrow(step3_frame))
    step3_class <- ifelse(step3_pred >= 0.5, 1, 0)
    step3_auc <- compute_auc(target, step3_pred)
    step3_metrics <- data.frame(
      metric = c("Positive variables retained", "Negative variables retained", "Grouped-sum AUC", "Grouped-sum accuracy"),
      value = c(length(final_positive), length(final_negative), round(step3_auc, 6), round(mean(step3_class == target), 6)),
      check.names = FALSE
    )
  } else {
    step3 <- data.frame(note = "No variables survived Step 2, so grouped-sum prediction was not run.", check.names = FALSE)
    step3_scores <- data.frame(note = "No grouped-sum scores available.", check.names = FALSE)
    step3_metrics <- data.frame(note = "No grouped-sum metrics available.", check.names = FALSE)
  }

  final_forest <- if ("kept_step2" %in% names(step2)) {
    step2[step2$kept_step2 == "Yes", c("group", "feature", "estimate", "ci_low", "ci_high", "z_value", "p_value", "direction", "estimate_label"), drop = FALSE]
  } else {
    data.frame(note = "No final variables available for the forest plot.", check.names = FALSE)
  }

  summary <- data.frame(
    metric = c(
      "Variables tested in Step 1",
      "Variables selected in Step 1 (p < 0.05 and CI not crossing 0)",
      "Positive variables kept in Step 2",
      "Negative variables kept in Step 2"
    ),
    value = c(nrow(step1), nrow(step1_selected), length(final_positive), length(final_negative)),
    check.names = FALSE
  )

  list(
    step1 = step1,
    step2 = step2,
    step3 = step3,
    step3_scores = step3_scores,
    step3_metrics = step3_metrics,
    final_forest = final_forest,
    summary = summary
  )
}

build_importance_forest_rows <- function(features, target, feature_names) {
  if (!length(feature_names)) {
    return(data.frame(note = "No selected variables available for the importance forest plot.", check.names = FALSE))
  }

  data_all <- data.frame(target = target, features[, feature_names, drop = FALSE], check.names = FALSE)
  rows <- lapply(feature_names, function(feature_name) {
    fit <- safe_glm_fit(stats::as.formula(paste0("target ~ `", feature_name, "`")), data_all[, c("target", feature_name), drop = FALSE])
    fit_rows <- extract_glm_feature_rows(fit, step_label = "Importance", group_label = "Final 10")
    if (!nrow(fit_rows)) {
      return(NULL)
    }
    fit_rows[, c("feature", "estimate", "ci_low", "ci_high", "z_value", "p_value", "direction", "estimate_label", "significant"), drop = FALSE]
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(note = "Importance forest plot could not be built for the selected variables.", check.names = FALSE))
  }
  forest_table <- do.call(rbind, rows)
  forest_table <- forest_table[order(-abs(forest_table$estimate), forest_table$feature), , drop = FALSE]
  rownames(forest_table) <- NULL
  forest_table
}

build_importance_results <- function(features, target, algorithm = "logistic_regression", seed = 20260720L, test_fraction = 0.25, final_n = 10L, max_candidates = 30L) {
  x <- safe_numeric_matrix(features)
  keep_cols <- vapply(x, function(col) {
    valid <- !is.na(col)
    sum(valid) >= 10 && length(unique(col[valid])) > 1
  }, logical(1))
  x <- x[, keep_cols, drop = FALSE]
  x[is.na(x)] <- 0

  if (!ncol(x)) {
    empty <- data.frame(note = "No analyzable variables were available for importance selection.", check.names = FALSE)
    return(list(summary = empty, elimination = empty, final_table = empty, forest = empty, final_features = character()))
  }

  set.seed(seed)
  n <- nrow(x)
  test_size <- max(2, floor(n * test_fraction))
  test_index <- sort(sample(seq_len(n), size = test_size))
  train_index <- setdiff(seq_len(n), test_index)
  train_features <- x[train_index, , drop = FALSE]
  test_features <- x[test_index, , drop = FALSE]
  train_target <- target[train_index]
  test_target <- target[test_index]

  fit_auc <- function(feature_names) {
    if (!length(feature_names)) {
      return(NA_real_)
    }
    fit <- suppressWarnings(tryCatch(
      fit_prediction_model(train_features[, feature_names, drop = FALSE], train_target, algorithm = algorithm),
      error = function(e) e
    ))
    if (inherits(fit, "error")) {
      return(NA_real_)
    }
    pred <- suppressWarnings(tryCatch(
      predict_model(fit, test_features[, feature_names, drop = FALSE]),
      error = function(e) e
    ))
    if (inherits(pred, "error")) {
      return(NA_real_)
    }
    compute_auc(test_target, pred$prob)
  }

  feature_screen <- data.frame(
    feature = names(x),
    single_auc = vapply(names(x), function(feature_name) fit_auc(feature_name), numeric(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  feature_screen <- feature_screen[order(-feature_screen$single_auc, feature_screen$feature), , drop = FALSE]
  screened_features <- feature_screen$feature[seq_len(min(nrow(feature_screen), max(as.integer(final_n), as.integer(max_candidates))))]

  current_features <- screened_features
  baseline_auc <- fit_auc(current_features)
  current_auc <- baseline_auc
  elimination_rows <- list()
  step_id <- 1L

  while (length(current_features) > final_n) {
    candidate_rows <- lapply(current_features, function(feature_name) {
      remaining <- setdiff(current_features, feature_name)
      auc_after <- fit_auc(remaining)
      data.frame(
        step = step_id,
        removed_feature = feature_name,
        remaining_count = length(remaining),
        auc_after_removal = round(auc_after, 6),
        auc_change = round(auc_after - current_auc, 6),
        check.names = FALSE
      )
    })
    candidate_table <- do.call(rbind, candidate_rows)
    candidate_table <- candidate_table[order(-candidate_table$auc_after_removal, -candidate_table$auc_change, candidate_table$removed_feature), , drop = FALSE]
    best_row <- candidate_table[1, , drop = FALSE]
    best_feature <- as.character(best_row$removed_feature[1])
    elimination_rows[[length(elimination_rows) + 1L]] <- best_row
    current_features <- setdiff(current_features, best_feature)
    current_auc <- as.numeric(best_row$auc_after_removal[1])
    step_id <- step_id + 1L
    if (length(current_features) <= 1) {
      break
    }
  }

  final_auc <- fit_auc(current_features)
  final_table <- data.frame(
    rank = seq_along(current_features),
    feature = current_features,
    final_test_auc = round(final_auc, 6),
    selected_algorithm = algorithm_label(algorithm),
    check.names = FALSE
  )
  forest_table <- build_importance_forest_rows(x, target, current_features)
  elimination_table <- if (length(elimination_rows)) do.call(rbind, elimination_rows) else data.frame(note = "No variables were removed because the screened dataset already had 10 or fewer analyzable variables.", check.names = FALSE)
  summary_table <- data.frame(
    metric = c("Algorithm", "Variables analyzed", "Variables screened for elimination", "Variables removed", "Final variables retained", "Baseline screened test AUC", "Final 10 test AUC"),
    value = c(
      algorithm_label(algorithm),
      ncol(x),
      length(screened_features),
      max(0, length(screened_features) - length(current_features)),
      length(current_features),
      round(baseline_auc, 6),
      round(final_auc, 6)
    ),
    check.names = FALSE
  )

  list(
    summary = summary_table,
    elimination = elimination_table,
    final_table = final_table,
    forest = forest_table,
    final_features = current_features,
    screened_features = screened_features,
    screening = feature_screen
  )
}

compute_auc_ci <- function(auc, actual) {
  n1 <- sum(actual == 1)
  n0 <- sum(actual == 0)
  if (is.na(auc) || n1 == 0 || n0 == 0) {
    return(c(NA_real_, NA_real_))
  }
  q1 <- auc / (2 - auc)
  q2 <- (2 * auc^2) / (1 + auc)
  se <- sqrt((auc * (1 - auc) + (n1 - 1) * (q1 - auc^2) + (n0 - 1) * (q2 - auc^2)) / (n0 * n1))
  c(max(0, auc - 1.96 * se), min(1, auc + 1.96 * se))
}

build_comparison_results <- function(features, target, comparison_methods = unname(model_configuration_choices), selected_features = NULL, ann_reference_model = NULL, seed = 20260720L, test_fraction = 0.25) {
  set.seed(seed)
  n <- nrow(features)
  test_size <- max(2, floor(n * test_fraction))
  test_index <- sort(sample(seq_len(n), size = test_size))
  train_index <- setdiff(seq_len(n), test_index)

  train_features <- features[train_index, , drop = FALSE]
  test_features <- features[test_index, , drop = FALSE]
  train_target <- target[train_index]
  test_target <- target[test_index]

  metric_row <- function(split_label, algo_label, n_value, actual, pred) {
    tp <- sum(pred$class == 1 & actual == 1)
    fp <- sum(pred$class == 1 & actual == 0)
    tn <- sum(pred$class == 0 & actual == 0)
    fn <- sum(pred$class == 0 & actual == 1)
    sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
    precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    accuracy <- mean(pred$class == actual)
    f1 <- if (!is.na(precision) && !is.na(sensitivity) && (precision + sensitivity) > 0) {
      2 * precision * sensitivity / (precision + sensitivity)
    } else {
      NA_real_
    }
    auc <- compute_auc(actual, pred$prob)
    ci <- compute_auc_ci(auc, actual)
    data.frame(
      algorithm = algo_label,
      split = split_label,
      n = n_value,
      sensitivity = round(sensitivity, 6),
      specificity = round(specificity, 6),
      precision = round(precision, 6),
      f1_score = round(f1, 6),
      accuracy = round(accuracy, 6),
      auc = round(auc, 6),
      ci = sprintf("%.2f-%.2f", ci[1], ci[2]),
      ci_low = round(ci[1], 6),
      ci_high = round(ci[2], 6),
      status = "OK",
      check.names = FALSE
    )
  }

  if (is.null(selected_features) || !length(selected_features)) {
    selected_features <- names(features)
  }
  selected_features <- selected_features[selected_features %in% names(features)]
  if (!length(selected_features)) {
    selected_features <- names(features)
  }

  fit_method <- function(method_key, feature_names) {
    algo_key <- if (identical(method_key, "cnn")) "logistic_regression" else if (identical(method_key, "ann")) "ann_workbook" else method_key
    fit <- tryCatch(
      fit_prediction_model(train_features[, feature_names, drop = FALSE], train_target, algorithm = algo_key),
      error = function(e) e
    )

    if (inherits(fit, "error")) {
      return(fit)
    }

    train_pred <- tryCatch(
      predict_model(fit, train_features[, feature_names, drop = FALSE]),
      error = function(e) e
    )
    test_pred <- tryCatch(
      predict_model(fit, test_features[, feature_names, drop = FALSE]),
      error = function(e) e
    )

    if (inherits(train_pred, "error") || inherits(test_pred, "error")) {
      err <- if (inherits(train_pred, "error")) train_pred$message else test_pred$message
      return(structure(list(message = err), class = "prediction_error"))
    }

    list(train_pred = train_pred, test_pred = test_pred)
  }

  result_rows <- lapply(comparison_methods, function(method_key) {
    method_label <- algorithm_label(method_key)
    variable_sets <- list(
      Original = names(features),
      `Top 10` = selected_features
    )

    set_rows <- lapply(names(variable_sets), function(set_name) {
      feature_names <- variable_sets[[set_name]]
      fit_result <- fit_method(method_key, feature_names)
      if (inherits(fit_result, "error")) {
        return(rbind(
          data.frame(algorithm = method_label, variable_set = set_name, split = "Training", n = length(train_target), sensitivity = NA_real_, specificity = NA_real_, precision = NA_real_, f1_score = NA_real_, accuracy = NA_real_, auc = NA_real_, ci = NA_character_, ci_low = NA_real_, ci_high = NA_real_, status = paste("Unavailable:", fit_result$message), check.names = FALSE),
          data.frame(algorithm = method_label, variable_set = set_name, split = "Testing", n = length(test_target), sensitivity = NA_real_, specificity = NA_real_, precision = NA_real_, f1_score = NA_real_, accuracy = NA_real_, auc = NA_real_, ci = NA_character_, ci_low = NA_real_, ci_high = NA_real_, status = paste("Unavailable:", fit_result$message), check.names = FALSE)
        ))
      }
      if (inherits(fit_result, "prediction_error")) {
        return(rbind(
          data.frame(algorithm = method_label, variable_set = set_name, split = "Training", n = length(train_target), sensitivity = NA_real_, specificity = NA_real_, precision = NA_real_, f1_score = NA_real_, accuracy = NA_real_, auc = NA_real_, ci = NA_character_, ci_low = NA_real_, ci_high = NA_real_, status = paste("Prediction failed:", fit_result$message), check.names = FALSE),
          data.frame(algorithm = method_label, variable_set = set_name, split = "Testing", n = length(test_target), sensitivity = NA_real_, specificity = NA_real_, precision = NA_real_, f1_score = NA_real_, accuracy = NA_real_, auc = NA_real_, ci = NA_character_, ci_low = NA_real_, ci_high = NA_real_, status = paste("Prediction failed:", fit_result$message), check.names = FALSE)
        ))
      }
      rbind(
        metric_row("Training", method_label, length(train_target), train_target, fit_result$train_pred),
        metric_row("Testing", method_label, length(test_target), test_target, fit_result$test_pred)
      ) |>
        transform(variable_set = set_name, .before = split)
    })
    do.call(rbind, set_rows)
  })

  comparison_table <- do.call(rbind, result_rows)
  comparison_table$split <- factor(comparison_table$split, levels = c("Training", "Testing"))
  comparison_table$variable_set <- factor(comparison_table$variable_set, levels = c("Original", "Top 10"))
  comparison_table <- comparison_table[order(comparison_table$algorithm, comparison_table$variable_set, comparison_table$split), , drop = FALSE]
  comparison_table$split <- as.character(comparison_table$split)
  comparison_table$variable_set <- as.character(comparison_table$variable_set)
  rownames(comparison_table) <- NULL
  testing_only <- comparison_table[comparison_table$split == "Testing" & comparison_table$status == "OK", , drop = FALSE]
  testing_only <- testing_only[order(-testing_only$auc, testing_only$algorithm, testing_only$variable_set), , drop = FALSE]
  top5_table <- utils::head(testing_only, 5)

  list(
    table = comparison_table,
    top5 = top5_table
  )
}

get_prediction_variables <- function(cnn2_model, max_vars = 10) {
  importance_table <- NULL
  if (!is.null(cnn2_model$importance) && is.data.frame(cnn2_model$importance$final_table)) {
    importance_table <- cnn2_model$importance$final_table
  }

  if (!is.null(importance_table) && nrow(importance_table) > 0 && "feature" %in% names(importance_table)) {
    feature_names <- importance_table$feature[seq_len(min(nrow(importance_table), max_vars))]
  } else {
    forest_table <- cnn2_model$forest$table
    sig <- forest_table[forest_table$significant == "Yes", , drop = FALSE]
    if (!nrow(sig)) {
      sig <- forest_table
    }
    sig <- utils::head(sig, max_vars)
    feature_names <- sig$feature
  }

  feature_frame <- cnn2_model$features[, feature_names, drop = FALSE]

  data.frame(
    feature = feature_names,
    default_value = vapply(feature_names, function(name) {
      stats::median(as.numeric(feature_frame[[name]]), na.rm = TRUE)
    }, numeric(1)),
    min_value = vapply(feature_names, function(name) {
      min(as.numeric(feature_frame[[name]]), na.rm = TRUE)
    }, numeric(1)),
    max_value = vapply(feature_names, function(name) {
      max(as.numeric(feature_frame[[name]]), na.rm = TRUE)
    }, numeric(1)),
    check.names = FALSE
  )
}

format_composite_formula <- function(model_object, feature_subset = NULL, digits = 4) {
  if (is.null(model_object) || is.null(model_object$algorithm)) {
    return("Composite score formula is unavailable.")
  }

  if (model_object$algorithm %in% c("logistic_regression", "linear_regression")) {
    coefs <- stats::coef(model_object$model)
    coef_names <- names(coefs)
    intercept <- if ("(Intercept)" %in% coef_names) unname(coefs["(Intercept)"]) else 0
    feature_names <- setdiff(coef_names, "(Intercept)")
    if (!is.null(feature_subset)) {
      feature_names <- feature_names[feature_names %in% feature_subset]
    }
    if (!length(feature_names)) {
      return(sprintf("Composite score = %.4f", intercept))
    }

    terms <- vapply(feature_names, function(name) {
      sprintf("%+.4f*%s", unname(coefs[name]), name)
    }, character(1))
    return(paste("Composite score =", sprintf("%.4f", intercept), paste(terms, collapse = " ")))
  }

  if (model_object$algorithm == "lda") {
    return("Composite score uses the LDA linear discriminant function on the selected variables.")
  }

  if (model_object$algorithm == "qda") {
    return("Composite score uses the QDA quadratic discriminant score; there is no single linear composite formula.")
  }

  if (model_object$algorithm == "naive_bayes") {
    return("Composite score uses Naive Bayes log-posterior aggregation; there is no single shared linear composite formula.")
  }

  if (model_object$algorithm %in% c("knn", "decision_tree", "neural_net", "ann_workbook", "random_forest", "svm")) {
    return(sprintf("%s does not have a single closed-form composite score formula.", algorithm_label(model_object$algorithm)))
  }

  sprintf("Composite score formula is unavailable for %s.", algorithm_label(model_object$algorithm))
}

read_ann_sheet <- function(path, sheet) {
  openxlsx::readWorkbook(
    xlsxFile = path,
    sheet = sheet,
    colNames = FALSE,
    rowNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  ) |>
    as.data.frame(stringsAsFactors = FALSE, check.names = FALSE)
}

read_ann_demo_csv_sheet <- function(path = demo_ann_csv_path) {
  utils::read.csv(
    file = path,
    header = FALSE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  ) |>
    as.data.frame(stringsAsFactors = FALSE, check.names = FALSE)
}

build_ann_workbook_model <- function(trainning_sheet, feature_names) {
  trimmed <- trim_matrix(trainning_sheet)
  if (!nrow(trimmed) || ncol(trimmed) < 2) {
    stop("`trainning` does not contain enough values for workbook-style ANN modeling.")
  }

  numeric_trainning <- safe_numeric_matrix(trimmed)
  hidden_bias_row <- 11L
  input_weight_row_start <- 12L
  if (nrow(numeric_trainning) < input_weight_row_start) {
    stop("`trainning` is missing the hidden-layer weight rows.")
  }

  output_bias <- suppressWarnings(as.numeric(unlist(numeric_trainning[1, 1:2, drop = TRUE], use.names = FALSE)))
  if (any(!is.finite(output_bias))) {
    stop("`trainning` output bias row is incomplete.")
  }

  candidate_units <- ncol(numeric_trainning)
  hidden_units <- 0L
  for (j in seq_len(candidate_units)) {
    weight_rows <- suppressWarnings(as.numeric(unlist(numeric_trainning[input_weight_row_start:(input_weight_row_start + length(feature_names) - 1), j, drop = TRUE], use.names = FALSE)))
    if (length(weight_rows) == length(feature_names) && any(is.finite(weight_rows))) {
      hidden_units <- j
    } else {
      break
    }
  }
  if (hidden_units < 1L) {
    stop("Could not detect any hidden-unit weight columns in `trainning`.")
  }

  hidden_bias <- suppressWarnings(as.numeric(unlist(numeric_trainning[hidden_bias_row, seq_len(hidden_units), drop = TRUE], use.names = FALSE)))
  hidden_bias[!is.finite(hidden_bias)] <- 0

  input_hidden_weights <- as.matrix(numeric_trainning[input_weight_row_start:(input_weight_row_start + length(feature_names) - 1), seq_len(hidden_units), drop = FALSE])
  input_hidden_weights[!is.finite(input_hidden_weights)] <- 0

  output_weight_rows <- 2:(hidden_units + 1)
  if (max(output_weight_rows) > nrow(numeric_trainning)) {
    stop("`trainning` does not contain enough hidden-to-output weight rows.")
  }
  hidden_output_weights <- as.matrix(numeric_trainning[output_weight_rows, 1:2, drop = FALSE])
  hidden_output_weights[!is.finite(hidden_output_weights)] <- 0

  list(
    algorithm = "ann_workbook",
    feature_names = feature_names,
    hidden_units = hidden_units,
    input_hidden_weights = input_hidden_weights,
    hidden_bias = hidden_bias,
    hidden_output_weights = hidden_output_weights,
    output_bias = output_bias
  )
}

predict_ann_workbook_model <- function(model_object, features) {
  aligned <- matrix(0, nrow = nrow(features), ncol = length(model_object$feature_names))
  colnames(aligned) <- model_object$feature_names
  common_names <- intersect(colnames(features), model_object$feature_names)
  if (length(common_names)) {
    aligned[, common_names] <- as.matrix(features[, common_names, drop = FALSE])
  }
  hidden_linear <- aligned %*% model_object$input_hidden_weights + matrix(model_object$hidden_bias, nrow(aligned), model_object$hidden_units, byrow = TRUE)
  hidden_prob <- sigmoid(hidden_linear)
  output_linear <- hidden_prob %*% model_object$hidden_output_weights + matrix(model_object$output_bias, nrow(aligned), 2, byrow = TRUE)
  output_prob <- sigmoid(output_linear)
  prob_one <- output_prob[, 1]
  prob_zero <- output_prob[, 2]
  denom <- prob_one + prob_zero
  denom[!is.finite(denom) | denom == 0] <- 1
  prob_positive <- prob_one / denom
  make_probability_prediction(prob = prob_positive)
}

normalize_data2_sheet <- function(data2_sheet, target_col = 1, feature_start_col = 2) {
  trimmed <- trim_matrix(data2_sheet)
  if (nrow(trimmed) < 2 || ncol(trimmed) < feature_start_col) {
    return(trimmed)
  }

  header <- trimmed[1, , drop = FALSE]
  body <- trimmed[-1, , drop = FALSE]
  numeric_body <- safe_numeric_matrix(body)
  feature_cols <- seq.int(feature_start_col, ncol(numeric_body))

  for (col_idx in feature_cols) {
    values <- as.numeric(numeric_body[[col_idx]])
    mean_value <- mean(values, na.rm = TRUE)
    sd_value <- stats::sd(values, na.rm = TRUE)
    if (!is.finite(sd_value) || sd_value == 0) {
      numeric_body[[col_idx]] <- 0
    } else {
      numeric_body[[col_idx]] <- (values - mean_value) / sd_value
    }
  }

  out <- rbind(header, numeric_body)
  rownames(out) <- NULL
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

load_ann_demo_csv_data <- function(path = demo_ann_csv_path) {
  trim_matrix(read_ann_demo_csv_sheet(path))
}

build_ann_model <- function(data2_sheet, target_col = 1, feature_start_col = 2, algorithm = "ann_workbook", include_advanced_analysis = FALSE, trainning_sheet = NULL) {
  trimmed <- trim_matrix(data2_sheet)
  if (nrow(trimmed) < 3 || ncol(trimmed) < 2) {
    stop("`data2` does not contain enough rows or columns for ANN modeling.")
  }

  header <- as.character(unlist(trimmed[1, , drop = TRUE], use.names = FALSE))
  body <- trimmed[-1, , drop = FALSE]
  numeric_body <- safe_numeric_matrix(body)
  numeric_body[is.na(numeric_body)] <- 0

  target <- as.numeric(numeric_body[[target_col]])
  feature_cols <- seq.int(feature_start_col, ncol(numeric_body))
  features <- numeric_body[feature_cols]
  if (!length(features)) {
    stop("`data2` does not contain feature columns for ANN modeling.")
  }

  feature_headers <- header[feature_cols]
  names(features) <- make.names(
    ifelse(
      is.na(feature_headers) | feature_headers == "",
      paste0("feature_", seq_along(feature_headers)),
      feature_headers
    ),
    unique = TRUE
  )

  ann_model_source <- "Fallback R neural net"
  model <- NULL
  if (!is.null(trainning_sheet) && is.data.frame(trainning_sheet) && nrow(trainning_sheet) > 0) {
    workbook_model <- tryCatch(
      build_ann_workbook_model(trainning_sheet, names(features)),
      error = function(e) e
    )
    if (!inherits(workbook_model, "error")) {
      model <- workbook_model
      ann_model_source <- "Workbook trainning sheet"
    }
  }
  if (is.null(model)) {
    fallback_algorithm <- if (identical(algorithm, "ann_workbook")) "neural_net" else algorithm
    model <- fit_prediction_model(features, target, algorithm = fallback_algorithm)
  }

  fitted_pred <- predict_model(model, features)
  prediction_table <- data.frame(
    sample_id = seq_along(target),
    actual = ifelse(target == 1, "Positive", "Negative"),
    actual_class = target,
    score = round(fitted_pred$score, 6),
    prob_positive = round(fitted_pred$prob, 6),
    predicted = ifelse(fitted_pred$class == 1, "Positive", "Negative"),
    predicted_class = fitted_pred$class,
    correct = ifelse(fitted_pred$class == target, "Correct", "Incorrect"),
    check.names = FALSE
  )

  auc_value <- compute_auc(target, fitted_pred$prob)
  roc_table <- build_roc_auc_table(target, fitted_pred$prob)
  metrics <- build_binary_metrics(target, fitted_pred$class, auc_value)
  confusion_table <- build_confusion_summary(target, fitted_pred$class, auc_value)
  validation_algorithm <- if (identical(model$algorithm, "ann_workbook")) "neural_net" else model$algorithm
  validation <- build_validation_results(features, target, algorithm = validation_algorithm)
  forest <- build_forest_data(features, target)
  advanced_analysis <- if (isTRUE(include_advanced_analysis)) {
    list(
      feature_extraction = build_feature_extraction_results(features, target),
      importance = build_importance_results(features, target, algorithm = validation_algorithm)
    )
  } else {
    empty_advanced_analysis("Feature steps and importance are available only for uploaded data and ANN demo CSV runs.")
  }
  feature_extraction <- advanced_analysis$feature_extraction
  importance <- advanced_analysis$importance
  comparison <- build_comparison_results(features, target, selected_features = importance$final_features, ann_reference_model = model)

  list(
    algorithm = algorithm,
    model = model,
    model_source = ann_model_source,
    features = features,
    target = target,
    prediction_table = prediction_table,
    roc_points = roc_table$points,
    auc_summary = rbind(
      data.frame(metric = "ANN engine", value = ann_model_source, check.names = FALSE),
      roc_table$summary
    ),
    auc_value = auc_value,
    metrics = metrics,
    confusion_table = confusion_table,
    validation = validation,
    forest = forest,
    feature_extraction = feature_extraction,
    comparison = comparison,
    importance = importance
  )
}

build_cnn2_model <- function(dataabc_sheet, cnn2_sheet, algorithm = "logistic_regression", include_advanced_analysis = FALSE) {
  dimension <- suppressWarnings(as.integer(cell_value(cnn2_sheet, 1, 11)))
  if (is.na(dimension) || dimension < 2) {
    dimension <- 2
  }

  trimmed <- trim_matrix(dataabc_sheet)
  if (nrow(trimmed) < 3 || ncol(trimmed) <= dimension) {
    stop("`dataabc` does not contain enough rows or feature columns to build the model.")
  }

  model_rows <- seq.int(2, nrow(trimmed))
  labels <- safe_numeric_matrix(trimmed[model_rows, seq_len(dimension), drop = FALSE])
  features <- safe_numeric_matrix(trimmed[model_rows, (dimension + 1):ncol(trimmed), drop = FALSE])
  features[is.na(features)] <- 0

  target <- ifelse(labels[[1]] == 1, 1, 0)
  model <- fit_prediction_model(features, target, algorithm = algorithm)
  fitted_pred <- predict_model(model, features)
  linear_score <- fitted_pred$score
  prob_positive <- fitted_pred$prob
  predicted_positive <- fitted_pred$class
  predicted_label <- ifelse(predicted_positive == 1, "Positive", "Negative")
  actual_label <- ifelse(target == 1, "Positive", "Negative")
  residual_sq <- (target - prob_positive) ^ 2

  prediction_table <- data.frame(
    sample_id = seq_along(target),
    actual = actual_label,
    actual_positive = target,
    actual_negative = 1 - target,
    linear_score = round(linear_score, 6),
    prob_positive = round(prob_positive, 6),
    prob_negative = round(1 - prob_positive, 6),
    predicted = predicted_label,
    predicted_positive = predicted_positive,
    predicted_negative = 1 - predicted_positive,
    residual_sq = round(residual_sq, 6),
    correct = ifelse(predicted_positive == target, "Correct", "Incorrect"),
    check.names = FALSE
  )

  roc_table <- build_roc_auc_table(target, prob_positive)
  metrics <- build_binary_metrics(target, predicted_positive, roc_table$auc)
  confusion_table <- build_confusion_summary(target, predicted_positive, roc_table$auc)
  validation <- build_validation_results(features, target, algorithm = algorithm)
  forest <- build_forest_data(features, target)
  advanced_analysis <- if (isTRUE(include_advanced_analysis)) {
    list(
      feature_extraction = build_feature_extraction_results(features, target),
      importance = build_importance_results(features, target, algorithm = algorithm)
    )
  } else {
    empty_advanced_analysis("Feature steps and importance are available only for uploaded data and ANN demo CSV runs.")
  }
  feature_extraction <- advanced_analysis$feature_extraction
  importance <- advanced_analysis$importance
  comparison <- build_comparison_results(features, target, selected_features = importance$final_features)

  list(
    algorithm = algorithm,
    dimension = dimension,
    model = model,
    features = features,
    target = target,
    prediction_table = prediction_table,
    roc_points = roc_table$points,
    auc_summary = roc_table$summary,
    auc_value = roc_table$auc,
    metrics = metrics,
    confusion_table = confusion_table,
    validation = validation,
    forest = forest,
    feature_extraction = feature_extraction,
    comparison = comparison,
    importance = importance
  )
}

build_roc_auc_table <- function(actual, score) {
  thresholds <- sort(unique(c(1, score, 0)), decreasing = TRUE)
  roc_rows <- lapply(thresholds, function(threshold) {
    predicted <- ifelse(score >= threshold, 1, 0)
    tp <- sum(predicted == 1 & actual == 1)
    fp <- sum(predicted == 1 & actual == 0)
    tn <- sum(predicted == 0 & actual == 0)
    fn <- sum(predicted == 0 & actual == 1)
    tpr <- if ((tp + fn) > 0) tp / (tp + fn) else 0
    fpr <- if ((fp + tn) > 0) fp / (fp + tn) else 0
    precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    data.frame(
      threshold = round(threshold, 6),
      TP = tp,
      FP = fp,
      TN = tn,
      FN = fn,
      TPR = round(tpr, 6),
      FPR = round(fpr, 6),
      precision = round(precision, 6),
      recall = round(tpr, 6)
    )
  })

  roc_points <- do.call(rbind, roc_rows)
  roc_points <- roc_points[order(roc_points$FPR, roc_points$TPR), , drop = FALSE]

  auc <- 0
  if (nrow(roc_points) > 1) {
    for (i in 2:nrow(roc_points)) {
      width <- roc_points$FPR[i] - roc_points$FPR[i - 1]
      height <- (roc_points$TPR[i] + roc_points$TPR[i - 1]) / 2
      auc <- auc + width * height
    }
  }

  auc_summary <- data.frame(
    metric = c("AUC", "ROC points", "Positive cases", "Negative cases"),
    value = c(
      round(auc, 6),
      nrow(roc_points),
      sum(actual == 1),
      sum(actual == 0)
    ),
    check.names = FALSE
  )

  list(points = roc_points, summary = auc_summary, auc = auc)
}

build_binary_metrics <- function(actual, predicted, auc) {
  tp <- sum(predicted == 1 & actual == 1)
  fp <- sum(predicted == 1 & actual == 0)
  tn <- sum(predicted == 0 & actual == 0)
  fn <- sum(predicted == 0 & actual == 1)

  accuracy <- if ((tp + tn + fp + fn) > 0) (tp + tn) / (tp + tn + fp + fn) else NA_real_
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
  f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    NA_real_
  }

  data.frame(
    metric = c("Accuracy", "Precision (PPV)", "Recall (Sensitivity)", "Specificity", "F1 Score", "AUC", "TP", "FP", "TN", "FN"),
    value = round(c(accuracy, precision, recall, specificity, f1, auc, tp, fp, tn, fn), 6),
    check.names = FALSE
  )
}

build_confusion_summary <- function(actual, predicted, auc) {
  tp <- sum(predicted == 1 & actual == 1)
  fp <- sum(predicted == 1 & actual == 0)
  tn <- sum(predicted == 0 & actual == 0)
  fn <- sum(predicted == 0 & actual == 1)

  sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  fnr <- if ((tp + fn) > 0) fn / (tp + fn) else NA_real_
  fpr <- if ((fp + tn) > 0) fp / (fp + tn) else NA_real_
  specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
  ppv <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
  for_value <- if ((fn + tn) > 0) fn / (fn + tn) else NA_real_
  fdr <- if ((tp + fp) > 0) fp / (tp + fp) else NA_real_
  npv <- if ((fn + tn) > 0) tn / (fn + tn) else NA_real_
  lr_pos <- if (!is.na(sensitivity) && !is.na(fpr) && fpr > 0) sensitivity / fpr else NA_real_
  lr_neg <- if (!is.na(fnr) && !is.na(specificity) && specificity > 0) fnr / specificity else NA_real_
  dor <- if (!is.na(lr_pos) && !is.na(lr_neg) && lr_neg > 0) lr_pos / lr_neg else NA_real_
  f1 <- if (!is.na(ppv) && !is.na(sensitivity) && (ppv + sensitivity) > 0) {
    2 * ppv * sensitivity / (ppv + sensitivity)
  } else {
    NA_real_
  }
  prevalence <- if (length(actual) > 0) mean(actual == 1) else NA_real_
  post_prob <- if (!is.na(ppv) && !is.na(for_value) && !is.na(prevalence)) {
    ppv * prevalence + for_value * (1 - prevalence)
  } else {
    NA_real_
  }
  accuracy <- if (length(actual) > 0) mean(actual == predicted) else NA_real_
  bm <- if (!is.na(sensitivity) && !is.na(specificity)) sensitivity + specificity - 1 else NA_real_
  mk <- if (!is.na(ppv) && !is.na(npv)) ppv + npv - 1 else NA_real_

  data.frame(
    section = c(
      "True condition", "",
      "Predicted", "",
      "LR+", "LR-",
      "DOR", "F1 score",
      "Prevalence(prior prob.)", "Accuracy (ACC)",
      "", "ROC"
    ),
    item = c(
      "Positive", "Negative",
      "Positive", "Negative",
      "Sensitivity", "FNR(Miss rate)",
      "", "",
      "", "",
      "", "AUC"
    ),
    `True Positive` = c(
      "Positive", "Negative",
      tp, fn,
      round(sensitivity, 6), round(fnr, 6),
      "", round(f1, 6),
      round(prevalence, 6), round(accuracy, 6),
      "", round(auc, 6)
    ),
    `True Negative` = c(
      "PPV/FOR", "FDR/NPV",
      fp, tn,
      round(fpr, 6), round(specificity, 6),
      round(dor, 6), "AUROC",
      "Post prob.", "",
      "", ""
    ),
    `Metric 1` = c(
      "", "",
      round(ppv, 6), round(for_value, 6),
      "FPR", "Specificity",
      "", round(auc, 6),
      round(post_prob, 6), "",
      "", ""
    ),
    `Metric 2` = c(
      "", "",
      round(fdr, 6), round(npv, 6),
      "", "",
      "", "",
      paste0("BM=", round(bm, 6)),
      paste0("MK=", round(mk, 6)),
      "", ""
    ),
    check.names = FALSE
  )
}

format_check_message <- function(check, generated_name, workbook_name) {
  if (check$match) {
    sprintf("%s matched `%s` in the workbook.", generated_name, workbook_name)
  } else {
    sprintf(
      "%s did not match `%s` (%s). This workbook appears to keep a different reference dataset in that sheet.",
      generated_name,
      workbook_name,
      check$message
    )
  }
}

load_demo_data <- function(path, algorithm = "logistic_regression") {
  data_sheet <- read_sheet_matrix(path, "data")
  data_demo <- extract_demo_data_block(data_sheet)
  data2_generated <- build_data2(data_sheet)
  data2_workbook <- trim_matrix(read_sheet_matrix(path, "data2"))
  cnn2_sheet <- read_sheet_matrix(path, "CNN2")
  window_sheet <- read_sheet_matrix(path, "window")
  dataabc_generated <- build_dataabc(data2_generated, cnn2_sheet, window_sheet)
  dataabc_workbook <- trim_matrix(read_sheet_matrix(path, "dataabc"))
  patterns <- extract_patterns(data_sheet)

  list(
    mode = "CNN",
    data_sheet = data_sheet,
    data_demo = data_demo,
    data2_generated = trim_matrix(data2_generated),
    data2_workbook = data2_workbook,
    cnn2_workbook = trim_matrix(cnn2_sheet),
    dataabc_generated = trim_matrix(dataabc_generated),
    dataabc_workbook = dataabc_workbook,
    cnn2_model = build_cnn2_model(dataabc_generated, cnn2_sheet, algorithm = algorithm),
    patterns = patterns$patterns,
    labels = patterns$labels,
    pattern_gallery = build_pattern_gallery(patterns$patterns, patterns$labels),
    data2_check = compare_frames(data2_generated, data2_workbook),
    dataabc_check = compare_frames(dataabc_generated, dataabc_workbook)
  )
}

load_ann_demo_data <- function(path, data_source = c("original", "normalization"), algorithm = "ann_workbook", include_advanced_analysis = FALSE, use_direct_data2 = FALSE) {
  data_source <- match.arg(data_source)
  data_sheet <- read_ann_sheet(path, "data")
  data_demo <- extract_demo_data_block(data_sheet)
  data2_workbook <- trim_matrix(read_ann_sheet(path, "data2"))
  trainning_workbook <- if ("trainning" %in% openxlsx::getSheetNames(path)) trim_matrix(read_ann_sheet(path, "trainning")) else data.frame()
  df_workbook <- if ("DF" %in% openxlsx::getSheetNames(path)) trim_matrix(read_ann_sheet(path, "DF")) else data.frame()
  patterns <- extract_patterns(data_sheet)

  if (isTRUE(use_direct_data2)) {
    ann_input <- if (identical(data_source, "original")) data2_workbook else normalize_data2_sheet(data2_workbook, target_col = 1, feature_start_col = 2)
    ann_model <- build_ann_model(ann_input, target_col = 1, feature_start_col = 2, algorithm = algorithm, include_advanced_analysis = include_advanced_analysis, trainning_sheet = trainning_workbook)
    data2_message <- if (identical(data_source, "original")) {
      "ANN uploaded data ran directly from `data2`."
    } else {
      "ANN uploaded data ran after z-score normalization of each predictor column."
    }
  } else if (identical(data_source, "original")) {
    ann_input <- trim_matrix(build_data2(data_sheet))
    ann_model <- build_ann_model(ann_input, target_col = 1, feature_start_col = 3, algorithm = algorithm, include_advanced_analysis = include_advanced_analysis, trainning_sheet = trainning_workbook)
    data2_message <- "ANN workbook rebuilt from original `data`."
  } else {
    ann_input <- data2_workbook
    ann_model <- build_ann_model(ann_input, target_col = 1, feature_start_col = 2, algorithm = algorithm, include_advanced_analysis = include_advanced_analysis, trainning_sheet = trainning_workbook)
    data2_message <- "ANN workbook uses normalized `data2` directly."
  }

  list(
    mode = "ANN",
    data_source = data_source,
    algorithm = algorithm,
    data_sheet = data_sheet,
    data_demo = data_demo,
    data2_generated = ann_input,
    data2_workbook = data2_workbook,
    cnn2_workbook = data.frame(),
    dataabc_generated = data.frame(),
    dataabc_workbook = data.frame(),
    trainning_workbook = trainning_workbook,
    df_workbook = df_workbook,
    cnn2_model = ann_model,
    patterns = patterns$patterns,
    labels = patterns$labels,
    pattern_gallery = build_pattern_gallery(patterns$patterns, patterns$labels),
    data2_check = list(match = TRUE, message = data2_message),
    dataabc_check = list(match = TRUE, message = "ANN workbook does not use `dataabc`.")
  )
}

load_ann_demo_csv_result <- function(path = demo_ann_csv_path, algorithm = "ann_workbook", include_advanced_analysis = FALSE) {
  data_sheet <- read_ann_sheet(demo_workbook_ann, "data")
  data_demo <- extract_demo_data_block(data_sheet)
  csv_data2 <- trim_matrix(read_ann_demo_csv_sheet(path))
  trainning_workbook <- if ("trainning" %in% openxlsx::getSheetNames(demo_workbook_ann)) trim_matrix(read_ann_sheet(demo_workbook_ann, "trainning")) else data.frame()
  patterns <- extract_patterns(data_sheet)
  ann_model <- build_ann_model(
    csv_data2,
    target_col = 1,
    feature_start_col = 2,
    algorithm = algorithm,
    include_advanced_analysis = include_advanced_analysis,
    trainning_sheet = trainning_workbook
  )

  list(
    mode = "ANN",
    data_source = "normalization",
    algorithm = algorithm,
    data_sheet = data_sheet,
    data_demo = data_demo,
    data2_generated = csv_data2,
    data2_workbook = csv_data2,
    cnn2_workbook = data.frame(),
    dataabc_generated = data.frame(),
    dataabc_workbook = data.frame(),
    trainning_workbook = trainning_workbook,
    df_workbook = data.frame(),
    cnn2_model = ann_model,
    patterns = patterns$patterns,
    labels = patterns$labels,
    pattern_gallery = build_pattern_gallery(patterns$patterns, patterns$labels),
    data2_check = list(match = TRUE, message = "ANN demo CSV is using the appended 15-column sample file directly."),
    dataabc_check = list(match = TRUE, message = "ANN workbook does not use `dataabc`.")
  )
}

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  gap = "1rem",
  padding = "1rem",
  tags$head(
    tags$style(HTML("
      .model-results-panel,
      .model-results-panel .tab-content,
      .model-results-panel .tab-pane,
      .model-results-panel .card,
      .model-results-panel .card-body {
        height: auto !important;
        max-height: none !important;
        overflow: visible !important;
      }

      .model-results-panel .datatables,
      .model-results-panel .dataTables_wrapper,
      .model-results-panel .html-widget {
        overflow: visible !important;
      }
    "))
  ),
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      h3("Prediction model comparison"),
      p("Load the bundled Excel demos or upload a workbook to reproduce the VBA-style modeling pipeline."),
      p("Uploaded data are treated as `data2-style` input with one status column. If you choose `CNN`, the app will transform status `0/1` into `[0,1] / [1,0]` before building `dataabc`. For `ANN` and other algorithms, the status column is left unchanged."),
      fileInput("workbook", "Upload `.xlsm` workbook", accept = c(".xlsm", ".xlsx")),
      div(
        style = paste(
          "border: 3px solid #7f8c8d;",
          "border-radius: 14px;",
          "padding: 12px;",
          "margin-top: 10px;",
          "margin-bottom: 10px;",
          "background: #f8fbfc;"
        ),
        tags$div(
          "Model selection",
          style = "font-weight: 700; color: #2c3e50; margin-bottom: 10px;"
        ),
        p("Use one model selection. CNN is the only special path: it transforms the first status column into `1 0` / `0 1` before the CNN-style expansion. ANN and the other prediction algorithms keep status in the first column and run directly on the remaining variables."),
        p("Algorithm steps: CNN = status recoding -> `dataabc` expansion -> prediction. ANN and the other algorithms = direct tabular prediction from first-column status plus columns 2 to n as predictors."),
        selectInput(
          "model_configuration",
          "Model configuration",
          choices = model_configuration_choices,
          selected = "ann"
        )
      ),
      downloadButton("download_ann_demo_csv", "Download ANN data2 demo CSV"),
      actionButton("run_demo_csv", "Run selected algorithm on demo CSV"),
      downloadButton("download_demo_sample", "Download sample example for demo run"),
      div(
        style = paste(
          "border: 3px solid #7f8c8d;",
          "border-radius: 14px;",
          "padding: 12px;",
          "margin-top: 10px;",
          "margin-bottom: 10px;",
          "background: #f8fbfc;"
        ),
        tags$div(
          "Uploaded Prediction",
          style = "font-weight: 700; color: #2c3e50; margin-bottom: 10px;"
        ),
        actionButton("run_uploaded_model_original", "Prediction model (Original data)", class = "btn-success"),
        tags$div(style = "height: 10px;"),
        actionButton("run_uploaded_model_normalized", "Prediction model (Normalization data)")
      ),
      actionButton("load_demo_cnn", "Load bundled demo button(CNN)", class = "btn-primary"),
      actionButton("load_demo_ann", "Load bundled demo button(ANN)"),
      uiOutput("pattern_picker"),
      hr(),
      h4("New Pattern"),
      p("Enter 12 binary pixels row by row, separated by spaces or commas."),
      textAreaInput(
        "new_pattern_text",
        NULL,
        value = "1 1 1\n1 0 1\n1 0 1\n1 1 1",
        rows = 5,
        resize = "vertical"
      ),
      verbatimTextOutput("new_pattern_prediction"),
      hr(),
      strong("Bundled file"),
      textOutput("bundled_path")
    ),
    card(
      full_screen = TRUE,
      card_header("Workbook status"),
      card_body(
        verbatimTextOutput("status")
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Original 4 x 3 pattern"),
      card_body(
        uiOutput("pattern_plot"),
        tableOutput("pattern_table")
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Training Pattern Gallery"),
      card_body(
        p("`#` means pixel = 1, `.` means pixel = 0."),
        DTOutput("pattern_gallery_table")
      )
    ),
    div(
      class = "model-results-panel",
      navset_card_tab(
        title = "Model results",
        nav_panel("Predictions", DTOutput("cnn2_predictions_table")),
        nav_panel("ROC-AUC summary", DTOutput("roc_auc_summary_table")),
        nav_panel(
          "AUC",
          card(
            card_header("AUC summary"),
            DTOutput("roc_auc_summary_table")
          ),
          card(
            card_header("Confusion table"),
            DTOutput("auc_confusion_table")
          ),
          card(
            card_header("ROC points table"),
            DTOutput("roc_points_table")
          ),
          card(
            card_header("ROC curve"),
            plotOutput("roc_plot", height = 320)
          )
        ),
        nav_panel("ROC points", DTOutput("roc_points_table")),
        nav_panel("Metrics", DTOutput("cnn2_metrics_table")),
        nav_panel(
          "Feature extraction",
          card(
            card_header("Feature extraction summary"),
            p("Step 1: single-variable logistic regression with p < 0.05 and confidence interval not crossing 0."),
            p("Step 2: positive and negative multivariable logistic regression to remove weak predictors."),
            p("Step 3: grouped positive/negative sums and prediction power."),
            p("Step 4: forest plot of retained variables."),
            DTOutput("feature_extraction_summary_table")
          ),
          card(
            card_header("Step 1: single-variable logistic regression"),
            DTOutput("feature_step1_table")
          ),
          card(
            card_header("Step 2: positive/negative multivariable logistic regression"),
            DTOutput("feature_step2_table")
          ),
          card(
            card_header("Step 3: grouped positive/negative sums"),
            p("Box plots show the summed positive and negative scores across status 0 and status 1."),
            plotOutput("feature_step3_box_plot", height = 420),
            DTOutput("feature_step3_scores_table"),
            DTOutput("feature_step3_table"),
            DTOutput("feature_step3_metrics_table")
          ),
          card(
            card_header("Step 4: forest plot of retained variables"),
            plotOutput("feature_extraction_forest_plot", height = 820, width = "100%"),
            DTOutput("feature_final_forest_table")
          )
        ),
        nav_panel(
          "Forest",
          card(
            card_header("Forest summary"),
            checkboxInput("forest_significant_only", "Show significant variables only", value = FALSE),
            DTOutput("forest_summary_table")
          ),
          card(
            card_header("Forest table"),
            DTOutput("forest_table")
          ),
          card(
            card_header("Forest plot"),
            plotOutput("forest_plot", height = 900, width = "100%"),
            uiOutput("forest_code_box")
          )
        ),
        nav_panel(
          "Importance",
          card(
            card_header("Importance summary"),
            p("Variables are removed one by one using the currently selected prediction algorithm. At each step, the app removes the variable whose deletion gives the best test ROC-AUC, until 10 variables remain."),
            DTOutput("importance_summary_table")
          ),
          card(
            card_header("Backward elimination by ROC-AUC"),
            DTOutput("importance_elimination_table")
          ),
          card(
            card_header("Final 10 selected variables"),
            DTOutput("importance_final_table")
          ),
          card(
            card_header("Importance forest plot"),
            plotOutput("importance_forest_plot", height = 920, width = "100%"),
            uiOutput("importance_code_box"),
            DTOutput("importance_forest_table")
          )
        ),
        nav_panel(
          "Comparison",
          card(
            card_header("Top 5 methods by test AUC"),
            DTOutput("comparison_top5_table")
          ),
          card(
            card_header("Algorithm comparison table"),
            DTOutput("comparison_table")
          ),
          card(
            card_header("Algorithm comparison plot"),
            plotOutput("comparison_plot", height = 1320, width = "100%"),
            uiOutput("comparison_original_code_box"),
            uiOutput("comparison_top10_code_box")
          )
        ),
        nav_panel(
          "Prediction mode",
          layout_columns(
            col_widths = c(4, 8),
            card(
              card_header("Input significant variables"),
              uiOutput("prediction_mode_inputs"),
              layout_columns(
                col_widths = c(6, 6),
                actionButton("clear_prediction_mode", "Clean value"),
                actionButton("run_prediction_mode", "Run prediction mode", class = "btn-primary")
              ),
              tags$div(style = "height: 10px;"),
              verbatimTextOutput("prediction_mode_formula"),
              tags$div(style = "height: 10px;"),
              verbatimTextOutput("prediction_mode_result")
            ),
            card(
              card_header("Probability category curve (PCC)"),
              div(style = "display:flex; justify-content:center;", plotOutput("pcc_plot", height = 700, width = "700px"))
            )
          )
        ),
        nav_panel(
          "Validation",
          card(
            card_header("Validation summary"),
            DTOutput("validation_summary_table")
          ),
          card(
            card_header("Cross-validation"),
            DTOutput("validation_cv_table")
          ),
          card(
            card_header("Test-set confusion matrix"),
            DTOutput("validation_confusion_table")
          ),
          card(
            card_header("Test-set predictions"),
            DTOutput("validation_test_predictions_table")
          )
        )
      )
    ),
    navset_card_tab(
      full_screen = TRUE,
      title = "Sheet contents",
      nav_panel("data", DTOutput("data_table")),
      nav_panel("data2", DTOutput("data2_table")),
      nav_panel("dataabc", DTOutput("dataabc_table")),
      nav_panel("Workbook CNN2 reference", DTOutput("cnn2_workbook_table")),
      nav_panel("Workbook trainning reference", DTOutput("trainning_workbook_table")),
      nav_panel("Workbook DF reference", DTOutput("df_workbook_table")),
      nav_panel("Workbook data2 reference", DTOutput("data2_workbook_table")),
      nav_panel("Workbook dataabc reference", DTOutput("dataabc_workbook_table")),`r`n      nav_panel("README", uiOutput("readme_content"))
    )
  )
)

server <- function(input, output, session) {
  workbook_data <- reactiveVal(NULL)
  load_message <- reactiveVal("No workbook loaded yet.")

  load_workbook_data <- function(path, label, data_source = c("original", "normalization"), algorithm = "logistic_regression", include_advanced_analysis = FALSE) {
    data_source <- match.arg(data_source)
    withProgress(message = sprintf("Loading %s", label), value = 0, {
      incProgress(0.15, detail = "Reading `data` sheet")
      data_sheet <- read_sheet_matrix(path, "data")

      incProgress(0.20, detail = "Preparing compact `data` view")
      data_demo <- extract_demo_data_block(data_sheet)

      incProgress(0.15, detail = "Reading workbook reference sheets")
      data2_workbook <- trim_matrix(read_sheet_matrix(path, "data2"))
      cnn2_sheet <- read_sheet_matrix(path, "CNN2")
      window_sheet <- read_sheet_matrix(path, "window")
      dataabc_workbook <- trim_matrix(read_sheet_matrix(path, "dataabc"))

      incProgress(0.20, detail = if (identical(data_source, "original")) "Running VBA-style `data2` transformation" else "Using workbook normalized `data2`")
      data2_generated <- if (identical(data_source, "original")) {
        build_data2(data_sheet)
      } else {
        convert_status_to_cnn_data2(data2_workbook)
      }

      incProgress(0.20, detail = "Running `dataabc` window expansion")
      dataabc_generated <- build_dataabc(data2_generated, cnn2_sheet, window_sheet)

      incProgress(0.10, detail = "Building CNN2-style classifier and ROC-AUC summary")
      cnn2_model <- build_cnn2_model(dataabc_generated, cnn2_sheet, algorithm = algorithm, include_advanced_analysis = include_advanced_analysis)

      incProgress(0.10, detail = "Preparing pattern preview")
      patterns <- extract_patterns(data_sheet)

        result <- list(
          mode = "CNN",
          data_source = data_source,
          algorithm = algorithm,
          data_sheet = data_sheet,
          data_demo = data_demo,
          data2_generated = trim_matrix(data2_generated),
        data2_workbook = data2_workbook,
        cnn2_workbook = trim_matrix(cnn2_sheet),
          dataabc_generated = trim_matrix(dataabc_generated),
          dataabc_workbook = dataabc_workbook,
          cnn2_model = cnn2_model,
          patterns = patterns$patterns,
          labels = patterns$labels,
          pattern_gallery = build_pattern_gallery(patterns$patterns, patterns$labels),
          data2_check = compare_frames(data2_generated, data2_workbook),
          dataabc_check = compare_frames(dataabc_generated, dataabc_workbook)
        )

      load_message(sprintf(
        paste(
          "Loaded workbook: %s",
          "Scaling/transform process completed.",
          "Algorithm: %s",
          "Data source: %s",
          "Visible source block: %s rows x %s columns.",
          "Generated data2: %s rows x %s columns.",
          "Generated dataabc: %s rows x %s columns.",
          "Model AUC: %s",
          sep = "\n"
        ),
        label,
        algorithm_label(algorithm),
        data_source,
        nrow(result$data_demo), ncol(result$data_demo),
        nrow(result$data2_generated), ncol(result$data2_generated),
        nrow(result$dataabc_generated), ncol(result$dataabc_generated),
        round(result$cnn2_model$auc_value, 6)
      ))

      result
    })
  }

  build_pipeline_comparison_bundle <- function(results) {
    labeled_tables <- Filter(Negate(is.null), lapply(results, function(result) {
      if (is.null(result) || is.null(result$cnn2_model) || is.null(result$cnn2_model$comparison)) {
        return(NULL)
      }

      comparison_table <- result$cnn2_model$comparison$table
      if (!is.data.frame(comparison_table) || !nrow(comparison_table)) {
        return(NULL)
      }

      comparison_table$pipeline <- if (is.null(result$mode)) "CNN" else result$mode
      comparison_table
    }))

    if (!length(labeled_tables)) {
      empty_table <- data.frame(
        algorithm = character(),
        split = character(),
        n = numeric(),
        sensitivity = numeric(),
        specificity = numeric(),
        precision = numeric(),
        f1_score = numeric(),
        accuracy = numeric(),
        auc = numeric(),
        ci = character(),
        ci_low = numeric(),
        ci_high = numeric(),
        status = character(),
        pipeline = character(),
        check.names = FALSE
      )
      return(list(table = empty_table, top5 = empty_table))
    }

    comparison_table <- do.call(rbind, labeled_tables)
    comparison_table$split <- factor(as.character(comparison_table$split), levels = c("Training", "Testing"))
    comparison_table$pipeline <- factor(as.character(comparison_table$pipeline), levels = c("CNN", "ANN"))
    comparison_table <- comparison_table[order(comparison_table$algorithm, comparison_table$pipeline, comparison_table$split), , drop = FALSE]
    comparison_table$split <- as.character(comparison_table$split)
    comparison_table$pipeline <- as.character(comparison_table$pipeline)
    rownames(comparison_table) <- NULL

    testing_only <- comparison_table[comparison_table$split == "Testing" & comparison_table$status == "OK" & !is.na(comparison_table$auc), , drop = FALSE]
    if ("variable_set" %in% names(testing_only)) {
      testing_only$variable_set <- factor(as.character(testing_only$variable_set), levels = c("Original", "Top 10"))
      testing_only <- testing_only[order(-testing_only$auc, testing_only$pipeline, testing_only$algorithm, testing_only$variable_set), , drop = FALSE]
      testing_only$variable_set <- as.character(testing_only$variable_set)
    }
    testing_only <- testing_only[order(-testing_only$auc, testing_only$pipeline, testing_only$algorithm), , drop = FALSE]
    top5_table <- utils::head(testing_only, 5)

    list(table = comparison_table, top5 = top5_table)
  }

  attach_pipeline_comparison_bundle <- function(primary_result, results) {
    primary_result$comparison_bundle <- build_pipeline_comparison_bundle(results)
    primary_result
  }

  safe_load_pipeline <- function(loader) {
    tryCatch(loader(), error = function(e) e)
  }
  first_successful_pipeline <- function(...) {
    candidates <- list(...)
    for (candidate in candidates) {
      if (!inherits(candidate, "error") && !is.null(candidate)) {
        return(candidate)
      }
    }
    NULL
  }


  summarize_pipeline_load <- function(label, result) {
    if (inherits(result, "error")) {
      sprintf("%s unavailable: %s", label, result$message)
    } else {
      sprintf("%s AUC: %s", label, round(result$cnn2_model$auc_value, 6))
    }
  }

  output$bundled_path <- renderText({
    paste(
      sprintf("CNN: %s", normalizePath(demo_workbook_cnn, winslash = "/", mustWork = FALSE)),
      sprintf("ANN: %s", normalizePath(demo_workbook_ann, winslash = "/", mustWork = FALSE)),
      sep = "\n"
    )
  })

  output$download_ann_demo_csv <- downloadHandler(
    filename = function() {
      "Ch032dimensionANNverticle_data2_demo.csv"
    },
    content = function(file) {
      file.copy(demo_ann_csv_path, file, overwrite = TRUE)
    }
  )

  output$download_demo_sample <- downloadHandler(
    filename = function() {
      if (identical(resolve_model_configuration(input$model_configuration)$mode, "ANN")) {
        "Ch032dimensionANNverticle_data2_demo.csv"
      } else {
        "Ch05PrembinaryCNNverticle.xlsm"
      }
    },
    content = function(file) {
      if (identical(resolve_model_configuration(input$model_configuration)$mode, "ANN")) {
        file.copy(demo_ann_csv_path, file, overwrite = TRUE)
      } else {
        file.copy(demo_workbook_cnn, file, overwrite = TRUE)
      }
    }
  )

  observeEvent(input$load_demo_cnn, {
    selected_config <- resolve_model_configuration(input$model_configuration)
    selected_algorithm <- selected_config$algorithm
    validate(need(file.exists(demo_workbook_cnn), paste("Missing bundled workbook:", demo_workbook_cnn)))
    validate(need(file.exists(demo_workbook_ann), paste("Missing bundled workbook:", demo_workbook_ann)))
    cnn_result <- safe_load_pipeline(function() {
      load_workbook_data(
        demo_workbook_cnn,
        demo_workbook_cnn,
        data_source = "original",
        algorithm = selected_algorithm,
        include_advanced_analysis = TRUE
      )
    })
    ann_result <- safe_load_pipeline(function() {
      load_ann_demo_data(
        demo_workbook_ann,
        data_source = "original",
        algorithm = selected_algorithm,
        include_advanced_analysis = TRUE
      )
    })
    primary_result <- first_successful_pipeline(cnn_result, ann_result)
    if (is.null(primary_result)) {
      stop("Both CNN and ANN demo pipelines failed to load.")
    }
    workbook_data(attach_pipeline_comparison_bundle(primary_result, list(cnn_result, ann_result)))
    load_message(paste(
      sprintf("Loaded workbook: %s", demo_workbook_cnn),
      "Side-by-side comparison built for bundled CNN and ANN demos.",
      sprintf("Algorithm: %s", algorithm_label(selected_algorithm)),
      sprintf("Data source: %s", "original"),
      summarize_pipeline_load("CNN", cnn_result),
      summarize_pipeline_load("ANN", ann_result),
      sep = "\n"
    ))
  })

  observeEvent(input$load_demo_ann, {
    selected_config <- resolve_model_configuration(input$model_configuration)
    selected_algorithm <- selected_config$algorithm
    validate(need(file.exists(demo_workbook_ann), paste("Missing bundled workbook:", demo_workbook_ann)))
    validate(need(file.exists(demo_workbook_cnn), paste("Missing bundled workbook:", demo_workbook_cnn)))
    ann_result <- load_ann_demo_data(
      demo_workbook_ann,
      data_source = "original",
      algorithm = selected_algorithm,
      include_advanced_analysis = TRUE
    )
    cnn_result <- safe_load_pipeline(function() {
      load_workbook_data(
        demo_workbook_cnn,
        demo_workbook_cnn,
        data_source = "original",
        algorithm = selected_algorithm,
        include_advanced_analysis = TRUE
      )
    })
    workbook_data(attach_pipeline_comparison_bundle(ann_result, list(cnn_result, ann_result)))
    load_message(paste(
      sprintf("Loaded workbook: %s", demo_workbook_ann),
      "Side-by-side comparison built for bundled CNN and ANN demos.",
      sprintf("Algorithm: %s", algorithm_label(selected_algorithm)),
      sprintf("Data source: %s", "original"),
      sprintf("Visible source block: %s rows x %s columns.", nrow(ann_result$data_demo), ncol(ann_result$data_demo)),
      sprintf("ANN data2: %s rows x %s columns.", nrow(ann_result$data2_workbook), ncol(ann_result$data2_workbook)),
      summarize_pipeline_load("CNN", cnn_result),
      summarize_pipeline_load("ANN", ann_result),
      sep = "\n"
    ))
  })

  observeEvent(input$run_uploaded_model_original, {
    req(input$workbook$datapath)
    selected_config <- resolve_model_configuration(input$model_configuration)
    selected_mode <- selected_config$mode
    selected_algorithm <- selected_config$algorithm
    primary_result <- if (identical(selected_mode, "ANN")) {
      load_ann_demo_data(
        input$workbook$datapath,
        data_source = "original",
        algorithm = selected_algorithm,
        include_advanced_analysis = TRUE,
        use_direct_data2 = TRUE
      )
    } else {
      load_workbook_data(
        input$workbook$datapath,
        input$workbook$name,
        data_source = "original",
        algorithm = selected_algorithm,
        include_advanced_analysis = if (identical(selected_mode, "ANN")) FALSE else TRUE
      )
    }
    cnn_result <- safe_load_pipeline(function() {
      load_workbook_data(
        input$workbook$datapath,
        input$workbook$name,
        data_source = "original",
        algorithm = selected_algorithm,
        include_advanced_analysis = TRUE
      )
    })
    ann_result <- safe_load_pipeline(function() {
      load_ann_demo_data(
        input$workbook$datapath,
        data_source = "original",
        algorithm = selected_algorithm,
        include_advanced_analysis = TRUE,
        use_direct_data2 = TRUE
      )
    })
    workbook_data(attach_pipeline_comparison_bundle(primary_result, list(cnn_result, ann_result)))
    load_message(paste(
      sprintf("Loaded workbook: %s", input$workbook$name),
      "Side-by-side comparison attempted for CNN and ANN using the uploaded workbook.",
      sprintf("Primary display pipeline: %s", selected_mode),
      sprintf("Algorithm: %s", algorithm_label(selected_algorithm)),
      sprintf("Data source: %s", "original"),
      summarize_pipeline_load("CNN", cnn_result),
      summarize_pipeline_load("ANN", ann_result),
      sep = "\n"
    ))
  })

  observeEvent(input$run_uploaded_model_normalized, {
    req(input$workbook$datapath)
    selected_config <- resolve_model_configuration(input$model_configuration)
    selected_mode <- selected_config$mode
    selected_algorithm <- selected_config$algorithm
    primary_result <- if (identical(selected_mode, "ANN")) {
      load_ann_demo_data(
        input$workbook$datapath,
        data_source = "normalization",
        algorithm = selected_algorithm,
        include_advanced_analysis = TRUE,
        use_direct_data2 = TRUE
      )
    } else {
      load_workbook_data(
        input$workbook$datapath,
        input$workbook$name,
        data_source = "normalization",
        algorithm = selected_algorithm,
        include_advanced_analysis = if (identical(selected_mode, "ANN")) FALSE else TRUE
      )
    }
    cnn_result <- safe_load_pipeline(function() {
      load_workbook_data(
        input$workbook$datapath,
        input$workbook$name,
        data_source = "normalization",
        algorithm = selected_algorithm,
        include_advanced_analysis = TRUE
      )
    })
    ann_result <- safe_load_pipeline(function() {
      load_ann_demo_data(
        input$workbook$datapath,
        data_source = "normalization",
        algorithm = selected_algorithm,
        include_advanced_analysis = TRUE,
        use_direct_data2 = TRUE
      )
    })
    workbook_data(attach_pipeline_comparison_bundle(primary_result, list(cnn_result, ann_result)))
    load_message(paste(
      sprintf("Loaded workbook: %s", input$workbook$name),
      "Side-by-side comparison attempted for CNN and ANN using the uploaded workbook.",
      sprintf("Primary display pipeline: %s", selected_mode),
      sprintf("Algorithm: %s", algorithm_label(selected_algorithm)),
      sprintf("Data source: %s", "normalization"),
      summarize_pipeline_load("CNN", cnn_result),
      summarize_pipeline_load("ANN", ann_result),
      sep = "\n"
    ))
  })

  observeEvent(input$run_demo_csv, {
    selected_config <- resolve_model_configuration(input$model_configuration)
    selected_mode <- selected_config$mode
    selected_algorithm <- selected_config$algorithm
    cnn_result <- safe_load_pipeline(function() {
      load_workbook_data(
        demo_workbook_cnn,
        demo_workbook_cnn,
        data_source = "normalization",
        algorithm = selected_algorithm,
        include_advanced_analysis = TRUE
      )
    })
    ann_result <- safe_load_pipeline(function() {
      load_ann_demo_csv_result(
        demo_ann_csv_path,
        algorithm = selected_algorithm,
        include_advanced_analysis = TRUE
      )
    })
    primary_result <- if (identical(selected_mode, "ANN")) ann_result else cnn_result
    if (inherits(primary_result, "error")) {
      stop(primary_result$message)
    }
    workbook_data(attach_pipeline_comparison_bundle(primary_result, list(cnn_result, ann_result)))
    load_message(paste(
      sprintf("Selected algorithm: %s", algorithm_label(selected_algorithm)),
      "Side-by-side comparison built from the normalized demo sources.",
      sprintf("Primary display pipeline: %s", selected_mode),
      summarize_pipeline_load("CNN", cnn_result),
      summarize_pipeline_load("ANN", ann_result),
      sep = "\n"
    ))
  })

  output$pattern_picker <- renderUI({
    req(workbook_data())
    selectInput(
      "pattern_id",
      "Pattern number",
      choices = seq_along(workbook_data()$patterns),
      selected = 1
    )
  })

  prediction_mode_variables <- reactive({
    req(workbook_data())
    get_prediction_variables(workbook_data()$cnn2_model)
  })

  output$status <- renderText({
    if (is.null(workbook_data())) {
      return(load_message())
    }

    loaded_from <- if (!is.null(input$workbook) && nzchar(input$workbook$name)) {
      input$workbook$name
    } else {
      if (!is.null(workbook_data()$mode) && identical(workbook_data()$mode, "ANN")) {
        demo_workbook_ann
      } else {
        demo_workbook_cnn
      }
    }

    checks <- workbook_data()
    paste(
      load_message(),
      "",
      sprintf("Mode: %s", if (is.null(checks$mode)) "CNN" else checks$mode),
      sprintf("Algorithm: %s", algorithm_label(if (is.null(checks$algorithm)) "logistic_regression" else checks$algorithm)),
      sprintf("Data source: %s", if (is.null(checks$data_source)) "original" else checks$data_source),
      sprintf("Loaded workbook: %s", loaded_from),
      format_check_message(checks$data2_check, "Generated `data2`", "data2"),
      if (identical(checks$mode, "CNN")) format_check_message(checks$dataabc_check, "Generated `dataabc`", "dataabc") else "ANN workbook uses the selected data source for modeling.",
      sprintf("Model AUC: %.6f", checks$cnn2_model$auc_value),
      sep = "\n"
    )
  })

  output$prediction_mode_inputs <- renderUI({
    req(prediction_mode_variables())
    vars <- prediction_mode_variables()
    controls <- lapply(seq_len(nrow(vars)), function(i) {
      textInput(
        inputId = paste0("pred_var_", i),
        label = sprintf("%s", vars$feature[i]),
        value = format(round(vars$default_value[i], 4), trim = TRUE, scientific = FALSE)
      )
    })
    do.call(tagList, controls)
  })

  observeEvent(input$clear_prediction_mode, {
    req(prediction_mode_variables())
    vars <- prediction_mode_variables()
    for (i in seq_len(nrow(vars))) {
      updateTextInput(session, paste0("pred_var_", i), value = "")
    }
  })

  prediction_mode_newdata <- reactive({
    req(prediction_mode_variables(), workbook_data())
    vars <- prediction_mode_variables()
    model_features <- workbook_data()$cnn2_model$features
    feature_names <- names(model_features)
    defaults <- as.list(as.data.frame(lapply(model_features, function(col) stats::median(as.numeric(col), na.rm = TRUE)), check.names = FALSE))
    names(defaults) <- feature_names

    for (i in seq_len(nrow(vars))) {
      raw_value <- input[[paste0("pred_var_", i)]]
      if (is.null(raw_value) || !nzchar(trimws(raw_value))) {
        stop(sprintf("Please enter a value for %s.", vars$feature[i]))
      }
      numeric_value <- suppressWarnings(as.numeric(raw_value))
      if (is.na(numeric_value)) {
        stop(sprintf("%s must be numeric.", vars$feature[i]))
      }
      defaults[[vars$feature[i]]] <- numeric_value
    }

    as.data.frame(defaults, check.names = FALSE)
  })

  prediction_mode_pred <- eventReactive(input$run_prediction_mode, {
    req(prediction_mode_newdata(), workbook_data())
    predict_model(workbook_data()$cnn2_model$model, prediction_mode_newdata())
  }, ignoreInit = TRUE)

  prediction_mode_error <- reactiveVal(NULL)

  observeEvent(input$run_prediction_mode, {
    prediction_mode_error(NULL)
    tryCatch(
      {
        prediction_mode_newdata()
      },
      error = function(e) {
        prediction_mode_error(conditionMessage(e))
      }
    )
  })

  output$prediction_mode_formula <- renderText({
    req(workbook_data(), prediction_mode_variables())
    vars <- prediction_mode_variables()
    model_config <- resolve_model_configuration(input$model_configuration)
    fitted_algorithm <- algorithm_label(workbook_data()$cnn2_model$model$algorithm)
    paste(
      sprintf("Prediction mode is using model configuration: %s", model_config$label),
      sprintf("Displayed fitting algorithm for readers: %s", fitted_algorithm),
      "Prediction mode is using the importance-selected variables (final top AUC features) when available.",
      format_composite_formula(
        workbook_data()$cnn2_model$model,
        feature_subset = vars$feature
      ),
      sep = "\n"
    )
  })

  output$prediction_mode_result <- renderText({
    if (!is.null(prediction_mode_error())) {
      return(prediction_mode_error())
    }
    req(prediction_mode_pred())
    pred <- prediction_mode_pred()
    sprintf(
      paste(
        "Composite score: %.4f",
        "Predicted category: %s",
        "Probability of class 1: %.4f",
        "Probability of class 0: %.4f",
        sep = "\n"
      ),
      pred$score[1],
      ifelse(pred$class[1] == 1, "1 (Positive)", "0 (Negative)"),
      pred$prob[1],
      1 - pred$prob[1]
    )
  })

  parse_new_pattern <- reactive({
    req(workbook_data())
    raw_text <- if (is.null(input$new_pattern_text)) "" else input$new_pattern_text
    values <- unlist(strsplit(raw_text, "[^01]+"))
    values <- values[nzchar(values)]
    if (length(values) != 12) {
      return(list(error = "Please enter exactly 12 binary values for a 4x3 pattern."))
    }

    numeric_values <- suppressWarnings(as.numeric(values))
    if (any(is.na(numeric_values)) || any(!numeric_values %in% c(0, 1))) {
      return(list(error = "Only 0 and 1 are allowed in the new pattern input."))
    }

    list(values = numeric_values)
  })

  output$new_pattern_prediction <- renderText({
    req(workbook_data())
    feature_names <- workbook_data()$cnn2_model$validation$train_columns
    if (length(feature_names) != 12) {
      return("New Pattern preview applies to 4x3 demo pixels only. The current dataset uses a different feature layout.")
    }
    parsed <- parse_new_pattern()
    if (!is.null(parsed$error)) {
      return(parsed$error)
    }

    validation_model <- workbook_data()$cnn2_model$validation$model
    new_features <- as.data.frame(as.list(parsed$values), check.names = FALSE)
    names(new_features) <- feature_names
    pred <- predict_binary_glm(validation_model, new_features)
    sprintf(
      paste(
        "Predicted class: %s",
        "Probability of class 1: %.6f",
        "Probability of class 0: %.6f",
        sep = "\n"
      ),
      ifelse(pred$class[1] == 1, "1 (Positive)", "0 (Negative)"),
      pred$prob[1],
      1 - pred$prob[1]
    )
  })

  output$pattern_plot <- renderUI({
    req(workbook_data(), input$pattern_id)
    idx <- as.integer(input$pattern_id)
    if (!is.finite(idx) || idx < 1 || length(workbook_data()$patterns) < idx) {
      return(tags$div(style = "height:260px; display:flex; align-items:center; justify-content:center; font-weight:700;", "Pattern is not available."))
    }
    raw_pattern <- workbook_data()$patterns[[idx]]
    if (is.null(raw_pattern)) {
      return(tags$div(style = "height:260px; display:flex; align-items:center; justify-content:center; font-weight:700;", "Pattern image is not available."))
    }
    pattern_matrix <- as.matrix(raw_pattern)
    pattern <- suppressWarnings(matrix(as.numeric(pattern_matrix), nrow = nrow(pattern_matrix), ncol = ncol(pattern_matrix)))
    if (!is.matrix(pattern) || any(!is.finite(pattern)) || nrow(pattern) < 1 || ncol(pattern) < 1) {
      return(tags$div(style = "height:260px; display:flex; align-items:center; justify-content:center; font-weight:700;", "Pattern image could not be drawn."))
    }
    pattern <- pattern[nrow(pattern):1, , drop = FALSE]
    rows <- lapply(seq_len(nrow(pattern)), function(r) {
      cells <- lapply(seq_len(ncol(pattern)), function(c) {
        tags$td(style = sprintf("width:28px; height:28px; border:1px solid #d2c9b8; background:%s;", ifelse(pattern[r, c] > 0, "#0f4c5c", "#f4f1ea")), "")
      })
      do.call(tags$tr, cells)
    })
    tags$div(
      style = "height:260px; display:flex; flex-direction:column; align-items:center; justify-content:center; gap:10px;",
      tags$div(style = "font-weight:700; color:#16324f;", sprintf("Pattern %s, label %s", idx, workbook_data()$labels[[idx]])),
      tags$table(style = "border-collapse:collapse; border:2px solid #16324f;", do.call(tagList, rows))
    )
  })

  output$pattern_table <- renderTable({
    req(workbook_data(), input$pattern_id)
    idx <- as.integer(input$pattern_id)
    pattern <- workbook_data()$patterns[[idx]]
    colnames(pattern) <- paste0("C", seq_len(ncol(pattern)))
    rownames(pattern) <- paste0("R", seq_len(nrow(pattern)))
    pattern
  }, rownames = TRUE)

  output$pattern_gallery_table <- renderDT({
    req(workbook_data())
    datatable(
      workbook_data()$pattern_gallery,
      escape = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollX = FALSE,
        columnDefs = list(list(width = "140px", targets = 2))
      ),
      rownames = FALSE
    ) |>
      formatStyle(
        "pattern_text",
        `white-space` = "pre",
        `font-family` = "monospace",
        `line-height` = "1.25",
        `font-size` = "14px"
      )
  })

  render_safe_plot <- function(expr, fallback = "Plot unavailable. Expand the panel or rerun after results load.") {
    renderPlot({
      tryCatch(
        force(expr),
        error = function(e) {
          old_par <- par(no.readonly = TRUE)
          on.exit(par(old_par), add = TRUE)
          par(mar = c(1, 1, 1, 1))
          plot.new()
          text(0.5, 0.5, fallback, cex = 1)
        }
      )
    })
  }
  output$roc_plot <- render_safe_plot({
    req(workbook_data())
    roc_points <- workbook_data()$cnn2_model$roc_points
    plot(
      roc_points$FPR,
      roc_points$TPR,
      type = "o",
      pch = 16,
      col = "#0f4c5c",
      xlab = "False Positive Rate",
      ylab = "True Positive Rate",
      main = sprintf("ROC Curve (AUC = %.4f)", workbook_data()$cnn2_model$auc_value),
      xlim = c(0, 1),
      ylim = c(0, 1)
    )
    abline(a = 0, b = 1, lty = 2, col = "#9aa5b1")
    grid(col = "#e7e4db")
  })

  filtered_forest_table <- reactive({
    req(workbook_data())
    forest_table <- workbook_data()$cnn2_model$forest$table
    if (isTRUE(input$forest_significant_only)) {
      forest_table <- forest_table[forest_table$significant == "Yes", , drop = FALSE]
    }
    forest_table
  })

  output$forest_plot <- render_safe_plot({
    req(session$clientData$output_forest_plot_width > 420, session$clientData$output_forest_plot_height > 420)
    req(workbook_data())
    forest_table <- filtered_forest_table()
    req(nrow(forest_table) > 0)

    plot_data <- utils::head(forest_table, 15)
    plot_data <- plot_data[nrow(plot_data):1, , drop = FALSE]
    y_pos <- seq_len(nrow(plot_data))
    x_min <- min(plot_data$CI_low, na.rm = TRUE)
    x_max <- max(plot_data$CI_high, na.rm = TRUE)
    pad <- max(0.2, 0.1 * (x_max - x_min))

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    layout(matrix(c(1, 2), nrow = 1), widths = c(5.2, 2.4))

    par(mar = c(4.2, 1.0, 3.0, 0.2))
    plot.new()
    plot.window(xlim = c(0, 1.02), ylim = c(0.5, nrow(plot_data) + 0.8))
    title(main = "Forest Plot by Feature", line = 1.4, font.main = 2, cex.main = 1.2)
    text(0.02, nrow(plot_data) + 0.45, "Variable", pos = 4, font = 2, cex = 0.92)
    text(0.48, nrow(plot_data) + 0.45, "Estimate (95% CI)", pos = 4, font = 2, cex = 0.92)
    text(0.79, nrow(plot_data) + 0.45, "Z-score", pos = 4, font = 2, cex = 0.88)
    text(0.90, nrow(plot_data) + 0.45, "p-value", pos = 4, font = 2, cex = 0.88)
    for (i in seq_len(nrow(plot_data))) {
      color_i <- ifelse(plot_data$significant[i] == "Yes", "#0b8f55", "#95a5a6")
      text(0.02, y_pos[i], sprintf("%d. %s", nrow(plot_data) - i + 1, truncate_plot_label(plot_data$feature[i], 34)), pos = 4, cex = 0.84, font = 2, col = color_i)
      text(0.48, y_pos[i], plot_data$estimate_label[i], pos = 4, cex = 0.80, font = 2)
      text(0.79, y_pos[i], sprintf("%.2f", plot_data$z_value[i]), pos = 4, cex = 0.78, font = 2)
      text(0.90, y_pos[i], as.character(plot_data$p_value[i]), pos = 4, cex = 0.78, font = 2)
    }
    abline(h = seq(0.5, nrow(plot_data) + 0.5, by = 1), col = "#f4f6f7", lty = 3)

    par(mar = c(4.2, 4.4, 3.0, 1.6))
    plot(
      x = plot_data$SMD,
      y = y_pos,
      xlim = c(x_min - pad, x_max + pad),
      ylim = c(0.5, nrow(plot_data) + 0.8),
      yaxt = "n",
      ylab = "",
      xlab = "SMD",
      main = ""
    )
    abline(v = 0, lty = 2, col = "#c0392b", lwd = 2)
    segments(plot_data$CI_low, y_pos, plot_data$CI_high, y_pos, lwd = 2.2, col = "#34495e")
    points(plot_data$SMD, y_pos, pch = 15, cex = 1.45, col = ifelse(plot_data$significant == "Yes", "#0b8f55", "#95a5a6"))
    text(x_min - pad * 0.08, nrow(plot_data) + 0.45, "Negative", col = "#117a65", cex = 0.82)
    text(x_max + pad * 0.08, nrow(plot_data) + 0.45, "Positive", col = "#c0392b", cex = 0.82)
    grid(nx = NULL, ny = NULL, col = "#ecf0f1")
  })

  output$feature_extraction_forest_plot <- render_safe_plot({
    req(session$clientData$output_feature_extraction_forest_plot_width > 320, session$clientData$output_feature_extraction_forest_plot_height > 320)
    req(workbook_data())
    forest_table <- workbook_data()$cnn2_model$feature_extraction$final_forest
    req(is.data.frame(forest_table), nrow(forest_table) > 0, "feature" %in% names(forest_table))

    plot_data <- forest_table[order(forest_table$estimate), , drop = FALSE]
    y_pos <- seq_len(nrow(plot_data))
    x_min <- min(plot_data$ci_low, na.rm = TRUE)
    x_max <- max(plot_data$ci_high, na.rm = TRUE)
    pad <- max(0.2, 0.1 * (x_max - x_min))

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    layout(matrix(c(1, 2), nrow = 1), widths = c(3.5, 2.8))

    par(mar = c(4.2, 1.5, 3.2, 0.3))
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0.5, nrow(plot_data) + 1.2))
    title(main = "Feature Extraction Forest Plot", line = 2)
    text(0.02, nrow(plot_data) + 0.8, "Variable", pos = 4, font = 2, cex = 0.85)
    text(0.50, nrow(plot_data) + 0.8, "Estimate (95% CI)", pos = 4, font = 2, cex = 0.82)
    text(0.80, nrow(plot_data) + 0.8, "Z-score", pos = 4, font = 2, cex = 0.78)
    text(0.90, nrow(plot_data) + 0.8, "p-value", pos = 4, font = 2, cex = 0.82)
    for (i in seq_len(nrow(plot_data))) {
      color_i <- ifelse(plot_data$direction[i] == "Positive", "#c0392b", "#117a65")
      text(0.02, y_pos[i], sprintf("%d. %s", i, plot_data$feature[i]), pos = 4, cex = 0.78, col = color_i)
      text(0.50, y_pos[i], plot_data$estimate_label[i], pos = 4, cex = 0.72)
      text(0.80, y_pos[i], sprintf("%.2f", plot_data$z_value[i]), pos = 4, cex = 0.68)
      text(0.90, y_pos[i], as.character(plot_data$p_value[i]), pos = 4, cex = 0.72)
    }
    abline(h = seq(0.5, nrow(plot_data) + 0.5, by = 1), col = "#f4f6f7", lty = 3)

    par(mar = c(4.2, 4.5, 3.2, 1.8))
    plot(
      x = plot_data$estimate,
      y = y_pos,
      xlim = c(x_min - pad, x_max + pad),
      ylim = c(0.5, nrow(plot_data) + 1.2),
      yaxt = "n",
      ylab = "",
      xlab = "Logistic regression estimate",
      main = ""
    )
    abline(v = 0, lty = 2, col = "#7f8c8d", lwd = 2)
    segments(plot_data$ci_low, y_pos, plot_data$ci_high, y_pos, lwd = 2, col = "#34495e")
    points(
      plot_data$estimate,
      y_pos,
      pch = 15,
      cex = 1.4,
      col = ifelse(plot_data$direction == "Positive", "#c0392b", "#117a65")
    )
    text(x_max + pad * 0.15, nrow(plot_data) + 0.7, "Positive variables", col = "#c0392b", cex = 0.9)
    text(x_min - pad * 0.15, nrow(plot_data) + 0.7, "Negative variables", col = "#117a65", cex = 0.9)
    grid(nx = NULL, ny = NULL, col = "#ecf0f1")
  })

  output$feature_step3_box_plot <- render_safe_plot({
    req(workbook_data())
    score_table <- workbook_data()$cnn2_model$feature_extraction$step3_scores
    req(is.data.frame(score_table), nrow(score_table) > 0, "status" %in% names(score_table))

    plot_frame <- rbind(
      data.frame(status = factor(score_table$status, levels = c(0, 1)), score_type = "Positive sum", score = score_table$positive_sum, check.names = FALSE),
      data.frame(status = factor(score_table$status, levels = c(0, 1)), score_type = "Negative sum", score = score_table$negative_sum, check.names = FALSE)
    )
    plot_frame$axis_group <- factor(
      paste0("Status ", plot_frame$status, " - ", plot_frame$score_type),
      levels = c("Status 0 - Positive sum", "Status 0 - Negative sum", "Status 1 - Positive sum", "Status 1 - Negative sum")
    )

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mar = c(8, 4.5, 3.5, 1.5))

    boxplot(
      score ~ axis_group,
      data = plot_frame,
      col = c("#f5b7b1", "#a3e4d7", "#ec7063", "#45b39d"),
      border = "#566573",
      outline = FALSE,
      las = 2,
      xlab = "Positive/negative grouped scores by status",
      ylab = "Summation score",
      main = "Step 3 Grouped Score Box Plot"
    )
    stripchart(score ~ axis_group, data = plot_frame, vertical = TRUE, method = "jitter", pch = 16, cex = 0.5, col = "#34495e", add = TRUE)
    grid(nx = NA, ny = NULL, col = "#ecf0f1")
  })

  comparison_table_with_mode <- reactive({
    req(workbook_data())
    comparison_table <- workbook_data()$comparison_bundle$table
    if (is.null(comparison_table)) {
      comparison_table <- workbook_data()$cnn2_model$comparison$table
    }
    req(is.data.frame(comparison_table))
    comparison_table
  })

  comparison_top5_with_mode <- reactive({
    req(workbook_data())
    top5_table <- workbook_data()$comparison_bundle$top5
    if (is.null(top5_table)) {
      top5_table <- workbook_data()$cnn2_model$comparison$top5
    }
    req(is.data.frame(top5_table))
    top5_table
  })

  output$importance_forest_plot <- render_safe_plot({
    req(session$clientData$output_importance_forest_plot_width > 420, session$clientData$output_importance_forest_plot_height > 420)
    req(workbook_data())
    forest_table <- workbook_data()$cnn2_model$importance$forest
    req(is.data.frame(forest_table), nrow(forest_table) > 0, "feature" %in% names(forest_table))

    plot_data <- forest_table[order(forest_table$estimate), , drop = FALSE]
    y_pos <- seq_len(nrow(plot_data))
    x_limits <- range(c(plot_data$ci_low, plot_data$ci_high), na.rm = TRUE)
    if (!all(is.finite(x_limits)) || diff(x_limits) == 0) {
      x_limits <- c(min(plot_data$estimate, na.rm = TRUE) - 1, max(plot_data$estimate, na.rm = TRUE) + 1)
    }
    pad <- max(0.2, 0.1 * diff(x_limits))

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    layout(matrix(c(1, 2), nrow = 1), widths = c(6.8, 2.8))

    par(mar = c(4.2, 1.0, 3.0, 0.2))
    plot.new()
    plot.window(xlim = c(0, 1.02), ylim = c(0.5, nrow(plot_data) + 0.8))
    title(main = "Importance Forest Plot", line = 1.4, font.main = 2, cex.main = 1.2)
    text(0.02, nrow(plot_data) + 0.45, "Variable", pos = 4, font = 2, cex = 0.92)
    text(0.47, nrow(plot_data) + 0.45, "Estimate (95% CI)", pos = 4, font = 2, cex = 0.92)
    text(0.79, nrow(plot_data) + 0.45, "Z-score", pos = 4, font = 2, cex = 0.88)
    text(0.90, nrow(plot_data) + 0.45, "p-value", pos = 4, font = 2, cex = 0.88)
    for (i in seq_len(nrow(plot_data))) {
      color_i <- ifelse(plot_data$direction[i] == "Positive", "#c0392b", "#117a65")
      text(0.02, y_pos[i], sprintf("%d. %s", i, truncate_plot_label(plot_data$feature[i], 36)), pos = 4, cex = 0.84, font = 2, col = color_i)
      text(0.47, y_pos[i], plot_data$estimate_label[i], pos = 4, cex = 0.80, font = 2)
      text(0.79, y_pos[i], sprintf("%.2f", plot_data$z_value[i]), pos = 4, cex = 0.78, font = 2)
      text(0.90, y_pos[i], as.character(plot_data$p_value[i]), pos = 4, cex = 0.78, font = 2)
    }
    abline(h = seq(0.5, nrow(plot_data) + 0.5, by = 1), col = "#f4f6f7", lty = 3)

    par(mar = c(4.2, 4.4, 3.0, 1.6))
    plot(
      x = plot_data$estimate,
      y = y_pos,
      xlim = c(x_limits[1] - pad, x_limits[2] + pad),
      ylim = c(0.5, nrow(plot_data) + 0.8),
      yaxt = "n",
      ylab = "",
      xlab = "Single-variable log-odds estimate",
      main = ""
    )
    abline(v = 0, lty = 2, col = "#c0392b", lwd = 2)
    segments(plot_data$ci_low, y_pos, plot_data$ci_high, y_pos, lwd = 2.2, col = "#34495e")
    points(plot_data$estimate, y_pos, pch = 15, cex = 1.45, col = ifelse(plot_data$direction == "Positive", "#c0392b", "#117a65"))
    text(x_limits[1] - pad * 0.08, nrow(plot_data) + 0.45, "Negative", col = "#117a65", cex = 0.82)
    text(x_limits[2] + pad * 0.08, nrow(plot_data) + 0.45, "Positive", col = "#c0392b", cex = 0.82)
    grid(nx = NULL, ny = NULL, col = "#ecf0f1")
  })

  truncate_plot_label <- function(x, max_chars = 34) {
    x <- as.character(x)
    ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars - 3), "..."), x)
  }

  build_qsubgroup_export <- function(study, effect, ci_low, ci_high, n, z = NULL, p = NULL) {
    se <- (ci_high - ci_low) / (2 * 1.96)
    valid <- is.finite(effect) & is.finite(ci_low) & is.finite(ci_high) & is.finite(se) & se > 0
    if (!any(valid)) {
      return("No valid rows available for QSubgrouptest export.")
    }

    study <- as.character(study[valid])
    effect <- as.numeric(effect[valid])
    ci_low <- as.numeric(ci_low[valid])
    ci_high <- as.numeric(ci_high[valid])
    se <- as.numeric(se[valid])
    n <- as.integer(round(as.numeric(n[valid])))

    if (is.null(z)) {
      z <- effect / se
    } else {
      z <- as.numeric(z[valid])
      z[!is.finite(z)] <- effect[!is.finite(z)] / se[!is.finite(z)]
    }

    if (is.null(p)) {
      p <- 2 * stats::pnorm(-abs(z))
    } else {
      p <- as.numeric(p[valid])
      p[!is.finite(p)] <- 2 * stats::pnorm(-abs(z[!is.finite(p)]))
    }

    weights <- 1 / (se ^ 2)
    weights[!is.finite(weights)] <- 0
    if (!any(weights > 0)) {
      weights <- rep(1, length(effect))
    }
    weight_pct <- 100 * weights / sum(weights)

    overall_effect <- stats::weighted.mean(effect, weights)
    overall_se <- sqrt(1 / sum(weights))
    overall_ci_low <- overall_effect - 1.96 * overall_se
    overall_ci_high <- overall_effect + 1.96 * overall_se
    overall_z <- overall_effect / overall_se
    overall_p <- 2 * stats::pnorm(-abs(overall_z))
    overall_n <- if (length(unique(stats::na.omit(n))) == 1) unique(stats::na.omit(n))[1] else sum(n, na.rm = TRUE)

    rows <- data.frame(
      Study = c(study, "Overall"),
      Effect = c(effect, overall_effect),
      SE = c(se, overall_se),
      CI_lower = c(ci_low, overall_ci_low),
      CI_upper = c(ci_high, overall_ci_high),
      n = c(n, overall_n),
      Z = c(z, overall_z),
      p = c(p, overall_p),
      W = c(weight_pct, 100),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    apply(rows, 1, function(row) {
      paste(
        row[["Study"]],
        formatC(as.numeric(row[["Effect"]]), format = "f", digits = 3),
        formatC(as.numeric(row[["SE"]]), format = "f", digits = 3),
        formatC(as.numeric(row[["CI_lower"]]), format = "f", digits = 3),
        formatC(as.numeric(row[["CI_upper"]]), format = "f", digits = 3),
        as.integer(round(as.numeric(row[["n"]]))),
        formatC(as.numeric(row[["Z"]]), format = "f", digits = 3),
        formatC(as.numeric(row[["p"]]), format = "fg", digits = 6),
        formatC(as.numeric(row[["W"]]), format = "f", digits = 3),
        sep = ","
      )
    }) |>
      paste(collapse = "\n")
  }

  render_export_box <- function(title, value, rows = 12) {
    tags$div(
      style = "margin-top:14px;",
      tags$label(style = "font-weight:600; display:block; margin-bottom:6px;", title),
      tags$textarea(
        readonly = "readonly",
        style = "width:100%; min-height:220px; font-family:Consolas, monospace; font-size:12px; line-height:1.35;",
        rows = rows,
        value
      )
    )
  }

  forest_code_text <- reactive({
    req(workbook_data())
    forest_table <- filtered_forest_table()
    req(nrow(forest_table) > 0)
    p_num <- suppressWarnings(as.numeric(gsub("^<", "", as.character(forest_table$p_value))))
    build_qsubgroup_export(
      study = forest_table$feature,
      effect = forest_table$SMD,
      ci_low = forest_table$CI_low,
      ci_high = forest_table$CI_high,
      n = rep(length(workbook_data()$cnn2_model$target), nrow(forest_table)),
      z = forest_table$z_value,
      p = p_num
    )
  })

  importance_code_text <- reactive({
    req(workbook_data())
    forest_table <- workbook_data()$cnn2_model$importance$forest
    req(is.data.frame(forest_table), nrow(forest_table) > 0, "feature" %in% names(forest_table))
    build_qsubgroup_export(
      study = forest_table$feature,
      effect = forest_table$estimate,
      ci_low = forest_table$ci_low,
      ci_high = forest_table$ci_high,
      n = rep(length(workbook_data()$cnn2_model$target), nrow(forest_table)),
      z = forest_table$z_value,
      p = forest_table$p_value
    )
  })

  comparison_code_text <- function(set_name) {
    comparison_table <- comparison_table_with_mode()
    comparison_table <- comparison_table[
      comparison_table$split == "Testing" & comparison_table$status == "OK" & comparison_table$variable_set == set_name & !is.na(comparison_table$auc),
      , drop = FALSE
    ]
    validate(need(nrow(comparison_table) > 0, sprintf("No testing comparison rows available for %s.", set_name)))
    build_qsubgroup_export(
      study = comparison_table$algorithm,
      effect = comparison_table$auc,
      ci_low = comparison_table$ci_low,
      ci_high = comparison_table$ci_high,
      n = comparison_table$n
    )
  }

  output$forest_code_box <- renderUI({
    req(forest_code_text())
    render_export_box("QSubgrouptest forest code", forest_code_text(), rows = 14)
  })

  output$importance_code_box <- renderUI({
    req(importance_code_text())
    render_export_box("QSubgrouptest importance code", importance_code_text(), rows = 14)
  })

  output$comparison_original_code_box <- renderUI({
    render_export_box("QSubgrouptest comparison code: Original variables", comparison_code_text("Original"), rows = 9)
  })

  output$comparison_top10_code_box <- renderUI({
    tags$div(
      render_export_box("QSubgrouptest comparison code: Top 10 variables", comparison_code_text("Top 10"), rows = 9),
      tags$div(style = "margin-top:8px;",
        tags$a(
          href = "https://www.raschonline.com/kpiall/QSubgrouptest.asp",
          target = "_blank",
          rel = "noopener noreferrer",
          "Open QSubgrouptest.asp"
        )
      )
    )
  })

  output$comparison_plot <- render_safe_plot({
    if (is.null(session$clientData$output_comparison_plot_width) || is.null(session$clientData$output_comparison_plot_height) || session$clientData$output_comparison_plot_width < 520 || session$clientData$output_comparison_plot_height < 520) {
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Expand the plot panel to view the comparison forest.", cex = 1)
      return(invisible())
    }
    comparison_table <- comparison_table_with_mode()
    comparison_table <- comparison_table[comparison_table$split == "Testing" & comparison_table$status == "OK" & !is.na(comparison_table$auc), , drop = FALSE]
    req(nrow(comparison_table) > 0)
    plot_data <- comparison_table[order(comparison_table$algorithm, comparison_table$pipeline, comparison_table$variable_set), , drop = FALSE]
    shared_min <- min(plot_data$ci_low, na.rm = TRUE)
    shared_max <- max(plot_data$ci_high, na.rm = TRUE)
    pad <- max(0.02, 0.08 * (shared_max - shared_min))

    draw_panel <- function(set_name, title_text, point_col) {
      set_data <- plot_data[plot_data$variable_set == set_name, , drop = FALSE]
      if (!nrow(set_data)) {
        par(mar = c(3.8, 1.2, 3.0, 0.2))
        plot.new()
        text(0.5, 0.5, sprintf("No testing rows available for %s.", set_name), cex = 1)
        par(mar = c(3.8, 4.2, 3.0, 1.6))
        plot.new()
        text(0.5, 0.5, "Forest plot unavailable.", cex = 1)
        return(invisible())
      }
      set_data <- set_data[order(set_data$auc, set_data$algorithm, set_data$pipeline), , drop = FALSE]
      set_data <- set_data[nrow(set_data):1, , drop = FALSE]
      y_pos <- seq_len(nrow(set_data))

      par(mar = c(3.8, 1.2, 3.0, 0.2))
      plot.new()
      plot.window(xlim = c(0, 1.02), ylim = c(0.5, nrow(set_data) + 1.1))
      title(main = title_text, line = 1.2, font.main = 2, cex.main = 1.15)
      text(0.02, nrow(set_data) + 0.7, "Algorithm", pos = 4, font = 2, cex = 0.94)
      text(0.40, nrow(set_data) + 0.7, "Pipeline", pos = 4, font = 2, cex = 0.94)
      text(0.58, nrow(set_data) + 0.7, "AUC (95% CI)", pos = 4, font = 2, cex = 0.94)
      text(0.93, nrow(set_data) + 0.7, "n", pos = 4, font = 2, cex = 0.94)
      for (i in seq_len(nrow(set_data))) {
        text(0.02, y_pos[i], truncate_plot_label(set_data$algorithm[i], 22), pos = 4, cex = 0.82, font = 2, col = "#2c3e50")
        text(0.40, y_pos[i], as.character(set_data$pipeline[i]), pos = 4, cex = 0.82, font = 2, col = point_col)
        text(0.58, y_pos[i], sprintf("%.3f (%s)", set_data$auc[i], set_data$ci[i]), pos = 4, cex = 0.80, font = 2)
        text(0.93, y_pos[i], as.character(set_data$n[i]), pos = 4, cex = 0.80, font = 2)
      }
      abline(h = seq(0.5, nrow(set_data) + 0.5, by = 1), col = "#f1f3f4", lty = 3)

      par(mar = c(3.8, 4.2, 3.0, 1.6))
      plot(
        x = set_data$auc,
        y = y_pos,
        xlim = c(max(0, shared_min - pad), min(1, shared_max + pad)),
        ylim = c(0.5, nrow(set_data) + 1.1),
        yaxt = "n",
        ylab = "",
        xlab = "Test ROC-AUC (95% CI)",
        main = ""
      )
      abline(v = 0.5, lty = 2, col = "#7f8c8d", lwd = 1.5)
      segments(set_data$ci_low, y_pos, set_data$ci_high, y_pos, lwd = 2.2, col = "#34495e")
      points(set_data$auc, y_pos, pch = 15, cex = 1.45, col = point_col)
      text(shared_min - pad * 0.08, nrow(set_data) + 0.45, "Lower", col = "#117a65", cex = 0.82)
      text(shared_max + pad * 0.08, nrow(set_data) + 0.45, "Higher", col = "#c0392b", cex = 0.82)
      grid(nx = NULL, ny = NULL, col = "#ecf0f1")
    }

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE), widths = c(5.0, 2.8), heights = c(1, 1))
    draw_panel("Original", "Original Variables", "#117a65")
    draw_panel("Top 10", "Top 10 Variables", "#c0392b")
  })

  output$pcc_plot <- render_safe_plot({
    if (!is.null(prediction_mode_error())) {
      plot.new()
      text(0.5, 0.5, prediction_mode_error(), cex = 1)
      return(invisible())
    }
    req(prediction_mode_pred(), workbook_data())
    pred <- prediction_mode_pred()
    model_config <- resolve_model_configuration(input$model_configuration)
    theta <- seq(-4, 4, length.out = 400)
    curve1 <- exp(theta) / (1 + exp(theta))
    curve0 <- 1 / (1 + exp(theta))
    mark_theta <- max(-4, min(4, pred$score[1]))
    mark_p1 <- exp(mark_theta) / (1 + exp(mark_theta))
    mark_p0 <- 1 / (1 + exp(mark_theta))

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mar = c(5, 5, 5.2, 2.5), pty = "s")

    plot(
      theta, curve1,
      type = "l",
      lwd = 3,
      col = "#c0392b",
      ylim = c(0, 1),
      xlim = c(-4, 4),
      yaxs = "i",
      yaxt = "n",
      xlab = "Theta (composite score in logit units)",
      ylab = "Probability",
      main = sprintf("Category Probability Curve (%s)", model_config$label)
    )
    mtext(sprintf("Displayed fitting algorithm: %s", algorithm_label(workbook_data()$cnn2_model$model$algorithm)), side = 3, line = 2.1, cex = 0.82, col = "#5d6d7e")
    mtext("P(1)=exp(theta)/(1+exp(theta)); P(0)=1/(1+exp(theta))", side = 3, line = 0.9, cex = 0.82, col = "#7f5a4f")
    axis(2, at = seq(0, 1, by = 0.2), labels = format(seq(0, 1, by = 0.2), nsmall = 1), las = 1)
    axis(1, at = -4:4)
    lines(theta, curve0, lwd = 3, col = "#117a65")
    abline(h = seq(0, 1, by = 0.1), lty = 3, col = "#e5e7e9")
    abline(v = 0, lty = 2, col = "#f5b7b1")
    abline(h = 0.5, lty = 2, col = "#d5dbdb")
    abline(v = mark_theta, lty = 3, col = "#7f8c8d")
    segments(mark_theta, mark_p0, mark_theta, mark_p1, col = "#7d3c98", lty = 3)
    points(mark_theta, mark_p1, pch = 18, cex = 1.6, col = "#7d3c98")
    points(mark_theta, mark_p0, pch = 15, cex = 1.3, col = "#1f618d")
    text(mark_theta + 0.18, min(0.97, mark_p1 + 0.04), labels = sprintf("P(1)=%.2f", mark_p1), pos = 4, cex = 0.92, col = "#4a235a")
    text(mark_theta + 0.18, max(0.03, mark_p0 - 0.04), labels = sprintf("P(0)=%.2f", mark_p0), pos = 4, cex = 0.92, col = "#154360")
    legend(
      "topright",
      legend = c("P(1)=exp(theta)/(1+exp(theta))", "P(0)=1/(1+exp(theta))", "Current score"),
      col = c("#c0392b", "#117a65", "#7d3c98"),
      lty = c(1, 1, 3),
      lwd = c(3, 3, 1.5),
      pch = c(NA, NA, 18),
      pt.cex = c(NA, NA, 1.3),
      bty = "n",
      cex = 0.9
    )
  })

  make_dt <- function(data_expr) {
    renderDT({
      req(workbook_data())
      datatable(
        data_expr(),
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })
  }

  make_full_dt <- function(data_expr) {
    renderDT({
      req(workbook_data())
      data <- data_expr()
      datatable(
        data,
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          searching = FALSE,
          info = FALSE,
          scrollX = FALSE
        ),
        rownames = FALSE,
        class = "compact stripe"
      )
    })
  }

  output$data_table <- make_dt(function() workbook_data()$data_demo)
  output$data2_table <- make_dt(function() workbook_data()$data2_generated)
  output$dataabc_table <- make_dt(function() workbook_data()$dataabc_generated)
  output$cnn2_workbook_table <- make_dt(function() workbook_data()$cnn2_workbook)
  output$trainning_workbook_table <- make_dt(function() workbook_data()$trainning_workbook)
  output$df_workbook_table <- make_dt(function() workbook_data()$df_workbook)
  output$data2_workbook_table <- make_dt(function() workbook_data()$data2_workbook)
  output$dataabc_workbook_table <- make_dt(function() workbook_data()$dataabc_workbook)
  output$cnn2_predictions_table <- make_full_dt(function() workbook_data()$cnn2_model$prediction_table)
  output$roc_auc_summary_table <- make_full_dt(function() workbook_data()$cnn2_model$auc_summary)
  output$auc_confusion_table <- make_full_dt(function() workbook_data()$cnn2_model$confusion_table)
  output$roc_points_table <- make_full_dt(function() workbook_data()$cnn2_model$roc_points)
  output$cnn2_metrics_table <- make_full_dt(function() workbook_data()$cnn2_model$metrics)
  output$feature_extraction_summary_table <- make_full_dt(function() workbook_data()$cnn2_model$feature_extraction$summary)
  output$feature_step1_table <- make_full_dt(function() workbook_data()$cnn2_model$feature_extraction$step1)
  output$feature_step2_table <- make_full_dt(function() workbook_data()$cnn2_model$feature_extraction$step2)
  output$feature_step3_scores_table <- make_full_dt(function() workbook_data()$cnn2_model$feature_extraction$step3_scores)
  output$feature_step3_table <- make_full_dt(function() workbook_data()$cnn2_model$feature_extraction$step3)
  output$feature_step3_metrics_table <- make_full_dt(function() workbook_data()$cnn2_model$feature_extraction$step3_metrics)
  output$feature_final_forest_table <- make_full_dt(function() workbook_data()$cnn2_model$feature_extraction$final_forest)
  output$forest_summary_table <- make_full_dt(function() {
    forest_table <- filtered_forest_table()
    data.frame(
      metric = c("Features shown", "Significant features shown", "Largest |SMD| shown"),
      value = c(
        nrow(forest_table),
        sum(forest_table$significant == "Yes"),
        if (nrow(forest_table)) max(forest_table$abs_SMD) else NA_real_
      ),
      check.names = FALSE
    )
  })
  output$forest_table <- make_full_dt(function() filtered_forest_table())
  output$importance_summary_table <- make_full_dt(function() workbook_data()$cnn2_model$importance$summary)
  output$importance_elimination_table <- make_full_dt(function() workbook_data()$cnn2_model$importance$elimination)
  output$importance_final_table <- make_full_dt(function() workbook_data()$cnn2_model$importance$final_table)
  output$importance_forest_table <- make_full_dt(function() workbook_data()$cnn2_model$importance$forest)
  output$comparison_top5_table <- make_full_dt(function() comparison_top5_with_mode())
  output$comparison_table <- make_full_dt(function() comparison_table_with_mode())
  output$validation_summary_table <- make_full_dt(function() workbook_data()$cnn2_model$validation$split_summary)
  output$validation_cv_table <- make_full_dt(function() workbook_data()$cnn2_model$validation$cv_table)
  output$validation_confusion_table <- make_full_dt(function() workbook_data()$cnn2_model$validation$confusion_matrix)
  output$validation_test_predictions_table <- make_full_dt(function() workbook_data()$cnn2_model$validation$test_predictions)
}

shinyApp(ui, server)






























