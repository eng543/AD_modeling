perf <- function(training_predictions, 
                 training_labels,
                 validation_predictions,
                 validation_labels,
                 description,
                 algorithm,
                 criteria) 
  {
  
  tp_train <- 0
  tn_train <- 0
  fp_train <- 0
  fn_train <- 0
  
  for (i in 1:length(training_predictions)) {
    #prediction_t <- as.numeric(training_predictions[i])
    prediction_t <- training_predictions[i]
    true_t <- training_labels[i]
    
    if (prediction_t == 1 & true_t == 1) {
      tp_train <- tp_train + 1
    } else if (prediction_t == 0 & true_t == 1) {
      fn_train <- fn_train + 1
    } else if (prediction_t == 1 & true_t == 0) {
      fp_train <- fp_train + 1
    } else if (prediction_t == 0 & true_t == 0) {
      tn_train <- tn_train + 1
    }
  }
  
  ppv_train <- tp_train / (tp_train + fp_train)
  recall_train <- tp_train / (tp_train + fn_train)
  specificity_train <- tn_train / (fp_train + tn_train)
  f_measure_train <- 2 * ((recall_train * ppv_train) / (recall_train + ppv_train))
  
  tp_valid <- 0
  tn_valid <- 0
  fp_valid <- 0
  fn_valid <- 0
  
  for (j in 1:length(validation_predictions)) {
    #prediction_v <- as.numeric(validation_predictions[j])
    prediction_v <- validation_predictions[j]
    true_v <- validation_labels[j]
    
    if (prediction_v == 1 & true_v == 1) {
      tp_valid <- tp_valid + 1
    } else if (prediction_v == 0 & true_v == 1) {
      fn_valid <- fn_valid + 1
    } else if (prediction_v == 1 & true_v == 0) {
      fp_valid <- fp_valid + 1
    } else if (prediction_v == 0 & true_v == 0) {
      tn_valid <- tn_valid + 1
    }
  }
  
  ppv_valid <- tp_valid / (tp_valid + fp_valid)
  recall_valid <- tp_valid / (tp_valid + fn_valid)
  specificity_valid <- tn_valid / (fp_valid + tn_valid)
  f_measure_valid <- 2 * ((recall_valid * ppv_valid) / (recall_valid + ppv_valid))
  
  time <- paste(strptime(Sys.time(), format = "%Y-%m-%d"), "_", strftime(Sys.time(), format = "%H:%M:%S"), sep = "")
  
  results <- data.frame(tp_train, tn_train, fp_train, fn_train, ppv_train, recall_train, specificity_train, f_measure_train, 
                        tp_valid, tn_valid, fp_valid, fn_valid, ppv_valid, recall_valid, specificity_valid, f_measure_valid, 
                        description, algorithm, criteria, time)
  return(results)
}

se <- function(x) sd(x)/sqrt(length(x))