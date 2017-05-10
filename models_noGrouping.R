library(glmnet)
library(dplyr)
library(ROCR)
library(caret)

setwd("~/AD_lasso")
options(digits=11)
set.seed(999)

# run preprocessing and load stratification functions
source("preprocessing.R")
source("diagnosis_codes.R")

# set description for experiments
description <- "concepts not grouped|note counts|Dx codes normalized and log transformed|no race"
criteria <- "HR"

colnames(dat_agg)[1] <- "patient_id"

# log transform everything
label_index <- grep("label", names(dat_agg))
col_index <- ncol(dat_agg)

if (col_index != label_index) {
  dat_agg[, c(3:(label_index-1),(label_index+1):col_index)] <- log(dat_agg[, c(3:(label_index-1), (label_index+1):col_index)] + 1)
} else {
  dat_agg[, c(3:(label_index-1))] <- log(dat_agg[, c(3:(label_index-1))] + 1)
}

# demographics
demo <- read.table("demographics.txt", sep = "\t", header = T)
colnames(demo)[1] <- "patient_id"

demo$age <- floor(as.numeric(as.Date("2017-03-15") - as.Date(demo$birth_dts))/365.25)
demo$gender.c <- ifelse(demo$gender == "Female",
                        1,
                        0)
demo$race.c <- ifelse(demo$race == "White or Caucasian",
                      1,
                      0)

demo <- demo[, c(1, 7:9)] #check indices

# make all patient_id 7 digits
demo$patient_id_fix <- NA
demo$patient_id <- as.character(demo$patient_id)
for (i in 1:nrow(demo)) {
  if (nchar(strsplit(demo$patient_id[i], "\\.")[[1]][2]) == 7) {
    demo$patient_id_fix[i] <- demo$patient_id[i]
  } else if (nchar(strsplit(demo$patient_id[i], "\\.")[[1]][2]) == 6) {
    demo$patient_id_fix[i] <- paste(demo$patient_id[i], "0", sep = "")
  } else if (nchar(strsplit(demo$patient_id[i], "\\.")[[1]][2]) == 5) {
    demo$patient_id_fix[i] <- paste(demo$patient_id[i], "00", sep = "")
  } else {
    print("problem")
    print(demo$patient_id[i])
  }
}
demo <- demo[,-1]
colnames(demo)[4] <- "patient_id"

dat_agg <- merge(dat_agg, demo)

# race_remove <- grep("^race.c$", names(dat_agg))
# dat_agg <- dat_agg[, -race_remove]

# merge with AD diagnosis codes
# if no code for patient, make it 0
all_diag_agg$ad_log <- log(all_diag_agg$ad_count + 1)
all_diag_agg$cd_log <- log(all_diag_agg$cd_count + 1)
all_diag_agg[, c(3:4)] <- all_diag_agg[, c(3:4)] / all_diag_agg$code_count # normalized by # of facts
ad_diag_agg <- all_diag_agg[, -c(2)]
# ad_diag_agg <- ad_diag_agg[, -c(7,8)]
dat_agg_codes <- merge(dat_agg, ad_diag_agg, all.x = TRUE)
dat_agg_codes[is.na(dat_agg_codes)] <- 0

# merge with coded medications
# if no code for patient, make it 0, otherwise 1
coded_meds <- read.csv("meds_coded_summarized.csv", as.is = T)
dat_agg_codes <- merge(dat_agg_codes, coded_meds, all.x = TRUE)
dat_agg_codes[is.na(dat_agg_codes)] <- 0

# merge with labs
# starting point: boolean whether any allergy test was performed at any time
labs <- read.table("labs_031517.txt", sep = "\t", header = TRUE)
colnames(labs)[1] <- "patient_id"
labs_summary <- labs %>%
  group_by(patient_id) %>%
  summarise(lab = 1)

dat_agg_codes <- merge(dat_agg_codes, labs_summary, all.x = TRUE)
dat_agg_codes[is.na(dat_agg_codes)] <- 0

# move label and remove HRLabel
dat_agg_codes <- dat_agg_codes[, c(label_index, (1:ncol(dat_agg_codes))[-label_index])]
dat_agg_codes <- dat_agg_codes[, -3]

# read in patient_id for those in train_set
train_patients <- read.table("training_set_patients_HR.txt", sep = "\t", as.is = T, header = T)
#train_patients <- read.table("training_set_patients_UKWP.txt", sep = "\t", as.is = T, header = T)
train_patients <- train_patients[,c(1,5)] # HR
#train_patients <- train_patients[,c(1,4)] # UKWP
colnames(train_patients)[2] <- "label"

# make all patient_id 7 digits
train_patients$patient_id_fix <- NA
train_patients$patient_id <- as.character(train_patients$patient_id)
for (i in 1:nrow(train_patients)) {
  if (nchar(strsplit(train_patients$patient_id[i], "\\.")[[1]][2]) == 7) {
    train_patients$patient_id_fix[i] <- train_patients$patient_id[i]
  } else if (nchar(strsplit(train_patients$patient_id[i], "\\.")[[1]][2]) == 6) {
    train_patients$patient_id_fix[i] <- paste(train_patients$patient_id[i], "0", sep = "")
  } else if (nchar(strsplit(train_patients$patient_id[i], "\\.")[[1]][2]) == 5) {
    train_patients$patient_id_fix[i] <- paste(train_patients$patient_id[i], "00", sep = "")
  } else {
    print("problem")
    print(train_patients$patient_id[i])
  }
}
train_patients <- train_patients[,-1]
colnames(train_patients)[2] <- "patient_id"

train_set_unsorted <- merge(dat_agg_codes, train_patients)
train_set <- train_set_unsorted[order(train_set_unsorted$patient_id),]

# create matrices of variables
train_matrix <- as.matrix(train_set[,-c(1,2)])

# create outcome variables
train_label <- as.factor(train_set$label)

### Train the model ###
## cv.glmnet to set best lambda value ##
# specify own folds so can test performance on validation set #
# write set of folds ONCE, use for all experiments
#folds <- createFolds(train_set$label, k = 10, list = F)
#folds_df <- data.frame(fold = folds, patient_id = train_set$patient_id)
#write.csv(folds_df, "cross_validation_folds_HR.csv", row.names = F)

folds <- read.csv("cross_validation_folds_HR.csv", as.is = T)

folds$patient_id_fix <- NA
folds$patient_id <- as.character(folds$patient_id)
for (i in 1:nrow(folds)) {
  if (nchar(strsplit(folds$patient_id[i], "\\.")[[1]][2]) == 7) {
    folds$patient_id_fix[i] <- folds$patient_id[i]
  } else if (nchar(strsplit(folds$patient_id[i], "\\.")[[1]][2]) == 6) {
    folds$patient_id_fix[i] <- paste(folds$patient_id[i], "0", sep = "")
  } else if (nchar(strsplit(folds$patient_id[i], "\\.")[[1]][2]) == 5) {
    folds$patient_id_fix[i] <- paste(folds$patient_id[i], "00", sep = "")
  } else {
    print("problem")
    print(folds$patient_id[i])
  }
}
folds <- folds[,-2]
colnames(folds)[2] <- "patient_id"

#train_set_folds <- merge(train_set, folds)
foldid <- folds$fold

model <- cv.glmnet(train_matrix, train_label, alpha=1, family="binomial", foldid=foldid)
best_lambda <- model$lambda.min

## run another round of 10-fold CV using best lambda value to evaluate average performance across validation sets ##
# write set of folds ONCE, use for all experiments
#folds <- createFolds(train_set$label, k = 10, list = F)
#folds_df <- data.frame(fold = folds, patient_id = train_set$patient_id)
#write.csv(folds_df, "cross_validation_folds.csv", row.names = F)

train_set_folds <- merge(train_set, folds)

cv_results <- data.frame(tp = numeric(), tn = numeric(), fp = numeric(), fn = numeric(), ppv = numeric(), recall = numeric(), specificity = numeric(), f_measure = numeric())

for (i in 1:10) {
  fold_index <- grep("^fold$", names(train_set_folds))
  fold_validation <- train_set_folds[train_set_folds$fold == i,]
  fold_train <- train_set_folds[train_set_folds$fold != i,]
  
  fold_train_label <- as.factor(fold_train$label)
  fold_valid_label <- as.factor(fold_validation$label)
  
  fold_train_m <- as.matrix(fold_train[, -c(1,2, fold_index)])
  fold_valid_m <- as.matrix(fold_validation[, -c(1,2, fold_index)])
  
  train_model <- glmnet(fold_train_m, fold_train_label, alpha = 1, family = "binomial", lambda = best_lambda)
  
  valid_predict <- predict(train_model, fold_valid_m, s = best_lambda, type = "class")
  
  tp <- 0
  tn <- 0
  fp <- 0
  fn <- 0
  
  for (i in 1:length(valid_predict)) {
    prediction <- as.numeric(valid_predict[i,])
    true <- fold_valid_label[i]
    
    if (prediction == 1 & true == 1) {
      tp <- tp + 1
    } else if (prediction == 0 & true == 1) {
      fn <- fn + 1
    } else if (prediction == 1 & true == 0) {
      fp <- fp + 1
    } else if (prediction == 0 & true == 0) {
      tn <- tn + 1
    }
  }
  
  result_ppv <- tp / (tp + fp)
  result_recall <- tp / (tp + fn)
  result_specificity <- tn / (fp + tn)
  result_f_measure <- 2 * ((result_recall * result_ppv) / (result_recall + result_ppv))
  
  fold_results <- data.frame(tp, tn, fp, fn, result_ppv, result_recall, result_specificity, result_f_measure)
  cv_results <- rbind(cv_results, fold_results)
}

colnames(cv_results) <- c("tp", "tn", "fp", "fn", "ppv", "recall", "specificity", "f_measure")

se <- function(x) sd(x)/sqrt(length(x))

avg_results <- cv_results %>%
  summarize_each(funs(mean))

se_results <- cv_results %>%
  summarize("ppv_se" = se(ppv),
            "recall_se" = se(recall),
            "specificity_se" = se(specificity),
            "f_measure_se" = se(f_measure))

avg_results <- cbind(avg_results, se_results)

avg_results$time <- paste(strptime(Sys.time(), format = "%Y-%m-%d"), "_", strftime(Sys.time(), format = "%H:%M:%S"), sep = "")

avg_results$description <- description
avg_results$algorithm <- "regular lasso"
avg_results$criteria <- criteria

# filename for independent archive of current experiment
filename <- paste("results/", strptime(Sys.time(), format="%Y-%m-%d"), "_", strftime(Sys.time(), format = "%H:%M:%S"), ".csv", sep = "")
filename <- gsub(":", "-", filename)
#write.csv(avg_results, filename, row.names = F)

# add current results to summary document
results_summary <- read.csv("results_summary.csv", as.is = T, header = T)
# first time
#results_summary <- avg_results

results_summary <- rbind(avg_results, results_summary)

##### Adaptive LASSO #####
# ridge regression to create the adaptive weights vector
ridge_adapt <- cv.glmnet(train_matrix, train_label, family = "binomial", alpha = 0, foldid=foldid) #, parallel = TRUE, standardize = TRUE)
w3 <- 1/abs(matrix(coef(ridge_adapt, s = ridge_adapt$lambda.min)[, 1][2:(ncol(train_matrix)+1)])) ^ 1 # using gamma = 1 (suggested values 0.5, 1, 2)
w3[w3[,1] == Inf] <- 999999999 # replacing values estimated as Inf for 999999999

# adaptive lasso using adaptive weights vector
lasso_adapt <- cv.glmnet(train_matrix, train_label, 
                         family = "binomial", 
                         alpha = 1, 
                         type.measure = 'auc',
                         penalty.factor = w3,
                         foldid=foldid)
best_lambda_adapt <- lasso_adapt$lambda.min

cv_results_adapt <- data.frame(tp = numeric(), tn = numeric(), fp = numeric(), fn = numeric(), ppv = numeric(), recall = numeric(), specificity = numeric(), f_measure = numeric())

for (i in 1:10) {
  fold_index <- grep("^fold$", names(train_set_folds))
  fold_validation <- train_set_folds[train_set_folds$fold == i,]
  fold_train <- train_set_folds[train_set_folds$fold != i,]
  
  fold_train_label <- as.factor(fold_train$label)
  fold_valid_label <- as.factor(fold_validation$label)
  
  fold_train_m <- as.matrix(fold_train[, -c(1,2, fold_index)])
  fold_valid_m <- as.matrix(fold_validation[, -c(1,2, fold_index)])
  
  train_model <- glmnet(fold_train_m, fold_train_label, alpha = 1, family = "binomial", lambda = best_lambda_adapt,
                        penalty.factor = w3)
  
  valid_predict <- predict(train_model, fold_valid_m, s = best_lambda_adapt, type = "class")
  
  tp <- 0
  tn <- 0
  fp <- 0
  fn <- 0
  
  for (i in 1:length(valid_predict)) {
    prediction <- as.numeric(valid_predict[i,])
    true <- fold_valid_label[i]
    
    if (prediction == 1 & true == 1) {
      tp <- tp + 1
    } else if (prediction == 0 & true == 1) {
      fn <- fn + 1
    } else if (prediction == 1 & true == 0) {
      fp <- fp + 1
    } else if (prediction == 0 & true == 0) {
      tn <- tn + 1
    }
  }
  
  result_ppv <- tp / (tp + fp)
  result_recall <- tp / (tp + fn)
  result_specificity <- tn / (fp + tn)
  result_f_measure <- 2 * ((result_recall * result_ppv) / (result_recall + result_ppv))
  
  fold_results <- data.frame(tp, tn, fp, fn, result_ppv, result_recall, result_specificity, result_f_measure)
  cv_results_adapt <- rbind(cv_results_adapt, fold_results)
}

colnames(cv_results_adapt) <- c("tp", "tn", "fp", "fn", "ppv", "recall", "specificity", "f_measure")

avg_results_adapt <- cv_results_adapt %>%
  summarize_each(funs(mean))

se_results_adapt <- cv_results_adapt %>%
  summarize("ppv_se" = se(ppv),
            "recall_se" = se(recall),
            "specificity_se" = se(specificity),
            "f_measure_se" = se(f_measure))

avg_results_adapt <- cbind(avg_results_adapt, se_results_adapt)

avg_results_adapt$time <- paste(strptime(Sys.time(), format = "%Y-%m-%d"), "_", strftime(Sys.time(), format = "%H:%M:%S"), sep = "")

avg_results_adapt$description <- description
avg_results_adapt$algorithm <- "adaptive lasso"
avg_results_adapt$criteria <- criteria

# add results to experiment archive
avg_results_all <- rbind(avg_results, avg_results_adapt)

# add current results to summary document
results_summary <- rbind(avg_results_adapt, results_summary)

# skip the other algorithms for now

# save results to file
write.csv(results_summary, "results_summary.csv", row.names = F)
write.csv(avg_results_all, filename, row.names = F)



##### Random Forest Classification #####
folds_list <- vector(mode="list", length=10)
folds$index <- 1:nrow(folds)
for (i in 1:10) {
  fold <- paste("^",i,"$",sep="")
  fold_index <- grep(eval(fold), folds$fold)
  folds_list[[i]] <- fold_index
}
train_set_factored <- train_set

train_set_factored$label <- factor(train_set_factored$label, levels = c("0", "1"), labels = c("level0", "level1"))

### this is broken... probably because of the way the variables are represented.
rfFit <- train(label~., train_set_factored[,-2],
               method = "rf",
               metric = "AUC",
               scale = TRUE,
               trControl = trainControl(method = "cv", number = 10, index = folds_list, savePredictions = TRUE, summaryFunction = prSummary, classProbs = TRUE))

results <- rfFit$resample
results$tp <- NA
results$tn <- NA
results$fp <- NA
results$fn <- NA
results$specificity <- NA

colnames(results)[2:4] <- c("ppv", "recall", "f_measure")
rf_results <- subset(results, select = c(tp:fn, ppv:recall, specificity, f_measure))

rf_avg_results <- rf_results %>%
  summarize_each(funs(mean))

rf_se_results <- rf_results %>%
  summarize("ppv_se" = se(ppv),
            "recall_se" = se(recall),
            "specificity_se" = se(specificity),
            "f_measure_se" = se(f_measure))

rf_avg_results <- cbind(rf_avg_results, rf_se_results)

rf_avg_results$time <- paste(strptime(Sys.time(), format = "%Y-%m-%d"), "_", strftime(Sys.time(), format = "%H:%M:%S"), sep = "")

rf_avg_results$description <- description
rf_avg_results$algorithm <- "random forest"
rf_avg_results$criteria <- criteria

# add to current experiment archive
avg_results_all <- rbind(avg_results_all, rf_avg_results)

# add to other results
results_summary <- rbind(rf_avg_results, results_summary)

##### Support vector machine #####
## linear kernel ##

svmFit <- train(label~., train_set_factored[,-2],
                method = "svmLinear",
                metric = "AUC",
                trControl = trainControl(method = "cv", number = 10, index = folds_list, savePredictions = TRUE, summaryFunction = prSummary, classProbs = TRUE),
                scale = FALSE)

results <- svmFit$resample
results$tp <- NA
results$tn <- NA
results$fp <- NA
results$fn <- NA
results$specificity <- NA

colnames(results)[2:4] <- c("ppv", "recall", "f_measure")
svm_results <- subset(results, select = c(tp:fn, ppv:recall, specificity, f_measure))

svm_avg_results <- svm_results %>%
  summarize_each(funs(mean))

svm_se_results <- svm_results %>%
  summarize("ppv_se" = se(ppv),
            "recall_se" = se(recall),
            "specificity_se" = se(specificity),
            "f_measure_se" = se(f_measure))

svm_avg_results <- cbind(svm_avg_results, svm_se_results)

svm_avg_results$time <- paste(strptime(Sys.time(), format = "%Y-%m-%d"), "_", strftime(Sys.time(), format = "%H:%M:%S"), sep = "")

svm_avg_results$description <- description
svm_avg_results$algorithm <- "svm-linear kernel"
svm_avg_results$criteria <- criteria

# add to current experiment archive
avg_results_all <- rbind(avg_results_all, svm_avg_results)

# add to other results
results_summary <- rbind(svm_avg_results, results_summary)

# save results to file
write.csv(results_summary, "results_summary.csv", row.names = F)
write.csv(avg_results_all, filename, row.names = F)





















### Train the model ###
## cv.glmnet to set best lambda value ##
# specify own folds so can test performance on validation set #
# write set of folds ONCE, use for all experiments
#folds <- createFolds(train_set$label, k = 10, list = F)
#folds_df <- data.frame(fold = folds, patient_id = train_set$patient_id)
#write.csv(folds_df, "cross_validation_folds.csv", row.names = F)

folds <- read.csv("cross_validation_folds.csv", as.is = T)
# make all patient_id 7 digits
folds$patient_id_fix <- NA
folds$patient_id <- as.character(folds$patient_id)
for (i in 1:nrow(folds)) {
  if (nchar(strsplit(folds$patient_id[i], "\\.")[[1]][2]) == 7) {
    folds$patient_id_fix[i] <- folds$patient_id[i]
  } else if (nchar(strsplit(folds$patient_id[i], "\\.")[[1]][2]) == 6) {
    folds$patient_id_fix[i] <- paste(folds$patient_id[i], "0", sep = "")
  } else if (nchar(strsplit(folds$patient_id[i], "\\.")[[1]][2]) == 5) {
    folds$patient_id_fix[i] <- paste(folds$patient_id[i], "00", sep = "")
  } else {
    print("problem")
    print(folds$patient_id[i])
  }
}
folds <- folds[,-2]
colnames(folds)[2] <- "patient_id"
#train_set_folds <- merge(train_set, folds)
foldid <- folds$fold

model <- cv.glmnet(train_matrix, train_label, alpha=1, family="binomial", foldid=foldid)
best_lambda <- model$lambda.min

## run another round of 10-fold CV using best lambda value to evaluate average performance across validation sets ##
# write set of folds ONCE, use for all experiments
#folds <- createFolds(train_set$label, k = 10, list = F)
#folds_df <- data.frame(fold = folds, patient_id = train_set$patient_id)
#write.csv(folds_df, "cross_validation_folds.csv", row.names = F)

train_set_folds <- merge(train_set, folds)

cv_results <- data.frame(tp = numeric(), tn = numeric(), fp = numeric(), fn = numeric(), ppv = numeric(), recall = numeric(), specificity = numeric(), f_measure = numeric())

for (i in 1:10) {
  fold_index <- grep("^fold$", names(train_set_folds))
  fold_validation <- train_set_folds[train_set_folds$fold == i,]
  fold_train <- train_set_folds[train_set_folds$fold != i,]
  
  fold_train_label <- as.factor(fold_train$label)
  fold_valid_label <- as.factor(fold_validation$label)
  
  fold_train_m <- as.matrix(fold_train[, -c(1,2, fold_index)])
  fold_valid_m <- as.matrix(fold_validation[, -c(1,2, fold_index)])
  
  #train_model <- glmnet(fold_train_m, fold_train_label, alpha = 1, family = "binomial", lambda = best_lambda)
  
  valid_predict <- predict(model, fold_valid_m, s = best_lambda, type = "class")
  
  tp <- 0
  tn <- 0
  fp <- 0
  fn <- 0
  
  for (i in 1:length(valid_predict)) {
    prediction <- as.numeric(valid_predict[i,])
    true <- fold_valid_label[i]
    
    if (prediction == 1 & true == 1) {
      tp <- tp + 1
    } else if (prediction == 0 & true == 1) {
      fn <- fn + 1
    } else if (prediction == 1 & true == 0) {
      fp <- fp + 1
    } else if (prediction == 0 & true == 0) {
      tn <- tn + 1
    }
  }
  
  result_ppv <- tp / (tp + fp)
  result_recall <- tp / (tp + fn)
  result_specificity <- tn / (fp + tn)
  result_f_measure <- 2 * ((result_recall * result_ppv) / (result_recall + result_ppv))
  
  fold_results <- data.frame(tp, tn, fp, fn, result_ppv, result_recall, result_specificity, result_f_measure)
  cv_results <- rbind(cv_results, fold_results)
}

colnames(cv_results) <- c("tp", "tn", "fp", "fn", "ppv", "recall", "specificity", "f_measure")

se <- function(x) sd(x)/sqrt(length(x))

avg_results <- cv_results %>%
  summarize_each(funs(mean))

se_results <- cv_results %>%
  summarize("ppv_se" = se(ppv),
            "recall_se" = se(recall),
            "specificity_se" = se(specificity),
            "f_measure_se" = se(f_measure))

avg_results <- cbind(avg_results, se_results)

avg_results$time <- paste(strptime(Sys.time(), format = "%Y-%m-%d"), "_", strftime(Sys.time(), format = "%H:%M:%S"), sep = "")

avg_results$description <- description
avg_results$algorithm <- "regular lasso"

##### Adaptive LASSO #####
# ridge regression to create the adaptive weights vector
ridge_adapt <- cv.glmnet(train_matrix, train_label, family = "binomial", alpha = 0, foldid=foldid) #, parallel = TRUE, standardize = TRUE)
w3 <- 1/abs(matrix(coef(ridge_adapt, s = ridge_adapt$lambda.min)[, 1][2:(ncol(train_matrix)+1)])) ^ 1 # using gamma = 1 (suggested values 0.5, 1, 2)
w3[w3[,1] == Inf] <- 999999999 # replacing values estimated as Inf for 999999999

# adaptive lasso using adaptive weights vector
lasso_adapt <- cv.glmnet(train_matrix, train_label, 
                      family = "binomial", 
                      alpha = 1, 
                      type.measure = 'auc',
                      penalty.factor = w3,
                      foldid=foldid)
best_lambda_adapt <- lasso_adapt$lambda.min

cv_results_adapt <- data.frame(tp = numeric(), tn = numeric(), fp = numeric(), fn = numeric(), ppv = numeric(), recall = numeric(), specificity = numeric(), f_measure = numeric())

for (i in 1:10) {
  fold_index <- grep("^fold$", names(train_set_folds))
  fold_validation <- train_set_folds[train_set_folds$fold == i,]
  fold_train <- train_set_folds[train_set_folds$fold != i,]
  
  fold_train_label <- as.factor(fold_train$label)
  fold_valid_label <- as.factor(fold_validation$label)
  
  fold_train_m <- as.matrix(fold_train[, -c(1,2, fold_index)])
  fold_valid_m <- as.matrix(fold_validation[, -c(1,2, fold_index)])
  
  #train_model <- glmnet(fold_train_m, fold_train_label, alpha = 1, family = "binomial", lambda = best_lambda)
  
  valid_predict <- predict(lasso_adapt, fold_valid_m, s = best_lambda_adapt, type = "class")
  
  tp <- 0
  tn <- 0
  fp <- 0
  fn <- 0
  
  for (i in 1:length(valid_predict)) {
    prediction <- as.numeric(valid_predict[i,])
    true <- fold_valid_label[i]
    
    if (prediction == 1 & true == 1) {
      tp <- tp + 1
    } else if (prediction == 0 & true == 1) {
      fn <- fn + 1
    } else if (prediction == 1 & true == 0) {
      fp <- fp + 1
    } else if (prediction == 0 & true == 0) {
      tn <- tn + 1
    }
  }
  
  result_ppv <- tp / (tp + fp)
  result_recall <- tp / (tp + fn)
  result_specificity <- tn / (fp + tn)
  result_f_measure <- 2 * ((result_recall * result_ppv) / (result_recall + result_ppv))
  
  fold_results <- data.frame(tp, tn, fp, fn, result_ppv, result_recall, result_specificity, result_f_measure)
  cv_results_adapt <- rbind(cv_results_adapt, fold_results)
}

colnames(cv_results_adapt) <- c("tp", "tn", "fp", "fn", "ppv", "recall", "specificity", "f_measure")

avg_results_adapt <- cv_results_adapt %>%
  summarize_each(funs(mean))

se_results_adapt <- cv_results_adapt %>%
  summarize("ppv_se" = se(ppv),
            "recall_se" = se(recall),
            "specificity_se" = se(specificity),
            "f_measure_se" = se(f_measure))

avg_results_adapt <- cbind(avg_results_adapt, se_results_adapt)

avg_results_adapt$time <- paste(strptime(Sys.time(), format = "%Y-%m-%d"), "_", strftime(Sys.time(), format = "%H:%M:%S"), sep = "")

description <- "concepts not grouped|add up counts|Dx codes normalized and log transformed|no race|HR criteria"
avg_results_adapt$description <- description
avg_results_adapt$algorithm <- "adaptive lasso"

avg_results_all <- rbind(avg_results, avg_results_adapt)

# save current results in archive
filename <- paste("results/", strptime(Sys.time(), format="%Y-%m-%d"), "_", strftime(Sys.time(), format = "%H:%M:%S"), ".csv", sep = "")
filename <- gsub(":", "-", filename)
write.csv(avg_results, filename, row.names = F)

# add current results to summary document
results_summary <- read.csv("results_summary.csv", as.is = T, header = T)
avg_results <- rbind(avg_results, results_summary)

write.csv(avg_results, "results_summary.csv", row.names = F)





### Apply the model ###
# apply gold standard model to gold standard and test set using best lambda
# type = "response" -> fitted probabilities, "class" -> class label with max probability
train_predict <- predict(model, train_matrix, s=best_lambda, type="class")
test_predict <- predict(model, test_matrix, s=best_lambda, type="class")

### Examine model ###
print(model)
coef(model, s = best_lambda) # .: beta=0
# Plot variable coefficients vs. shrinkage parameter lambda.
#plot(model, xvar="lambda")

coef(model, s = model$lambda.1se)
coef <- coef(model, s = "lambda.1se")
selected_attributes <- (coef@i[-1]+1)

### Results ###
train_result_accuracy <- sum(train_predict == train_label) / length(train_label)
test_result_accuracy <- sum(test_predict == test_label) / length(test_label)

train_tp <- 0
train_tn <- 0
train_fp <- 0
train_fn <- 0

test_tp <- 0
test_tn <- 0
test_fp <- 0
test_fn <- 0

for (i in 1:length(train_predict)) {
  prediction <- as.numeric(train_predict[i,])
  true <- train_label[i]
  
  if (prediction == 1 & true == 1) {
    train_tp <- train_tp + 1
  } else if (prediction == 0 & true == 1) {
    train_fn <- train_fn + 1
  } else if (prediction == 1 & true == 0) {
    train_fp <- train_fp + 1
  } else if (prediction == 0 & true == 0) {
    train_tn <- train_tn + 1
  }
}

train_result_ppv <- train_tp / (train_tp + train_fp)
train_result_recall <- train_tp / (train_tp + train_fn)
train_result_specificity <- train_tn / (train_fp + train_tn)
train_result_f_measure <- 2 * ((train_result_recall * train_result_ppv) / (train_result_recall + train_result_ppv))

for (i in 1:length(test_predict)) {
  prediction <- as.numeric(test_predict[i,])
  true <- test_label[i]
  
  if (prediction == 1 & true == 1) {
    test_tp <- test_tp + 1
  } else if (prediction == 0 & true == 1) {
    test_fn <- test_fn + 1
  } else if (prediction == 1 & true == 0) {
    test_fp <- test_fp + 1
  } else if (prediction == 0 & true == 0) {
    test_tn <- test_tn + 1
  }
}

test_result_ppv <-test_tp / (test_tp + test_fp)
test_result_recall <- test_tp / (test_tp + test_fn)
test_result_specificity <- test_tn / (test_fp + test_tn)
test_result_f_measure <- 2 * ((test_result_recall * test_result_ppv) / (test_result_recall + test_result_ppv))

### ROC ###
#### use probability output of model #### 
#data(ROCR.simple)
#head(cbind(ROCR.simple$predictions, ROCR.simple$labels), 5)
#pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)

train_predict_prob <- predict(model, train_matrix, s=best_lambda, type="response")
test_predict_prob <- predict(model, test_matrix, s=best_lambda, type="response")

train_label <- as.numeric(train_label) - 1
test_label <- as.numeric(test_label) - 1

pred <- prediction(train_predict_prob, train_label)

# ROC curve
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# specificity cutoff vs. f measure
perf <- performance(pred, "f", "spec")
plot(perf)

ind <- which.max(slot (perf, "y.values")[[1]])
f <- slot(perf, "y.values")[[1]][ind]
cutoff <- slot(perf, "x.values")[[1]][ind]
print(c(f_score=f, cutoff=cutoff))

# partial ROC curve: only accept FPR below certain threshold (above)
pROC = function(pred, fpr.stop){
  perf <- performance(pred,"tpr","fpr")
  for (iperf in seq_along(perf@x.values)){
    ind = which(perf@x.values[[iperf]] <= fpr.stop)
    perf@y.values[[iperf]] = perf@y.values[[iperf]][ind]
    perf@x.values[[iperf]] = perf@x.values[[iperf]][ind]
  }
  return(perf)
}

proc.perf <- pROC(pred, fpr.stop=1-cutoff)
plot(proc.perf)
abline(a=0, b=1)

# vertical line for cutoff (can either specify a desired cutoff or pick the one that maximizes f-score)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

abline(v=1-cutoff, col = "red") # maximum f-score
abline(v=0.03, col = "green") # desired cutoff

auc <- paste("AUC =", round(performance(pred, "auc")@y.values[[1]], 3))
pauc <- paste("Partial AUC =", round(performance(pred, "auc", fpr.stop=1-cutoff)@y.values[[1]], 3)) # change fpr.stop
legend(0.7, 0.15, c(auc, pauc), lty=c(1,1), col=c("black", "red"))

##### Adaptive LASSO #####
# ridge regression to create the adaptive weights vector
cv.ridge <- cv.glmnet(train_matrix, train_label, family = "binomial", alpha = 0) #, parallel = TRUE, standardize = TRUE)
w3 <- 1/abs(matrix(coef(cv.ridge, s = cv.ridge$lambda.min)[, 1][2:(ncol(train_matrix)+1)])) ^ 1 # using gamma = 1 (suggested values 0.5, 1, 2)
w3[w3[,1] == Inf] <- 999999999 # replacing values estimated as Inf for 999999999

# adaptive lasso using adaptive weights vector
cv.lasso <- cv.glmnet(train_matrix, train_label, 
                      family = "binomial", 
                      alpha = 1, 
                      #parallel = TRUE, 
                      #standarize = TRUE,
                      type.measure = 'auc',
                      penalty.factor = w3)
best_lambda <- cv.lasso$lambda.min

### Apply the model ###
# apply gold standard model to gold standard and test set using best lambda
# type = "response" -> fitted probabilities, "class" -> class label with max probability
al_train_predict <- predict(cv.lasso, train_matrix, s=best_lambda, type="class")
al_test_predict <- predict(cv.lasso, test_matrix, s=best_lambda, type="class")

### Results ###
al_train_result_accuracy <- sum(al_train_predict == train_label) / length(train_label)
al_test_result_accuracy <- sum(al_test_predict == test_label) / length(test_label)

al_train_tp <- 0
al_train_tn <- 0
al_train_fp <- 0
al_train_fn <- 0

al_test_tp <- 0
al_test_tn <- 0
al_test_fp <- 0
al_test_fn <- 0

for (i in 1:length(al_train_predict)) {
  prediction <- as.numeric(al_train_predict[i,])
  true <- train_label[i]
  
  if (prediction == 1 & true == 1) {
    al_train_tp <- al_train_tp + 1
  } else if (prediction == 0 & true == 1) {
    al_train_fn <- al_train_fn + 1
  } else if (prediction == 1 & true == 0) {
    al_train_fp <- al_train_fp + 1
  } else if (prediction == 0 & true == 0) {
    al_train_tn <- al_train_tn + 1
  }
}

al_train_result_ppv <- al_train_tp / (al_train_tp + al_train_fp)
al_train_result_recall <- al_train_tp / (al_train_tp + al_train_fn)
al_train_result_specificity <- al_train_tn / (al_train_fp + al_train_tn)
al_train_result_f_measure <- 2 * ((al_train_result_recall * al_train_result_ppv) / (al_train_result_recall + al_train_result_ppv))

for (i in 1:length(al_test_predict)) {
  prediction <- as.numeric(al_test_predict[i,])
  true <- test_label[i]
  
  if (prediction == 1 & true == 1) {
    al_test_tp <- al_test_tp + 1
  } else if (prediction == 0 & true == 1) {
    al_test_fn <- al_test_fn + 1
  } else if (prediction == 1 & true == 0) {
    al_test_fp <- al_test_fp + 1
  } else if (prediction == 0 & true == 0) {
    al_test_tn <- al_test_tn + 1
  }
}

al_test_result_ppv <- al_test_tp / (al_test_tp + al_test_fp)
al_test_result_recall <- al_test_tp / (al_test_tp + al_test_fn)
al_test_result_specificity <- al_test_tn / (al_test_fp + al_test_tn)
al_test_result_f_measure <- 2 * ((al_test_result_recall * al_test_result_ppv) / (al_test_result_recall + al_test_result_ppv))

### ROC ###
#### use probability output of model #### 
#data(ROCR.simple)
#head(cbind(ROCR.simple$predictions, ROCR.simple$labels), 5)
#pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)

al_train_predict_prob <- predict(cv.lasso, train_matrix, s=best_lambda, type="response")
al_test_predict_prob <- predict(cv.lasso, test_matrix, s=best_lambda, type="response")

train_label <- as.numeric(train_label) - 1
test_label <- as.numeric(test_label) - 1

al_pred <- prediction(al_train_predict_prob, train_label)

# ROC curve
al_perf <- performance(al_pred, "tpr", "fpr")
plot(al_perf)

# specificity cutoff vs. f measure
al_perf <- performance(al_pred, "f", "spec")
plot(al_perf)

al_ind <- which.max(slot (al_perf, "y.values")[[1]])
al_f <- slot(al_perf, "y.values")[[1]][al_ind]
al_cutoff <- slot(al_perf, "x.values")[[1]][al_ind]
print(c(f_score=al_f, cutoff=al_cutoff))

# partial ROC curve: only accept FPR below certain threshold (above)
pROC = function(pred, fpr.stop){
  perf <- performance(pred,"tpr","fpr")
  for (iperf in seq_along(perf@x.values)){
    ind = which(perf@x.values[[iperf]] <= fpr.stop)
    perf@y.values[[iperf]] = perf@y.values[[iperf]][ind]
    perf@x.values[[iperf]] = perf@x.values[[iperf]][ind]
  }
  return(perf)
}

al_proc.perf <- pROC(al_pred, fpr.stop=1-cutoff)
plot(al_proc.perf)
abline(a=0, b=1)

# vertical line for cutoff (can either specify a desired cutoff or pick the one that maximizes f-score)
al_perf <- performance(al_pred, "tpr", "fpr")
plot(al_perf)

abline(v=1-al_cutoff, col = "red") # maximum f-score
abline(v=0.03, col = "green") # desired cutoff

al_auc <- paste("AUC =", round(performance(al_pred, "auc")@y.values[[1]], 3))
al_pauc <- paste("Partial AUC =", round(performance(al_pred, "auc", fpr.stop=1-al_cutoff)@y.values[[1]], 3)) # change fpr.stop
legend(0.7, 0.15, c(al_auc, al_pauc), lty=c(1,1), col=c("black", "red"))




# change medication groups to binary variables
# log transform those that aren't binary
#groups <- read.table("Dictionary_Concepts_byCui.txt", sep = "\t", header = FALSE, as.is = TRUE)

#med_groups <- c("topical_steroid", "topical_calcineurin_inhibitors", "emollients", "antihistimines" ,
#                "oral_steroids", "phototheraphy", "other", "oral_antibiotics", "oral_tacrolimus",
#                "alternative_medicine")

#for (i in names(dat_agg_codes)) {
#  if (i != "patient_id" & i != "UKWPLabel" & i != "label") {
#    split <- strsplit(i, "_")
#    print(split[[1]][1])
#    group <- grep(split[[1]][1], groups$v2)
#    if (length(group) == 0) {
#      index <- grep(i, colnames(dat_agg_codes))
#      colnames(dat_agg_codes)[index] <- "discard"
#    } else {
#      if (groups$V1[group] %in% med_groups) {
#        for (j in 1:nrow(dat_agg_codes)) {
#          if (dat_agg_codes[j, i] > 0) {
#            dat_agg_codes[j, i] <- 1
#          }
#        }
#      } else {
#        if (i != 1 & i != 2) {
#          dat_agg_codes[,i] <- log(dat_agg_codes[,i] + 1)
#        }
#      }
#    }
#  }
#}
