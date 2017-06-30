library(glmnet)
library(caret)

setwd("~/AD_modeling")
options(digits=11)
set.seed(999)

# load helper functions
source("helper_functions/preprocessing.R")
source("helper_functions/preprocessing_relations.R")
source("helper_functions/dimensionality_reduction.R")
source("helper_functions/demographics.R")
source("helper_functions/diagnosis_codes.R")
source("helper_functions/meds_labs.R")
source("helper_functions/read_subsets.R")
source("helper_functions/performance_measures.R")

# change description/settings of current experiment
#description <- "concepts grouped|note counts|phenOnly Dx codes normalized and all log transformed|relations|meds grouped"
#description <- "structured variables only|phenOnly Dx codes normalized and log transformed"
description <- "NLP only|concepts grouped|add up counts|relations|meds labs grouped"

# NLP
nlp_only <- T
criteria <- "HR" # HR or UKWP
count_type <- "note" # add or note
group_meds <- F
relations <- T

# coded
code_only <- F
race <- T
log_transform <- T
norm_set <- "all" # phenOnly, all, none

if (!code_only & !nlp_only) {
  # preprocess(source_file, criteria(hr or ukwp), count_type(add or note))
  dat_agg <- preprocess("data_sources/output_replication_test_053117.csv", criteria, count_type)
  
  if (relations) {
    dat_agg_rel <- preprocess_relations("data_sources/output_location_relation_060117.csv", criteria, count_type)
  }
  
  # group_codes(aggregated_data, criteria, group_meds_labs)
  dat_grouped <- group_codes(dat_agg, criteria, group_meds, relations)
  
  remove(dat_agg)
  
  # load_demographics(source_file, race(boolean))
  demo <- load_demographics("data_sources/demographics.txt", race)
  
  # merge with dataset
  # losing patients with merge
  dat_grouped <- merge(dat_grouped, demo)
  
  remove(demo)
  
  # load_dx_codes(source_file, log_transform(boolean), norm_set(phenOnly, all, none))
  ad_diag_agg <- load_dx_codes("data_sources/diagnosis_codes_ids_deduped_all_sources.txt", log_transform, norm_set)
  
  # merge with dataset
  dat_grouped_codes <- merge(dat_grouped, ad_diag_agg, all.x = TRUE)
  dat_grouped_codes[is.na(dat_grouped_codes)] <- 0
  
  remove(ad_diag_agg)
  remove(dat_grouped)
  
  # load coded medications and labs
  dat_grouped_codes <- meds_labs_read(dat_grouped_codes, "data_sources/meds_coded_summarized.csv", "data_sources/labs_031517.txt", group_meds)
  
  
} else if (code_only & !nlp_only) {
  # load_demographics(source_file, race(boolean))
  demo <- load_demographics("data_sources/demographics.txt", race)
  
  # load_dx_codes(source_file, log_transform(boolean), norm_set(phenOnly, all, none))
  ad_diag_agg <- load_dx_codes("data_sources/diagnosis_codes_ids_deduped_all_sources.txt", log_transform, norm_set)
  
  # merge with dataset
  dat_grouped_codes <- merge(demo, ad_diag_agg, all.x = TRUE)
  dat_grouped_codes[is.na(dat_grouped_codes)] <- 0
  
  remove(ad_diag_agg)

  # load coded medications and labs
  dat_grouped_codes <- meds_labs_read(dat_grouped_codes, "data_sources/meds_coded_summarized.csv", "data_sources/labs_031517.txt", group_meds)

  
} else if (nlp_only & !code_only) {
  # preprocess(source_file, criteria(hr or ukwp), count_type(add or note))
  dat_agg <- preprocess("data_sources/output_replication_test_053117.csv", criteria, count_type)
  
  if (relations) {
    dat_agg_rel <- preprocess_relations("data_sources/output_location_relation_060117.csv", criteria, count_type)
  }
  
  # group_codes(aggregated_data, criteria, group_meds_labs)
  dat_grouped_codes <- group_codes(dat_agg, criteria, group_meds, relations)
  
  remove(dat_agg)
}
#remove(data_meds_labs)


# DO THIS ONCE! GET THE SET OF PATIENTS REPRESENTED IN DATASET FOR DATA PARTITIONING
# create_train_test.R
#write.table(dat_grouped_codes[,c(1,2)], "candidate_patients_class.txt", sep = "\t", row.names = F)

# read in patient_id for those in train_set/valid_set
train_set <- load_subset(dat_grouped_codes, "train", criteria)
valid_set <- load_subset(dat_grouped_codes, "valid", criteria)

# create matrices of variables
if (!code_only) {
  train_matrix <- as.matrix(train_set[,-c(1,2)])
  valid_matrix <- as.matrix(valid_set[,-c(1,2)])
} else {
  train_matrix <- as.matrix(train_set[,-c(1,14)])
  valid_matrix <- as.matrix(valid_set[,-c(1,14)])
}

# create outcome variables
train_label <- as.factor(train_set$label)
valid_label <- as.factor(valid_set$label)

### LASSO ###
## cv.glmnet to set best lambda value ##
# specify own folds so can test performance on validation set #
  # write set of folds ONCE, use for all experiments
#folds <- createFolds(train_set$label, k = 10, list = F)
#folds_df <- data.frame(fold = folds, patient_id = train_set$patient_id)
#write.csv(folds_df, "cross_validation_folds_UKWP.csv", row.names = F)

if (tolower(criteria) == "hr") {
  folds <- read.csv("dataset_splits/cross_validation_folds_HR.csv", as.is = T)  
} else if (tolower(criteria) == "ukwp") {
  folds <- read.csv("dataset_splits/cross_validation_folds_UKWP.csv", as.is = T) 
} else {
  print("ERROR!! Criteria not valid")
}

foldid <- folds$fold

model <- cv.glmnet(train_matrix, train_label, alpha=1, family="binomial", foldid=foldid)
#plot(model)
best_lambda <- model$lambda.min

train_predict <- predict(model, train_matrix, s = best_lambda, type = "class")
valid_predict <- predict(model, valid_matrix, s = best_lambda, type = "class")

lasso_results <- perf(train_predict, train_label, valid_predict, valid_label, description, "regular lasso", criteria)

lasso_results

# features left in model
lasso_coefs <- coef(model, s = best_lambda)
lasso_features <- lasso_coefs@i[-1]
lasso_features <- paste(lasso_features, collapse = "|")
lasso_results$features <- lasso_features

# output status for each patient for visualization
#label_num <- as.numeric(valid_label) - 1
#predict_num <- as.numeric(valid_predict)

#valid_patients$predict <- predict_num
#write.table(valid_patients, "patient_info/best_results_validation.txt", sep = "\t", row.names = F)

# filename for independent archive of current experiment
filename <- paste("results_individual_experiments/", strptime(Sys.time(), format="%Y-%m-%d"), "_", strftime(Sys.time(), format = "%H:%M:%S"), ".csv", sep = "")
filename <- gsub(":", "-", filename)
#write.csv(lasso_results, filename, row.names = F)

# add current results to summary document
# or create new summary document if first experiment
if (file.exists("results/results_summary_lasso.csv")) {
  results_summary <- read.csv("results/results_summary_lasso.csv", as.is = T, header = T)  
  results_summary <- rbind(results_summary, lasso_results)
} else {
  results_summary <- lasso_results
}

##### ADAPTIVE LASSO #####
# ridge regression to create the adaptive weights vector
ridge_adapt <- cv.glmnet(train_matrix, train_label, family = "binomial", alpha = 0, foldid=foldid)
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

train_predict_adapt <- predict(lasso_adapt, train_matrix, s = best_lambda_adapt, type = "class")
valid_predict_adapt <- predict(lasso_adapt, valid_matrix, s = best_lambda_adapt, type = "class")

adapt_lasso_results <- perf(train_predict_adapt, train_label, valid_predict_adapt, valid_label, description, "adaptive lasso", criteria)

adapt_lasso_results

adapt_lasso_coefs <- coef(lasso_adapt, s = best_lambda)
adapt_lasso_features <- adapt_lasso_coefs@i[-1]
adapt_lasso_features <- paste(adapt_lasso_features, collapse = "|")
adapt_lasso_results$features <- adapt_lasso_features

# add results to experiment archive
avg_results_all <- rbind(lasso_results, adapt_lasso_results)

# add current results to summary document
results_summary <- rbind(results_summary, adapt_lasso_results)

# save results to file
write.csv(results_summary, "results/results_summary_lasso.csv", row.names = F)
write.csv(avg_results_all, filename, row.names = F)








##### Random Forest Classification #####
## change metric to OOB estimate of error rate?
folds_list <- vector(mode="list", length=10)
folds$index <- 1:nrow(folds)
for (i in 1:10) {
  fold <- paste("^",i,"$",sep="")
  fold_index <- grep(eval(fold), folds$fold)
  folds_list[[i]] <- fold_index
}
train_set_factored <- train_set
valid_set_factored <- valid_set

train_set_factored$label <- factor(train_set_factored$label, levels = c("0", "1"), labels = c("level0", "level1"))
valid_set_factored$label <- factor(valid_set_factored$label, levels = c("0", "1"), labels = c("level0", "level1"))

set.seed(33)
rfFit <- train(label~., train_set_factored[,-1],
               method = "rf",
               metric = "AUC",
               trControl = trainControl(method = "cv", number = 10, index = folds_list, savePredictions = TRUE, summaryFunction = prSummary, classProbs = TRUE))

#print(rfFit$finalModel)

train_predict_rf <- predict(rfFit, train_set_factored[,-c(1,2)])
train_predict_rf<- factor(train_predict_rf, levels = c("level0", "level1"), c("0","1"))

valid_predict_rf <- predict(rfFit, valid_set_factored[,-c(1,2)])
valid_predict_rf <- factor(valid_predict_rf, levels = c("level0", "level1"), c("0","1"))

rf_results <- perf(train_predict_rf, train_label, valid_predict_rf, valid_label, description, "random forest", criteria)

# variable importance (scaled from 0 to 100)
rfImp <- varImp(rfFit)$importance
rfImp$featureNumber <- 1:ncol(train_set[,-c(1,2)])
rfImp <- rfImp[order(-rfImp$Overall),]
rf_features <- paste(rfImp$featureNumber[1:20], collapse = '|')
rf_results$features <- rf_features

# add to current experiment archive
avg_results_all <- rbind(avg_results_all, rf_results)

# add to other results
results_summary <- rbind(results_summary, rf_results)

##### Support vector machine #####
## linear kernel ##

train_set_factored <- train_set
valid_set_factored <- valid_set

train_set_factored$label <- factor(train_set_factored$label, levels = c("0", "1"), labels = c("level0", "level1"))
valid_set_factored$label <- factor(valid_set_factored$label, levels = c("0", "1"), labels = c("level0", "level1"))

set.seed(55)
svmFit <- train(label~., train_set_factored[,-1],
               method = "svmLinear",
               metric = "AUC",
               trControl = trainControl(method = "cv", number = 10, index = folds_list, savePredictions = TRUE, summaryFunction = prSummary, classProbs = TRUE),
               scale = FALSE)

train_predict_svm <- predict(svmFit, train_set_factored[,-c(1,2)])
train_predict_svm <- factor(train_predict_svm, levels = c("level0", "level1"), c("0","1"))

valid_predict_svm <- predict(svmFit, valid_set_factored[,-c(1,2)])
valid_predict_svm <- factor(valid_predict_svm, levels = c("level0", "level1"), c("0","1"))

svm_results <- perf(train_predict_svm, train_label, valid_predict_svm, valid_label, description, "svm linear kernel", criteria)

# variable importance (scaled from 0 to 100)
svmImp <- varImp(svmFit)$importance
svmImp$featureNumber <- 1:ncol(train_set[,-c(1,2)])
svmImp <- svmImp[order(-svmImp$level0),]
svm_features <- paste(svmImp$featureNumber[1:20], collapse = '|')
svm_results$features <- svm_features

# add to current experiment archive
avg_results_all <- rbind(avg_results_all, svm_results)

# add to other results
results_summary <- rbind(results_summary, svm_results)

avg_results_all

# save results to file
write.csv(results_summary, "results/results_summary_allModels.csv", row.names = F)
write.csv(avg_results_all, filename, row.names = F)
  




### Learning curves ### (DON'T TOTALLY TRUST THESE, BUT GOOD ENOUGH FOR DECIDING WE HAVE ENOUGH PATIENTS)
train_set_factored <- train_set[,-1]
valid_set_factored <- valid_set[,-1]

train_set_factored$label <- factor(train_set_factored$label, levels = c("0", "1"), labels = c("level0", "level1"))
valid_set_factored$label <- factor(valid_set_factored$label, levels = c("0", "1"), labels = c("level0", "level1"))

## V1 with built-in function

lda_data <- learing_curve_dat(train_set_factored,
                              outcome = "label",
                              method = "rf",
                              metric = "ROC",
                              trControl = trainControl(method = "cv",
                                                       number = 10,
                                                       classProbs = TRUE,
                                                       summaryFunction = twoClassSummary)
)

ggplot(lda_data, aes(x = Training_Size, y = ROC, color = Data)) +
  geom_smooth(method = loess, span = .8) + 
  theme_bw()


## V2 with custom loop (http://stackoverflow.com/questions/20370827/plot-learning-curves-with-caret-package-and-r)
# RF #
learnCurve <- data.frame(m = integer(length(c(seq(50, 338, 10), 338))),
                         trainROC = integer(length(c(seq(50, 338, 10), 338))),
                         cvROC = integer(length(c(seq(50, 338, 10), 338))))

testY <- valid_set_factored$label

trainControl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = prSummary)
metric <- "AUC"

count <- 1
for (i in c(seq(50, 338, 10), 338)) {
  learnCurve$m[count] <- i
  
  rfFit <- train(label~., train_set_factored[1:i,],
                 method = "rf",
                 metric = metric,
                 trControl = trainControl,
                 linout = TRUE)
  learnCurve$trainROC[count] <- rfFit$results$AUC[1]
  print(rfFit$results$AUC[1])
  
  prediction <- predict(rfFit, valid_set_factored[,-1])
  roc <- postResample(prediction, testY)
  learnCurve$cvROC[count] <- roc[1]
  
  count <- count + 1
}

learnCurve_train <- learnCurve[,c(1,2)]
colnames(learnCurve_train)[2] <- "auc"
learnCurve_train$source <- "train"
learnCurve_train$algorithm <- "rf"
learnCurve_valid <- learnCurve[,c(1,3)]
colnames(learnCurve_valid)[2] <- "auc"
learnCurve_valid$source <- "valid"
learnCurve_valid$algorithm <- "rf"

learnCurve_rf <- rbind(learnCurve_train, learnCurve_valid)

ggplot(learnCurve_rf, aes(m, auc, color = source)) + 
  geom_line()

# SVM #
learnCurve <- data.frame(m = integer(length(c(seq(50, 338, 10), 338))),
                         trainROC = integer(length(c(seq(50, 338, 10), 338))),
                         cvROC = integer(length(c(seq(50, 338, 10), 338))))

testY <- valid_set_factored$label

trainControl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = prSummary)
metric <- "AUC"

count <- 1
for (i in c(seq(50, 338, 10), 338)) {
  learnCurve$m[count] <- i
  
  rfFit <- train(label~., train_set_factored[1:i,],
                 method = "svmLinear",
                 metric = metric,
                 scale = F,
                 trControl = trainControl)
  learnCurve$trainROC[count] <- rfFit$results$AUC[1]
  
  prediction <- predict(rfFit, valid_set_factored[,-1])
  roc <- postResample(prediction, testY)
  learnCurve$cvROC[count] <- roc[1]
  
  count <- count + 1
}

learnCurve_train <- learnCurve[,c(1,2)]
colnames(learnCurve_train)[2] <- "auc"
learnCurve_train$source <- "train"
learnCurve_train$algorithm <- "svm"
learnCurve_valid <- learnCurve[,c(1,3)]
colnames(learnCurve_valid)[2] <- "auc"
learnCurve_valid$source <- "valid"
learnCurve_valid$algorithm <- "svm"

learnCurve_svm <- rbind(learnCurve_train, learnCurve_valid)

learnCurves <- rbind(learnCurve_rf, learnCurve_svm)

# lasso #
learnCurve <- data.frame(m = integer(length(c(seq(100, 338, 10), 338))),
                         trainROC = integer(length(c(seq(100, 338, 10), 338))),
                         cvROC = integer(length(c(seq(100, 338, 10), 338))))

testY <- valid_set$label

count <- 1
for (i in c(seq(100, 338, 10), 338)) {
  learnCurve$m[count] <- i
  
  model <- cv.glmnet(train_matrix[1:i,], train_label[1:i], alpha=1, family="binomial")
  best_lambda <- model$lambda.min
  
  train_predict <- predict(model, train_matrix[1:i,], s = best_lambda, type = "response")
  valid_predict <- predict(model, valid_matrix, s = best_lambda, type = "response")
  
  train_predict <- as.numeric(train_predict)
  train_label_num <- as.numeric(train_label) - 1
  pred <- prediction(train_predict, train_label_num[1:i])
  learnCurve$trainROC[count] <- slot(performance(pred, 'auc'), "y.values")[[1]][1]
  
  valid_predict <- as.numeric(valid_predict)
  valid_label_num <- as.numeric(valid_label) - 1
  pred_valid <- prediction(valid_predict, valid_label_num)
  learnCurve$cvROC[count] <- slot(performance(pred_valid, "auc"), "y.values")[[1]][1]
  
  count <- count + 1
}

learnCurve_train <- learnCurve[,c(1,2)]
colnames(learnCurve_train)[2] <- "auc"
learnCurve_train$source <- "train"
learnCurve_train$algorithm <- "lasso"
learnCurve_valid <- learnCurve[,c(1,3)]
colnames(learnCurve_valid)[2] <- "auc"
learnCurve_valid$source <- "valid"
learnCurve_valid$algorithm <- "lasso"

learnCurve_lasso <- rbind(learnCurve_train, learnCurve_valid)

learnCurves <- rbind(learnCurves, learnCurve_lasso)

ggplot(learnCurves, aes(m, auc, color = source)) + 
  geom_line() + 
  facet_grid(algorithm ~ .)


