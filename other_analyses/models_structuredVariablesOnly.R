library(glmnet)
library(dplyr)
library(ROCR)
library(caret)

setwd("~/AD_lasso")
options(digits=11)
set.seed(999)

# run preprocessing, dimensionality reduction (concept grouping) steps
source("diagnosis_codes.R")
source("performance_measures.R")

# to change label scheme, see end of preprocessing.R

#### starting with dat_grouped
#dat_grouped <- read.csv("AD_data_patientAggregated_withLabels_grouped.csv", as.is = T)

## change description of current experiment
description <- "structured variables only|primary and secondary Dx codes"
criteria <- "HR"

# demographics
demo <- read.table("demographics.txt", sep = "\t", header = T)
colnames(demo)[1] <- "patient_id"

#ggplot(demo, aes(x = race)) +
#  geom_histogram(stat = "count") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))

demo$age <- floor(as.numeric(as.Date("2017-03-15") - as.Date(demo$birth_dts))/365.25)
demo$gender.c <- ifelse(demo$gender == "Female",
                        1,
                        0)
demo$race.c <- ifelse(demo$race == "White or Caucasian",
                      1,
                      0)

demo <- demo[, c(1, 7:9)] #check indices
#demo <- demo[, c(1, 7:8)]

# merge with AD diagnosis codes
# if no code for patient, make it 0
all_diag_agg$ad_log <- log(all_diag_agg$ad_count + 1)
all_diag_agg$cd_log <- log(all_diag_agg$cd_count + 1)
all_diag_agg[, c(3:4)] <- all_diag_agg[, c(3:4)] / all_diag_agg$code_count # normalized by # of facts
ad_diag_agg <- all_diag_agg[, -c(2)]
# dat_grouped_codes <- all_diag_agg[, -c(2,5:7)] # primary dx codes only
# dat_grouped_codes <- all_diag_agg[, -c(2)]
dat_grouped_codes <- merge(demo, ad_diag_agg, all.x = TRUE)
dat_grouped_codes[is.na(dat_grouped_codes)] <- 0

# merge with coded medications
# if no code for patient, make it 0, otherwise 1
coded_meds <- read.csv("meds_coded_summarized.csv", as.is = T)
dat_grouped_codes <- merge(dat_grouped_codes, coded_meds, all.x = TRUE)
dat_grouped_codes[is.na(dat_grouped_codes)] <- 0

# merge with labs
# starting point: boolean whether any allergy test was performed at any time
labs <- read.table("labs_031517.txt", sep = "\t", header = TRUE)
colnames(labs)[1] <- "patient_id"
labs_summary <- labs %>%
  group_by(patient_id) %>%
  summarise(lab = 1)

dat_grouped_codes <- merge(dat_grouped_codes, labs_summary, all.x = TRUE)
dat_grouped_codes[is.na(dat_grouped_codes)] <- 0

# read in patient_id for those in train_set
train_patients <- read.table("training_set_patients_HR.txt", sep = "\t", as.is = T, header = T)
valid_patients <- read.table("validation_set_patients_HR.txt", sep = "\t", as.is = T, header = T)
#train_patients <- read.table("training_set_patients_UKWP.txt", sep = "\t", as.is = T, header = T)
#valid_patients <- read.table("validation_set_patients_UKWP.txt", sep = "\t", as.is = T, header = T)
train_patients <- train_patients[,c(1,5)] # HR
valid_patients <- valid_patients[,c(1,5)] # HR
#train_patients <- train_patients[,c(1,4)] # UKWP
#valid_patients <- valid_patients[,c(1,4)] # UKWP
colnames(train_patients)[2] <- "label"
colnames(valid_patients)[2] <- "label"

train_set_unsorted <- merge(dat_grouped_codes, train_patients)
valid_set_unsorted <- merge(dat_grouped_codes, valid_patients)

train_set <- train_set_unsorted[order(train_set_unsorted$patient_id),]
valid_set <- valid_set_unsorted[order(valid_set_unsorted$patient_id),]

# label index
label_index <- grep("^label$", names(train_set))

# create matrices of variables
train_matrix <- as.matrix(train_set[,-c(1,label_index)])
valid_matrix <- as.matrix(valid_set[,-c(1,label_index)])

# create outcome variables
train_label <- as.factor(train_set$label)
valid_label <- as.factor(valid_set$label)

### Train the model ###
## cv.glmnet to set best lambda value ##
# specify own folds so can test performance on validation set #
# write set of folds ONCE, use for all experiments
#folds <- createFolds(train_set$label, k = 10, list = F)
#folds_df <- data.frame(fold = folds, patient_id = train_set$patient_id)
#write.csv(folds_df, "cross_validation_folds_UKWP.csv", row.names = F)

folds <- read.csv("cross_validation_folds_HR.csv", as.is = T)
#folds <- read.csv("cross_validation_folds_UKWP.csv", as.is = T)
#train_set_folds <- merge(train_set, folds)
foldid <- folds$fold

model <- cv.glmnet(train_matrix, train_label, alpha=1, family="binomial", foldid=foldid)
#plot(model)
best_lambda <- model$lambda.min

train_predict <- predict(model, train_matrix, s = best_lambda, type = "class")
valid_predict <- predict(model, valid_matrix, s = best_lambda, type = "class")

lasso_results <- perf(train_predict, train_label, valid_predict, valid_label, description, "regular lasso", criteria)

# features left in model
lasso_coefs <- coef(model, s = best_lambda)
lasso_features <- lasso_coefs@i[-1]
lasso_features <- paste(lasso_features, collapse = "|")
lasso_results$features <- lasso_features

# filename for independent archive of current experiment
filename <- paste("results/", strptime(Sys.time(), format="%Y-%m-%d"), "_", strftime(Sys.time(), format = "%H:%M:%S"), ".csv", sep = "")
filename <- gsub(":", "-", filename)
#write.csv(lasso_results, filename, row.names = F)

# add current results to summary document
# or create new summary document if first experiment
if (file.exists("results_summary.csv")) {
  results_summary <- read.csv("results_summary.csv", as.is = T, header = T)  
  results_summary <- rbind(results_summary, lasso_results)
} else {
  results_summary <- lasso_results
}

##### Adaptive LASSO #####
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

adapt_lasso_coefs <- coef(lasso_adapt, s = best_lambda)
adapt_lasso_features <- adapt_lasso_coefs@i[-1]
adapt_lasso_features <- paste(adapt_lasso_features, collapse = "|")
adapt_lasso_results$features <- adapt_lasso_features

# add results to experiment archive
avg_results_all <- rbind(lasso_results, adapt_lasso_results)

# add current results to summary document
results_summary <- rbind(results_summary, adapt_lasso_results)


##### Random Forest Classification #####
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

rfFit <- train(label~., train_set_factored[,-1],
               method = "rf",
               metric = "AUC",
               trControl = trainControl(method = "cv", number = 10, index = folds_list, savePredictions = TRUE, summaryFunction = prSummary, classProbs = TRUE))

train_predict_rf <- predict(rfFit, train_set_factored[,-1])
train_predict_rf<- factor(train_predict_rf, levels = c("level0", "level1"), c("0","1"))

valid_predict_rf <- predict(rfFit, valid_set_factored[,-1])
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

svmFit <- train(label~., train_set_factored[,-1],
                method = "svmLinear",
                metric = "AUC",
                trControl = trainControl(method = "cv", number = 10, index = folds_list, savePredictions = TRUE, summaryFunction = prSummary, classProbs = TRUE),
                scale = FALSE)

train_predict_svm <- predict(svmFit, train_set_factored[,-1])
train_predict_svm <- factor(train_predict_svm, levels = c("level0", "level1"), c("0","1"))

valid_predict_svm <- predict(svmFit, valid_set_factored[,-1])
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
write.csv(results_summary, "results_summary.csv", row.names = F)
write.csv(avg_results_all, filename, row.names = F)
