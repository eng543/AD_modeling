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
description <- "replication|concepts grouped|note counts|all Dx codes normalized and all log transformed|relations"

# NLP
nlp_only <- F
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
    preprocess_relations("data_sources/output_location_relation_060117.csv", criteria, count_type)
  }
  
  # group_codes(aggregated_data, criteria, group_meds_labs)
  dat_grouped <- group_codes(dat_agg, criteria, group_meds, relations)
  
  remove(dat_agg)
  
  # load_demographics(source_file, race(boolean))
  demo <- load_demographics("data_sources/demographics.txt", race)
  
  # merge with dataset
  dat_grouped <- merge(dat_grouped, demo)
  
  remove(demo)
  
  # load_dx_codes(source_file, log_transform(boolean), norm_set(phenOnly, all, none))
  ad_diag_agg <- load_dx_codes("data_sources/diagnosis_codes_ids_deduped_all_sources.txt", log_transform, norm_set)
  
  # merge with dataset
  dat_grouped_codes <- merge(dat_grouped, ad_diag_agg, all.x = TRUE)
  #dat_grouped_codes[is.na(dat_grouped_codes)] <- 0
  
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
  
  dat_grouped_codes <- group_codes(dat_agg, criteria, group_meds, relations)

  remove(dat_agg)
}

# DO THIS ONCE! GET THE SET OF PATIENTS REPRESENTED IN DATASET FOR DATA PARTITIONING
# dataset_splits/create_train_test.R
#write.table(dat_grouped_codes[,c(1,2)], "candidate_patients_class.txt", sep = "\t", row.names = F)

# read in patient_id for those in train_set/valid_set
train_set <- load_subset(dat_grouped_codes, "train", criteria)
valid_set <- load_subset(dat_grouped_codes, "valid", criteria)

# create outcome variables
train_label <- as.factor(train_set$label)
valid_label <- as.factor(valid_set$label)

# read in predefined folds
if (tolower(criteria) == "hr") {
  folds <- read.csv("dataset_splits/cross_validation_folds_HR.csv", as.is = T)  
} else if (tolower(criteria) == "ukwp") {
  folds <- read.csv("dataset_splits/cross_validation_folds_UKWP.csv", as.is = T) 
} else {
  print("ERROR!! Criteria not valid")
}

foldid <- folds$fold
folds_list <- vector(mode="list", length=10)
folds$index <- 1:nrow(folds)
for (i in 1:10) {
  fold <- paste("^",i,"$",sep="")
  fold_index <- grep(eval(fold), folds$fold)
  folds_list[[i]] <- fold_index
}



# refactor the label for training
train_set_factored <- train_set
valid_set_factored <- valid_set

train_set_factored$label <- factor(train_set_factored$label, levels = c("0", "1"), labels = c("level0", "level1"))
valid_set_factored$label <- factor(valid_set_factored$label, levels = c("0", "1"), labels = c("level0", "level1"))

# make sure train_set organized by patient_id for replicability (training order must be fixed)
train_set_factored <- train_set_factored[order(train_set_factored$patient_id),]

# train model
# 10 fold cv
set.seed(33)
rfFit <- train(label~., train_set_factored[,-1],
               method = "rf",
               metric = "AUC",
               trControl = trainControl(method = "cv", number = 10, index = folds_list, savePredictions = TRUE, summaryFunction = prSummary, classProbs = TRUE))

print(rfFit$finalModel)

train_predict_rf <- predict(rfFit, train_set_factored[,-c(1)])
train_predict_rf<- factor(train_predict_rf, levels = c("level0", "level1"), c("0","1"))

valid_predict_rf <- predict(rfFit, valid_set_factored[,-c(1)])
valid_predict_rf <- factor(valid_predict_rf, levels = c("level0", "level1"), c("0","1"))

# create results output
rf_results <- perf(train_predict_rf, train_label, valid_predict_rf, valid_label, description, "random forest", criteria)

rf_results

# variable importance (scaled from 0 to 100)
rfImp <- varImp(rfFit)$importance#[order(-rfImp$Overall),]
rfImp$featureNumber <- 1:ncol(train_set[,-c(1,2)])
rfImp <- rfImp[order(-rfImp$Overall),]
rf_features <- paste(rfImp$featureNumber[1:20], collapse = '|')
rf_results$features <- rf_features

# save results to file
# filename for independent archive of current experiment
filename <- paste("results_individual_experiments/", strptime(Sys.time(), format="%Y-%m-%d"), "_", strftime(Sys.time(), format = "%H:%M:%S"), ".csv", sep = "")
filename <- gsub(":", "-", filename)
#write.csv(lasso_results, filename, row.names = F)

# add current results to summary document
# or create new summary document if first experiment
if (file.exists("results/results_summary_rf.csv")) {
  results_summary <- read.csv("results/results_summary_rf.csv", as.is = T, header = T)  
  results_summary <- rbind(results_summary, rf_results)
} else {
  results_summary <- rf_results
}

# save results to file
write.csv(results_summary, "results/results_summary_rf.csv", row.names = F)
write.csv(rf_results, filename, row.names = F)



# attempt to make a tree from trained model
# train using randomForest package
library(tree)

set.seed(71)
names(train_set_factored) <- make.names(names(train_set_factored))
rf <- randomForest(label ~ ., train_set_factored[,-1], importance = T)
reprtree:::plot.getTree(rf)

rf_tr <- tree(label ~ ., data=train_set_factored[,-1])
rf_tr
rf_tr$frame

