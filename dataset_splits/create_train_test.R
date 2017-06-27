library(caret)
setwd("~/AD_modeling")

dat <- read.table("data_sources/output_032717_allNotes.csv", as.is = T, sep = ",", header = T)

patients = c()
dat$patient_id <- dat$note_id
for(i in 1:nrow(dat)) {
  #split note_id on underscore to get patient_id [1] and note_id [2]
  split = strsplit(dat[i, 1], "_")
  # take patient part of split
  patient = split[[1]][1]
  # add new patient id to data frame
  dat[i, "patient_id"] <- patient
  # add patient id to list of patients
  patients <- c(patients, patient)
}
# get set of unique patients
unique_patients <- unique(patients)

unique_patients_fix <- c()
for (i in 1:length(unique_patients)) {
  if (nchar(strsplit(unique_patients[i], "\\.")[[1]][2]) == 7) {
    unique_patients_fix <- c(unique_patients_fix, unique_patients[i])
  } else if (nchar(strsplit(unique_patients[i], "\\.")[[1]][2]) == 6) {
    unique_patients_fix <- c(unique_patients_fix, paste(unique_patients[i], "0", sep = ""))
  } else if (nchar(strsplit(unique_patients[i], "\\.")[[1]][2]) == 5) {
    unique_patients_fix <- c(unique_patients_fix, paste(unique_patients[i], "00", sep = ""))
  } else {
    print("problem")
    print(unique_patients[i])
  }
}

# read in diagnosis label from HR Criteria
cat_HR <- read.table("data_sources/HR_labels.txt", sep = "\t", header = TRUE, as.is = T)
colnames(cat_HR)[1] <- "patient_id"

# make all patient_id 7 digits
# should be okay by default, but run it anyway to be safe and get column type/name to match dat_agg
cat_HR$patient_id_fix <- NA
cat_HR$patient_id <- as.character(cat_HR$patient_id)
for (i in 1:nrow(cat_HR)) {
  if (nchar(strsplit(cat_HR$patient_id[i], "\\.")[[1]][2]) == 7) {
    cat_HR$patient_id_fix[i] <- cat_HR$patient_id[i]
  } else if (nchar(strsplit(cat_HR$patient_id[i], "\\.")[[1]][2]) == 6) {
    cat_HR$patient_id_fix[i] <- paste(cat_HR$patient_id[i], "0", sep = "")
  } else if (nchar(strsplit(cat_HR$patient_id[i], "\\.")[[1]][2]) == 5) {
    cat_HR$patient_id_fix[i] <- paste(cat_HR$patient_id[i], "00", sep = "")
  } else {
    print("problem")
    print(cat_HR$patient_id[i])
  }
}
cat_HR <- cat_HR[,-1]

# remove patients not found during note pull
cat_HR <- cat_HR[cat_HR$patient_id_fix %in% unique_patients_fix,]

# read in diagnosis label from UKWP Criteria
cat_UKWP <- read.table("data_sources/UKWP_labels.txt", sep = "\t", header = TRUE, as.is = T)
colnames(cat_UKWP)[1] <- "patient_id"

# make all patient_id 7 digits
# should be okay by default, but run it anyway to be safe and get column type/name to match dat_agg
cat_UKWP$patient_id_fix <- NA
cat_UKWP$patient_id <- as.character(cat_UKWP$patient_id)
for (i in 1:nrow(cat_UKWP)) {
  if (nchar(strsplit(cat_UKWP$patient_id[i], "\\.")[[1]][2]) == 7) {
    cat_UKWP$patient_id_fix[i] <- cat_UKWP$patient_id[i]
  } else if (nchar(strsplit(cat_UKWP$patient_id[i], "\\.")[[1]][2]) == 6) {
    cat_UKWP$patient_id_fix[i] <- paste(cat_UKWP$patient_id[i], "0", sep = "")
  } else if (nchar(strsplit(cat_UKWP$patient_id[i], "\\.")[[1]][2]) == 5) {
    cat_UKWP$patient_id_fix[i] <- paste(cat_UKWP$patient_id[i], "00", sep = "")
  } else {
    print("problem")
    print(cat_UKWP$patient_id[i])
  }
}
cat_UKWP <- cat_UKWP[,-1]

# merge data and labels
cat <- merge(cat_UKWP, cat_HR)

cat$label_UKWP <- ifelse(cat$UKWPLabel == "UKWPDefinite" | cat$UKWPLabel == "UKWPProbable",
                    1,
                    0)

cat$label_HR <- ifelse(cat$HRLabel == "HRDefinite" | cat$HRLabel == "HRProbable",
                              1,
                              0)

#write.table(cat, "candidate_patients_class.txt", row.names = F)
#cat <- read.table("candidate_patients_class.txt", as.is = T, sep = "\t", header = T)

# make all patient_id 7 digits
# should be okay by default, but run it anyway to be safe and get column type/name to match dat_agg
# cat$patient_id_fix <- NA
# cat$patient_id <- as.character(cat$patient_id)
# for (i in 1:nrow(cat)) {
#   if (nchar(strsplit(cat$patient_id[i], "\\.")[[1]][2]) == 7) {
#     cat$patient_id_fix[i] <- cat$patient_id[i]
#   } else if (nchar(strsplit(cat$patient_id[i], "\\.")[[1]][2]) == 6) {
#     cat$patient_id_fix[i] <- paste(cat$patient_id[i], "0", sep = "")
#   } else if (nchar(strsplit(cat$patient_id[i], "\\.")[[1]][2]) == 5) {
#     cat$patient_id_fix[i] <- paste(cat$patient_id[i], "00", sep = "")
#   } else {
#     print("problem")
#     print(cat$patient_id[i])
#   }
# }
# 
# cat <- cat[,-1]
colnames(cat)[1] <- "patient_id"

# stratification based on UKWP label
train_UKWP <- createDataPartition(cat$label_UKWP, p = 0.6, list = T)
train_set_UKWP <- cat[train_UKWP,]
not_train_set_UKWP <- cat[-train_UKWP,]

valid_UKWP <- createDataPartition(not_train_set_UKWP$label_UKWP, p = 0.5, list = F)
valid_set_UKWP <- not_train_set_UKWP[valid_UKWP,]
test_set_UKWP <- not_train_set_UKWP[-valid_UKWP,]

write.table(train_set_UKWP, "training_set_patients_UKWP.txt", row.names = F, sep = "\t")
write.table(valid_set_UKWP, "validation_set_patients_UKWP.txt", row.names = F, sep = "\t")
write.table(test_set_UKWP, "test_set_patients_UKWP.txt", row.names = F, sep = "\t")

# stratification based on HR label
train_HR <- createDataPartition(cat$label_HR, p = 0.6, list = F)
train_set_HR <- cat[train_HR,]
not_train_set_HR <- cat[-train_HR,]

valid_HR <- createDataPartition(not_train_set_HR$label_HR, p = 0.5, list = F)
valid_set_HR <- not_train_set_HR[valid_HR,]
test_set_HR <- not_train_set_HR[-valid_HR,]

write.table(train_set_HR, "training_set_patients_HR.txt", row.names = F, sep = "\t")
write.table(valid_set_HR, "validation_set_patients_HR.txt", row.names = F, sep = "\t")
write.table(test_set_HR, "test_set_patients_HR.txt", row.names = F, sep = "\t")
