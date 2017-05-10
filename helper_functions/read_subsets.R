options(digits=11)

load_subset <- function(dataset, set, criteria) {
	if (tolower(set) == "train" | tolower(set) == "training") {
		if (tolower(criteria) == "ukwp") {
			train_patients <- read.table("dataset_splits/training_set_patients_UKWP.txt", sep = "\t", as.is = T, header = T)
			
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
			train_patients <- train_patients[,c(3,5)]
			colnames(train_patients)[2] <- "patient_id"
			
		} else if (tolower(criteria) == "hr") {
			train_patients <- read.table("dataset_splits/training_set_patients_HR.txt", sep = "\t", as.is = T, header = T)
			
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
			train_patients <- train_patients[,c(4,5)]
			colnames(train_patients)[2] <- "patient_id"
		
		} else {
			print("ERROR! Choose valid criteria")
			break
		}
		
		colnames(train_patients)[1] <- "label"
		train_set_unsorted <- merge(dataset, train_patients)
		train_set <- train_set_unsorted[order(train_set_unsorted$patient_id),]
		
		remove(train_set_unsorted)
		remove(train_patients)
		
		return(train_set)
		
	} else if (tolower(set) == "valid" | tolower(set) == "validation") {
		if (tolower(criteria) == "ukwp") {
			valid_patients <- read.table("dataset_splits/validation_set_patients_UKWP.txt", sep = "\t", as.is = T, header = T)
			
			valid_patients$patient_id_fix <- NA
			valid_patients$patient_id <- as.character(valid_patients$patient_id)
			for (i in 1:nrow(valid_patients)) {
			  if (nchar(strsplit(valid_patients$patient_id[i], "\\.")[[1]][2]) == 7) {
				valid_patients$patient_id_fix[i] <- valid_patients$patient_id[i]
			  } else if (nchar(strsplit(valid_patients$patient_id[i], "\\.")[[1]][2]) == 6) {
				valid_patients$patient_id_fix[i] <- paste(valid_patients$patient_id[i], "0", sep = "")
			  } else if (nchar(strsplit(valid_patients$patient_id[i], "\\.")[[1]][2]) == 5) {
				valid_patients$patient_id_fix[i] <- paste(valid_patients$patient_id[i], "00", sep = "")
			  } else {
				print("problem")
				print(valid_patients$patient_id[i])
			  }
			}
			valid_patients <- valid_patients[,-1]	
			valid_patients <- valid_patients[,c(3,5)]
			colnames(valid_patients)[2] <- "patient_id"
			
		} else if (tolower(criteria) == "hr") {
			valid_patients <- read.table("dataset_splits/validation_set_patients_HR.txt", sep = "\t", as.is = T, header = T)
			
			valid_patients$patient_id_fix <- NA
			valid_patients$patient_id <- as.character(valid_patients$patient_id)
			for (i in 1:nrow(valid_patients)) {
			  if (nchar(strsplit(valid_patients$patient_id[i], "\\.")[[1]][2]) == 7) {
				valid_patients$patient_id_fix[i] <- valid_patients$patient_id[i]
			  } else if (nchar(strsplit(valid_patients$patient_id[i], "\\.")[[1]][2]) == 6) {
				valid_patients$patient_id_fix[i] <- paste(valid_patients$patient_id[i], "0", sep = "")
			  } else if (nchar(strsplit(valid_patients$patient_id[i], "\\.")[[1]][2]) == 5) {
				valid_patients$patient_id_fix[i] <- paste(valid_patients$patient_id[i], "00", sep = "")
			  } else {
				print("problem")
				print(valid_patients$patient_id[i])
			  }
			}
			valid_patients <- valid_patients[,-1]
			valid_patients <- valid_patients[,c(4,5)]
			colnames(valid_patients)[2] <- "patient_id"
			
		} else {
			print("ERROR! Choose valid criteria")
			break
		}
		
		colnames(valid_patients)[1] <- "label"
		valid_set_unsorted <- merge(dataset, valid_patients)
		valid_set <- valid_set_unsorted[order(valid_set_unsorted$patient_id),]
		
		remove(valid_set_unsorted)
		remove(valid_patients)
		
		return(valid_set)
	}
}
