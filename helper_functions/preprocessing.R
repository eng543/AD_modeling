library(dplyr)

setwd("~/AD_modeling")
options(digits=11)

### aggregate concept counts at patient level (collapsing across notes)

preprocess <- function(source_file, criteria, count_type) {
  #dat <- read.csv("data_sources/output_replication_test_053117.csv", as.is = T)
	dat <- read.csv(source_file, as.is = T)
	
	dat$patient_id <- sapply(strsplit(dat$note_id, "_"), "[[", 1)

	# remove note id column so can use summarize
	dat <- dat[,-1]

	# control flow for aggregation type
	if (tolower(count_type) == "add") {
		# add up counts across notes for each patient
		dat_agg <- dat %>%
			group_by(patient_id) %>%
			summarise_each(funs(sum))
			
	} else if (tolower(count_type) == "note") {
		# add up how many notes have concept mention for each patient
		dat_binary <- dat
		patient_col <- dat_binary$patient_id
		patient_col_index <- grep("patient_id", names(dat_binary))
		dat_binary <- dat_binary[, -patient_col_index]

		dat_binary[dat_binary > 0] <- 1
		dat_binary$patient_id <- patient_col

		dat_agg <- dat_binary %>%
			group_by(patient_id) %>%
			summarise_each(funs(sum))
			
		remove(dat_binary)
	} else {
		print("ERROR! Choose valid aggregation type")
		break
	}

	dat_agg_backup <- dat_agg
	remove(dat)

	# make all patient_id 7 digits
	#dat_agg$patient_id <- as.character(dat_agg$patient_id)
	dat_agg$patient_id_fix <- NA
	for (i in 1:nrow(dat_agg)) {
	  if (nchar(strsplit(dat_agg$patient_id[i], "\\.")[[1]][2]) == 7) {
		dat_agg$patient_id_fix[i] <- dat_agg$patient_id[i]
	  } else if (nchar(strsplit(dat_agg$patient_id[i], "\\.")[[1]][2]) == 6) {
		dat_agg$patient_id_fix[i] <- paste(dat_agg$patient_id[i], "0", sep = "")
	  } else if (nchar(strsplit(dat_agg$patient_id[i], "\\.")[[1]][2]) == 5) {
		dat_agg$patient_id_fix[i] <- paste(dat_agg$patient_id[i], "00", sep = "")
	  } else {
		print("problem")
		print(dat_agg$patient_id[i])
	  }
	}
	dat_agg <- dat_agg[,-1]

	# read in diagnosis labels
	if (tolower(criteria) == "ukwp") {
		cat <- read.table("data_sources/UKWP_labels.txt", sep = "\t", header = TRUE, as.is = T)
	
	
	} else if (tolower(criteria) == "hr") {
		cat <- read.table("data_sources/HR_labels.txt", sep = "\t", header = TRUE, as.is = T)

	} else {
		print("ERROR! Pick a valid criteria")
		break
	}
	
	colnames(cat)[1] <- "patient_id"

	# make all patient_id 7 digits
	# should be okay by default, but run it anyway to be safe and get column type/name to match dat_agg
	cat$patient_id_fix <- NA
	cat$patient_id <- as.character(cat$patient_id)
	for (i in 1:nrow(cat)) {
	  if (nchar(strsplit(cat$patient_id[i], "\\.")[[1]][2]) == 7) {
		cat$patient_id_fix[i] <- cat$patient_id[i]
	  } else if (nchar(strsplit(cat$patient_id[i], "\\.")[[1]][2]) == 6) {
		cat$patient_id_fix[i] <- paste(cat$patient_id[i], "0", sep = "")
	  } else if (nchar(strsplit(cat$patient_id[i], "\\.")[[1]][2]) == 5) {
		cat$patient_id_fix[i] <- paste(cat$patient_id[i], "00", sep = "")
	  } else {
		print("problem")
		print(cat$patient_id[i])
	  }
	}
	cat <- cat[,-1]

	# merge data and labels
	dat_agg <- merge(cat, dat_agg)

	# group definites and probables
	if (tolower(criteria) == "ukwp") {
		dat_agg$label <- ifelse(dat_agg$UKWPLabel == "UKWPDefinite" | dat_agg$UKWPLabel == "UKWPProbable",
							1,
	                        0)

	} else if (tolower(criteria) == "hr") {
		dat_agg$label <- ifelse(dat_agg$HRLabel == "HRDefinite" | dat_agg$HRLabel == "HRProbable",
							1,
							0)		
	}
	
	# write to file
	write.csv(dat_agg, "data_sources/AD_data_patientAggregated_withLabels.csv", row.names = F)

	remove(dat_agg_backup)
	return(dat_agg)

}
