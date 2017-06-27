library(dplyr)

setwd("~/AD_modeling")
options(digits=11)



### aggregate at patient level


preprocess_relations <- function(source_file, criteria, count_type) {
  #dat <- read.csv("data_sources/output_location_relation_060117.csv", as.is = T)
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
	write.csv(dat_agg, "data_sources/AD_relationData_patientAggregated_withLabels.csv", row.names = F)

	remove(dat_agg_backup)
	remove(cat)
	#return(dat_agg)

}



# write candidate_patients_labels.txt for creating train/test sets


#### code block for combining two sets of results from different sources (nmff&nmh) ####
# source("preprocessing_nmh.R")
# dat_patients <- unique(dat_agg$patient_id_fix)
# 
# for (i in 1:nrow(dat_agg2)) {
#   pat <- dat_agg2[i, 1]
#   
#   # if patient common to nmh/nmff
#   if (pat %in% dat_patients) {
#     for (j in names(dat_agg2)) {
#       if (j != "patient_id_fix" & j != "patient_id" & j != "HRLabel" & j != "label") {
#         rw <- grep(pat, dat_agg$patient_id_fix)
# 
#         # if feature common to nmh/nmff, add counts across dataframes
#         if (j %in% names(dat_agg)) {
#           col <- grep(j, names(dat_agg))
#           dat_agg[rw, col] <- dat_agg[rw, col] + dat_agg2[i, j]
#           
#           # if feature unique to nmh, add new column and value from nmh data frame
#         } else { 
#           dat_agg[[j]] <- 0
#           col <- grep(j, names(dat_agg))
#           dat_agg[rw, col] <- dat_agg2[i, j]
#         }
#       }
#     }
#     # if patient in nmh but not nmff
#   } else {
#     # create new row for new patient
#     temp_row <- matrix(c(rep.int(NA, length(dat_agg))), nrow=1, ncol=length(dat_agg))
#     new_row <- data.frame(temp_row)
#     colnames(new_row) <- colnames(dat_agg)
#     new_row$patient_id_fix <- pat
#     #new_row$UKWPLabel <- dat_agg2$UKWPLabel[i]
#     new_row$HRLabel <- dat_agg2$HRLabel[i]
#     new_row$label <- dat_agg2$label[i]
#     dat_agg <- rbind(dat_agg, new_row)
#     
#     for (k in names(dat_agg2)) {
#       if (k != "patient_id_fix" & k != "patient_id" & k != "HRLabel" & k != "label") {
#         rw <- grep(pat, dat_agg$patient_id_fix)
#         
#         # if feature common to nmh/nmff, add counts across data frames
#         if (k %in% names(dat_agg)) {
#           col <- grep(k, names(dat_agg))
#           dat_agg[rw, col] <- dat_agg2[i, k]
#           
#           # if feature unique to nmh, add new column and value from nmh data frame
#         } else {
#           dat_agg[[k]] <- 0
#           col <- grep(k, names(dat_agg))
#           dat_agg[rw, col] <- dat_agg2[i, k]
#         }
#       }
#     }
#   }
# }
# dat_agg[is.na(dat_agg)] <- 0

# definites and negatives only
#dat_agg$label2 <- ifelse(dat_agg$classification == "UKWPDefinite",
#                         1,
#                         ifelse(dat_agg$classification == "UKWPNegative",
#                                0,
#                                -1))


