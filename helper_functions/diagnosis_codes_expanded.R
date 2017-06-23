library(dplyr)

# natural log transformed variable for each diagnosis code
# normalized (/total count icd9) variable for each(?) diagnosis code (or just the AD & CD/eczema ones)

load_dx_codes <- function(source_file, log_transform, normSet) {
	all_diag <- read.delim(source_file, header = T)
  #all_diag <- read.delim("data_sources/diagnosis_codes_ids_deduped_all_sources.txt", header = T)
	
	# make all patient_id 7 digits
	all_diag$patient_id <- as.character(all_diag$patient_id)
	all_diag$patient_id_fix <- NA
	for (i in 1:nrow(all_diag)) {
	  if (nchar(strsplit(all_diag$patient_id[i], "\\.")[[1]][2]) == 7) {
	    all_diag$patient_id_fix[i] <- all_diag$patient_id[i]
	  } else if (nchar(strsplit(all_diag$patient_id[i], "\\.")[[1]][2]) == 6) {
	    all_diag$patient_id_fix[i] <- paste(all_diag$patient_id[i], "0", sep = "")
	  } else if (nchar(strsplit(all_diag$patient_id[i], "\\.")[[1]][2]) == 5) {
	    all_diag$patient_id_fix[i] <- paste(all_diag$patient_id[i], "00", sep = "")
	  } else {
	    print("problem")
	    print(all_diag$patient_id[i])
	  }
	}
	all_diag <- subset(all_diag, select = -patient_id)
	names(all_diag)[6] <- "patient_id"
	
	# get by patient code counts
	all_diag_agg <- all_diag %>%
	group_by(patient_id) %>%
	summarise(code_count = length(value),
		ad9_count = sum(value == "691.8"),
		cd9_count = sum(value == "692.9"),
        cd10_count = sum(value == "L30.9"),
        ad10_count = length(grep("L20", value)),
        asthma_count1 = length(grep("493", value)),
        asthma_count2 = length(grep("J45", value)),
        hayfever_count1 = length(grep("477", value)),
        hayfever_count2 = length(grep("J30", value)),
        food_count1 = length(grep("955.6", value)),
        food_count2 = sum(value == "955.7"),
        food_count3 = sum(value == "995.3"),
        food_count4 = sum(value == "693.1"))

	# combine icd9 and icd10 codes
	all_diag_agg$ad_count <- all_diag_agg$ad10_count + all_diag_agg$ad9_count
	all_diag_agg$cd_count <- all_diag_agg$cd10_count + all_diag_agg$cd9_count

	# combine comorbidity codes
	all_diag_agg$asthma_count <- all_diag_agg$asthma_count1 + all_diag_agg$asthma_count2
	all_diag_agg$hayfever_count <- all_diag_agg$hayfever_count1 + all_diag_agg$hayfever_count2
	all_diag_agg$food_count <- all_diag_agg$food_count1 + all_diag_agg$food_count2 + all_diag_agg$food_count3 + all_diag_agg$food_count4

	if (normSet == "phenOnly") {
	  all_diag_agg$ad_norm <- all_diag_agg$ad_count / all_diag_agg$code_count # normalized by # of facts
	  all_diag_agg$cd_norm <- all_diag_agg$cd_count / all_diag_agg$code_count
	  
	} else if (normSet == "all") {
	  all_diag_agg$ad_norm <- all_diag_agg$ad_count / all_diag_agg$code_count # normalized by # of facts
	  all_diag_agg$cd_norm <- all_diag_agg$cd_count / all_diag_agg$code_count
	  all_diag_agg$asthma_norm <- all_diag_agg$asthma_count / all_diag_agg$code_count
	  all_diag_agg$food_norm <- all_diag_agg$food_count / all_diag_agg$code_count
	  all_diag_agg$hayfever_norm <- all_diag_agg$hayfever_count / all_diag_agg$code_count
	  
	} else if (normSet == "none") {
	  
	} else {
	  print("ERROR! Invalid normSet value")
	  break
	}
	
	
	if (log_transform) {
	  all_diag_agg$ad_log <- log(all_diag_agg$ad_count + 1)
	  all_diag_agg$cd_log <- log(all_diag_agg$cd_count + 1)
	  all_diag_agg$asthma_log <- log(all_diag_agg$asthma_count + 1)
	  all_diag_agg$hayfever_log <- log(all_diag_agg$hayfever_count + 1)
	  all_diag_agg$food_log <- log(all_diag_agg$food_count + 1)
	}
	
	ad_diag_agg <- subset(all_diag_agg, select = -c(code_count, 
	                                                ad9_count, ad10_count, 
	                                                cd9_count, cd10_count, 
	                                                asthma_count1, asthma_count2,
	                                                hayfever_count1, hayfever_count2,
	                                                food_count1, food_count2, food_count3, food_count4,
	                                                ad_count, cd_count, asthma_count, hayfever_count, food_count))

	remove(all_diag)
	
	return(ad_diag_agg)
}







