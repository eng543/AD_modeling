library(dplyr)

# natural log transformed variable for each diagnosis code
# normalized (/total count icd9) variable for each(?) diagnosis code (or just the AD & CD/eczema ones)

load_dx_codes <- function(source_file, log_transform, normSet) {
	all_diag <- read.delim(source_file, header = T)
  #all_diag <- read.delim("data_sources/diagnosis_codes_ids_deduped.txt", header = T)
	
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
	
	if (log_transform) {
		all_diag_agg$ad_log <- log(all_diag_agg$ad_count + 1)
		all_diag_agg$cd_log <- log(all_diag_agg$cd_count + 1)
	}

	# combine comorbidity codes
	all_diag_agg$asthma_count <- all_diag_agg$asthma_count1 + all_diag_agg$asthma_count2
	all_diag_agg$hayfever_count <- all_diag_agg$hayfever_count1 + all_diag_agg$hayfever_count2
	all_diag_agg$food_count <- all_diag_agg$food_count1 + all_diag_agg$food_count2 + all_diag_agg$food_count3 + all_diag_agg$food_count4

	all_diag_agg <- all_diag_agg[, -c(3:14)]
	if (log_transform) {
	  all_diag_agg[,c(5:7)] <- log(all_diag_agg[,c(5:7)] + 1)
	}
	
	if (normSet == "phenOnly") {
	  all_diag_agg[, c(3:4)] <- all_diag_agg[, c(3:4)] / all_diag_agg$code_count # normalized by # of facts
	} else if (normSet == "all") {
	  all_diag_agg[, c(3:7)] <- all_diag_agg[, c(3:7)] / all_diag_agg$code_count
	} else if (normSet == "none") {
	  
	} else {
	  print("ERROR! Invalid normSet value")
	  break
	}
	
	ad_diag_agg <- all_diag_agg[, -c(2)]

	remove(all_diag)
	
	return(ad_diag_agg)
}







