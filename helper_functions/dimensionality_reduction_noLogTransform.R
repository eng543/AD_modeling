setwd("~/AD_lasso")

group_codes <- function(dat, criteria, logTransform) {
	#dat <- read.csv("data_sources/AD_data_patientAggregated_withLabels.csv", as.is = T)
	
	label_text <- paste(toupper(criteria), "Label", sep = "")
	
	# figure out what family hx/patient hx variables we need to deal with
	hist <- read.delim("data_sources/history_concepts.txt", sep = "\t", header = F)

	#for (i in names(dat)) {
	#  if (i != "label" & i != "UKWPLabel" & i != "patient_id") {
	#    split <- strsplit(i, "_")
	#    cui <- split[[1]][1]
	#    patient <- split[[1]][2]
	#    history <- split[[1]][3]
	#    negation <- split[[1]][4]
	#    if (patient == "family") {
	#      famhx <- paste("family:", cui)
	#      print(famhx)
	#    } else if (patient == "patient" & history == "true") {
	#      perhx <- paste("personal hx:", cui)
	#    }
	#  }
	#}

	# read in concept groups
	groups <- read.table("data_sources/Dictionary_Concepts_byCui.txt", sep = "\t", header = FALSE, as.is = TRUE)

	# replace original column names with indexed group names
	  # group~agent~negation~colcount
	# will throw warning if cui assigned to more than one group in Dictionary_Concepts_byCui.txt
	dat_groups <- dat

	colcount = 0
	# loops through the concepts (columns)
	for (i in names(dat_groups)) {
	  colcount = colcount + 1
	  
	  if (i != "patient_id_fix" & i != label_text & i != "label") {
		# splits into cui, family, history (on _)
		split <- strsplit(i, "_")
		cui <- split[[1]][1]
		family <- split[[1]][2]
		history <- split[[1]][3]
		negation <- split[[1]][4]
		
		# lookup concept grouping
		group_index <- grep(cui, groups$V2)
		
		if (length(group_index) > 0 ) {
		  new_col_name <- NA
		  group <- groups$V1[group_index]
		  # group positive patient (non-history) concepts
		  if (family == "patient" & history == "false" & negation == "0") {
			# creates new column name for concept: group~column#
			new_col_name <- paste(group, "patient", "positive", colcount, sep = "~")
			
			# group negative patient (non-history) concepts
		  } else if (family == "patient" & history == "false" & negation == ".1") {
			new_col_name <- paste(group, "patient", "negated", colcount, sep = "~")
			
			# group positive patient history concepts
		  } else if (family == "patient" & history == "true" & negation == "0") {
			# check if acceptable history concept
			hx_index <- grep(cui, hist$V2)
			if (length(hx_index) > 0) {
			  new_col_name <- paste(group, "perhx", "positive", colcount, sep = "~")
			} else {
			  # throw out the concept if not in the approved history concept list
			  new_col_name <- paste("dicard", "discard", "discard", colcount, sep = "~")
			}
			
			# group negative patient history concepts
		  } else if (family == "patient" & history == "true" & negation == ".1") {
			# check if acceptable history concept
			hx_index <- grep(cui, hist$V2)
			if (length(hx_index) > 0) {
			  new_col_name <- paste(group, "perhx", "negated", colcount, sep = "~")
			} else {
			  # throw out the concept if not in the approved history concept list
			  new_col_name <- paste("dicard", "discard", "discard", colcount, sep = "~")
			}
			
			# group positive family history concepts (include both true and false history)
		  } else if (family == "family" & negation == "0") {
			# check if acceptable history concept
			hx_index <- grep(cui, hist$V2)
			if (length(hx_index) > 0) {
			  new_col_name <- paste(group, "famhx", "positive", colcount, sep = "~")
			} else {
			  # throw out the concept if not in the approved history concept list
			  new_col_name <- paste("dicard", "discard", "discard", colcount, sep = "~")
			}
			
			# group negative family history concepts
		  } else if (family == "family" & negation == ".1") {
			# check if acceptable history concept
			hx_index <- grep(cui, hist$V2)
			if (length(hx_index) > 0) {
			  new_col_name <- paste(group, "famhx", "negative", colcount, sep = "~")
			} else {
			  # throw out the concept if not in the approved history concept list
			  new_col_name <- paste("dicard", "discard", "discard", colcount, sep = "~")
			}
			
		  } else {
			#print(paste("problem:", cui))
			# throw out concepts relating to "other" person
			new_col_name <- paste("discard", "discard", "discard", colcount, sep = "~")
		  }
		  
		} else {
		  # discard cuis not in group mapping (e.g., contact dermatitis just removed)
		  new_col_name <- paste("discard", "discard", "discard", colcount, sep = "~")
		}
		
		# set new column name
		colnames(dat_groups)[colcount] <- new_col_name
	  }
	}

	# combine columns (add counts) with the same group name
	dat_grouped <- data.frame("patient_id" = dat_groups$patient_id, "label" = dat_groups$label)

	unique_groups <- c()
	for (i in names(dat_groups)) {
	  if (i != "patient_id_fix" & i != label_text & i != "label") {
		col_long <- strsplit(i, "~")
		col <- paste(col_long[[1]][1], col_long[[1]][2], col_long[[1]][3], sep = "~")
		unique_groups <- c(unique_groups, col)
	  }
	}

	unique_groups <- unique(unique_groups)

	# loop through the unique set of groups
	for (i in unique_groups) {
	  # create list for concepts that belong to group
	  cols = c()
	  # loop through concepts assigned group~agent~negation~column #
	  for (j in names(dat_groups)) {
		# if concept is in current group
		split <- strsplit(j, "~") # split to get rid of the tailing column #
	  if (paste(split[[1]][1], split[[1]][2], split[[1]][3], sep = "~") == i) {
		  # add column to list, add values
		  if (length(cols) == 0) {
			cols <- c(dat_groups[,eval(j)])
		  } else {
			cols <- cols + c(dat_groups[,eval(j)])
		  }
		}
	  }
	  # create new column with group counts aggregated across individual concepts
	  dat_grouped[,eval(i)] <- cols
	  # move on to next group
	}

	# change medication groups to binary variables
	# log transform those that aren't binary
	med_cols <- grep("topical_steroid|topical_calcineurin_inhibitors|emollients|antihistimines|oral_steroids|
					 phototheraphy|other|oral_antibiotics|oral_tacrolimus|alternative_medicine", names(dat_grouped))

	for (i in 1:ncol(dat_grouped)) {
	  if (i %in% med_cols) {
  		for (j in 1:nrow(dat_grouped)) {
  		  if (dat_grouped[j, i] > 0) {
  			  dat_grouped[j, i] <- 1
  		  }
  		}
	  } else {
		  if (i != 1 & i != 2 & logTransform == TRUE) {
		    dat_grouped[,i] <- log(dat_grouped[,i] + 1)
		  }
	  }
	}


	# get rid of discard column
	discard_cols <- grep("discard", names(dat_grouped))
	dat_grouped <- dat_grouped[,-discard_cols]

	# just in case there are cocnepts that do not belong to a group, make sure they're still included
	# don't think this applies anymore after another round of dictionary building
	#for (j in names(dat_groups)) {
	#  if (!(strsplit(j, "~")[[1]][1] %in% unique(groups$V1)) & j != "patient_id" & j != "label" & j != "classification" & j != "patient_id_fix") {
	#    dat_grouped[, eval(j)] <- dat_groups[, eval(j)]
	#  }
	#}

	remove(dat_groups)
	remove(dat)
	write.csv(dat_grouped, "AD_data_patientAggregated_withLabels_grouped.csv", row.names = F)
	
	return(dat_grouped)
}




