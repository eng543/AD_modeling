setwd("~/AD_modeling")

group_codes_relations <- function(dat, criteria) {
	#dat <- read.csv("data_sources/AD_relationData_patientAggregated_withLabels.csv", as.is = T)
	
	label_text <- paste(toupper(criteria), "Label", sep = "")
	#label_text <- "HRLabel"
	
	# read in concept groups
	groups <- read.table("data_sources/Location_Concepts_byCui.txt", sep = "\t", header = FALSE, as.is = TRUE)
	phen_groups <- read.table("data_sources/Dictionary_Concepts_byCui.txt", sep = "\t", header = FALSE, as.is = TRUE)

	### first pass: any symptom in one of the target locations counts, excluding history concepts
	# later: more specific symptoms in locations
	
	# replace original column names with indexed group names
	  # group~subject~negation~colcount
	# will throw warning if cui assigned to more than one group in Dictionary_Concepts_byCui.txt
	dat_groups <- dat

	colcount = 0
	# loops through the concepts (columns)
	for (i in names(dat_groups)) {
	  colcount = colcount + 1
	  
	  if (i != "patient_id_fix" & i != label_text & i != "label") {
  		# splits into cui, family, history (on _)
  		split <- strsplit(i, "_")
  		cui_phen <- split[[1]][1]
  		cui_loc <- split[[1]][2]
  		family <- split[[1]][3]
  		history <- split[[1]][4]
  		negation <- split[[1]][5]
  		
  		if (negation == "0") {
  		  negation <- "positive"
  		} else {
  		  negation <- "negative"
  		}

  		# lookup concept grouping
  		group_index <- grep(cui_loc, groups$V2)

  		
  		if ((length(group_index) > 0) & family != "other" & family != "family" & history == "false") {
  		  new_col_name <- NA
  		  group <- groups$V1[group_index]
  		  
  		  if (negation == "positive") {
			    new_col_name <- paste(group, "patient", negation, colcount, sep = "~")
			    
			    
			  } else if (family == "patient" & negation == "negative") {
			    new_col_name <- paste(group, "patient", negation, colcount, sep = "~")
			  
			  } else {
			#print(paste("problem:", cui))
          new_col_name <- paste("discard", "discard", "discard", colcount, sep = "~")
		    }
		  
		  } else {
		  #print(paste(i, "not in group"))
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

	 # get rid of discard column
	discard_cols <- grep("discard", names(dat_grouped))
	dat_grouped <- dat_grouped[,-discard_cols]
	
	dat_grouped[,c(3:ncol(dat_grouped))] <- log(dat_grouped[,c(3:ncol(dat_grouped))] + 1)

	remove(dat_groups)
	remove(dat)
	write.csv(dat_grouped, "AD_locationData_patientAggregated_withLabels_grouped.csv", row.names = F)
	
	return(dat_grouped)
}




