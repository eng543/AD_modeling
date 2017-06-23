setwd("~/AD_modeling")

group_codes <- function(dat, criteria, group_meds_labs, relations) {
	#dat <- read.csv("data_sources/AD_data_patientAggregated_withLabels.csv", as.is = T)
	# dat <- dat_agg
  
	label_text <- paste(toupper(criteria), "Label", sep = "")
	
	# figure out what family hx/patient hx variables we need to deal with
	hist <- read.delim("data_sources/history_concepts.txt", sep = "\t", header = F)

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
  		
  		if (negation == "0") {
  		  negation <- "positive"
  		} else {
  		  negation <- "negative"
  		}

  		# lookup concept grouping
  		group_index <- grep(cui, groups$V2)

  		
  		if ((length(group_index) > 0) & family != "other") {
  		  new_col_name <- NA
  		  group <- groups$V1[group_index]
  		  
  		  # pre-approve history concepts, choosing what code says when conflict with cTAKES assertion label for the subject
  		  if (length(grep(cui, hist$V2)) > 1) {
  		    hx_index <- grep(cui, hist$V2)
  		    
  		    if (length(hx_index) > 1) {
  		      topic <- strsplit(as.character(hist$V1[hx_index][1]), "_")[[1]][2]
  		      hx <- paste(family, "hx", sep = "")
  		      new_col_name <- paste(topic, hx, negation, colcount, sep = "~")
  		    } else {
  		      topic <- strsplit(as.character(hist$V1[hx_index][1]), "_")[[1]][2]
  		      subject <- strsplit(hist$V1[hx_index], "_")[[1]][1]
  		      new_col_name <- paste(topic, subject, negation, colcount, sep = "~")
  		    }
  		  }
  		  
		  # group positive patient (non-history) concepts
		   #else if (family == "patient" & history == "false" & negation == "positive") {
  		  ##### remove history qualifier to get these concepts represented
		     else if (family == "patient" & negation == "positive") {
			# creates new column name for concept: group~column#
		       if (group == "atopy~familyhx" | group == "atopy~patienthx" | group == "eczema~familyhx" | group == "eczema~patienthx") {
		         new_col_name <- paste(group, negation, colcount, sep = "~")
		       } else {
		         new_col_name <- paste(group, "patient", negation, colcount, sep = "~")
		       }

			# group negative patient (non-history) concepts
		  #} else if (family == "patient" & history == "false" & negation == "negative") {
			  ##### remove history qualifer to get these concepts represented
		   } else if (family == "patient" & negation == "negative") {
		     if (group == "atopy~familyhx" | group == "atopy~patienthx" | group == "eczema~familyhx" | group == "eczema~patienthx") {
		       new_col_name <- paste(group, negation, colcount, sep = "~")
		     } else {
		       new_col_name <- paste(group, "patient", negation, colcount, sep = "~")
		     }
		     
		  } else {
			#print(paste("problem:", cui))
			# throw out concepts family concepts not in approved famhx list
		    #print(i)
			  new_col_name <- paste("discard", "discard", "discard", colcount, sep = "~")
		  }
		  
		} else {
		   #discard cuis not in group mapping (e.g., contact dermatitis just removed)
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
	
	remove(groups)
	remove(dat_groups)
	remove(group)
	remove(group_index)
	remove(discard_cols)
	
	# group relations
	if (relations) {
		rel <- read.csv("data_sources/AD_relationData_patientAggregated_withLabels.csv", as.is = T)
		
		# read in concept groups
		rel_groups <- read.table("data_sources/Location_Concepts_byCui.txt", sep = "\t", header = FALSE, as.is = TRUE)
		
		colcount = 0
		# loops through the concepts (columns)
		for (i in names(rel)) {
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
			group_index <- grep(cui_loc, rel_groups$V2)

			
			if ((length(group_index) > 0) & family != "other" & family != "family" & history == "false") {
			  new_col_name <- NA
			  group <- rel_groups$V1[group_index]
			  
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
			colnames(rel)[colcount] <- new_col_name
		  }
		}

		# combine columns (add counts) with the same group name
		rel_grouped <- data.frame("patient_id" = rel$patient_id, "label" = rel$label)

		unique_groups <- c()
		for (i in names(rel)) {
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
		  for (j in names(rel)) {
			# if concept is in current group
			split <- strsplit(j, "~") # split to get rid of the tailing column #
		  if (paste(split[[1]][1], split[[1]][2], split[[1]][3], sep = "~") == i) {
			  # add column to list, add values
			  if (length(cols) == 0) {
				cols <- c(rel[,eval(j)])
			  } else {
				cols <- cols + c(rel[,eval(j)])
			  }
			}
		  }
		  # create new column with group counts aggregated across individual concepts
		  rel_grouped[,eval(i)] <- cols
		  # move on to next group
		}

		 # get rid of discard column
		discard_cols <- grep("discard", names(rel_grouped))
		rel_grouped <- rel_grouped[,-discard_cols]
	}

	# change medication groups and lab values to binary variables
	# log transform those that aren't binary
	med_cols <- grep("topical_steroid|topical_calcineurin_inhibitors|emollients|antihistimines|oral_steroids|
					 phototheraphy|other|oral_antibiotics|oral_tacrolimus|alternative_medicine", names(dat_grouped))
	
	lab_cols <- grep("skinreactivity|ige", names(dat_grouped))
	
	## configure whether both meds and labs should be grouped to single category or not
	if (group_meds_labs) {
	  # separate meds/labs from other types of features
  	meds_labs <- dat_grouped[,c(med_cols, lab_cols)]
  	no_meds_labs <- dat_grouped[, -c(med_cols, lab_cols)]
  	
  	# new columns for groups
  	meds_labs$meds_positive <- 0
  	meds_labs$meds_negated <- 0
  	
  	meds_labs$labs_positive <- 0
  	meds_labs$labs_negated <- 0
  	
  	# new indices in subset
  	med_cols_sub <- grep("topical_steroid|topical_calcineurin_inhibitors|emollients|antihistimines|oral_steroids|
  					 phototheraphy|other|oral_antibiotics|oral_tacrolimus|alternative_medicine", names(meds_labs))
  	
  	lab_cols_sub <- grep("skinreactivity|ige", names(meds_labs))
  	
  	# create groups, aggregate counts
  	# to do: implement with dplyr?
    for (i in 1:ncol(meds_labs)) {
      if (names(meds_labs[i]) != "meds_positive" & names(meds_labs[i]) != "meds_negated" & names(meds_labs[i]) != "labs_positive" & names(meds_labs[i]) != "labs_negated") {
        #print(names(meds_labs[i]))
        
        if (grepl("positive", names(meds_labs[i]))) {
        if (i %in% med_cols_sub) {
            #print(paste("positive med: ", names(meds_labs[i])))
            
            meds_labs$meds_positive <- meds_labs[, i] + meds_labs$meds_positive
          }
          
          else if (i %in% lab_cols_sub) {
            #print(paste("positive lab: ", names(meds_labs[i])))
          
            meds_labs$labs_positive <- meds_labs[, i] + meds_labs$labs_positive
          }
          
          else {
            print("problem!")
          }
        }
        
        else if (grepl("negative", names(meds_labs[i]))) {
          if (i %in% med_cols_sub) {
            #print(paste("negative med: ", names(meds_labs[i])))
            
            meds_labs$meds_negated <- meds_labs[, i] + meds_labs$meds_negated
          }
          
          else if (i %in% lab_cols_sub) {
            #print(paste("negative labs: ", names(meds_labs[i])))
            
            meds_labs$labs_negated <- meds_labs[, i] + meds_labs$labs_negated
          }
          
          else {
            print("problem!")
          }
        }
      }
    }
  	
  	# just take the groups
  	meds_labs_summary <- meds_labs[,c(23:26)]
  	
  	# make groups binary
  	meds_labs_summary$meds_positive <- ifelse(meds_labs_summary$meds_positive > 0,
  	                                          1,
  	                                          0)
  	meds_labs_summary$meds_negated <- ifelse(meds_labs_summary$meds_negated > 0,
  	                                          1,
  	                                          0)
  	meds_labs_summary$labs_positive <- ifelse(meds_labs_summary$labs_positive > 0,
  	                                          1,
  	                                          0)
  	meds_labs_summary$labs_negated <- ifelse(meds_labs_summary$labs_negated > 0,
  	                                          1,
  	                                          0)
  	
  	# log transform everything else
  	# if relations, combine those first
  	if (relations) {
  		rel_groups <- grep("handfoot_dermatitis|flexural_dermatitis", names(no_meds_labs))
  		rel_cols <- no_meds_labs[, rel_groups]
  		no_meds_labs <- no_meds_labs[, -rel_groups]
  		rel_cols <- rel_cols[, order(names(rel_cols))]
  		
  		for (i in names(rel_grouped)) {
  			if (i %in% names(rel_cols) & i != "patient_id" & i != "label") {
  				col <- grep(i, names(rel_cols))
  				rel_cols[, col] <- rel_cols[, col] + rel_grouped[, i]
  			} else if (!(i %in% names(rel_cols)) & i != "patient_id" & i != "label") {
  			  #print(i)
  				rel_cols[, i] <- rel_grouped[, i]
  			}
  		}
  		
  		no_meds_labs <- cbind(no_meds_labs, rel_cols)
  	}
	
  	no_meds_labs_log <- log(no_meds_labs[,-c(1,2)] + 1)
  	no_meds_labs_log <- cbind(no_meds_labs[,c(1,2)], no_meds_labs_log)
  	
  	# re-combine
  	dat_grouped <- cbind(no_meds_labs_log, meds_labs_summary)
  	
	} else if (group_meds_labs == FALSE) {
	  labs <- dat_grouped[,lab_cols]
	  no_labs <- dat_grouped[, -lab_cols]
	  
	  # new columns for groups
    #labs$labs_positive <- 0
	  #labs$labs_negated <- 0
	  
	  # vectors to create new columns for groups
	  labs_positive <- c()
	  labs_negative <- c()
	  
	  # patient IDs
	  pats <- no_labs$patient_id
	  
	  labs_t <- as.data.frame(t(labs))
	  
	  # new indices in subset
	  #lab_cols_sub <- grep("skinreactivity|ige", names(labs))
	  
	  # positive vs. negative
	  labs_neg <- grep("negative", rownames(labs_t))
	  
	  # label labs
	  labs_t$category <- NA
	  
	  for (i in 1:nrow(labs_t)) {
	    if (i %in% labs_neg) {
	      labs_t$category[i] <- "labs_negative"
	    } else {
	      labs_t$category[i] <- "labs_positive"
	    }
	  }
	  
	  labs_sum <- labs_t %>% 
	    group_by(category) %>%
	    summarise_each(funs(sum))
	  
	  labs_summary <- as.data.frame(t(labs_sum))
	  names(labs_summary) <- as.character(unlist(labs_summary[1,]))
	  labs_summary <- labs_summary[-1,]
	  
	  # create groups, aggregate counts
	  # to do: implement with dplyr?
	  # for (i in 1:ncol(labs)) {
	  #   if (names(labs[i]) != "meds_positive" & names(labs[i]) != "meds_negated" & names(labs[i]) != "labs_positive" & names(labs[i]) != "labs_negated") {
	  #     #print(names(labs[i]))
	  #     
	  #     if (grepl("positive", names(labs[i]))) {
	  #       if (i %in% lab_cols_sub) {
	  #         #print(paste("positive lab: ", names(labs[i])))
	  #         
	  #         labs$labs_positive <- labs[, i] + labs$labs_positive
	  #       }
	  #       
	  #       else {
	  #         print("problem!")
	  #       }
	  #     }
	  #     
	  #     else if (grepl("negative", names(labs[i]))) {
	  #       if (i %in% lab_cols_sub) {
	  #         #print(paste("negative labs: ", names(labs[i])))
	  #         
	  #         labs$labs_negated <- labs[, i] + labs$labs_negated
	  #       }
	  #       
	  #       else {
	  #         print("problem!")
	  #       }
	  #     }
	  #   }
	  # }
	  
	  # just take the groups
	  #labs_summary <- labs[,c(5,6)]
	  
	  # add patient names back in
	  #labs_summary$patient_id <- pats
	  
	  # make groups binary
	  labs_summary$labs_positive <- ifelse(as.numeric(as.character(labs_summary$labs_positive)) > 0,
	                                            1,
	                                            0)
	  labs_summary$labs_negative <- ifelse(as.numeric(as.character(labs_summary$labs_negative)) > 0,
	                                           1,
	                                           0)
	  
	  # if relations, combine those
	  if (relations) {
	    rel_groups <- grep("handfoot_dermatitis|flexural_dermatitis", names(no_labs))
	    rel_cols <- no_labs[, rel_groups]
	    no_labs <- no_labs[, -rel_groups]
	    rel_cols <- rel_cols[, order(names(rel_cols))]
	    
	    for (i in names(rel_grouped)) {
	      if (i %in% names(rel_cols) & i != "patient_id" & i != "label") {
	        col <- grep(i, names(rel_cols))
	        rel_cols[, col] <- rel_cols[, col] + rel_grouped[, i]
	      } else if (!(i %in% names(rel_cols)) & i != "patient_id" & i != "label") {
	        #print(i)
	        rel_cols[, i] <- rel_grouped[, i]
	      }
	    }
	    
	    no_labs <- cbind(no_labs, rel_cols)
	  }
	  
	  # re-combine
	  dat_grouped <- cbind(no_labs, labs_summary)
	  
	  # new lab cols
	  new_lab_cols <- grep("labs_positive|labs_negative", names(dat_grouped))
	  new_med_cols <- grep("topical_steroid|topical_calcineurin_inhibitors|emollients|antihistimines|oral_steroids|
					 phototheraphy|other|oral_antibiotics|oral_tacrolimus|alternative_medicine", names(dat_grouped))
	  
	  # leave meds ungrouped, still make binary
	  for (i in 1:ncol(dat_grouped)) {
	    if (i %in% new_med_cols) {
	      #print(names(dat_grouped[i]))
	      for (j in 1:nrow(dat_grouped)) {
	        if (dat_grouped[j, i] > 0) {
	          dat_grouped[j, i] <- 1
	        }
	      }
	    } else {
	      if (i != 1 & i != 2 & !(i %in% new_lab_cols)) {
	        #print(names(dat_grouped[i]))
	        # log transform everything else
	        dat_grouped[, i] <- log(dat_grouped[, i] + 1)
	      }
	    }
	  }
	}

	remove(dat)
	write.csv(dat_grouped, "AD_data_patientAggregated_withLabels_grouped.csv", row.names = F)
	
	return(dat_grouped)
}




