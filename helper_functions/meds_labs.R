library(dplyr)

# group medications into single category or not

meds_labs_read = function(data, med_source, lab_source, group) {
  # read in meds, fix patient_id
  #meds <- read.csv("data_sources/meds_coded_summarized.csv", as.is = T)
  meds <- read.csv(med_source, as.is = T)

  meds$patient_id <- as.character(meds$patient_id)
  meds$patient_id_fix <- NA
  for (i in 1:nrow(meds)) {
    if (nchar(strsplit(meds$patient_id[i], "\\.")[[1]][2]) == 7) {
      meds$patient_id_fix[i] <- meds$patient_id[i]
    } else if (nchar(strsplit(meds$patient_id[i], "\\.")[[1]][2]) == 6) {
      meds$patient_id_fix[i] <- paste(meds$patient_id[i], "0", sep = "")
    } else if (nchar(strsplit(meds$patient_id[i], "\\.")[[1]][2]) == 5) {
      meds$patient_id_fix[i] <- paste(meds$patient_id[i], "00", sep = "")
    } else {
      print("problem")
      print(meds$patient_id[i])
    }
  }
  meds <- meds[,-1]
  names(meds)[ncol(meds)] <- "patient_id" 
  
  # read in labs, fix patient ids
  #labs <- read.table("data_sources/labs_031517.txt", sep = "\t", header = TRUE)
  labs <- read.table(lab_source, sep = "\t", header = TRUE)
  colnames(labs)[1] <- "patient_id"
  
  labs$patient_id <- as.character(labs$patient_id)
  labs$patient_id_fix <- NA
  for (i in 1:nrow(labs)) {
    if (nchar(strsplit(labs$patient_id[i], "\\.")[[1]][2]) == 7) {
      labs$patient_id_fix[i] <- labs$patient_id[i]
    } else if (nchar(strsplit(labs$patient_id[i], "\\.")[[1]][2]) == 6) {
      labs$patient_id_fix[i] <- paste(labs$patient_id[i], "0", sep = "")
    } else if (nchar(strsplit(labs$patient_id[i], "\\.")[[1]][2]) == 5) {
      labs$patient_id_fix[i] <- paste(labs$patient_id[i], "00", sep = "")
    } else {
      print("problem")
      print(labs$patient_id[i])
    }
  }
  labs <- labs[,-1]
  names(labs)[ncol(labs)] <- "patient_id" 
  
  # not sure about the categories.. so just leave them grouped no matter what
  labs_summary <- labs %>%
    group_by(patient_id) %>%
    summarise(lab = 1)
  
  if (group) {
    meds$med_cd_bin <- 1
    meds <- subset(meds, select = c(patient_id, med_cd_bin))
  }
  
  med_pat <- unique(meds$patient_id)
  all_pat <- unique(dat_grouped_codes$patient_id)
  
  # merge with dataset
  # if no code for patient, make it 0, otherwise 1
  data_meds <- merge(dat_grouped_codes, meds, all.x = TRUE)
  data_meds[is.na(data_meds)] <- 0
  
  data_meds_labs <- merge(data_meds, labs_summary, all.x = TRUE)
  data_meds_labs[is.na(data_meds_labs)] <- 0
  
  remove(meds)
  remove(labs)
  remove(labs_summary)
  remove(data_meds)
  
  return(data_meds_labs)
  
}