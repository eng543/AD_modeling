library(dplyr)

# group medications into single category or not

meds_labs_read = function(data, med_source, lab_source, group) {
  meds <- read.csv(med_source, as.is = T)

  labs <- read.table(lab_source, sep = "\t", header = TRUE)
  
  # not sure about the categories.. so just leave them grouped no matter what
  colnames(labs)[1] <- "patient_id"
  labs_summary <- labs %>%
    group_by(patient_id) %>%
    summarise(lab = 1)
  
  if (group) {
    meds$med_cd_bin <- 1
    meds <- subset(meds, select = c(patient_id, med_cd_bin))
  }
  
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