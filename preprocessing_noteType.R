library(dplyr)

setwd("~/AD_lasso")
options(digits=11)

dat <- read.csv("output_032717_allNotes.csv", as.is = T)

type <- read.table("all_note_types.txt", sep = "\t", header = F, as.is = T)
derm <- type[type$V2 == "derm",]
#allergy <- type[type$V2 == "allergy",]
#other <- type[type$V2 == "other",]
other <- type[type$V2 != "derm",]

derm_dat <- dat[dat$note_id %in% derm$V1,]
#allergy_dat <- dat[dat$note_id %in% allergy$V1,]
other_dat <- dat[dat$note_id %in% other$V1,]

# add note type tag to features
colnames(derm_dat)[2:ncol(derm_dat)] <- paste("derm", colnames(derm_dat[2:ncol(derm_dat)]), sep = "_")
derm_cols <- colnames(derm_dat)[2:ncol(derm_dat)]
colnames(other_dat)[2:ncol(other_dat)] <- paste("other", colnames(other_dat[2:ncol(other_dat)]), sep = "_")
other_cols <- colnames(other_dat)[2:ncol(other_dat)]

derm_dat[other_cols] <- 0
other_dat[derm_cols] <- 0

# recombine datasets
dat_tag <- rbind(derm_dat, other_dat)

### aggregate at patient level

# get patient id separate from note id
#notes$patient_id <- strsplit(notes$note_id, "_")[[1]][1]
  # faster way if don't actually need to use unique_patients for anything anymore
  # doesn't work, just does first pateitn

patients = c()
dat_tag$patient_id <- dat_tag$note_id
for(i in 1:nrow(dat_tag)) {
  #split note_id on underscore to get patient_id [1] and note_id [2]
  split = strsplit(dat_tag[i, 1], "_")
  # take patient part of split
  patient = split[[1]][1]
  # add new patient id to data frame
  dat_tag[i, "patient_id"] <- patient
  # add patient id to list of patients
  patients <- c(patients, patient)
}
# get set of unique patients
unique_patients <- unique(patients)

# remove note id column so can use summarize
dat_tag <- dat_tag[,-1]

# add up counts across notes for each patient
dat_agg <- dat_tag %>%
  group_by(patient_id) %>%
  summarise_each(funs(sum))
  
# add up how many notes have concept mention for each patient
dat_binary <- dat_tag
patient_col <- dat_binary$patient_id
patient_col_index <- grep("patient_id", names(dat_binary))
dat_binary <- dat_binary[, -patient_col_index]

dat_binary[dat_binary > 0] <- 1
dat_binary$patient_id <- patient_col

dat_agg <- dat_binary %>%
  group_by(patient_id) %>%
  summarise_each(funs(sum))

dat_agg_backup <- dat_agg

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

# read in diagnosis label from UKWP Criteria
cat <- read.table("UKWP_labels.txt", sep = "\t", header = TRUE, as.is = T)
#cat <- read.table("HR_labels.txt", sep = "\t", header = TRUE, as.is = T)
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
dat_agg$label <- ifelse(dat_agg$UKWPLabel == "UKWPDefinite" | dat_agg$UKWPLabel == "UKWPProbable",
                    1,
                    0)

#dat_agg$label <- ifelse(dat_agg$HRLabel == "HRDefinite" | dat_agg$HRLabel == "HRProbable",
#                        1,
#                        0)

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


# write to file
# read into lasso script
write.csv(dat_agg, "AD_data_patientAggregated_withLabels.csv", row.names = F)

