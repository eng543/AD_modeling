library(dplyr)
library(ggplot2)

setwd("~/AD_lasso")
options(digits=11)

# number of notes per patient
notes <- read.csv("data_sources/output_032717_allNotes.csv", as.is = T)

# get patient id separate from note id
notes$patient_id <- notes$note_id
for(i in 1:nrow(notes)) {
  #split note_id on underscore to get patient_id [1] and note_id [2]
  split = strsplit(notes[i, 1], "_")
  # take patient part of split
  patient = split[[1]][1]
  # add new patient id to data frame
  notes[i, "patient_id"] <- patient
}

note_sum <- notes %>%
  group_by(patient_id) %>%
  summarize(count = length(patient_id)) %>%
  arrange(count)

ggplot(note_sum, aes(x = count)) + geom_histogram(bins = 100) +
  xlab("Note count") + 
  ylab("Number of patients")

ggsave("patient_info/notes_per_patient_hist.pdf")

ggplot(note_sum, aes(x = reorder(patient_id, count), y = count)) + geom_point() + 
  xlab("Patient") + 
  ylab("Note count")

ggsave("patient_info/notes_per_patient_scatter.pdf")



# number of encounters per patient
nmh_enc <- read.table("patient_info/nmh_encounters_department.txt", as.is = T, sep = "\t", header = T)
nmh_enc <- nmh_enc[, -3]
colnames(nmh_enc) <- c("patient_id", "enc_id", "dept")

# make all patient_id 7 digits
nmh_enc$patient_id_fix <- NA
nmh_enc$patient_id <- as.character(nmh_enc$patient_id)
for (i in 1:nrow(nmh_enc)) {
  if (nchar(strsplit(nmh_enc$patient_id[i], "\\.")[[1]][2]) == 7) {
    nmh_enc$patient_id_fix[i] <- nmh_enc$patient_id[i]
  } else if (nchar(strsplit(nmh_enc$patient_id[i], "\\.")[[1]][2]) == 6) {
    nmh_enc$patient_id_fix[i] <- paste(nmh_enc$patient_id[i], "0", sep = "")
  } else if (nchar(strsplit(nmh_enc$patient_id[i], "\\.")[[1]][2]) == 5) {
    nmh_enc$patient_id_fix[i] <- paste(nmh_enc$patient_id[i], "00", sep = "")
  } else {
    print("problem")
    print(nmh_enc$patient_id[i])
  }
}
nmh_enc <- nmh_enc[,-1]
colnames(nmh_enc)[3] <- "patient_id"

nmff_enc <- read.table("patient_info/nmff_encounters_department.txt", as.is = T, sep = "\t", header = T, quote = "")
nmff_enc <- nmff_enc[, -3]
colnames(nmff_enc) <- c("patient_id", "enc_id", "dept")

# make all patient_id 7 digits
nmff_enc$patient_id_fix <- NA
nmff_enc$patient_id <- as.character(nmff_enc$patient_id)
for (i in 1:nrow(nmff_enc)) {
  if (nchar(strsplit(nmff_enc$patient_id[i], "\\.")[[1]][2]) == 7) {
    nmff_enc$patient_id_fix[i] <- nmff_enc$patient_id[i]
  } else if (nchar(strsplit(nmff_enc$patient_id[i], "\\.")[[1]][2]) == 6) {
    nmff_enc$patient_id_fix[i] <- paste(nmff_enc$patient_id[i], "0", sep = "")
  } else if (nchar(strsplit(nmff_enc$patient_id[i], "\\.")[[1]][2]) == 5) {
    nmff_enc$patient_id_fix[i] <- paste(nmff_enc$patient_id[i], "00", sep = "")
  } else {
    print("problem")
    print(nmff_enc$patient_id[i])
  }
}
nmff_enc <- nmff_enc[,-1]
colnames(nmff_enc)[3] <- "patient_id"

enc <- rbind(nmh_enc, nmff_enc)

enc_sum <- enc %>%
  group_by(patient_id) %>%
  summarize(count = length(patient_id))

enc_mean <- paste("mean:", mean(enc_sum$count))
enc_med <- paste("median:", median(enc_sum$count))

ggplot(enc_sum, aes(x = count)) + geom_histogram(bins = 100) + 
  xlab("Encounter count") + 
  ylab ("Number of patients") + 
  annotate("text", x = 800, y = 75, label = enc_mean) + 
  annotate("text", x = 800, y = 70, label = enc_med)

ggsave("patient_info/encounters_per_patient_hist.pdf")

ggplot(enc_sum, aes(x = reorder(patient_id, count), y = count)) + geom_point() + 
  xlab("Patient") + 
  ylab("Encounter count")

ggsave("patient_info/encounters_per_patient_scatter.pdf")

# remove outlier so easier to see
enc_sum_sub <- enc_sum[enc_sum$count != max(enc_sum$count),]

enc_mean_sub <- paste("mean:", mean(enc_sum_sub$count))
enc_med_sub <- paste("median:", median(enc_sum_sub$count))

ggplot(enc_sum_sub, aes(x = count)) + geom_histogram(bins = 100) + 
  xlab("Encounter count") + 
  ylab ("Number of patients") + 
  annotate("text", x = 250, y = 32, label = enc_mean_sub) + 
  annotate("text", x = 250, y = 30, label = enc_med_sub)

ggsave("patient_info/encounters_per_patient_noOutlier_hist.pdf")

ggplot(enc_sum_sub, aes(x = reorder(patient_id, count), y = count)) + geom_point() + 
  xlab("Patient") + 
  ylab("Encounter count")

ggsave("patient_info/encounters_per_patient_noOutlier_scatter.pdf")

# number of dermatology vs. other encounters per patient
derm_row <- grep("derm", tolower(enc$dept))

enc$derm <- 0

for (i in 1:nrow(enc)) {
  if (i %in% derm_row) {
    enc$derm[i] <- 1
  }
}

enc_sum_derm <- enc %>%
  group_by(patient_id) %>%
  summarize(count = length(patient_id[derm == 1]))

enc_sum_derm$type <- "derm"

enc_sum_other <- enc %>%
  group_by(patient_id) %>%
  summarize(count = length(patient_id[derm == 0]))

enc_sum_other$type <- "other"

enc_sum_2 <- rbind(enc_sum_derm, enc_sum_other)
enc_sum_2$count <- ifelse(enc_sum_2$count > 1200,
                          0,
                          enc_sum_2$count)

ggplot(enc_sum_2, aes(x = count)) + geom_histogram(bins = 100) + 
  facet_grid(.~type) + 
  xlab("Encounter count") + 
  ylab("Number of patients")

ggsave("patient_info/encounters_per_patient_by_department_hist.pdf")

ggplot(enc_sum_2, aes(x = reorder(patient_id, count), y = count)) + geom_point() + 
  facet_grid(.~type) +
  xlab("Patient") + 
  ylab("Encounter count")

ggsave("patient_info/encounters_per_patient_by_department_scatter.pdf")

# combine with outcomes of best performing model
res <- read.table("patient_info/best_results_validation.txt", sep = "\t", as.is = T, header = T)

res$out <- NA
for (i in 1:nrow(res)) {
  if (res$label[i] == 1) {
    if (res$predict[i] == 1) {
      res$out[i] <- "tp"
    } else {
      res$out[i] <- "fn"
    }
  } else if (res$label[i] == 0) {
    if (res$predict[i] == 0) {
      res$out[i] <- "tn"
    } else {
      res$out[i] <- "fp"
    }
  }
}

res$error <- ifelse(res$out == "tp" | res$out == "tn",
                    0,
                    1)

# reality check (match saved results)
#length(res$out[res$out == "tp"]) #yes
#length(res$out[res$out == "tn"]) #yes
#length(res$out[res$out == "fp"]) #yes
#length(res$out[res$out == "fn"]) #yes

# merge results with note data
notes_res <- merge(note_sum, res)

ggplot(notes_res, aes(x = count, fill = as.factor(error))) + geom_histogram() +
  xlab("Note count") + 
  ylab("Number of patients")

ggsave("patient_info/notes_per_patient_classifications_hist.pdf")

ggplot(notes_res, aes(x = reorder(patient_id, count), y = count, color = as.factor(error))) + geom_point() + 
  xlab("Patient") + 
  ylab("Note count")

ggsave("patient_info/notes_per_patient_classifications_scatter.pdf")

# encounters with results
# two patients missing
enc_res <- merge(enc_sum, res)

ggplot(enc_res, aes(x = count, fill = as.factor(error))) + geom_histogram() + 
  xlab("Encounter count") + 
  ylab("Number of patients")

ggsave("patient_info/encounters_per_patient_classifications_hist.pdf")

ggplot(enc_res, aes(x = reorder(patient_id, count), y = count, color = as.factor(error))) + geom_point() + 
  xlab("Patient") + 
  ylab("Encounter count")

ggsave("patient_info/encounters_per_patient_classifications_scatter.pdf")

# dermatology vs. other encounters with results
enc2_res <- merge(enc_sum_2, res)

ggplot(enc2_res, aes(x = count, fill = as.factor(error))) + geom_histogram() + 
  xlab("Encounter count") + 
  ylab("Number of patients") +
  facet_grid(.~type)

ggsave("patient_info/encounters_per_patient_by_department_classifications_hist.pdf")

ggplot(enc2_res, aes(x = reorder(patient_id, count), y = count, color = as.factor(error))) + geom_point() + 
  facet_grid(.~type) + 
  xlab("Patient") + 
  ylab("Encounter count")

ggsave("patient_info/encounters_per_patient_by_department_classifications_scatter.pdf")

