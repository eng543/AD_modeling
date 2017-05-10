library(dplyr)

setwd("~/AD_lasso")
options(digits=11)

meds <- read.table("meds_prep/meds_040717.txt", sep = "\t", as.is = T, header = T)
meds <- as.data.frame(sapply(meds, tolower))
colnames(meds)[1] <- "patient_id"
meds$date <- as.Date(meds$rx_start_dts)

chop <- read.table("meds_prep/chop_meds.txt", sep = "\t", as.is = T)

## just not worrying about duplicates right now
# all that matters is presence or absense of a class of medication
meds <- meds %>% 
  group_by(patient_id, rx_start_dts)

meds$category <- NA
for (i in 1:nrow(meds)) {
  # check if any of the columns match CHOP (something should or shouldn't be in table!)
  choices <- list(meds$pharmctcl_class_nm[i],
             meds$pharmctcl_class_nm_in_src[i],
             meds$generic_nm[i],
             meds$generic_nm_in_src[i],
             meds$trade_nm[i])
  for (desc in choices) {
    for (j in 1:nrow(chop)) {
      if (pmatch(chop$V1[j], desc, nomatch=0) == 1 | pmatch(desc, chop$V1[j], nomatch=0) == 1) { ## can't just be an exact match
        meds$category[i] <- chop$V1[j]
      }
    }
  }
}

# remove those that didn't match (NA)
meds_normed <- meds[complete.cases(meds), ]

# group meds into same categories as NLP
meds_normed$group <- NA

for (i in 1:nrow(meds_normed)) {
  nm <- paste("^", meds_normed$category[i], "$", sep="")
  chop_lookup <- grep(nm, chop$V1)
  grp <- chop$V2[chop_lookup]
  print(grp)
  meds_normed$group[i] <- grp
}

meds_summary <- meds_normed %>%
  group_by(patient_id) %>%
  summarise(topical_corticosteroids = length(group[group == "topical_corticosteroids_cd"]),
            topical_calcineurin_inhibitors = length(group[group == "topical_calcineurin_inhibitors_cd"]),
            emollients = length(group[group == "emollients_cd"]),
            antihistimines = length(group[group == "antihistimines_cd"]),
            oral_corticosteroids = length(group[group == "oral_corticosteroids_cd"]),
            phototherapy = length(group[group == "phototherapy_cd"]),
            other = length(group[group == "other_cd"]))

# make binary
meds_summary$topical_corticosteroids_cd_bin <- ifelse(meds_summary$topical_corticosteroids > 0,
                                                  1,
                                                  0)
meds_summary$topical_calcineurin_inhibitors_cd_bin <- ifelse(meds_summary$topical_calcineurin_inhibitors > 0,
                                                  1,
                                                  0)
meds_summary$emollients_cd_bin <- ifelse(meds_summary$emollients > 0,
                                                  1,
                                                  0)
meds_summary$antihistimines_cd_bin <- ifelse(meds_summary$antihistimines > 0,
                                                  1,
                                                  0)
meds_summary$oral_corticosteroids_cd_bin <- ifelse(meds_summary$oral_corticosteroids > 0,
                                                  1,
                                                  0)
meds_summary$phototherapy_cd_bin <- ifelse(meds_summary$phototherapy > 0,
                                                  1,
                                                  0)
meds_summary$other_cd_bin <- ifelse(meds_summary$other > 0,
                                                  1,
                                                  0)
meds_summary <- meds_summary[, -c(2:8)]

write.csv(meds_summary, "data_sources/meds_coded_summarized.csv", row.names = F)
