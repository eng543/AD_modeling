library(dplyr)

# natural log transformed variable for each diagnosis code
# normalized (/total count icd9) variable for each(?) diagnosis code (or just the AD & CD/eczema ones)

#ad_diag <- read.delim("ad_specific_diagnosis_codes_deduped.txt", header = T)
#colnames(ad_diag)[1] <- "patient_id"

#ad_diag_agg <- ad_diag %>%
#  group_by(patient_id) %>%
#  summarise(ad_count = sum(icd9_cd == "691.8"),
#            cd_count = sum(icd9_cd == "692.9"))

# labels
ukwp_labels <- read.table("UKWP_Labels.txt", sep = "\t", header = T)
colnames(ukwp_labels)[1] <- "patient_id"
hr_labels <- read.table("HR_Labels.txt", sep = "\t", header = T)
colnames(hr_labels)[1] <- "patient_id"

# should also dedupe the other diagnosis codes?
all_diag <- read.delim("all_diagnosis_codes_ids_deduped.txt", header = T)
colnames(all_diag)[1] <- "patient_id"

# get by patient code counts
all_diag_agg <- all_diag %>%
  group_by(patient_id) %>%
  summarise(code_count = length(vocabulary_value),
            ad9_count = sum(vocabulary_value == "691.8"),
            cd9_count = sum(vocabulary_value == "692.9"),
            cd10_count = sum(vocabulary_value == "L30.9"),
            ad10_count = length(grep("L20", vocabulary_value)),
            asthma_count1 = length(grep("493", vocabulary_value)),
            asthma_count2 = length(grep("J45", vocabulary_value)),
            hayfever_count1 = length(grep("477", vocabulary_value)),
            hayfever_count2 = length(grep("J30", vocabulary_value)),
            food_count1 = length(grep("955.6", vocabulary_value)),
            food_count2 = sum(vocabulary_value == "955.7"),
            food_count3 = sum(vocabulary_value == "995.3"),
            food_count4 = sum(vocabulary_value == "693.1"))

# (co)occurrence of ICD-9 codes
cd_only_multiple <- all_diag_agg[all_diag_agg$ad9_count == 0 & all_diag_agg$cd9_count >= 1,]
cd_only_single <- all_diag_agg[all_diag_agg$ad9_count == 0 & all_diag_agg$cd9_count == 1,]

ad_only_multiple <- all_diag_agg[all_diag_agg$ad9_count >= 1 & all_diag_agg$cd9_count == 0,]
ad_only_single <- all_diag_agg[all_diag_agg$ad9_count == 1 & all_diag_agg$cd9_count == 0,]

ad_single_cd_single <- all_diag_agg[all_diag_agg$ad9_count == 1 & all_diag_agg$cd9_count == 1,]
ad_single_cd_multiple <- all_diag_agg[all_diag_agg$ad9_count == 1 & all_diag_agg$cd9_count >= 1,]

ad_multiple_cd_single <- all_diag_agg[all_diag_agg$ad9_count >= 1 & all_diag_agg$cd9_count == 1,]
ad_multiple_cd_multiple <- all_diag_agg[all_diag_agg$ad9_count >= 1 & all_diag_agg$cd9_count >= 1,]

## first split: single occurrence of 691.8
  # 31.3% PPV definite daignosis (both HR and UKWP)
first_pos <- rbind(ad_only_single, ad_single_cd_multiple)
first_pos <- merge(first_pos, hr_labels)
first_pos <- merge(first_pos, ukwp_labels)

first_pos$ukwp_result <- ifelse(first_pos$UKWPLabel == "UKWPDefinite",
                           "tp",
                           "fp")

first_pos$hr_result <- ifelse(first_pos$HRLabel == "HRDefinite",
                                "tp",
                                "fp")

first_pos_patients <- unique(first_pos$patient_id)

first_neg <- all_diag_agg[!all_diag_agg$patient_id %in% first_pos_patients,]
first_neg <- merge(first_neg, hr_labels)
first_neg <- merge(first_neg, ukwp_labels)

first_neg$ukwp_result <- ifelse(first_neg$UKWPLabel == "UKWPDefinite",
                           "fn",
                           "tn")

first_neg$hr_result <- ifelse(first_neg$HRLabel == "HRDefinite",
                                "fn",
                                "tn")

first <- rbind(first_pos, first_neg)

first_result <- first %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
first_result$desc <- "1 occurrence of 691.8 (definite only)"

results <- data.frame()
results <- rbind(results, first_result)

#        ukwp_ppv     ukwp_sens        hr_ppv hr_sens
#1 0.17669172932 0.28658536585 0.14661654135    0.26

# combine probable and definite
first_pos$ukwp_result <- ifelse(first_pos$UKWPLabel == "UKWPDefinite" | first_pos$UKWPLabel == "UKWPProbable",
                                "tp",
                                "fp")

first_pos$hr_result <- ifelse(first_pos$HRLabel == "HRDefinite" | first_pos$HRLabel == "HRProbable",
                              "tp",
                              "fp")

first_neg$ukwp_result <- ifelse(first_neg$UKWPLabel == "UKWPDefinite" | first_neg$UKWPLabel == "UKWPProbable",
                                "fn",
                                "tn")

first_neg$hr_result <- ifelse(first_neg$HRLabel == "HRDefinite" | first_neg$HRLabel == "UKWPProbable",
                              "fn",
                              "tn")

first <- rbind(first_pos, first_neg)

first_result <- first %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
first_result$desc <- "1 occurrence of 691.8 (definite and probable)"

results <- rbind(results, first_result)

#       ukwp_ppv     ukwp_sens        hr_ppv       hr_sens
#1 0.28947368421 0.33920704846 0.37218045113 0.47142857143

## second split: multiple occurrences of 691.8
  # PPV = 30.8(UKWP) / 29.9(HR)
second_pos <- rbind(ad_only_multiple, ad_multiple_cd_multiple)
second_pos <- merge(second_pos, hr_labels)
second_pos <- merge(second_pos, ukwp_labels)

second_pos$ukwp_result <- ifelse(second_pos$UKWPLabel == "UKWPDefinite",
                                "tp",
                                "fp")

second_pos$hr_result <- ifelse(second_pos$HRLabel == "HRDefinite",
                              "tp",
                              "fp")

second_pos_patients <- unique(second_pos$patient_id)

second_neg <- all_diag_agg[!all_diag_agg$patient_id %in% second_pos_patients,]
second_neg <- merge(second_neg, hr_labels)
second_neg <- merge(second_neg, ukwp_labels)

second_neg$ukwp_result <- ifelse(second_neg$UKWPLabel == "UKWPDefinite",
                                "fn",
                                "tn")

second_neg$hr_result <- ifelse(second_neg$HRLabel == "HRDefinite",
                              "fn",
                              "tn")

second <- rbind(second_pos, second_neg)

second_result <- second %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
second_result$desc <- "multiple occurrences of 691.8 (definite only)"

results <- rbind(results, second_result)

#        ukwp_ppv     ukwp_sens        hr_ppv       hr_sens
#1 0.31285988484 0.99390243902 0.28406909789 0.98666666667

# combine probable and definite
second_pos$ukwp_result <- ifelse(second_pos$UKWPLabel == "UKWPDefinite" | second_pos$UKWPLabel == "UKWPProbable",
                                "tp",
                                "fp")

second_pos$hr_result <- ifelse(second_pos$HRLabel == "HRDefinite" | second_pos$HRLabel == "HRProbable",
                              "tp",
                              "fp")

second_neg$ukwp_result <- ifelse(second_neg$UKWPLabel == "UKWPDefinite" | second_neg$UKWPLabel == "UKWPProbable",
                                "fn",
                                "tn")

second_neg$hr_result <- ifelse(second_neg$HRLabel == "HRDefinite" | second_neg$HRLabel == "UKWPProbable",
                              "fn",
                              "tn")

second <- rbind(second_pos, second_neg)

second_result <- second %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
second_result$desc <- "multiple occurrences of 691.8 (definite and probable)"

results <- rbind(results, second_result)


# ukwp_ppv     ukwp_sens        hr_ppv      hr_sens
#1 0.42802303263 0.98237885463 0.50863723608 0.9925093633

## third split: multiple occurrences of 691.8 with asthma/hay fever
# PPV = 47.1(UKWP) / 46.6(HR)
ad_only_multiple_co1 <- all_diag_agg[all_diag_agg$ad9_count >= 1 & all_diag_agg$cd9_count == 0 &
                                       ((all_diag_agg$asthma_count1 >= 1 | all_diag_agg$asthma_count2 >= 1) |
                                          (all_diag_agg$hayfever_count1 >= 1 | all_diag_agg$asthma_count2 >= 1)),]

ad_multiple_cd_multiple_co1 <- all_diag_agg[all_diag_agg$ad9_count >= 1 & all_diag_agg$cd9_count >= 1 &
                                              ((all_diag_agg$asthma_count1 >= 1 | all_diag_agg$asthma_count2 >= 1) |
                                                 (all_diag_agg$hayfever_count1 >= 1 | all_diag_agg$asthma_count2 >= 1)),]

third_pos <- rbind(ad_only_multiple_co1, ad_multiple_cd_multiple_co1)
third_pos <- merge(third_pos, hr_labels)
third_pos <- merge(third_pos, ukwp_labels)

third_pos$ukwp_result <- ifelse(third_pos$UKWPLabel == "UKWPDefinite",
                                 "tp",
                                 "fp")

third_pos$hr_result <- ifelse(third_pos$HRLabel == "HRDefinite",
                               "tp",
                               "fp")

third_pos_patients <- unique(third_pos$patient_id)

third_neg <- all_diag_agg[!all_diag_agg$patient_id %in% third_pos_patients,]
third_neg <- merge(third_neg, hr_labels)
third_neg <- merge(third_neg, ukwp_labels)

third_neg$ukwp_result <- ifelse(third_neg$UKWPLabel == "UKWPDefinite",
                                 "fn",
                                 "tn")

third_neg$hr_result <- ifelse(third_neg$HRLabel == "HRDefinite",
                               "fn",
                               "tn")

third <- rbind(third_pos, third_neg)

third_result <- third %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
third_result$desc <- "multiple occurrences of 691.8 with asthma/hay fever"

results <- rbind(results, third_result)

#ukwp_ppv     ukwp_sens        hr_ppv       hr_sens
#1 0.36073059361 0.48170731707 0.36529680365 0.53333333333

# combine probable and definite
third_pos$ukwp_result <- ifelse(third_pos$UKWPLabel == "UKWPDefinite" | third_pos$UKWPLabel == "UKWPProbable",
                                 "tp",
                                 "fp")

third_pos$hr_result <- ifelse(third_pos$HRLabel == "HRDefinite" | third_pos$HRLabel == "HRProbable",
                               "tp",
                               "fp")

third_neg$ukwp_result <- ifelse(third_neg$UKWPLabel == "UKWPDefinite" | third_neg$UKWPLabel == "UKWPProbable",
                                 "fn",
                                 "tn")

third_neg$hr_result <- ifelse(third_neg$HRLabel == "HRDefinite" | third_neg$HRLabel == "UKWPProbable",
                               "fn",
                               "tn")

third <- rbind(third_pos, third_neg)

third_result <- third %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
third_result$desc <- "multiple occurrences of 691.8 with asthma/hay fever (definite and probable)"

results <- rbind(results, third_result)

#ukwp_ppv     ukwp_sens        hr_ppv       hr_sens
#1 0.47488584475 0.45814977974 0.57534246575 0.64285714286

## fourth split: multiple occurrences of 691.8 with (asthma or hay fever) and food allergy
# PPV = 65.8(UKWP) / 76.3(HR)
ad_only_multiple_co2 <- all_diag_agg[all_diag_agg$ad9_count >= 1 & all_diag_agg$cd9_count == 0 &
                                       ((all_diag_agg$asthma_count1 >= 1 | all_diag_agg$asthma_count2 >= 1) |
                                          (all_diag_agg$hayfever_count1 >= 1 | all_diag_agg$asthma_count2 >= 1)) &
                                       (all_diag_agg$food_count1 >= 1 | all_diag_agg$food_count2 >= 1 | all_diag_agg$food_count3 >= 1 | all_diag_agg$food_count4 >= 1),]

ad_multiple_cd_multiple_co2 <- all_diag_agg[all_diag_agg$ad9_count >= 1 & all_diag_agg$cd9_count >= 1 &
                                              ((all_diag_agg$asthma_count1 >= 1 | all_diag_agg$asthma_count2 >= 1) |
                                                 (all_diag_agg$hayfever_count1 >= 1 | all_diag_agg$asthma_count2 >= 1)) &
                                              (all_diag_agg$food_count1 >= 1 | all_diag_agg$food_count2 >= 1 | all_diag_agg$food_count3 >= 1 | all_diag_agg$food_count4 >= 1),]

fourth_pos <- rbind(ad_only_multiple_co2, ad_multiple_cd_multiple_co2)
fourth_pos <- merge(fourth_pos, hr_labels)
fourth_pos <- merge(fourth_pos, ukwp_labels)

fourth_pos$ukwp_result <- ifelse(fourth_pos$UKWPLabel == "UKWPDefinite",
                                "tp",
                                "fp")

fourth_pos$hr_result <- ifelse(fourth_pos$HRLabel == "HRDefinite",
                              "tp",
                              "fp")

fourth_pos_patients <- unique(fourth_pos$patient_id)

fourth_neg <- all_diag_agg[!all_diag_agg$patient_id %in% fourth_pos_patients,]
fourth_neg <- merge(fourth_neg, hr_labels)
fourth_neg <- merge(fourth_neg, ukwp_labels)

fourth_neg$ukwp_result <- ifelse(fourth_neg$UKWPLabel == "UKWPDefinite",
                                "fn",
                                "tn")

fourth_neg$hr_result <- ifelse(fourth_neg$HRLabel == "HRDefinite",
                              "fn",
                              "tn")

fourth <- rbind(fourth_pos, fourth_neg)

fourth_result <- fourth %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
fourth_result$desc <- "multiple occurrences of 691.8 with (asthma or hay fever) and food allergy (definite only)"

results <- rbind(results, fourth_result)

#ukwp_ppv      ukwp_sens        hr_ppv        hr_sens
#1 0.53846153846 0.085365853659 0.53846153846 0.093333333333

# combine probable and definite
fourth_pos$ukwp_result <- ifelse(fourth_pos$UKWPLabel == "UKWPDefinite" | fourth_pos$UKWPLabel == "UKWPProbable",
                                "tp",
                                "fp")

fourth_pos$hr_result <- ifelse(fourth_pos$HRLabel == "HRDefinite" | fourth_pos$HRLabel == "HRProbable",
                              "tp",
                              "fp")

fourth_neg$ukwp_result <- ifelse(fourth_neg$UKWPLabel == "UKWPDefinite" | fourth_neg$UKWPLabel == "UKWPProbable",
                                "fn",
                                "tn")

fourth_neg$hr_result <- ifelse(fourth_neg$HRLabel == "HRDefinite" | fourth_neg$HRLabel == "UKWPProbable",
                              "fn",
                              "tn")

fourth <- rbind(fourth_pos, fourth_neg)

fourth_result <- fourth %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
fourth_result$desc <- "multiple occurrences of 691.8 with (asthma or hay fever) and food allergy (definite and probable)"

results <- rbind(results, fourth_result)

#ukwp_ppv      ukwp_sens        hr_ppv       hr_sens
#1 0.57692307692 0.066079295154 0.88461538462 0.14465408805

## fifth split: single occurrence of 692.9
# PPV = 31(UKWP) / 26.2(HR)
fifth_pos <- rbind(cd_only_single, ad_multiple_cd_single)
fifth_pos <- merge(fifth_pos, hr_labels)
fifth_pos <- merge(fifth_pos, ukwp_labels)

fifth_pos$ukwp_result <- ifelse(fifth_pos$UKWPLabel == "UKWPDefinite",
                                 "tp",
                                 "fp")

fifth_pos$hr_result <- ifelse(fifth_pos$HRLabel == "HRDefinite",
                               "tp",
                               "fp")

fifth_pos_patients <- unique(fifth_pos$patient_id)

fifth_neg <- all_diag_agg[!all_diag_agg$patient_id %in% fifth_pos_patients,]
fifth_neg <- merge(fifth_neg, hr_labels)
fifth_neg <- merge(fifth_neg, ukwp_labels)

fifth_neg$ukwp_result <- ifelse(fifth_neg$UKWPLabel == "UKWPDefinite",
                                 "fn",
                                 "tn")

fifth_neg$hr_result <- ifelse(fifth_neg$HRLabel == "HRDefinite",
                               "fn",
                               "tn")

fifth <- rbind(fifth_pos, fifth_neg)

fifth_result <- fifth %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
fifth_result$desc <- "single occurrence of 692.9 (definite only)"

results <- rbind(results, fifth_result)

#ukwp_ppv     ukwp_sens        hr_ppv hr_sens
#1 0.24778761062 0.17073170732 0.21238938053    0.16

# combine probable and definite
fifth_pos$ukwp_result <- ifelse(fifth_pos$UKWPLabel == "UKWPDefinite" | fifth_pos$UKWPLabel == "UKWPProbable",
                                 "tp",
                                 "fp")

fifth_pos$hr_result <- ifelse(fifth_pos$HRLabel == "HRDefinite" | fifth_pos$HRLabel == "HRProbable",
                               "tp",
                               "fp")

fifth_neg$ukwp_result <- ifelse(fifth_neg$UKWPLabel == "UKWPDefinite" | fifth_neg$UKWPLabel == "UKWPProbable",
                                 "fn",
                                 "tn")

fifth_neg$hr_result <- ifelse(fifth_neg$HRLabel == "HRDefinite" | fifth_neg$HRLabel == "UKWPProbable",
                               "fn",
                               "tn")

fifth <- rbind(fifth_pos, fifth_neg)

fifth_result <- fifth %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
fifth_result$desc <- "single occurrence of 692.9 (definite and probable)"

results <- rbind(results, fifth_result)

#ukwp_ppv     ukwp_sens        hr_ppv       hr_sens
#1 0.38053097345 0.18942731278 0.41592920354 0.27167630058

## sixth split: multiple occurrences of 692.9
# PPV = 32.2(UKWP) / 33.7(HR)
sixth_pos <- rbind(cd_only_multiple, ad_multiple_cd_multiple)
sixth_pos <- merge(sixth_pos, hr_labels)
sixth_pos <- merge(sixth_pos, ukwp_labels)

sixth_pos$ukwp_result <- ifelse(sixth_pos$UKWPLabel == "UKWPDefinite",
                                "tp",
                                "fp")

sixth_pos$hr_result <- ifelse(sixth_pos$HRLabel == "HRDefinite",
                              "tp",
                              "fp")

sixth_pos_patients <- unique(sixth_pos$patient_id)

sixth_neg <- all_diag_agg[!all_diag_agg$patient_id %in% sixth_pos_patients,]
sixth_neg <- merge(sixth_neg, hr_labels)
sixth_neg <- merge(sixth_neg, ukwp_labels)

sixth_neg$ukwp_result <- ifelse(sixth_neg$UKWPLabel == "UKWPDefinite",
                                "fn",
                                "tn")

sixth_neg$hr_result <- ifelse(sixth_neg$HRLabel == "HRDefinite",
                              "fn",
                              "tn")

sixth <- rbind(sixth_pos, sixth_neg)

sixth_result <- sixth %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
sixth_result$desc <- "multiple occurrences of 692.9 (definite only)"

results <- rbind(results, sixth_result)

#ukwp_ppv     ukwp_sens        hr_ppv hr_sens
#1 0.32298136646 0.63414634146 0.30745341615    0.66

# combine probable and definite
sixth_pos$ukwp_result <- ifelse(sixth_pos$UKWPLabel == "UKWPDefinite" | sixth_pos$UKWPLabel == "UKWPProbable",
                                "tp",
                                "fp")

sixth_pos$hr_result <- ifelse(sixth_pos$HRLabel == "HRDefinite" | sixth_pos$HRLabel == "HRProbable",
                              "tp",
                              "fp")

sixth_neg$ukwp_result <- ifelse(sixth_neg$UKWPLabel == "UKWPDefinite" | sixth_neg$UKWPLabel == "UKWPProbable",
                                "fn",
                                "tn")

sixth_neg$hr_result <- ifelse(sixth_neg$HRLabel == "HRDefinite" | sixth_neg$HRLabel == "UKWPProbable",
                              "fn",
                              "tn")

sixth <- rbind(sixth_pos, sixth_neg)

sixth_result <- sixth %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
sixth_result$desc <- "multiple occurrences of 692.9 (definite and probable)"

results <- rbind(results, sixth_result)

#ukwp_ppv     ukwp_sens        hr_ppv       hr_sens
#1 0.44409937888 0.62995594714 0.54968944099 0.77631578947

## seventh split: multiple occurrences of 692.9 with (asthma or hay fever)
# PPV = 54.9(UKWP) / 59.8(HR)
cd_only_multiple_co1 <- all_diag_agg[all_diag_agg$ad9_count == 0 & all_diag_agg$cd9_count >= 1 &
                                       ((all_diag_agg$asthma_count1 >= 1 | all_diag_agg$asthma_count2 >= 1) |
                                          (all_diag_agg$hayfever_count1 >= 1 | all_diag_agg$asthma_count2 >= 1)),]

cd_multiple_cd_multiple_co1 <- all_diag_agg[all_diag_agg$ad9_count >= 1 & all_diag_agg$cd9_count >= 1 &
                                              ((all_diag_agg$asthma_count1 >= 1 | all_diag_agg$asthma_count2 >= 1) |
                                                 (all_diag_agg$hayfever_count1 >= 1 | all_diag_agg$asthma_count2 >= 1)),]

seventh_pos <- rbind(cd_only_multiple_co1, ad_multiple_cd_multiple_co1)
seventh_pos <- merge(seventh_pos, hr_labels)
seventh_pos <- merge(seventh_pos, ukwp_labels)

seventh_pos$ukwp_result <- ifelse(seventh_pos$UKWPLabel == "UKWPDefinite",
                                "tp",
                                "fp")

seventh_pos$hr_result <- ifelse(seventh_pos$HRLabel == "HRDefinite",
                              "tp",
                              "fp")

seventh_pos_patients <- unique(seventh_pos$patient_id)

seventh_neg <- all_diag_agg[!all_diag_agg$patient_id %in% seventh_pos_patients,]
seventh_neg <- merge(seventh_neg, hr_labels)
seventh_neg <- merge(seventh_neg, ukwp_labels)

seventh_neg$ukwp_result <- ifelse(seventh_neg$UKWPLabel == "UKWPDefinite",
                                "fn",
                                "tn")

seventh_neg$hr_result <- ifelse(seventh_neg$HRLabel == "HRDefinite",
                              "fn",
                              "tn")

seventh <- rbind(seventh_pos, seventh_neg)

seventh_result <- seventh %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
seventh_result$desc <- "multiple occurrences of 692.9 with (asthma or hay fever) (definite only)"

results <- rbind(results, seventh_result)

#ukwp_ppv     ukwp_sens        hr_ppv hr_sens
#1 0.4064516129 0.38414634146 0.42580645161    0.44

# combine probable and definite
seventh_pos$ukwp_result <- ifelse(seventh_pos$UKWPLabel == "UKWPDefinite" | seventh_pos$UKWPLabel == "UKWPProbable",
                                "tp",
                                "fp")

seventh_pos$hr_result <- ifelse(seventh_pos$HRLabel == "HRDefinite" | seventh_pos$HRLabel == "HRProbable",
                              "tp",
                              "fp")

seventh_neg$ukwp_result <- ifelse(seventh_neg$UKWPLabel == "UKWPDefinite" | seventh_neg$UKWPLabel == "UKWPProbable",
                                "fn",
                                "tn")

seventh_neg$hr_result <- ifelse(seventh_neg$HRLabel == "HRDefinite" | seventh_neg$HRLabel == "UKWPProbable",
                              "fn",
                              "tn")

seventh <- rbind(seventh_pos, seventh_neg)

seventh_result <- seventh %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
seventh_result$desc <- "multiple occurrences of 692.9 with (asthma or hay fever) (definite or probable)"

results <- rbind(results, seventh_result)

#ukwp_ppv     ukwp_sens        hr_ppv       hr_sens
#1 0.55483870968 0.37885462555 0.66451612903 0.55080213904

## eighth split: multiple occurrences of 692.9 with (asthma or hay fever) and food allergy
# PPV = 66.7(UKWP) / 81(HR)
cd_only_multiple_co2 <- all_diag_agg[all_diag_agg$ad9_count == 0 & all_diag_agg$cd9_count >= 1 &
                                       ((all_diag_agg$asthma_count1 >= 1 | all_diag_agg$asthma_count2 >= 1) |
                                          (all_diag_agg$hayfever_count1 >= 1 | all_diag_agg$asthma_count2 >= 1)) &
                                       (all_diag_agg$food_count1 >= 1 | all_diag_agg$food_count2 >= 1 | all_diag_agg$food_count3 >= 1 | all_diag_agg$food_count4 >= 1),]

cd_multiple_cd_multiple_co2 <- all_diag_agg[all_diag_agg$ad9_count >= 1 & all_diag_agg$cd9_count >= 1 &
                                              ((all_diag_agg$asthma_count1 >= 1 | all_diag_agg$asthma_count2 >= 1) |
                                                 (all_diag_agg$hayfever_count1 >= 1 | all_diag_agg$asthma_count2 >= 1)) &
                                              (all_diag_agg$food_count1 >= 1 | all_diag_agg$food_count2 >= 1 | all_diag_agg$food_count3 >= 1 | all_diag_agg$food_count4 >= 1),]

eighth_pos <- rbind(cd_only_multiple_co2, ad_multiple_cd_multiple_co2)
eighth_pos <- merge(eighth_pos, hr_labels)
eighth_pos <- merge(eighth_pos, ukwp_labels)

eighth_pos$ukwp_result <- ifelse(eighth_pos$UKWPLabel == "UKWPDefinite",
                                  "tp",
                                  "fp")

eighth_pos$hr_result <- ifelse(eighth_pos$HRLabel == "HRDefinite",
                                "tp",
                                "fp")

eighth_pos_patients <- unique(eighth_pos$patient_id)

eighth_neg <- all_diag_agg[!all_diag_agg$patient_id %in% eighth_pos_patients,]
eighth_neg <- merge(eighth_neg, hr_labels)
eighth_neg <- merge(eighth_neg, ukwp_labels)

eighth_neg$ukwp_result <- ifelse(eighth_neg$UKWPLabel == "UKWPDefinite",
                                  "fn",
                                  "tn")

eighth_neg$hr_result <- ifelse(eighth_neg$HRLabel == "HRDefinite",
                                "fn",
                                "tn")

eighth <- rbind(eighth_pos, eighth_neg)

eighth_result <- eighth %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
eighth_result$desc <- "multiple occurrences of 692.9 with (asthma or hay fever) and food allergy (definite only)"

results <- rbind(results, eighth_result)

#ukwp_ppv      ukwp_sens hr_ppv        hr_sens
#1     0.52 0.079268292683   0.52 0.086666666667

# combine probable and definite
eighth_pos$ukwp_result <- ifelse(eighth_pos$UKWPLabel == "UKWPDefinite" | eighth_pos$UKWPLabel == "UKWPProbable",
                                  "tp",
                                  "fp")

eighth_pos$hr_result <- ifelse(eighth_pos$HRLabel == "HRDefinite" | eighth_pos$HRLabel == "HRProbable",
                                "tp",
                                "fp")

eighth_neg$ukwp_result <- ifelse(eighth_neg$UKWPLabel == "UKWPDefinite" | eighth_neg$UKWPLabel == "UKWPProbable",
                                  "fn",
                                  "tn")

eighth_neg$hr_result <- ifelse(eighth_neg$HRLabel == "HRDefinite" | eighth_neg$HRLabel == "UKWPProbable",
                                "fn",
                                "tn")

eighth <- rbind(eighth_pos, eighth_neg)

eighth_result <- eighth %>%
  summarise(ukwp_ppv = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fp")),
            ukwp_sens = sum(ukwp_result == "tp")/(sum(ukwp_result == "tp")+sum(ukwp_result == "fn")),
            hr_ppv = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fp")),
            hr_sens = sum(hr_result == "tp")/(sum(hr_result == "tp")+sum(hr_result == "fn")))
eighth_result$desc <- "multiple occurrences of 692.9 with (asthma or hay fever) and food allergy (definite and probable)"

results <- rbind(results, eighth_result)

#   ukwp_ppv      ukwp_sens hr_ppv       hr_sens
#1     0.56 0.061674008811   0.88 0.13836477987

write.csv(results, "codeAlgorithm_replication.csv", row.names = F)
