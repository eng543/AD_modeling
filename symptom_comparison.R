library(dplyr)
library(xtable)

setwd("~/AD_modeling")
options(digits=11)
set.seed(999)

source("helper_functions/preprocessing.R")
source("helper_functions/dimensionality_reduction.R")

criteria <- "HR" # HR or UKWP
count_type <- "add" # add or note
group_meds <- F

dat_agg <- preprocess("data_sources/output_042617_defaultTerm.csv", criteria, count_type)
dat_grouped <- group_codes(dat_agg, criteria, group_meds)
remove(dat_agg)

# only positive mentions
dat_pos_index <- grep("~positive$", names(dat_grouped))
dat_pos <- dat_grouped[, dat_pos_index]

# summary statistics for positive pruritus
summary_itch_pos <- dat_pos %>%
  summarize(mean = mean(`pruritus~patient~positive`),
            median = median(`pruritus~patient~positive`),
            sd = sd(`pruritus~patient~positive`),
            min = min(`pruritus~patient~positive`),
            max = max(`pruritus~patient~positive`))

kable(summary_itch_pos)

# positive pruritus correlated with other positive mentions
z <- cor(dat_pos$`pruritus~patient~positive`, dat_pos)
zdf <- as.data.frame(as.table(z))
zdf_sub <- subset(zdf, abs(Freq) > 0.4)
zdf_sort <- zdf_sub %>% arrange(-Freq)

# patient count for each of top positive features
summary_pos_only <- dat_pos %>%
  summarize(pruritus = length(`pruritus~patient~positive`[`pruritus~patient~positive` >= 1]),
            hx_eczema = length(`eczema~patienthx~positive`[`eczema~patienthx~positive` >= 1]),
            facial_erythema = length(`facial_erythema~patient~positive`[`facial_erythema~patient~positive` >= 1]),
            icthyosis = length(`ichthyosis~patient~positive`[`ichthyosis~patient~positive` >= 1]),
            xerosis = length(`xerosis~patient~positive`[`xerosis~patient~positive` >= 1]),
            nipple_eczema = length(`nipple_eczema~patient~positive`[`nipple_eczema~patient~positive` >= 1]),
            infraorbital_folds = length(`infraorbital_folds~patient~positive`[`infraorbital_folds~patient~positive` >= 1]),
            contact_dermatitis = length(`contact_dermatitis~patient~positive`[`contact_dermatitis~patient~positive` >= 1]))

sum_pos_t <- as.data.frame(t(summary_pos_only))
sum_pos_t <- cbind("symptom" = rownames(sum_pos_t), sum_pos_t)
zdf_sort$symptom <- sum_pos_t$symptom
zdf_sort$counts <- sum_pos_t$V1
zdf_sort$correlation <- zdf_sort$Freq

positive_correlations <- zdf_sort[,-c(1:3)]
kable(positive_correlations)


# positive pruritus correlated with negative mentions
dat_neg_index <- grep("~negative$", names(dat_grouped))
dat_neg <- dat_grouped[, dat_neg_index]

y <- cor(dat_grouped$`pruritus~patient~positive`, dat_neg)
ydf <- as.data.frame(as.table(y))  
ydf_sub <- subset(ydf, abs(Freq) > 0.4)  
ydf_sort <- ydf_sub %>% arrange(-Freq)

# patient count for each of the top positive features
summary_neg_only <- dat_neg %>%
  summarize(pruritus = length(`pruritus~patient~negative`[`pruritus~patient~negative` >= 1]),
            facial_erythema = length(`facial_erythema~patient~negative`[`facial_erythema~patient~negative` >= 1]),
            hx_eczema = length(`eczema~patienthx~negative`[`eczema~patienthx~negative` >= 1]),
            ichthyosis = length(`ichthyosis~patient~negative`[`ichthyosis~patient~negative` >= 1]),
            cutaneous_infections = length(`cutaneous_infections~patient~negative`[`cutaneous_infections~patient~negative` >= 1]),
            cheilitis = length(`cheilitis~patient~negative`[`cheilitis~patient~negative` >= 1]),
            pytiriasis_alba = length(`pytiriasis_alba~patient~negative`[`pytiriasis_alba~patient~negative` >= 1]),
            infraorbital_folds = length(`infraorbital_folds~patient~negative`[`infraorbital_folds~patient~negative` >= 1]),
            hx_atopy = length(`atopy~patienthx~negative`[`atopy~patienthx~negative` >= 1]),
            nipple_eczema = length(`nipple_eczema~patient~negative`[`nipple_eczema~patient~negative` >= 1]))

sum_neg_t <- as.data.frame(t(summary_neg_only))
sum_neg_t <- cbind("symptom" = rownames(sum_neg_t), sum_neg_t)
ydf_sort$symptom <- sum_neg_t$symptom
ydf_sort$counts <- sum_neg_t$V1
ydf_sort$correlation <- ydf_sort$Freq

negative_correlations <- ydf_sort[,-c(1:3)]
kable(negative_correlations)

