library(dplyr)

setwd("~/AD_lasso")
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

#z <- cor(dat_grouped$`pruritus~patient~positive`, dat_grouped[,-c(1,2)])
z <- cor(dat_pos$`pruritus~patient~positive`, dat_pos)
zdf <- as.data.frame(as.table(z))
subset(zdf, abs(Freq) > 0.4)

# patient count for each of top features
summary_pos_only <- dat_pos %>%
  summarize(pruritus = length(`pruritus~patient~positive`[`pruritus~patient~positive` >= 1]),
            hx_eczema = length(`eczema~patienthx~positive`[`eczema~patienthx~positive` >= 1]),
            facial_erythema = length(`facial_erythema~patient~positive`[`facial_erythema~patient~positive` >= 1]),
            icthyosis = length(`ichthyosis~patient~positive`[`ichthyosis~patient~positive` >= 1]),
            xerosis = length(`xerosis~patient~positive`[`xerosis~patient~positive` >= 1]),
            nipple_eczema = length(`nipple_eczema~patient~positive`[`nipple_eczema~patient~positive` >= 1]))