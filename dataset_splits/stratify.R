library(dplyr)
library(magrittr)

strat_sample <- function(data, gr_variab, tr_percent, seed) {
  
  stopifnot(tr_percent > 0 & tr_percent < 1)
  
  if(require(dplyr) & require(magrittr)) {
    
    if (!missing(seed)) set.seed(seed)
    
    names0 <- names(data)
    
    gr_variab <- which(names0 == gr_variab)
    
    names(data) <- make.unique(c("n", "tRows", "SET", names0))[-(1:3)]
    gr_variab <- names(data)[gr_variab]
    
    data %<>%
      sample_frac %>%
      group_by_(gr_variab) %>%
      mutate(n = n(), tRows = round(tr_percent * n))
    
    data %<>%
      mutate(SET = ifelse(row_number() <= tRows, "Train", "Test")) %>%
      select(-n, -tRows) %>%
      ungroup
    
    names(data) <- make.unique(c(names0, "SET"))
    
    data
    
  }
  
}


extract_set <- function(data, whichSET) {
  
  stopifnot(is.element(whichSET, c("Train", "Test")))
  
  if (require(dplyr)) {
    
    variab <- names(data)[ncol(data)]
    condit <- get(variab, data) == whichSET
    
    data %>%
      filter_(~ condit) %>%
      select_(paste0("-", variab))
    
  }
  
}

## example ##
#n <- 1e+5
#set.seed(386)
#Df <- data.frame(V1 = rnorm(n),
#                 V2 = rt(n, df = 4),
#                 V3 = rpois(n, lambda = 1),
#                 y = sample(letters[1:4], n, replace = T,
#                            prob = c(.33, .33, .33, .01)))

#groups <- strat_sample(Df, "y", .75)

#with(groups, prop.table(table(y, SET), 1))

# a tibble
#extract_set(groups, "Train")
#extract_set(groups, "Test")


#samples <- strat_sample(dat_grouped_codes, "label", 0.8)
#with(samples, prop.table(table(label, SET), 1)) # check!
#train_set <- extract_set(samples, "Train")
#test_set <- extract_set(samples, "Test")
