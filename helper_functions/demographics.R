# demographics

#ggplot(demo, aes(x = race)) +
#  geom_histogram(stat = "count") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))

load_demographics <- function(source_file, race) {
	demo <- read.table(source_file, sep = "\t", header = T)
	colnames(demo)[1] <- "patient_id"

	demo$patient_id_fix <- NA
	demo$patient_id <- as.character(demo$patient_id)
	for (i in 1:nrow(demo)) {
	  if (nchar(strsplit(demo$patient_id[i], "\\.")[[1]][2]) == 7) {
		demo$patient_id_fix[i] <- demo$patient_id[i]
	  } else if (nchar(strsplit(demo$patient_id[i], "\\.")[[1]][2]) == 6) {
		demo$patient_id_fix[i] <- paste(demo$patient_id[i], "0", sep = "")
	  } else if (nchar(strsplit(demo$patient_id[i], "\\.")[[1]][2]) == 5) {
		demo$patient_id_fix[i] <- paste(demo$patient_id[i], "00", sep = "")
	  } else {
		print("problem")
		print(demo$patient_id[i])
	  }
	}
	demo <- demo[,-1]
	
	demo$age <- floor(as.numeric(as.Date("2017-03-15") - as.Date(demo$birth_dts))/365.25)
	
	demo$gender.c <- ifelse(demo$gender == "Female",
                        1,
                        0)
	
	if (race == T) {
		demo$race.c <- ifelse(demo$race == "White or Caucasian",
                      1,
                      0)

		demo <- demo[, c(6:9)] #check indices
	} else {
		demo <- demo[, c(6:8)]
	}
	
	colnames(demo)[1] <- "patient_id"
	
	return(demo);
}