# library(tidyverse)

# data
interventions <- read.csv(file = "./interventions.csv", na.strings=c("","NA"))

# constants
tpmControlEnd <- as.Date("2020-04-17")

# functions
closedBorder <- function (row, closed_border_i, closed_border_region_i, closed_border_schengen_i) {
  closed_border <- row[closed_border_i]
  closed_border_date <- as.Date(closed_border)
  closed_border_within <- !is.na(closed_border_date) && closed_border_date <= tpmControlEnd
  closed_border_partial <- c(row[closed_border_region_i], row[closed_border_schengen_i], row[closed_border_partial_i])
  partial_i <- which(!is.na(closed_border_partial))[1]

  if (closed_border_within) {
    return(1)
  } else if (is.na(partial_i)){
    return(0)
  } else if (as.Date(closed_border_partial[partial_i]) <= tpmControlEnd) {
    return(0.5)
  } else {
    return(0)
  }
}



# Te Pūnaha Matatini model
tpm <- subset(interventions, (!is.na(interventions$TPM_early_phase_r_eff)))
tpm$region[tpm$region == ""] <- NA
tpm$jurisdiction <-  paste(tpm$country_code, tpm$region, sep="_")
tpm$jurisdiction <- gsub("_NA", "", tpm$jurisdiction)

closed_border_i <- which(colnames(tpm) == "closed_border") 
closed_border_region_i <- which(colnames(tpm) == "closed_border_region")
closed_border_schengen_i <- which(colnames(tpm) == "closed_border_schengen")
closed_border_partial_i <- which(colnames(tpm) == "closed_border_partial")


tpm$closed_border <- apply(tpm, 1, closedBorder, closed_border_i, closed_border_region_i, closed_border_schengen_i)





print(tpm$closed_border)







