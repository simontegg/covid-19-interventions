# library(tidyverse)

# data
interventions <- read.csv(file = "./interventions.csv", na.strings=c("","NA"))

# constants
tpmControlEnd <- as.Date("2020-04-17")


# Te Pūnaha Matatini model
tpm <- subset(interventions, (!is.na(interventions$TPM_early_phase_r_eff)))
tpm$region[tpm$region == ""] <- NA
tpm$jurisdiction <-  paste(tpm$country_code, tpm$region, sep="_")
tpm$jurisdiction <- gsub("_NA", "", tpm$jurisdiction)

print(tpm$jurisdiction)







