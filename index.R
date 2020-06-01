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


# closed_border
# partial or regional or schengen border closures scored as 0.5
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

closed_border_i <- which(colnames(tpm) == "closed_border") 
closed_border_region_i <- which(colnames(tpm) == "closed_border_region")
closed_border_schengen_i <- which(colnames(tpm) == "closed_border_schengen")
closed_border_partial_i <- which(colnames(tpm) == "closed_border_partial")
tpm$closed_border <- apply(tpm, 1, closedBorder, closed_border_i, closed_border_region_i, closed_border_schengen_i)


# limited movement
limitedMvmt <- function (row, limited_mvmt_i) {
  limited_mvmt_date <- as.Date(row[limited_mvmt_i])

  if (is.na(limited_mvmt_date)) {
    return(0)
  } else if (limited_mvmt_date <= tpmControlEnd) {
    return(1)
  } else {
    return(0)
  }
}

limited_mvmt_i <- which(colnames(tpm) == "limited_mvmt") 
tpm$limited_mvmt <- apply(tpm, 1, limitedMvmt, limited_mvmt_i)


# school closed
schoolClosed <- function (row, school_closed_i) {
  school_closed_date <- as.Date(row[school_closed_i])
  school_closed_weak_date <- as.Date(row[school_closed_i + 1])

  if ((!is.na(school_closed_date)) & (school_closed_date <= tpmControlEnd)) {
    return(1)
  } else if ((!is.na(school_closed_weak_date)) & (school_closed_weak_date <= tpmControlEnd)) {
    return(0.5)
  } else {
    return(0)
  }
}

school_closed_i <- which(colnames(tpm) == "school_closed") 
tpm$school_closed <- apply(tpm, 1, schoolClosed, school_closed_i)


# social group limits
smallGroup <- function (row, small_group_50_i) {
  ten_date <- as.Date(row[small_group_50_i + 1])
  less_ten_date <- as.Date(row[small_group_50_i + 2])
  ten_within <- (!is.na(ten_date) & (ten_date <= tpmControlEnd))
  less_ten_within <- (!is.na(less_ten_date) & (less_ten_date <= tpmControlEnd))

  fifty_date <- as.Date(row[small_group_50_i])
  hundred_date <- as.Date(row[small_group_50_i - 1])
  two_fifty_date <- as.Date(row[small_group_50_i - 2])
  five_hundred_date <- as.Date(row[small_group_50_i - 3])
  thousand_date <- as.Date(row[small_group_50_i - 4])

  l10 <- log(10)


  if ((ten_within) | (less_ten_within)) {
    return(1)
  } else if (!is.na(fifty_date) & fifty_date <= tpmControlEnd) {
    return(l10 / log(50))
  } else if (!is.na(hundred_date) & hundred_date <= tpmControlEnd) {
    return(l10 / log(100))
  } else if (!is.na(two_fifty_date) & two_fifty_date <= tpmControlEnd) {
    return(l10 / log(250))
  } else if (!is.na(five_hundred_date) & five_hundred_date <= tpmControlEnd) {
    return(l10 / log(500))
  } else if (!is.na(thousand_date) & thousand_date <= tpmControlEnd) {
    return(l10 / log(1000))
  } else {
    return(0)
  }
}

small_group_50_i <- which(colnames(tpm) == "social_group_limits_50") 
tpm$group <- apply(tpm, 1, smallGroup, small_group_50_i)


# contact tracing
contactTracing <- function (row, contact_tracing_i) {
  contact_tracing_date <- as.Date(row[contact_tracing_i])
  contact_tracing_weak_date <- as.Date(row[contact_tracing_i + 1])

  if (!is.na(contact_tracing_date) & contact_tracing_date <= tpmControlEnd) {
    return(1)
  } else if (!is.na(contact_tracing_weak_date) & contact_tracing_weak_date <= tpmControlEnd) {
    return(0.5)
  } else {
    return(0)
  }
}

contact_tracing_i <- which(colnames(tpm) == "contact_tracing") 
tpm$contact_tracing <- apply(tpm, 1, contactTracing, contact_tracing_i)


## symptom screeneing at border
symptomScreening <- function (row, symptom_screening_i) {
  symptom_screening_date <- as.Date(row[symptom_screening_i])

  if (!is.na(symptom_screening_date) & symptom_screening_date <= tpmControlEnd) {
    return(1)
  } else {
    return(0)
  }
}


symptom_screening_i <- which(colnames(tpm) == "symp_screening_border") 
tpm$symp_screening <- apply(tpm, 1, symptomScreening, symptom_screening_i)


## quarantine
quarantine <- function (row, i, j, k, l) {
  quar_trav <- as.Date(row[i])
  quar_trav_weak <- as.Date(row[j])
  quar_susp <- as.Date(row[k])
  quar_confirm <- as.Date(row[l])

  result <- 0

  if (!is.na(quar_trav) & quar_trav <= tpmControlEnd) {
    result <- result + 1/3
  }

  if (result == 0 & !is.na(quar_trav_weak) & quar_trav_weak <= tpmControlEnd) {
    result <- result + 1/6
  }

  if (!is.na(quar_susp) & quar_susp <= tpmControlEnd) {
    result <- result + 1/3
  }

  if (!is.na(quar_confirm) & quar_confirm <= tpmControlEnd) {
    result <- result + 1/3
  }

  return (result)
}


quar_trav_i <- which(colnames(tpm) == "quar_trav") 
quar_susp_i <- which(colnames(tpm) == "quar_susp") 
quar_confirm_i <- which(colnames(tpm) == "quar_confirm") 

tpm$quarantine <- apply(tpm, 1, quarantine, quar_trav_i, quar_trav_i + 1, quar_susp_i, quar_confirm_i)

print(tpm$quarantine)







