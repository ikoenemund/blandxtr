# read data (csv)
dataOlofsen<-fread("data/dataOlofsen.csv")

# data.frame to data.table
setDT(dataOlofsen)

# add measurement IDs in dataOlofsen
dataOlofsen[, measurement_id := rowid(patient)]

# ----------------------------

# number of subjects (n)
count_subjects <- uniqueN(dataOlofsen, by="patient")

# ----------------------------

# number of measurements of each subject (m=subject, m_i)
summing_measurements_m <- function(m) {
  sum_measurements_m <- dataOlofsen[, sum( patient==m )]
}

    # subjects<-dataOlofsen[, .N, by = "patient"]

# ----------------------------

# total number of observations (pairs of values, n_obs)
total_observ<-nrow(dataOlofsen)

# ----------------------------

# difference_mk (subject m, measurement k) as double (D_ij)
difference_mk<- function(m, k) {
  measurement_mk <- dataOlofsen[patient == m & measurement_id == k]
  measurementX_mk <- as.double(measurement_mk[1,"measurementX"])
  measurementY_mk <- as.double(measurement_mk[1,"measurementY"])
  difference_mk <- measurementX_mk - measurementY_mk
}

    # difference_mk for all measurements
    # functional: return a new data table (don't alter the existing one)
    # dataOlofsen[, difference_mk := (measurementX-measurementY)]

# ----------------------------

# mean_mk (subject m, measurement k) as double (M_ij)
mean_mk<- function(m, k) {
  measurement_mk <- dataOlofsen[patient == m & measurement_id == k]
  measurementX_mk <- as.double(measurement_mk[1,"measurementX"])
  measurementY_mk <- as.double(measurement_mk[1,"measurementY"])
  mean_mk <- (measurementX_mk + measurementY_mk)/2
}

    # mean_mk for all measurements
    # dataOlofsen[, mean_mk := (measurementX+measurementY)/2]
