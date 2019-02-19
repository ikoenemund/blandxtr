# set working directory
setwd("~/blandxtr")

# read data (csv)
library(data.table)
dataOlofsen<-fread("data/dataOlofsen.csv")

# data.frame to data.table
setDT(dataOlofsen)

# copy input data
outputMeasurements <- copy (dataOlofsen)
outputSubjects <- dataOlofsen[, .(.N), by = .(subject)]
# rename column
setnames(outputSubjects,"N", "sum_measurements")

# add measurement IDs in outputMeasurements
outputMeasurements[, measurement_id := rowid(subject)]

# ----------------------------

# number of subjects (n)
n <- uniqueN(dataOlofsen, by="subject")

# ----------------------------

# number of measurements of each subject (i=subject, m_i)
summing_measurements_i <- function(i) {
  m_i <- dataOlofsen[, sum( subject==i )]
}

# ----------------------------

# total number of observations (pairs of values, n_obs)
n_obs <-nrow(dataOlofsen)

# ----------------------------

# difference_mk (subject i, measurement j) as double (D_ij)
difference_ij<- function(i, j) {
  measurement_ij <- dataOlofsen[subject == i & measurement_id == j]
  measurementX_ij <- as.double(measurement_ij[1,"measurementX"])
  measurementY_ij <- as.double(measurement_ij[1,"measurementY"])
  d_ij <- measurementX_ij - measurementY_ij
}

    # difference_mk for all measurements
    # functional: return a new data table (don't alter the existing one)
    outputMeasurements[, d_ij := (measurementX-measurementY)]

# ----------------------------

# mean_mk (subject m, measurement k) as double (M_ij)
mean_ij <- function(i, j) {
  measurement_ij <- dataOlofsen[subject == i & measurement_id == j]
  measurementX_ij <- as.double(measurement_ij[1,"measurementX"])
  measurementY_ij <- as.double(measurement_ij[1,"measurementY"])
  m_ij <- (measurementX_ij + measurementY_ij)/2
}

    # mean_mk for all measurements
    outputMeasurements[, m_ij := (measurementX+measurementY)/2]

# ------------------------------

# single subject: mean_diff_i mean of differences of subject i as double (D_i)
library(data.table)
mean_diff_i <- function(i) {
  d_i <- dataOlofsen[subject == i,
    mean(m_ij)]
}

# all subjects (each subject): mean of differences between measurements (each subject)
ans <- outputMeasurements[, mean(d_ij), by = .(subject)]
setnames(ans,"V1", "d_i")

outputSubjects <- merge(ans, outputSubjects, by="subject")

      # TO DO: make nicer (above), some ideas:
      # works
      # outputSubjects[, mean_diff:=outputMeasurements[, .(mean(difference_mk)), by = .(subject)]]
      # test
      #outputSubjects[, mean_diff:=[, outputMeasurements[, .(mean(difference_mk)), by =.(subject)]]

# -------------------------------
# variance of differences (each subject)
# TO DO: leave out? (not needed), make nicer (sum)
# i: number of subject

variance_diff <- function(i) {
  d_i <- outputSubjects[subject == i,
    d_i]
  m_i <- outputSubjects[subject == i,
    sum_measurements]
  ans <- (outputMeasurements[subject == i,
    d_ij]-d_i)^2

  var_d_i <- sum(ans)/(m_i-1)
}

# variance of differences for all subjects
ans <- outputSubjects[, variance_diff(subject), by = .(subject)]
setnames(ans,"V1", "var_d_i")

outputSubjects <- merge(ans, outputSubjects, by="subject")
