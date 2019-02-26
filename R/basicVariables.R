# calculating basic variables for datatable (dt)
# dt: 3 columns (subject, measurementX, measurementY) necessary

calc_basicVariables <- function(dt){

  # ----------------------------
  # some preparation

  # copy input data for modification as output
  outputMeasurements <- copy (dt)
  # add measurement IDs in outputMeasurements
  outputMeasurements[, measurement_id := rowid(subject)]

  outputSubjects <- dt[, .(.N), by = .(subject)]
  # rename column
  setnames(outputSubjects,"N", "m_i")

  # ----------------------------

  # number of subjects (n)
  n <- uniqueN(dt, by="subject")

  # ----------------------------

  # total number of observations (pairs of values, n_obs)
  n_obs <-nrow(dt)

  # ----------------------------

  # difference_ij for all measurements
  # functional: return a new data table (don't alter the existing one)
  outputMeasurements[, d_ij := (measurementX-measurementY)]

  # ----------------------------

  # mean_ij for all measurements
  outputMeasurements[, m_ij := (measurementX+measurementY)/2]

  # ------------------------------

  # all subjects (each subject): mean of differences between measurements (each subject)
  ans <- outputMeasurements[, mean(d_ij), by = .(subject)]
  setnames(ans,"V1", "d_i")

  outputSubjects <- merge(ans, outputSubjects, by="subject")
  rm(ans)

        # TO DO: make nicer (above), some ideas:
        # works
        # outputSubjects[, mean_diff:=outputMeasurements[, .(mean(difference_mk)), by = .(subject)]]
        # test
        #outputSubjects[, mean_diff:=[, outputMeasurements[, .(mean(difference_mk)), by =.(subject)]]

  # -------------------------------

  # variance of differences for each subject
  # TO DO: leave out? (not needed), make nicer (sum)
  # i: number of subject

  variance_diff <- function(i) {
    d_i <- outputSubjects[subject == i,
      d_i]
    m_i <- outputSubjects[subject == i,
      m_i]
    ans <- (outputMeasurements[subject == i,
      d_ij]-d_i)^2

    var_d_i <- sum(ans)/(m_i-1)
  }

  # variance of differences for each subject (calculation for all subjects)
  ans <- outputSubjects[, variance_diff(subject), by = .(subject)]
  setnames(ans,"V1", "var_d_i")

  outputSubjects <- merge(ans, outputSubjects, by="subject")
  rm(ans)
  rm(variance_diff)

  # -------------------------------------

  # mean of all differences/ bias (D/ B)
  ans <- outputMeasurements[, d_ij]
  d <- mean (ans)

  rm(ans)
  # -------------------------------------

  # alternative mean of all differences/ bias (D_a/ B_a)
  ans <- outputSubjects[, d_i]
  d_a <- mean (ans)

  rm(ans)

  return(
    list(
      outputMeasurements = outputMeasurements,
      outputSubjects = outputSubjects,
      n = n,
      n_obs = n_obs,
      d = d,
      d_a = d_a
    )
  )
}
