# calculating variance of limits of agreement (var_loa) with some helper variables

# expected value (ev) of variance of all differnces (ev_var_d)
# variance of mean of all differences (bias, var_d)
# alternative variance of mean of all differences (bias, var_d_a)
# variance of the standard deviation of the differences (var_sd_d)

# case discrimination for Var(LoA) depending on bsv/ wsv
# meth = 0 for standard (small bsv)
# meth = 1 for modified (small wsv)

calc_var_loa <- function (n, n_obs, bsv, wsv, outputSubjects, var_var_d, meth){
  ans1 <- 0
  ans2 <- 0
  for(i in 1:n) {
    m_i <- outputSubjects[subject == i,
      m_i]
    ans1 <- ans1 + (m_i^2)
    ans2 <- ans2 + (1/m_i)
  }
  ev_var_d <- ((1- (1/n_obs))*wsv)+((1-(ans1/(n_obs^2)))*bsv)
  var_d <- (wsv/n_obs)+((ans1/(n_obs^2))*bsv)

  var_d_a <- ((1/(n^2))*ans2*wsv)+(bsv/n)

  var_sd_d <- var_var_d/(4*ev_var_d)

  if (meth==0) {
    var_loa <- var_d+((1.96^2)*var_sd_d)
  }else{
    var_loa <- var_d_a+((1.96^2)*var_sd_d)
  }
  # return(
  #   list(
  #     var_loa = var_loa
  #     )
  # )
}

# ---------------------------
# # variance of mean of all differences (bias)
# ans <- 0
# calc_var_d <- function (n, n_obs, bsv, wsv, outputSubjects){
#   for(i in 1:n) {
#     m_i <- outputSubjects[subject == i,
#       m_i]
#     ans <- ans + (m_i^2)
#   }
#   var_d <- (wsv/n_obs)+((ans/(n_obs^2))*bsv)
# }
#
# # ---------------------------
# # alternative variance of mean of all differences (bias)
# ans <- 0
# calc_var_d_a <- function (n, bsv, wsv, outputSubjects){
#   for(i in 1:n) {
#     m_i <- outputSubjects[subject == i,
#       m_i]
#     ans <- ans + (1/m_i)
#   }
#   var_d_a <- ((1/(n^2))*ans*wsv)+(bsv/n)
# }
#
# # ---------------------------
# # variance of the standard deviation of the differences
#
# calc_var_sd_d <- function (var_var_d, ev_var_d){
#   var_sd_d <- var_var_d/(4*ev_var_d)
# }
#
# # ---------------------------
# # variance of limits of agreement (var_loa)
#
# calc_var_loa <- function(var_d, var_sd_d){
#   var_loa <- var_d+((1.96^2)*var_sd_d)
# }
