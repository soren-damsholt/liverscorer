peld_cr <- function(age, sex, height, weight, albumin, bilirubin, inr,
                    creatinine, dialysis = FALSE, units = "US",
                    albumin_conversion_factor = 0.1,
                    bilirubin_conversion_factor = 0.0584,
                    creatinine_conversion_factor =0.0113) {
  
  
  # age should be below 12, throw warning if higher!
  # Bilirubin, albumin, inr values below 1.0 are set to 1.0
  # dialysis = TRUE should hard set creatninin to 1.3 mg/dL.
  
  # zdc_z = min(CDC height z-score, CDC weight z-score):
  cdc_height <- data.frame(age = c(0, seq(0.5, 12.5, 1 )),
                            power = c(-1.295960857, -0.809249882, -0.050782985,
                                      0.476851407, 0.843299612, 1.097562257,
                                      1.272509641, 1.390428859, 1.466733925,
                                      1.512301976, 1.534950767, 1.540390875,
                                      1.532852892, 1.51550947),
                            medians = c(49.28639612, 51.68358057, 55.28612813,
                                        58.09381906, 60.45980763, 62.53669656,
                                        64.40632762, 66.11841553, 67.70574419,
                                        69.19123614, 70.59163924, 71.91961673,
                                        73.1850104, 74.39564379),
                            variation = c(0.05008556, 0.046818545, 0.0434439,
                                          0.041716103, 0.040705173, 0.040079765,
                                          0.039686845, 0.039444555, 0.039304738,
                                          0.03923711, 0.039221665, 0.039244672,
                                          0.03929642, 0.039369875))
  cdc_weight <- data.frame(age = c(0, seq(0.5, 12.5, 1 )),
                           power = c(1.509187507, 1.357944315, 1.105537708,
                                     0.902596648, 0.734121414, 0.590235275,
                                     0.464391566, 0.352164071, 0.250497889,
                                     0.15724751, 0.070885725, -0.00968493,
                                     -0.085258, -0.15640945),
                           medians = c(3.39918645, 3.79752846, 4.544776513,
                                       5.230584214, 5.859960798, 6.437587751,
                                       6.967850457, 7.454854109, 7.902436186,
                                       8.314178377, 8.693418423, 9.043261854,
                                       9.366593571, 9.666089185),
                           variation = c(0.142106724, 0.138075916, 0.131733888,
                                         0.126892697,	0.123025182, 0.119840911, 
                                         0.117166868, 0.11489384, 0.112949644,
                                         0.11128469, 0.109863709, 0.10866078,
                                         0.10765621, 0.106834517)
                           )
  cdc_height
  
  z_score_height <- ((height/cdc_height$medians[cdc_height$age == age]))^cdc_height
  
  
  
  # Unit conversion:
  if(units == "SI") {
    albumin <- albumin * albumin_conversion_factor
    bilirubin <- bilirubin * bilirubin_conversion_factor
    creatinine <- creatinine * creatinine_conversion_factor
  }
  
  # Age subscore:
  age_score <- rep(NA_real_, length(age))
  age_score[which(age < 1)] <- -0.1967
  age_score[which(age >= 1 & age <= 5.5)] <- -0.1967 * age
  age_score[which(age > 5.5)] <- -0.1967 * 5.5
  
  # Albumin score (NB. g/dL, need to convert)
  albu_score <- rep(NA_real_, length(albumin))
  albu_score[which(albumin >= 1 & albumin <= 1.9)] <- -1.842 * log(albumin)
  albu_score[which(albumin > 1.9)] <- -1.842 * log(1.9)
  
  #Bilirubin score
  bili_score <- rep(NA_real_, length(bilirubin))
  bili_score[which(bilirubin >= 1 & bilirubin <= 4)] <- 0.7854 * log(bilirubin) + 0.3434 * log(4)
  bili_score[which(bilirubin > 4 & bilirubin <= 40)] <- 0.7854 * log(4) + 0.3434 * log(bilirubin)
  bili_score[which(bilirubin > 40)] <- 0.7854 * log(4) + 0.3434 * log(40)
  
  # INR score
  inr_score <- rep(NA_real_, length(inr))
  inr_score[which(inr >= 1 & inr <= 2)] <- 1.981 * log(inr) + 0.7298 * log(2)
  inr_score[which(inr > 2 & inr <= 10)] <- 1.981 * log(2) + 0.7298 * log(inr)
  inr_score[which(inr > 10)] <- 1.981 * log(2) + 0.7298 * log(10)
  
  # #Height score
  # height_score <- rep(NA_real_,length(cdc_z))
  # height_score[which(cdc_z < -5)] <- -0.1807 * (-5)
  # height_score[which(cdc_z >= -5 <= -2.1)] <- -0.1807 * (cdc_z)
  # height_score[which(cdc_z > -2.1)] <- -0.1807 * (-2.1)
  
  # Creatinine score
  crea_score <- rep(NA_real_, length(creatinine))
  crea_score[which(creatinine < 0.2)] <- 1.453 * log(0.2)
  crea_score[which(creatinine >= 0.2 & creatinine <= 1.3)] <- 1.453 * log(creatinine)
  crea_score[which(creatinine > 1.3)] <- 1.453 * log(1.3)
  
  x <- age_score + albu_score + bili_score + inr_score + height_score + creatinine_score
  
  # formula = PELD CR: (x + 1.5287) * 10 + 2.82, rounded to nearest integer
  peld <- round((x + 1.5287) * 10 + 2.82)
  
  peld
}