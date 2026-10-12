clif_of <- function(bilirubin, creatinine, inr, encephalopathy,
                    map, pao2_fio2, spo2_fio2,
                    rrt = FALSE, vasopressors = FALSE) {
  
  
  liver <- ifelse(bilirubin >= 12, 3,
                  ifelse(bilirubin >= 6, 2, 1)
                  )
  
  kidney <- ifelse(creatinine >= 3.5 | rrt, 3,
                   ifelse(creatinine >= 2, 2, 1)
                   )
  
  coag <- ifelse(inr >= 2.5, 3,
                 ifelse(inr >= 2, 2, 1)
                 )
    
  brain <- ifelse(encephalopathy >= 3, 3,
                  ifelse(encephalopathy >= 1, 2,0)
                  )
  
  circulation <- ifelse(vasopressors, 3,
                        ifelse(map < 70, 2, 1)
                        )
  # respiration <- ifelse(ratio)
  
  of_score <- liver + kidney + coag + brain + circulation
  return(of_score)
}
