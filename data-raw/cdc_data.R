## code to prepare `cdc_data` dataset goes here
weight_inf <- read.csv("data-raw/wtageinf.csv")
weight_child <- read.csv("data-raw/wtage.csv")

cdc_weight <- rbind(weight_inf[weight_inf$Agemos < 24, ],
                    weight_child[weight_child$Agemos >= 24, ])

cdc_weight[] <- lapply(cdc_weight, as.numeric)

height_inf <- read.csv("data-raw/lenageinf.csv")
height_stature_child <- read.csv("data-raw/statage.csv")

cdc_height <- rbind(height_inf[height_inf$Agemos <= 24.5, ],
                    height_stature_child[height_stature_child$Agemos >= 25.5, ])

cdc_height[] <- lapply(cdc_height, as.numeric)

# recode sex to character
recode_sex <- function(x) {
  variable <- rep(NA_character_, length(x))
  
  variable[x == 1] <- "male"
  variable[x == 2] <- "female"
  
  if(anyNA(variable)){
    stop("unexpected sex-code in CDC reference")
  }
  
  variable
}

cdc_weight$Sex <- recode_sex(cdc_weight$Sex)
cdc_height$Sex <- recode_sex(cdc_height$Sex)


cdc_weight <- setNames(cdc_weight,
                       c("sex", "age_months", "boxcox_lambda", "median", "cov_variance",
                         "p3", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p97")) 
cdc_height <- setNames(cdc_height,
                       c("sex", "age_months", "boxcox_lambda", "median", "cov_variance",
                         "p3", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p97")) 

stopifnot(
  !anyDuplicated(cdc_weight[c("sex", "age_months")]),
  !anyDuplicated(cdc_height[c("sex", "age_months")]),
  all(cdc_weight$sex %in% c("male", "female")),
  all(cdc_height$sex %in% c("male", "female"))
)

usethis::use_data(cdc_weight,
                  cdc_height,
                  overwrite = TRUE, internal = TRUE)
