## code to prepare `cdc_data` dataset goes here
weight_inf <- read.csv("data-raw/wtageinf.csv")
weight_child <- read.csv("data-raw/wtage.csv")

cdc_weight <- rbind(weight_inf[weight_inf$Agemos < 24, ],
                    weight_child[weight_child$Agemos >= 24, ])

height_inf <- read.csv("data-raw/lenageinf.csv")
height_stature_child <- read.csv("data-raw/statage.csv")

cdc_height <- rbind(height_inf[height_inf$Agemos <= 24.5, ],
                    height_stature_child[height_stature_child$Agemos >= 25.5, ])

cdc_weight <- setNames(cdc_weight,
                       c("sex", "age_months", "boxcox_lambda", "median", "cov_variance",
                         "p3", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p97")) 
cdc_height <- setNames(cdc_height,
                       c("sex", "age_months", "boxcox_lambda", "median", "cov_variance",
                         "p3", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p97")) 

usethis::use_data(cdc_weight,
                  cdc_height,
                  overwrite = TRUE, internal = TRUE)
