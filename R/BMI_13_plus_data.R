library(dplyr)
library(fable)
load('data/ALSPAC_Cleaned.RData') 
load('data/fcast_data.RData')
load('data/13_16.RData')
source('R/add_bmi_vars.R')
source('R/solve_for_wt.R')
source('R/bmiz_to_bmi.R')

sex_vars <- bmi_df |> 
  select(id, sex_z) |> 
  distinct()

bmi_df_13_plus <- bmi_df |> 
  filter(agemos_asess_1 >= 13*12)
fcast <- left_join(fcast_raw, sex_vars)  # adds sex as a variable to the forecasted datasets
fcast <- add_bmi_vars(fcast) #adds bmi variables to the forecasted dataset, including 95 and 99% forecasted ranges from each model that was run
bmi_df_13_plus <- merge(fcast, bmi_df_13_plus, by = c('id', 'agemos_asess_1', 'sex_z'))
data_by_model <- split(bmi_df_13_plus, bmi_df_13_plus$.model)
mean_model_13_plus_BMIz <- data_by_model$Mean

save(mean_model_13_plus_BMIz, file = 'data/BMIz_13_plus.RData')