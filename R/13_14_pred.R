library(dplyr)
library(cgwtools)

load('data/bmi_13_14.RData')
load('data/forecast_data.RData')

bmi_filtered <- bmi_13_14 %>% 
  group_by(id, .model) %>% 
  slice(which.max(agemos_asess_1)) %>% 
  ungroup

bmi_pred <- left_join(bmi_filtered, sex_vars)  #joins sex variables with the bmi predictions at age 13-14

bmi_pred_Mean <- bmi_pred |> 
  filter(.model == 'Mean')
bmi_pred_Mean.f <- bmi_pred_Mean %>% 
  filter(sex_z == 2)
bmi_pred_Mean.m <- bmi_pred_Mean %>% 
  filter(sex_z == 1)
bmi_pred_RW <- bmi_pred |> 
  filter(.model == 'RW')
bmi_pred_RW.f <- bmi_pred_RW %>% 
  filter(sex_z == 2)
bmi_pred_RW.m <- bmi_pred_RW %>% 
  filter(sex_z == 1)
bmi_pred_M1 <- bmi_pred |> 
  filter(.model == 'M1')
bmi_pred_M1.f <- bmi_pred_M1 %>% 
  filter(sex_z == 2)
bmi_pred_M1.m <- bmi_pred_M1 %>% 
  filter(sex_z == 1)

resave(bmi_pred, bmi_pred_Mean, bmi_pred_Mean.f, bmi_pred_Mean.m, bmi_pred_RW, bmi_pred_RW.f, bmi_pred_RW.m, bmi_pred_M1, bmi_pred_M1.f, bmi_pred_M1.m, file = 'data/bmi_13_14.RData')