
# Combines age 14 and 16 data for a combination analysis

load('data/ALSPAC_cleaned.RData')
load('data/BMI_13_16.RData')
library(haven)
library(dplyr)

purging_14_16 <- ALSPAC_cleaned$Compensatory_Behaviors |>
  filter(assess_agemos < 200) %>% 
  group_by(id) %>% 
  summarise(purging_14_16  = case_when(sum(purging_any, na.rm =TRUE)>0 ~1,
                                       sum(purging_any, na.rm = TRUE)==0 ~0)) %>% 
  filter(!is.na(purging_14_16))

purging_14_16_monthly <- ALSPAC_cleaned$Compensatory_Behaviors |>
  filter(assess_agemos < 200) |>
  group_by(id) |>
  summarise(purging_monthly  = case_when(sum(purging_monthly, na.rm =TRUE)>0 ~1,
                                         sum(purging_monthly, na.rm = TRUE)==0 ~0)) |>
  filter(!is.na(purging_monthly))

fasting_14_16 <- ALSPAC_cleaned$Compensatory_Behaviors |>
  filter(assess_agemos < 200) |>
  group_by(id) |>
  summarise(fasting_14_16  = case_when(sum(fasting_any, na.rm =TRUE)>0 ~1,
                                       sum(fasting_any, na.rm = TRUE)==0 ~0)) %>% 
  filter(!is.na(fasting_14_16))

fasting_14_16_monthly <- ALSPAC_cleaned$Compensatory_Behaviors |>
  filter(assess_agemos < 200) |>
  group_by(id) |>
  summarise(fasting_monthly  = case_when(sum(fasting_monthly, na.rm =TRUE)>0 ~1,
                                         sum(fasting_monthly, na.rm = TRUE)==0 ~0)) |>
  filter(!is.na(fasting_monthly))

exercise_14_16 <- ALSPAC_cleaned$Driven_Exercise |>
  filter(assess_agemos < 200) |>
  group_by(id) |>
  summarise(exercise_14_16  = case_when(sum(driven_exercise, na.rm =TRUE)>0 ~1,
                                   sum(driven_exercise, na.rm = TRUE)==0 ~0)) 

reg_exercise_14_16 <- ALSPAC_cleaned$Driven_Exercise |> 
  filter(assess_agemos < 200) |>
  group_by(id) |>
  summarise(reg_exercise_14_16  = case_when(sum(driven_exercise_2, na.rm =TRUE)>0 ~1,
                                        sum(driven_exercise_2, na.rm = TRUE)==0 ~0)) 

binge_14_16 <- ALSPAC_cleaned$Binge |>
  filter(assess_agemos < 200) |>
  group_by(id) |>
  summarise(binge_14_16  = case_when(sum(binge_freq_d, na.rm =TRUE)>0 ~1,
            sum(binge_freq_d, na.rm = TRUE)==0 ~0)) |>
  filter(!is.na(binge_14_16))



df_16 <- left_join(bmi_pred_Mean, exercise_14_16)
df_16 <- left_join(df_16, reg_exercise_14_16)
df_16 <- left_join(df_16, fasting_14_16)
df_16 <- left_join(df_16, fasting_14_16_monthly)
df_16 <- left_join(df_16, purging_14_16)
df_16 <- left_join(df_16, purging_14_16_monthly)
df_16 <- left_join(df_16, binge_14_16)



df_16 <- df_16 |>
  mutate(sum_compensatory_any  = rowSums(across(c(purging_14_16, fasting_14_16, exercise_14_16)))) |>
  mutate(any_compensatory = case_when(sum_compensatory_any >0 ~1,
                                      sum_compensatory_any ==0 ~0)) %>% 
  mutate(sum_compensatory_reg  = rowSums(across(c(purging_monthly, fasting_monthly, reg_exercise_14_16)))) |>
  mutate(reg_compensatory = case_when(sum_compensatory_reg>0 ~1,
                                      sum_compensatory_reg==0 ~0))


#sets 'no low weight' and 'no high weight' as the reference categories for their 
df_16 <- within(df_16, low_wt_category <- relevel(low_wt_category, ref = '0.No Low Wt'))
df_16 <- within(df_16, high_wt_category <- relevel(high_wt_category, ref = '0.No High Wt'))



bmi_13_16 <- bmi_13_16 |>  group_by(id, .model) |> 
  summarise(bmi_loss_99 = case_when (sum(lower_99_cutoff, na.rm =TRUE) >0 ~1,
                                    sum(lower_99_cutoff, na.rm = TRUE)==0 ~0),
            bmi_loss_95 =  case_when(sum(lower_99_cutoff, na.rm =TRUE)>0 ~1,
                                     sum(lower_99_cutoff, na.rm = TRUE)==0 ~0),
            thin_cutoff =  case_when(sum(grade_1_cutoff, na.rm =TRUE)>0 ~1,
                                     sum(grade_1_cutoff, na.rm = TRUE)==0 ~0),
            bmi_gain_99 =  case_when(sum(upper_99_cutoff, na.rm =TRUE)>0 ~1,
                                     sum(upper_99_cutoff, na.rm = TRUE)==0 ~0),
            bmi_gain_95 =  case_when(sum(upper_95_cutoff, na.rm =TRUE)>0 ~1,
                                     sum(upper_95_cutoff, na.rm = TRUE)==0 ~0),
            ow_cutoff =  case_when(sum(overweight_cutoff, na.rm =TRUE)>0 ~1,
                                   sum(overweight_cutoff, na.rm = TRUE)==0 ~0)) |>
  mutate(low_wt_category = factor(case_when(bmi_loss_99 == 0 & thin_cutoff == 0 ~ '0.No Low Wt',
                                            bmi_loss_99 == 0 & thin_cutoff == 1 ~ '1.UW - No BMIZ Reduce',
                                            bmi_loss_99 == 1 & thin_cutoff == 0 ~ '2.BMIZ Reduce - No UW',
                                            bmi_loss_99 == 1 & thin_cutoff == 1 ~ '3.UW and BMIZ Reduced'))) |>
  mutate(high_wt_category = factor(case_when(bmi_gain_99 == 0 & ow_cutoff == 0 ~ '0.No High Wt',
                                             bmi_gain_99 == 0 & ow_cutoff == 1 ~ '1.OW - No BMIZ Elevate',
                                             bmi_gain_99 == 1 & ow_cutoff == 0 ~ '2.BMIZ Elevated - no OW',
                                             bmi_gain_99 == 1 & ow_cutoff == 1 ~ '3.OW plus BMIZ Elevate')))


load('data/forecast_data.RData')

bmi_pred <- left_join(bmi_13_16, sex_vars)  #joins sex variables with the bmi predictions at age 13-16

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
bmi_pred_RWM <- bmi_pred |> 
  filter(.model == 'M1')
bmi_pred_RWM.f <- bmi_pred_M1 %>% 
  filter(sex_z == 2)
bmi_pred_RWM.m <- bmi_pred_M1 %>% 
  filter(sex_z == 1)

library(cgwtools)

save(bmi_13_16, df_16, bmi_pred, bmi_pred_Mean, bmi_pred_Mean.f, bmi_pred_Mean.m, bmi_pred_RW, bmi_pred_RW.f, bmi_pred_RW.m, bmi_pred_M1, bmi_pred_M1.f, bmi_pred_M1.m, file = 'data/13_16.RData')
