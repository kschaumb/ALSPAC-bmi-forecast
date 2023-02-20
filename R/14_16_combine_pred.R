
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
  summarise(DEx_14_16  = case_when(sum(driven_exercise_2, na.rm =TRUE)>0 ~1,
                                   sum(driven_exercise_2, na.rm = TRUE)==0 ~0)) |>
  filter(!is.na(DEx_14_16))

binge_14_16 <- ALSPAC_cleaned$Binge |>
  filter(assess_agemos < 200) |>
  group_by(id) |>
  summarise(binge_14_16  = case_when(sum(binge_freq_d, na.rm =TRUE)>0 ~1,
            sum(binge_freq_d, na.rm = TRUE)==0 ~0)) |>
  filter(!is.na(binge_14_16))

bmi_13_16 <- bmi_df_13_16 |>  group_by(id) |>
  summarise(bmi_loss_99 = case_when(sum(lower_99_cutoff, na.rm =TRUE)>0 ~1
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
  mutate(low_wt_category = factor(case_when(bmi_loss_99 == 0 & thin_cutoff == 0 ~ 'No Low Wt',
                                            bmi_loss_99 == 0 & thin_cutoff == 1 ~ 'UW - No BMIZ Reduce',
                                            bmi_loss_99 == 1 & thin_cutoff == 0 ~ 'BMIZ Reduced - No UW',
                                            bmi_loss_99 == 1 & thin_cutoff == 1 ~ 'UW and BMIZ Reduced'))) |>
  mutate(high_wt_category = factor(case_when(bmi_gain_99 == 0 & ow_cutoff == 0 ~ 'No High Wt',
                                             bmi_gain_99 == 0 & ow_cutoff == 1 ~ 'OW - No BMIZ Elevate',
                                             bmi_gain_99 == 1 & ow_cutoff == 0 ~ 'BMIZ Elevated - no OW',
                                             bmi_gain_99 == 1 & ow_cutoff == 1 ~ 'OW plus BMIZ Elevate')))


