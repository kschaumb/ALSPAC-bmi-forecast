
# Combines age 14 and 16 data for a combination analysis

load('data/ALSPAC_cleaned.RData')
library(haven)

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


#Binge eating table

binge_table <- frq_table_by_wt_sex_high(df_14, binge_freq_d) |>
  filter(binge_freq_d == '1x/mo or more; present') |>
  mutate (sex_z = recode(sex_z, '1' = 'Boys', '2' = 'Girls'))


ggplot(data = binge_table,
       aes(x = high_wt_category, y = pct, fill = high_wt_category, label = sprintf('%0.1f%%', pct*100))) + #set the aesthetic features, including the labels at one decimal place
  geom_bar(position = position_stack(reverse = TRUE), stat = 'identity') + #add column graph, with male and female separated
  scale_y_continuous(labels = scales::percent, limits = c(0,0.2))+ #set the lower and upper limits of the y axis ( 0-100 percent)
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), text = element_text(size = 15))+ #make legend text a bit bigger
  labs(x = element_blank(), y = 'Percent', title = 'Binge Eating Across Weight Elevation Groups') +
  geom_text(aes(label = sprintf('%0.1f%%', pct*100)), position = position_stack(reverse = TRUE, vjust = 0.5), color = 'white', size = 4, fontface = 'bold') +
  facet_wrap(~sex_z) +
  theme_classic()+
  theme(legend.position = 'none', axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), text = element_text(size = 15)) + #make legend text a bit bigger
  scale_fill_manual(values = wes_palette(name = 'Rushmore', n = 4))

```