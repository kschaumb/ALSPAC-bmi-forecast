
# Below is a function that creates a 'bmi prediction variable' to look up the bmi (translated from bmiz) that is the predicted bmi for each participant at each age. The below code also calculates the upper and lower (95 and 99% values) for each participant. This is done with a sex = 2 and sex = 1 and then a sex-specific prediction is chosen based on whether the participant is male or female as the sex predictions were not showing up after 200 months without specifying the code this particular way

add_bmi_vars <- function(df) {
  

  # Make new variables
  df <- df |> 
    # Mean bmi based on gender
    mutate (bmi_f := bmi_lookup_cdc(sex = 2, age = df$agemos_asess_1, bmiz = df$.mean), 
            bmi_m := bmi_lookup_cdc(sex = 1, age = df$agemos_asess_1, bmiz = df$.mean),
            bmi_pred := case_when(sex_z == 2 ~ bmi_f, 
                                      sex_z == 1 ~ bmi_m),
            # 99% window - picks out lower and upper values from the CI
            bmiz_lower_99 = df$`99%`$lower, 
            bmiz_upper_99 = df$`99%`$upper,
            
            # creates lower and uppper bmi values based on gender
            bmi_lower_f_99 = bmi_lookup_cdc(sex = 2, age = agemos_asess_1, bmiz = bmiz_lower_99),
            bmi_upper_f_99 = bmi_lookup_cdc(sex = 2, age = agemos_asess_1, bmiz = bmiz_upper_99),
            bmi_lower_m_99 = bmi_lookup_cdc(sex = 1, age = agemos_asess_1, bmiz = bmiz_lower_99),
            bmi_upper_m_99 = bmi_lookup_cdc(sex = 1, age = agemos_asess_1, bmiz = bmiz_upper_99),
            bmi_lower_pred_99 = case_when(sex_z == 2 ~ bmi_lower_f_99, 
                                               sex_z == 1 ~ bmi_lower_m_99),
            bmi_upper_pred_99 = case_when(sex_z == 2 ~ bmi_upper_f_99, 
                                               sex_z == 1 ~ bmi_upper_m_99),
            #95% window
            bmiz_lower_95 = df$`95%`$lower, 
            bmiz_upper_95 = df$`95%`$upper, 
            bmi_lower_f_95 = bmi_lookup_cdc(sex = 2, age = agemos_asess_1, bmiz = bmiz_lower_95),
            bmi_upper_f_95 = bmi_lookup_cdc(sex = 2, age = agemos_asess_1, bmiz = bmiz_upper_95),
            bmi_lower_m_95 = bmi_lookup_cdc(sex = 1, age = agemos_asess_1, bmiz = bmiz_lower_95),
            bmi_upper_m_95 = bmi_lookup_cdc(sex = 1, age = agemos_asess_1, bmiz = bmiz_upper_95),
            bmi_lower_pred_95 = case_when(sex_z == 2 ~ bmi_lower_f_95, 
                                               sex_z == 1 ~ bmi_lower_m_95),
            bmi_upper_pred_95 = case_when(sex_z == 2 ~ bmi_upper_f_95, 
                                               sex_z == 1 ~ bmi_upper_m_95), 
            
    bmi_grade_1_m = bmi_lookup_cdc(sex = 1, age = agemos_asess_1, bmiz = -1), 
    bmi_grade_1_f = bmi_lookup_cdc(sex = 2, age = agemos_asess_1, bmiz = -1),
    bmi_grade_1 = case_when(sex_z == 2 ~ bmi_grade_1_f, 
                            sex_z == 1 ~ bmi_grade_1_m),
    bmi_grade_2_m = bmi_lookup_cdc(sex = 1, age = agemos_asess_1, bmiz = -2), 
    bmi_grade_2_f = bmi_lookup_cdc(sex = 2, age = agemos_asess_1, bmiz = -2),
    bmi_grade_2 = case_when(sex_z == 2 ~ bmi_grade_2_f, 
                            sex_z == 1 ~ bmi_grade_2_m)) |> 
    select(-c('bmi_f', 'bmi_m', 'bmi_lower_f_99', 'bmi_upper_f_99', 'bmi_lower_m_99', 'bmi_upper_m_99', 'bmi_grade_1_m', 'bmi_grade_1_f', 'bmi_grade_2_m', 'bmi_grade_2_f','bmi_lower_f_95', 'bmi_upper_f_95', 'bmi_lower_m_95', 'bmi_upper_m_95', 'bmiz'))
  
  return(df)
}
