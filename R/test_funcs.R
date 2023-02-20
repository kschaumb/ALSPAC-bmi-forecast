

add_ind_bmi_vars <- function(df, mod = df$.model[[1]]) {
 
# Make variable Names for use
  bmi_f <- paste0('bmi_f.', mod)
  bmi_m <- paste0('bmi_m.', mod)
  bmi_pred <- paste0('bmi_pred.', mod)
  bmi_lower_f_99 <- paste0('bmi_lower_f_99.', mod)
  bmi_upper_f_99 <- paste0('bmi_upper_f_99.', mod)
  bmi_lower_m_99 <- paste0('bmi_lower_m_99.', mod)
  bmi_upper_m_99 <- paste0('bmi_upper_m_99.', mod)
  bmi_lower_pred_99 <- paste0('bmi_lower_pred_99.', mod)
  bmi_upper_pred_99 <- paste0('bmi_upper_pred_99.', mod)
  
  bmi_lower_f_95 <- paste0('bmi_lower_f_95.', mod)
  bmi_upper_f_95 <- paste0('bmi_upper_f_95.', mod)
  bmi_lower_m_95 <- paste0('bmi_lower_m_95.', mod)
  bmi_upper_m_95 <- paste0('bmi_upper_m_95.', mod)
  bmi_lower_pred_95 <- paste0('bmi_lower_pred_95.', mod)
  bmi_upper_pred_95 <- paste0('bmi_upper_pred_95.', mod)
  
  
# Make new variables
  df <- df |> 
# Mean bmi based on gender
  mutate ({{bmi_f}} := bmi_lookup_cdc(sex = 2, age = df$agemos_asess_1, bmiz = df$.mean), 
          {{bmi_m}} := bmi_lookup_cdc(sex = 1, age = df$agemos_asess_1, bmiz = df$.mean),
          {{bmi_pred}} := case_when(sex_z == 2 ~ get(bmi_f), 
                                    sex_z == 1 ~ get(bmi_m)),
# 99% window - picks out lower and upper values from the CI
        bmiz_lower_99 = df$`99%`$lower, 
        bmiz_upper_99 = df$`99%`$upper,

# creates lower and uppper bmi values based on gender
        {{bmi_lower_f_99}} := bmi_lookup_cdc(sex = 2, age = agemos_asess_1, bmiz = bmiz_lower_99),
        {{bmi_upper_f_99}} := bmi_lookup_cdc(sex = 2, age = agemos_asess_1, bmiz = bmiz_upper_99),
        {{bmi_lower_m_99}} := bmi_lookup_cdc(sex = 1, age = agemos_asess_1, bmiz = bmiz_lower_99),
        {{bmi_upper_m_99}} := bmi_lookup_cdc(sex = 1, age = agemos_asess_1, bmiz = bmiz_upper_99),
        {{bmi_lower_pred_99}} := case_when(sex_z == 2 ~ get(bmi_lower_f_99), 
                                           sex_z == 1 ~ get(bmi_lower_m_99)),
        {{bmi_upper_pred_99}} := case_when(sex_z == 2 ~ get(bmi_upper_f_99), 
                                           sex_z == 1 ~ get(bmi_upper_m_99)),
#95% window
        bmiz_lower_95 = df$`95%`$lower, 
        bmiz_upper_95 = df$`95%`$upper, 
          {{bmi_lower_f_95}} := bmi_lookup_cdc(sex = 2, age = agemos_asess_1, bmiz = bmiz_lower_95),
          {{bmi_upper_f_95}} := bmi_lookup_cdc(sex = 2, age = agemos_asess_1, bmiz = bmiz_upper_95),
          {{bmi_lower_m_95}} := bmi_lookup_cdc(sex = 1, age = agemos_asess_1, bmiz = bmiz_lower_95),
          {{bmi_upper_m_95}} := bmi_lookup_cdc(sex = 1, age = agemos_asess_1, bmiz = bmiz_upper_95),
          {{bmi_lower_pred_95}} := case_when(sex_z == 2 ~ get(bmi_lower_f_95), 
                                         sex_z == 1 ~ get(bmi_lower_m_95)),
          {{bmi_upper_pred_95}} := case_when(sex_z == 2 ~ get(bmi_upper_f_95), 
                                         sex_z == 1 ~ get(bmi_upper_m_95)))

 return(df)
}


df_list_func <- function(df) {
df_list <- split(df, df$.model)
  return(df_list)}
  

add_bmi_vars <- function(df) { 
  
  data_by_model <- lapply(df_list_func(df), add_ind_bmi_vars)
  data <- data_by_model[[1]]
  j = 2
  while (j <= length(data_by_model)) {
    data <- full_join(data, data_by_model[[j]])
    j = j+1
  }
  
data <- data |> 
  mutate(
bmi_grade_1_m = bmi_lookup_cdc(sex = 1, age = agemos_asess_1, bmiz = -1), 
bmi_grade_1_f = bmi_lookup_cdc(sex = 2, age = agemos_asess_1, bmiz = -1),
bmi_grade_1 = case_when(sex_z == 2 ~ bmi_grade_1_f, 
                          sex_z == 1 ~ bmi_grade_1_m),
bmi_grade_2_m = bmi_lookup_cdc(sex = 1, age = agemos_asess_1, bmiz = -2), 
bmi_grade_2_f = bmi_lookup_cdc(sex = 2, age = agemos_asess_1, bmiz = -2),
bmi_grade_2 = case_when(sex_z == 2 ~ bmi_grade_2_f, 
                        sex_z == 1 ~ bmi_grade_2_m)) |> 
select(-c(starts_with(c('bmi_f', 'bmi_m', 'bmi_lower_f_99', 'bmi_upper_f_99', 'bmi_lower_m_99', 'bmi_upper_m_99', 'bmi_grade_1_m', 'bmi_grade_1_f', 'bmi_grade_2_m', 'bmi_grade_2_f','bmi_lower_f_95', 'bmi_upper_f_95', 'bmi_lower_m_95', 'bmi_upper_m_95')))) 

  return(data)
}


y3 <- add_bmi_vars(y)

