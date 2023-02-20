library(cgwtools)
load('data/ALSPAC_Cleaned.RData') 

# Adds in columns combining clinic and pub data, and specifying type of assessment, filters only rows with available data

bmi_df <- ALSPAC_cleaned$BMI |> 
  #creates new variables that combine clinic and puberty data
  mutate(bmiz = if_else( !is.na(bmiz_clinic), bmiz_clinic, bmiz_pub)) |> 
  mutate(assess_type = case_when(!is.na(bmiz_clinic) ~ 'clinic', !is.na(bmiz_pub) ~ 'pub_q')) |> 
  mutate(agemos_asess_1 = case_when(!is.na(bmiz_clinic) ~ as.numeric (agemos_clinic),!is.na(bmiz_pub) ~ as.numeric(agemos_pub))) |> 
  filter(!is.na(agemos_asess_1)) |> 
  ## removes pubQ assessment measurment if done in same month as clinic assessment
  arrange(assess_type) |> 
  distinct(agemos_asess_1, id, .keep_all = TRUE) 

save(bmi_df, file = 'data/fcast_data.RData')

#selects ages before 13yo to get a u13 dataset
bmi_df_u13 <- bmi_df |> 
  filter(agemos_asess_1 < 13*12)

#identifies the sparse variables (only 1-3 bmi measurements) and removes these participants
n_lows <-bmi_df_u13 |> count(id) |> 
  filter(n< 4)
sparsen <- n_lows$id
bmi_df_u13 <- bmi_df_u13 |> 
  filter(!id %in% sparsen)

resave(bmi_df_u13, file =  'data/fcast_data.RData' )


## creates tsibble with id as participant key
ts_data <- bmi_df_u13 |> 
  select(id, sex_z, agemos_asess_1, assess_type, bmiz) |> 
  as_tsibble(index = agemos_asess_1, key = id) |> 
  tsibble::fill_gaps() 

#creates fit data based on a few models THIS TAKES FOREVER and creates over a million obs 
bmiz_fit_13 <- ts_data |> 
  model(Mean = MEAN(bmiz), #Mean of all measurements
        RW = ARIMA(bmiz ~ 0 + pdq(0,1,0))) %>%  # A 'Naive' Model which uses the last observation and includes a random walk from the last obs
  mutate(M1 = (RW + Mean)/2) |> #Combines the RW model with the Mean model, such that predictions are between mean and most recent
  forecast(h = 220)  %>% #forecasts across 220 months - e.g. 18 years 
  filter(agemos_asess_1 <240) #removes ages > 20, assumes 19.9 is terminal age

resave(bmiz_fit_13, file = 'data/fcast_data.RData') # Saves model fits 

#This also takes a long time - applies the model fit to create 95 and 99% prediction intervals 
fcast_raw <- bmiz_fit_13 %>% 
  hilo(level = c(95,99)) #choosing 95% and 99% confidence bands

resave(fcast_raw_13, file = 'data/fcast_data.RData') # saves forecasts based on bmi data before age 13
