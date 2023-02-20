# This contains functions used to make tables and graphs employed throughout the analysis.

#makes frequency tables for each variable by low weight status and sex
frq_table_by_wt_sex <- function(df, x_var) {
  x_var <- enquo(x_var)
  df_1 <- df %>%
    filter(!is.na(!! x_var) & !is.na(sex_z)) %>%
    select (!! x_var, sex_z, low_wt_category) %>%
    mutate (sex_z = as_factor(sex_z)) %>%
    mutate( !!x_var := as_factor(!!x_var)) %>%
    group_by(!!x_var, sex_z, low_wt_category) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(sex_z, low_wt_category) %>%
    mutate(pct = prop.table(n)) %>%
    ungroup()
  return(df_1)
  
}


#makes frequency tables for each variable by high weight status and sex
frq_table_by_wt_sex_high <- function(df, x_var) {
  x_var <- enquo(x_var)
  df_1 <- df %>%
    filter(!is.na(!! x_var) & !is.na(sex_z)) %>%
    select (!! x_var, sex_z, high_wt_category) %>%
    mutate (sex_z = as_factor(sex_z)) %>%
    mutate( !!x_var := as_factor(!!x_var)) %>%
    group_by(!!x_var, sex_z, high_wt_category) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    group_by(sex_z, high_wt_category) %>%
    mutate(pct = prop.table(n)) %>%
    ungroup()
  return(df_1)
  
}



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