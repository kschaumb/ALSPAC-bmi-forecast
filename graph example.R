
ggplot(data = fcast_bypx[[1]], mapping = aes(x = agemos, y = bmi)) +
    geom_point(mapping = aes(agemos, bmi), data = bmi_data_bypx_1[[1]], stat = 'identity', position = 'identity') +
    geom_point(mapping = aes(agemos, bmi), data = bmi_data_bypx_2[[1]], stat = 'identity', position = 'identity', color = 'red') +
    stat_smooth(mapping = aes(x = agemos, y = bmi_upper), col = 'purple', linetype = 'dashed', data = fcast_bypx[[1]],  position = 'identity' ) +
    stat_smooth(mapping = aes(x = agemos, y = bmi_lower), col = 'purple', linetype = 'dashed', data = fcast_bypx[[1]],  position = 'identity' ) + 
    stat_smooth(mapping = aes(x = agemos, y = median_bmi), col = 'orange', linetype = 'dotted', data = bmi_data_by_px_all[[1]], position = 'identity')+
    stat_smooth(mapping = aes(x = agemos, y = ow_bmi), col = 'green', linetype = 'dotted', data = bmi_data_by_px_all[[1]], position = 'identity')+
    stat_smooth(mapping = aes(x = agemos, y = uw_bmi), col = 'blue', linetype = 'dotted', data = bmi_data_by_px_all[[1]], position = 'identity')+
    scale_x_continuous(breaks = 12*0:192, label = m2y(12*0:192), limits = c(24,192)) +
  geom_vline(xintercept = 13*12)
    xlab('Age') + 
    ylim(c(10,30))+
    ylab('BMI') 



