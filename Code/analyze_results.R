source('1_parameters.R')

find_total = grep(pattern = 'indTotData', x = list.files(path = folder_outputs))
find_length = grep(pattern = 'indLenData', x = list.files(path = folder_outputs))
save_total = list()
save_length = list()
for(i in seq_along(find_total)) { 
  save_total[[i]] = read.csv(file.path(folder_outputs, list.files(path = folder_outputs)[find_total[i]]))
  save_length[[i]] = read.csv(file.path(folder_outputs, list.files(path = folder_outputs)[find_length[i]]))
}

data_total = dplyr::bind_rows(save_total)
data_total$precision = factor(data_total$precision, levels = precisionScenarios$name)
data_length = dplyr::bind_rows(save_length)
data_length$precision = factor(data_length$precision, levels = precisionScenarios$name)


# Plot 1: -----------------------------------------------------------------
ggplot(data_total, aes(x=factor(nObservers), y = error)) +
  geom_boxplot(aes(fill = factor(n_sample), color = factor(n_sample)), alpha = 0.2, outlier.size = 0.5) +
  xlab('Number of observers') +
  ylab('Error') +
  labs(col = "Number fish sampled", fill = "Number fish sampled") +
  theme_bw() +
  scale_fill_brewer(palette = 'Spectral') +
  scale_color_brewer(palette = 'Spectral') +
  theme(legend.position = c(0.85, 0.7), legend.background = element_rect(fill = "transparent"),
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, 'cm'))  +
  facet_wrap(. ~ precision)
ggsave(filename = file.path(folder_figures, 'error_total_nObs.png'),
         width = 190, height = 90, units = 'mm', dpi = 500)

ggplot(data_total, aes(x=factor(nObservers), y = bias)) +
  geom_boxplot(aes(fill = factor(n_sample), color = factor(n_sample)), alpha = 0.2, outlier.size = 0.5) +
  xlab('Number of observers') +
  ylab('Bias %') +
  labs(col = "Number fish sampled", fill = "Number fish sampled") +
  theme_bw() +
  scale_fill_brewer(palette = 'Spectral') +
  scale_color_brewer(palette = 'Spectral') +
  theme(legend.position = c(0.2, 0.25), legend.background = element_rect(fill = "transparent"),
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, 'cm'))  +
  facet_wrap(. ~ precision)
ggsave(filename = file.path(folder_figures, 'bias_total_nObs.png'),
       width = 190, height = 90, units = 'mm', dpi = 500)


# Plot 2 ------------------------------------------------------------------

for(k in 1:nObservers) {
  
  # Plot 2a: Error per length and sex
  tmp_data = data_length %>% 
              dplyr::filter(nObservers == k) %>% 
              dplyr::group_by(fish_lengths, precision, n_sample, gender2) %>% 
              dplyr::summarise(q1 = quantile(error, probs = 0.05), 
                               q2 = quantile(error, probs = 0.5),
                               q3 = quantile(error, probs = 0.95), .groups = 'drop')
  
  
  ggplot(tmp_data, aes(x = fish_lengths )) +
    geom_ribbon(aes(ymin = q1, ymax = q3, fill = factor(gender2)), alpha=0.2) +
    geom_line(aes(y = q2, colour = factor(gender2))) +
    theme_bw() +
    scale_color_manual(values = c('red', 'blue')) +
    theme(legend.position = 'none') +
    xlab('Length (cm)') +
    ylab('Error') +
    facet_grid(factor(n_sample) ~ precision, scales = 'free_y')
  ggsave(filename = file.path(folder_figures, paste0('error_lengths_nObs_', k, '.png')),
             width = 190, height = 230, units = 'mm', dpi = 500)

  
  # Plot 2b: Error per length and sex
  tmp_data = data_length %>% 
    dplyr::filter(nObservers == k) %>% 
    dplyr::group_by(fish_lengths, precision, n_sample, gender2) %>% 
    dplyr::summarise(q1 = quantile(bias, probs = 0.05, na.rm = TRUE), 
                     q2 = quantile(bias, probs = 0.5, na.rm = TRUE),
                     q3 = quantile(bias, probs = 0.95, na.rm = TRUE), .groups = 'drop')
  
  
  ggplot(tmp_data, aes(x = fish_lengths )) +
    geom_ribbon(aes(ymin = q1, ymax = q3, fill = factor(gender2)), alpha=0.2) +
    geom_line(aes(y = q2, colour = factor(gender2))) +
    theme_bw() +
    scale_color_manual(values = c('red', 'blue')) +
    theme(legend.position = 'none') +
    coord_cartesian(ylim = c(-100, 100)) +
    xlab('Length (cm)') +
    ylab('Bias (%)') +
    facet_grid(factor(n_sample) ~ precision, scales = 'free_y')
  ggsave(filename = file.path(folder_figures, paste0('bias_lengths_nObs_', k, '.png')),
         width = 190, height = 230, units = 'mm', dpi = 500)


}
