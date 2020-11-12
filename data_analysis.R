library(ggplot2)
library(RColorBrewer)

evangelicals <- c(2, 3, 4, 5)
main_religions <- c(1, 2, 97)
dataset_condensed <- mutate(full_dataset,
                            religion_condensed = ifelse(S_700 %in% evangelicals, 2, S_700))

year_count_table <- full_dataset %>%
  group_by(X_002) %>%
  count() %>%
  rename('year_count' = n)

religion_count <- dataset_condensed %>%
  group_by(religion_condensed, X_002) %>%
  count() %>%
  left_join(year_count_table, by = 'X_002') %>%
  rename(religion_obs = n) %>%
  mutate(religion_yr_prop = religion_obs / year_count) %>%
  filter(religion_yr_prop >= 0.05)

ggplot(religion_count) +
  geom_col(aes(x = as.factor(X_002), y = religion_yr_prop,
               fill = as.factor(religion_condensed)), position = "dodge") +
  labs(x = 'Year', y = "Proportion",
       fill = "Religious Group",
       title = "Proportion of Religious Group Members to the General Population by Year") +
  scale_fill_brewer(palette = 'Dark2',
                    labels = c('1' = 'Catholic', '2' = 'Evangelical', '97' = 'No Religion'))

support_democracy_count <- dataset_condensed %>%
  group_by(religion_condensed, X_002, A_001_001) %>%
  count() %>%
  inner_join(religion_count) %>%
  mutate(dem_support_prop = n / religion_obs) %>%
  filter(religion_condensed %in% main_religions & A_001_001 %in% c(1, 2, 3))

ggplot(support_democracy_count) +
  geom_line(aes(x = X_002, y = dem_support_prop,
               color = as.factor(religion_condensed))) +
  facet_wrap(~A_001_001) +
  labs(x = 'Year', y = "Proportion",
       color = "Religious Group",
       title = "Proportion of Democracy Supporters within each Religious Group by Year") +
  scale_color_brewer(palette = 'Dark2',
                     labels = c('1' = 'Catholic', '2' = 'Evangelical', '97' = 'No Religion'))

#Line of averages for the scales. Easier visualization for time changes.
dataset_condensed %>%
  filter(A_003_031 > 0 & S_700 %in% main_religions) %>%
  group_by(X_002, religion_condensed) %>%
  mutate(mean = mean(A_003_031)) %>%
  ggplot() +
  geom_line(aes(x = X_002, y = mean, color = as.factor(religion_condensed))) +
  labs(x = 'Year', y = "Mean",
       color = "Religious Group",
       title = "Mean of Satisfaction with Democracy within each Religious Group by Year") +
  scale_color_brewer(palette = 'Dark2', 
                     labels = c('1' = 'Catholic', '2' = 'Evangelical', '97' = 'No Religion'))