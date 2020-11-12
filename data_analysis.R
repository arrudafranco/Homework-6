library(dplyr)

#Combining the four types of evangelicals was more representative for my purposes.
evangelicals <- c(2, 3, 4, 5)
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
  #Filtering out very small religions.
  filter(religion_yr_prop >= 0.05)

#The main religious groups by far are Catholics, Evangelicals and Non-Religious.
#Thus I created the following list for future reference.
main_religions <- c(1, 2, 97)

support_democracy_count <- dataset_condensed %>%
  group_by(religion_condensed, X_002, A_001_001) %>%
  count() %>%
  inner_join(religion_count, by = c("religion_condensed", "X_002")) %>%
  mutate(dem_support_prop = n / religion_obs) %>%
  #Filtering out NAs and very small religions.
  filter(religion_condensed %in% main_religions & A_001_001 %in% c(1, 2, 3))
