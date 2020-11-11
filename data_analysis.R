library(ggplot2)

full_dataset <- group_by(full_dataset, 'S_700')

#Possibilities for the categorical variables throughout the years. Should probably use
#proportions instead of counts. Maybe use dodge?
full_dataset %>% 
  group_by(S_700, X_002) %>%
  count() %>%
ggplot() +
  geom_col(aes(x = as.factor(S_700), y = n, fill = as.factor(X_002)))

#Boxplot for the scales so I can try seeing all observations at once.
full_dataset %>%
  filter(A_003_031 > 0) %>%
ggplot() +
  geom_boxplot(aes(x = as.factor(X_002), y = A_003_031)) +
  facet_wrap(~S_700)

#Line of averages for the scales. Easier visualization for time changes.
#Should probably filter the religions in which I am most theoretically interested.
full_dataset %>%
  filter(A_003_031 > 0) %>%
  group_by(X_002, S_700) %>%
  mutate(mean = mean(A_003_031)) %>%
  ggplot() +
  geom_line(aes(x = X_002, y = mean)) +
  facet_wrap(~S_700)