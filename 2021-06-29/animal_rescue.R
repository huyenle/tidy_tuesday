library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

animal_rescues %>%
  ggplot()+
  geom_bar(aes(property_type))

animal_rescues$animal_group_parent %>% table(exclude = NULL)

animal_rescues_plot <- animal_rescues %>%
  # filter(grepl(x = special_service_type, pattern = 'Domestic pet', fixed = T)) %>%
  mutate(animal = ifelse(grepl(x = animal_group_parent, fixed = T,pattern = 'Unknown'),
                         yes = 'Others', 
                         no = tolower(animal_group_parent))) 

plot_data <- animal_rescues_plot %>%
  group_by(animal, property_category) %>%
  summarise(n_cases = n()) %>%
  group_by(animal) %>%
  mutate(rank = rank(-n_cases, ties.method = "first")) %>%
  mutate(place = if_else(rank >= 5, "Others", property_category)) %>%
  group_by(animal, place) %>%
  summarise(n_cases = sum(n_cases)) %>%
  arrange(animal,- n_cases) 

plot_animals <- 
  plot_data %>% 
  group_by(animal) %>%
  summarise(n_rows = n()) %>%
  filter(n_rows > 3) %>%
  select(animal) %>%
  unlist()

# plot_data <- plot_data %>%
#   filter(animal %in% plot_animals) 

plot_fn <- function(d_, a_) {
  ggplot(d_) + 
    geom_col(aes(x = place, y = n_cases)) +
    labs(title = a_, y = 'cases', x = element_blank()) +
    coord_flip() 
} 

plots <- plot_animals %>% 
  lapply(function(x) filter(plot_data, animal == x) %>% 
           plot_fn(x))

