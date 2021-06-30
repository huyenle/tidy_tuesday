library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(waffle)
library(wesanderson)
library(xkcd)

animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv', na = 'NULL')

animal_rescues %>%
  ggplot()+
  geom_bar(aes(property_type))

animal_rescues$animal_group_parent %>% table(exclude = NULL)

animal_rescues_plot <- animal_rescues %>%
  mutate(animal = ifelse(grepl(x = animal_group_parent, fixed = T,pattern = 'Unknown'),
                         yes = 'OTHERS', 
                         no = toupper(animal_group_parent))) 

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


colorss <- c(
  "Dwelling" = wes_palette("Cavalcanti1")[5],
  "Other Residential" = wes_palette("Moonrise2")[2],
  "Outdoor" = wes_palette("Chevalier1")[1],
  "Outdoor Structure" =  wes_palette("Moonrise2")[1],
  "Non Residential" = wes_palette("Moonrise3")[3],
  "Road Vehicle" = wes_palette("Chevalier1")[2],
  "Others" = wes_palette("Moonrise3")[4]
)

plot_data$place <- factor(plot_data$place, 
                          levels = c("Dwelling", "Other Residential",
                                         "Outdoor", "Outdoor Structure",
                                         "Non Residential", "Road Vehicle",
                                         "Others"))
plot_data$animal <- factor(plot_data$animal, 
                           levels = c(plot_animals[plot_animals!='OTHERS'], 'OTHERS'))

waffle_plot <- plot_data  %>% ggplot(aes(fill = place, values = n_cases)) +
  geom_waffle(color = "white", size=.15, n_rows = 10, flip = T, make_proportional = T) +
  facet_wrap(vars(animal)) +
  theme_xkcd() +
  scale_fill_manual(values =  colorss) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text = element_blank()) +
  labs(title = 'The London Fire brigade rescuing scenes')

# 

plot_data_2 <- animal_rescues_plot %>%
  filter(!is.na(incident_notional_cost)) %>%
  group_by(animal) %>%
  summarise(avg_cost = mean(incident_notional_cost)) %>%
  arrange(-avg_cost) %>%
  top_n(8, wt = avg_cost)

bar_plot <- ggplot(plot_data_2) +
  geom_col(aes(x = reorder(animal, avg_cost), y = avg_cost), 
           fill = wes_palette("Darjeeling2")[3]) +
  coord_flip() +
  theme_xkcd() +
  labs(title = 'Top 8 most expensive animals to rescue',
       y = 'Cost', x = element_blank()) +
  geom_text(x =8, y =100, label = 'Really?', family = 'xkcd')

# 
library("gridExtra")
plot_combined <- grid.arrange(waffle_plot,bar_plot,ncol = 2)  
ggsave(plot = plot_combined, "animal_rescue.png", width = 50, height = 20, units = "cm")

