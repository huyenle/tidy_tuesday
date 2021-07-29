library(dplyr)
library(tidyr)
library(ggplot2)
library(waffle)
library(emojifont)
library(ggdark)
library(ggthemes)
# library(ggimage)

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')
summary(olympics)
glimpse(olympics)

vietnam <- olympics %>% filter(team == 'Vietnam')
head(vietnam)
vietnam$sport %>% unique()

hot.sports <- c("Shooting", "Taekwondo", "Weightlifting", "Swimming", "Athletics", "Gymnastics")


sport.year.data <- vietnam %>%
  mutate(sport = if_else(!sport %in% hot.sports, "Others", sport)) %>%
  count(sport, year, medal) %>%
  mutate(medal = replace_na(data = medal, replace = 'No medal')) %>%
  mutate(sport = factor(sport, c(hot.sports, "Others")), medal = factor(medal, levels = c("Gold", "Silver", 'No medal'))) %>%
  arrange(year, medal, n) 

font.size = 30

final <- sport.year.data %>% 
  ggplot(aes(fill = sport, linetype = medal, color = medal, 
             y = n, x = factor(year))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_linetype_manual(values = c(Gold = "solid", Silver = "solid", `No medal` = 'blank')) +
  scale_color_manual(values = c(Gold = "black", Silver = "black", `No medal` = "white")) +
  geom_text(x = 9.5, y = 34, label ='Hoang Xuan Vinh', color = "black", size = font.size/2) +
  geom_text(x = 10.5, y = 37, label =emoji('1st_place_medal'), 
            color = '#F97A1F', family='EmojiOne', size = font.size) +
  geom_text(x = 10.5, y = 38, label =emoji('2nd_place_medal'), 
            color = '#141F52', family='EmojiOne', size = font.size)+
  geom_text(x = 5.2, y = 10, label ='Tran Hieu Ngan', color = "black", size = font.size/2) +
  geom_text(x = 6.2, y = 13, label =emoji('2nd_place_medal'), 
            color = '#141F52', family='EmojiOne', size = font.size)+
  geom_text(x = 7.2, y = 21.5, label ='Hoang Anh Tuan', color = "black", size = font.size/2)+
  geom_text(x =8.2, y = 24.5,  label =emoji('2nd_place_medal'), 
            color = '#141F52', family='EmojiOne', size = font.size)+
  coord_flip() +
  theme_economist()+
  scale_fill_manual( values = c("#23576E", "#F9C31F", 
                                 "#F97A1F", "#208F84", 
                                 "#E2365B", "#924F3E",
                                "#B3B3B3"))+
  theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(size = font.size*1.5),legend.justification='left') +
  labs(x = element_blank(), y = "Number of athletes", 
       title = "Vietnam Olympics",
       subtitle = "Since 1980, swimming is shrinking, shooting and weight-lifting are gaining momentum") +
  guides(linetype = "none", color = ) 

final

ggsave(plot = final, "vn_olypics.png", width = 40, height = 20, units = "cm")
  
 

