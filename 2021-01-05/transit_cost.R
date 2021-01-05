library(tidytuesdayR)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ISOcodes)
library(reshape2)
library(cowplot)
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv') 

transit_cost <- filter(transit_cost, !is.na(e))

summary(transit_cost)
glimpse(transit_cost)

unique(transit_cost$country)
names(transit_cost)
transit_cost <- 
  dplyr::left_join(transit_cost, 
                  iso3166, 
                by = c("country" = "a2"))
transit_cost$real_cost <- as.numeric(transit_cost$real_cost)
summary(transit_cost)
glimpse(transit_cost)
unique(transit_cost$ISOname)

SEA.countries <- c("Vietnam", 
                   "Singapore", 
                   "Philippines",
                   "Indonesia",
                   "Bangladesh",
                   "Thailand",
                   "Malaysia")

SEA.data <- transit_cost %>%
  filter(ISOname %in% SEA.countries) %>%
  mutate(country = factor(ISOname, levels = SEA.countries)) %>%
  group_by(country) %>%
  summarise(cost.usd = sum(real_cost), length = sum(length), tunnel = sum(tunnel), start_year = min(start_year), end_year = max(end_year, na.rm = T)) %>%
  mutate(incomplete = length - tunnel) %>%
  mutate(cost.usd.per.km = cost.usd/length) 

theme_ <- theme_bw()
theme_$plot.margin <- margin(r = -0.8, unit = "cm")
theme_set(theme_)

p1 <- SEA.data %>%
  # filter(variable %in% c("length", "incomplete")) %>%
  ggplot(aes(x = country)) +
  geom_bar(aes(y = length), fill = "grey" , stat = "identity", width = .1) +
  geom_errorbar(aes(y = tunnel, ymax = tunnel, ymin = tunnel), 
                width = .2, color = "blue") +
  # geom_bar(aes(y = length, fill = "Proposed"), stat = "identity", width = .1) +
  coord_flip() +
  # scale_fill_manual(values = c(Proposed = "grey", Complete = "blue")) +
  geom_text(aes(y = tunnel, label = scales::percent(tunnel/length)), vjust = 2, size = 3) +
  annotate("text", x = first(SEA.data$country), y = first(SEA.data$tunnel) , label = "completed" , vjust = 1.8, hjust = 1.4, size = 3, parse = T) +
  labs(y = "km", x = element_blank()) 
  # scale_color_manual(values = c(Proposed = "grey", Complete = "blue"))

p2 <- 
  SEA.data %>%
  mutate(cost.usd.per.km = cost.usd/length) %>%
  ggplot(aes(x = country)) +
  geom_bar(aes(y = cost.usd.per.km, x = country),fill = "grey", stat = "identity", width = .1) +
  geom_text(aes( y = cost.usd.per.km, label = paste(round(cost.usd.per.km),  "mil.")), hjust = "left") + 
  coord_flip() +
  theme(axis.title.y =element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(limits = c(0, 800)) +
  labs(y = "USD mil./km")

p3 <- 
  SEA.data %>%
  # mutate(cost.usd.per.km = cost.usd/length) %>%
  ggplot(aes(x = country)) +
  geom_bar(aes(y = cost.usd/1000, x = country), fill = "grey", stat = "identity", width = .1) +
  geom_text(aes( y = cost.usd/1000, label = paste(round(cost.usd/1000),  "bil.")), hjust = "left") + 
  coord_flip() +
  theme(axis.title.y =element_blank(),
      axis.text.y =element_blank(),
      axis.ticks.y = element_blank()) +
  labs(y = "USD bil.")

plot_grid(p1, p2, p3, nrow = 1, align = "v", labels = c("Length", "Cost per km", "Total cost"),
          label_x = c(0.25, 0.15, 0.18))
ggsave(filename = "transit_Cost_sea.png", height = 15, width = 40, units = "cm")

