library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

glimpse(mobile)
glimpse(landline)
identical(
    unique(mobile$code),
    unique(landline$code))

merged.data <- merge(mobile, landline, by = c("code", "year"))

merged.data <- 
    merged.data %>%
        mutate(total_pop = ifelse(is.na(total_pop.x), total_pop.y, total_pop.x)) %>%
        mutate(gdp_per_cap = ifelse(is.na(gdp_per_cap.x), gdp_per_cap.y, gdp_per_cap.x)) %>%
        mutate(continent = continent.x) %>%
        mutate(entity = entity.x) %>%
        select(code, year, entity, mobile_subs, landline_subs, continent, total_pop, gdp_per_cap)

summary(merged.data)

merged.data %>%
    ggplot(aes(x = gdp_per_cap, y = mobile_subs)) +
        geom_point(aes( size = total_pop, color = continent)) +
        geom_smooth(method = lm, se = FALSE, fullrange = TRUE, linetype = "dashed", ) +
        facet_wrap(year ~. )
merged.data %>%
    ggplot(aes(x = gdp_per_cap, y = landline_subs)) +
        geom_point(aes( size = total_pop, color = continent)) +
        geom_smooth(method = lm, se = FALSE, fullrange = TRUE, linetype = "dashed", ) +
        facet_wrap(year ~. )
vn.plot <- merged.data %>%
    filter(entity == 'Vietnam') %>%
    ggplot() +
    geom_point(aes(x = year, y = landline_subs, color = "Landline")) +
    geom_point(aes(x = year, y = mobile_subs, color = "Mobile")) +
    scale_x_continuous(breaks = seq(1990, 2017, 1)) +
    scale_y_continuous(breaks = seq(0, 150, 10)) +
    annotate(geom = "text", x = 2006, y = 30, label = "Vinaphone was\nthe first\nto cover\n100% Vietnam districts") +
    annotate(geom = "text", x = 2009, y = 106, label = "Vinaphone \nintroduced\n 3G") +
    annotate(geom = "text", x = 2017, y = 133, label = "Viettel \nintroduced\n 4G") +
    annotate(geom = "text", x = 2018, y = 8, label = "bold(LANDLINE)", color = "#244747", parse = T) +
    annotate(geom = "text", x = 2013, y = 138, label = "bold(MOBILE)", color = "#e3120b", parse = T) +
    labs(x = element_blank(), y = element_blank(), title = "Mobile take-over in Vietnam", subtitle = "Subcriptions per 100 pp.") +
    theme_fivethirtyeight() +
    theme(legend.position = "None") +
    scale_color_manual(values = c("Landline" = "#244747", "Mobile" = "#e3120b"))
    

ggsave(width = 15, height = 10, filename = "Vietnam_mobile_take_over.png")