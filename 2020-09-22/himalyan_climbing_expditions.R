library(ggplot2)
library(tidyr)
library(dplyr)
members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

# number of expeditions with deaths, injured and success
count.cases <- members %>%
    group_by(expedition_id) %>%
    summarise(n.deaths = sum(died), n.injuries  = sum(injured), n.success = sum(success))

expeditions <- 
    mutate(expeditions, season = factor(season, level = c("Spring", "Summer", "Autumn", "Winter"))) %>%
    filter(!is.na(season))

merged.count.cases <- 
    merge(count.cases, expeditions, by = "expedition_id", all = F)

# summarised tables
summarised.table <- merged.count.cases %>%
    group_by(year, season) %>%
    summarise(`Expeditions`s = n(),
              `Exp. w. deaths` = sum(n.deaths > 0), 
              `Pct.  Exp. w. deaths` = sum(n.deaths > 0) / n() * 100, 
              `Exp. w. injuries` = sum(n.injuries),
              `Pct. w. injuries` = sum(n.injuries) / n() * 100,
              `Success` = sum(n.success >0),
              `Pct. success` = sum(n.success > 0)/n(),
              `Avg.  Exp. w. deaths` = mean(n.deaths), 
              `Avg.  Exp. w. injuries` = mean(n.injuries))
summarised.season.table <- merged.count.cases %>%
    group_by(season) %>%
    summarise(`Expeditions` = n(),
              `Exp. w. deaths` = sum(n.deaths > 0), 
              `Pct.  Exp. w. deaths` = sum(n.deaths > 0) / n(), 
              `Exp. w.  injuries` = sum(n.injuries),
              `Pct. w. injuries` = sum(n.injuries) / n(),
              `Success` = sum(n.success >0),
              `Pct. success` = sum(n.success > 0)/n(),
              `Avg.  Exp. w. deaths` = mean(n.deaths), 
              `Avg.  Exp. w. injuries` = mean(n.injuries))


# Charts
summarised.table %>%
    mutate(year_plot = 
        year + ifelse(season == "Spring", 0.2, 
                    ifelse(season == "Summer", 0.4, 
                        ifelse(season == "Autumn", 0.6, 
                            0.8)))) %>%
    # filter(`Expeditions`s != 1) %>%
    ggplot(aes(x = year, y = `Exp. w. deaths`, col = season)) +
    geom_point() 

plot <- 
    summarised.season.table %>%
        gather(key = "metrics", value = "value",  -season) %>% 
        ggplot(aes(x = season, y = value, fill = season)) +
        geom_bar(stat = "identity") + 
        facet_wrap(metrics ~ ., scale = "free") 

ggsave(plot, filename = "seasonal_hiking_pattern.png", width = 10, height = 10)





