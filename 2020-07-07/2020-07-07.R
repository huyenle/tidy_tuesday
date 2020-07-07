library(tidytuesdayR)
library(skimr)
library(dplyr)
library(googleVis)
library(ggplot2)
library(countrycode)
library(xkcd)

coffee.ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')
dim(coffee.ratings)
names(coffee.ratings)
summary(coffee.ratings)
skim(coffee.ratings)

unique(coffee.ratings$country_of_origin)
coffee_ratings %>% 
        group_by(country_of_origin) %>% 
        summarise(n.sample = n()) %>%  
        arrange(- n.sample) %>% View 

vietnam.coffee <- 
    coffee.ratings %>%
        filter(country_of_origin == "Vietnam")
plot(gvis(vietnam.coffee))

coffee.ratings <-
    coffee.ratings %>%
        mutate(vietnam = country_of_origin == "Vietnam") %>%
        mutate()
        mutate(id.coffee = seq_along(coffee.ratings$total_cup_points))


vietnam.average <- 
    coffee.ratings %>%
    filter(vietnam) %>%
    select("total_cup_points") %>%
    unlist() %>%
    mean()

overall.average <- 
    coffee.ratings %>%
    select("total_cup_points") %>%
    unlist() %>%
    mean()

total.cup.points.plots <- 
    coffee.ratings %>%
        ggplot()+
        geom_histogram(aes(x=total_cup_points), bins = 50, color = "grey", fill = "white") +
        geom_vline(aes(xintercept = vietnam.average, color = "Vietnam"))+
        geom_text(aes(x = vietnam.average * 0.92, y = 400, label = round(vietnam.average, 2), color = "Vietnam")) +
        geom_vline(aes(xintercept = mean(total_cup_points), color = "Average")) +
        geom_text(aes(x = overall.average * 1.08, y = 400, label = round(overall.average, 2), color = "Average")) +
        labs(title = "Total cupper points")

my.theme <- theme_xkcd()
my.theme$text$size <- 11
theme_set(my.theme)

total.cup.points.plots

ggsave(total.cup.points.plots, filename = "total.cup.points.png", width = 9, height = 9)

for (col.name in c("flavor", "aroma", "cupper_points")){
    message(col.name)
    
    vietnam.average <- 
        coffee.ratings %>%
        filter(vietnam) %>%
        select(col.name) %>%
        unlist() %>%
        mean()

    overall.average <- 
        coffee.ratings %>%
        select(col.name) %>%
        unlist() %>%
        mean()


    plot <- 
        coffee.ratings %>%
            ggplot()+
            geom_histogram(aes_string(x=col.name), bins = 50, color = "grey", fill = "white") +
            geom_vline(aes(xintercept = vietnam.average, color = "Vietnam"))+
            geom_text(aes(x = vietnam.average *0.92, y = 400, label = round(vietnam.average, 2), color = "Vietnam")) +
            geom_vline(aes(xintercept = overall.average, color = "Average")) +
            geom_text(aes(x = overall.average * 1.08, y = 400, label = round(overall.average, 2), color = "Average")) +
            labs(title = tools::toTitleCase(gsub("_", " ", col.name, fixed = TRUE)))

    ggsave(plot, filename = paste0(col.name, ".png"), width = 9, height = 9)
}
