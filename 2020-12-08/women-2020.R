library(dplyr)
library(maps)
library(rbokeh)
women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')
View(women)
summary(women)
table(women$category)
table(women$country)
filter(women, country == "Vietnam")


caps <- dplyr::filter(world.cities, capital == 1)
caps$population <- prettyNum(caps$pop, big.mark = ",")
plot <- 
suppressWarnings(
    figure(width = 800, height = 450, padding_factor = 0) %>%
        ly_map("world", col = "gray") %>%
        ly_points(long, lat, data = caps, size = 5, color = name,
                    hover = c(name, country.etc, population))
                    )


world.cities <- 
    world.cities %>%
        group_by(country.etc) %>%
        mutate(city.rank = rank(-cap, ties.method = "first"))

women<- 
    women %>%
        group_by(country) %>%
        mutate(rank = rank(name))

merged <- merge(women, world.cities,
                by.x = c("country", "rank"),
                by.y = c("country.etc", "city.rank"),
                suffixes = c("", ".city"), all.x = T) 

# Filter missing data
filter(merged, is.na(capital)) %>% select(country) %>% unique  

fixed.names <- 
    c("DR Congo" = "Congo Democratic Republic",
      "South Korea" = "Korea South",
      "Exiled Uighur from Ghulja (in Chinese, Yining)" = "China",
      "Wales, UK" = "UK",
      "Northern Ireland" = "UK",
      "Republic of Ireland" = "Ireland",
      "Somaliland"= "Somalia",
      "US" = "USA",
      "UAE" = "United Arab Emirates",
      "Iraq/UK" = "UK")

women <- 
    women %>%
        mutate(fixed.country = if_else(is.na(fixed.names[country]), country, fixed.names[country])) 

world.cities <- rbind(world.cities,
        data.frame(name = "Hong Kong", country.etc = "Hong Kong", 
            pop = 7451, lat = 22.32, long = 114.17, capital = 1, city.rank = 1)) 

merged <- merge(women, world.cities,
                by.x = c("fixed.country", "rank"), 
                by.y = c("country.etc", "city.rank"),
                suffixes = c("", ".city"), all.x = T) 

plot.data <- merged[!duplicated(select(merged, name, fixed.country)),]

plot.data$formatted.desc <- gsub(x = plot.data$description, 
                                pattern = ".", replacement  = ".\n", fixed = T)

# Chart
color.map <- wes_palette("Moonrise3", n = 5)   
plot <- 
    suppressWarnings(
        figure(width = 1500, height = 1000, padding_factor = 0) %>%
            ly_map("world", col = "lightgray") %>%
            ly_points(long, lat, data = plot.data, size = 10, 
                        color= category, 
                        hover = c(name, formatted.desc)) #%>%
           # set_palette(discrete_color = pal_color(color.map)
)

# TODO:
# titke: "In an extraordinary year, when countless women around the world have made a sacrifice to help others, the first place on the list is left open to acknowledge their work and to remember those who have lost their lives while making a difference
# better color
# fixed position for tooltips

