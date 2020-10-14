library(tidyr)
library(dplyr)
library(tidytext)
library(fmsb)
library(data.table)
library(wesanderson)
beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')

taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')

sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')

charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')


sales[sales$title == "Reputation", ]$title <- "reputation" 
released <- select(
    sales[sales$country == "US", ], title, released) %>% mutate(year = year(as.Date(released, "%B %d, %Y")))

taylor_swift_lyrics <- taylor_swift_lyrics %>%
    left_join(select(released, title, year), by = c("Album"= "title"))

taylor_swift_lyrics[taylor_swift_lyrics$Album == "folklore", ]$year <- 2020






# Tokenize
tw_tokens <- taylor_swift_lyrics %>% 
    unnest_tokens(output = word, input = Lyrics)# %>%

tw_tokens %>%
    group_by(Artist, Album, Title) %>%
    summarize(length = n())

# Sentiment lexicon
nrc_dict <- get_sentiments("nrc")

tw_tokens <- tw_tokens %>% 
    left_join(nrc_dict, by = "word") 

tw_tokens %>%
    filter(is.na(sentiment)) %>%
    group_by(word) %>%
    summarize(count = n()) %>%
    arrange(-count) %>%
    head(30)

tw_total_sentiment <- tw_tokens %>%
    filter(!is.na(sentiment)) %>%
    group_by(sentiment) %>%
    summarize(`Taylor Swift` = n()) %>%
    arrange(-`Taylor Swift`) %>%
    mutate(`Taylor Swift` = `Taylor Swift`/sum(`Taylor Swift`))


#  Taylor Swift by album
tw_album_sentiment <- tw_tokens %>%
    filter(!is.na(sentiment)) %>%
    group_by(Album, year, sentiment) %>%
    summarize(count = n()) %>%
    arrange(year,-count)
tw_album_count <- tw_tokens %>%
   filter(!is.na(sentiment)) %>%
   group_by(Album) %>%
   summarize(total.count = n()) 


library(ggiraphExtra)
library(ggplot2)

tw_plot_data <- 
    merge(tw_album_sentiment, tw_album_count, by = c("Album")) %>%
    mutate(sentiment.pct = count/total.count) %>%
    reshape2::dcast(value.var = "sentiment.pct", Album+year~sentiment) 



ordered.albums <- tw_plot_data %>% 
    arrange(year) %>% 
    select(Album) 
ordered.albums <- ordered.albums$Album

tw_plot_data$Album <- factor(tw_plot_data$Album, levels = ordered.albums)

colors <- c( wes_palette("Darjeeling1"), wes_palette("GrandBudapest1"))

ggRadar(tw_plot_data, aes(color = Album, facet = year), ncol = 8) +
     scale_fill_manual(values = colors) +
     scale_color_manual(values = colors)

ggsave(filename = "taylor_swift.png", width = 35, height = 35, unit = "cm")






