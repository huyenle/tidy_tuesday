library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(waffle)
library(wesanderson)
library(xkcd)

holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv', na = 'NA', 
                            col_types = cols(date_parsed = col_date("%Y-%m-%d"))
                            )

table(holidays$year_of_event, holidays$independence_from)  
table(holidays$independence_from)  %>% sort
holidays %>%
  group_by(year) %>%
  summarise(n_countries = n())

holidays %>%
  group_by(country) %>%
  summarise(n = n(), text = paste(name_of_holiday, collapse = ',')) %>%
  filter(n > 1) %>% View

# Select the later days
holidays <- holidays %>%
  group_by(country) %>%
  slice_max(order_by = date_parsed)
# Clean country
holidays <-holidays %>%
  mutate(cleaned_ind_from = ifelse(
    grepl(independence_from, pattern = 'and'), 
    last(unlist(strsplit(independence_from, " , | and ", fixed = F))),
    independence_from)
)
# Remove weird thing in independence_from
holidays$cleaned_ind_from <- 
  gsub(pattern = '\\[\\d+\\]', 
     replacement= "", 
     x = holidays$cleaned_ind_from, 
     fixed = F) %>%
  recode("United Netherlands" = "Netherlands",
         "the United Kingdom" = "United Kingdom",
         "Spanish Empire" = "Spain",
         "the British Mandate for Palestine"= "United Kingdom",
         "Soviet Union)" = "Soviet Union",
         "Socialist Federal Republic of Yugoslavia" = "SFR Yugoslavia",
         "Qing China" = "China",
         "Nazi Germany"  = "Germany",
         "Kingdom of Great Britain" = "United Kingdom",
         "German Empire"  = "Germany",
         "Empire of Japan"  = "Japan",
         "Russian Soviet Federative Socialist Republic" = "Soviet Union")

# Group year
# holidays$century <- 
#   cut(holidays$year, breaks = seq(1100 - 1, 2100 -1, 100), labels = seq(12, 21, 1))
# holidays$century <- recode(holidays$century, 
#                            "16" = "Centuries 16-18", "17" = "Centuries 16-18", "18" = "Centuries 16-18",
#                            "19" ="Century 19",
#                            "21" = "Century 21")
# # Add the two world wars
significant_years <- c(1960, 1991, 1990, 1821, 1975)
holidays$ww <- ifelse(holidays$year < 1945 | holidays$year > 1999, NA,
                      # ifelse(holidays$year < 1945, "Before 1945",
                             ifelse(holidays$year < 1960 , "1945-1959",
                                    ifelse(holidays$year %in% significant_years, holidays$year,
                                           NA))) %>%
  recode("1990" = "1990-1991", "1991" = "1990-1991")

# holidays$labels <- ifelse(is.na(holidays$ww), as.character(holidays$century), holidays$ww)
# 
# holidays %>%
#   filter(!is.na(ww)) 

# 
plot_data <- holidays %>%
  filter(!is.na(ww))  %>%
  mutate(short_ind_from = if_else(cleaned_ind_from %in% c('Ethiopia', 'New Guinea', 'South Africa', 'Netherlands'), "Others", cleaned_ind_from)) %>%
  group_by(short_ind_from, ww) %>%
  summarise(n = n()) %>%
  filter(short_ind_from != "") %>%
  ungroup() %>%
  # arrange(-n) %>%
  mutate(short_ind_from = factor(short_ind_from,
                                 levels = c(setdiff(unique(short_ind_from), "Others"), "Others"), ordered =T))

# 
plot_ <- ggplot(plot_data, aes(fill = short_ind_from, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 2, flip = TRUE) +
  facet_wrap(~ww, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 2, breaks = seq(0, 30, 2),
                     expand = c(0,0)) +
  coord_equal() +
  labs(title = "Significant periods in the 20th century for the indepedence of many countries",
       x = "Year", y = "#Countries")+ 
  ggthemes::scale_fill_tableau() +
  theme_minimal(base_family = 'xkcd') +
  theme(panel.grid = element_blank(), axis.ticks.y = element_blank()) 
# 
ggsave(plot = plot_, "independence_day.png", width = 40, height = 20, units = "cm")
