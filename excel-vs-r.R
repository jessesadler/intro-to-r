### Analyzing a correspondence network with dplyr and ggplot ###

# This script derives from the brief introduction to R, in the blog post
# Excel vs R: A Brief Introduction to R,
# which can be found at https://www.jessesadler.com/post/excel-vs-r/

library(tidyverse)
library(lubridate)

# Load the data
letters <- read_csv("data/correspondence-data-1585.csv")

### Creating new data frames ###
# Unique correspondents
writers <- distinct(letters, writer)
writers

# Number of correspondents
nrow(distinct(letters, writer))

# Unique sources and destinations in the correspondence
sources <- distinct(letters, source)
destinations <- distinct(letters, destination)

# First attempt to get letters per correspondent
per_correspondent <- summarise(letters, count = n())

# Letters per correspondent
per_correspondent <- letters %>% 
  group_by(writer) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Letters per source
per_source <- letters %>% 
  group_by(source) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

## ggplot charts of letters per source and correspondent ##

# Per source barchart
ggplot(data = per_source) +
  geom_bar(aes(x = source, y = count), stat = "identity") +
  labs(x = NULL, y = "Letters written")

# Per correspondent barchart
ggplot(data = letters) +
  geom_bar(aes(x = writer)) +
  labs(x = NULL, y = "Letters written") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

## Working with dates ##

# Letters per month without labels
per_month <- letters %>% 
  mutate(month = month(date)) %>% 
  group_by(month) %>% 
  summarise(count = n())

# Letters per month with labels
per_month <- letters %>% 
  mutate(month = month(date, label = TRUE)) %>% 
  group_by(month) %>% 
  summarise(count = n())

# Letters per month barchart
ggplot(data = per_month) +
  geom_bar(aes(x = month, y = count), stat = "identity") +
  labs(x = 1585, y = "Letters sent")

# Letters per weekday
per_wday <- letters %>% 
  mutate(wday = wday(date, label = TRUE)) %>% 
  group_by(wday) %>% 
  summarise(count = n())

# Letters per weekday barchart
ggplot(data = letters) +
  geom_bar(aes(x = wday(date, label = TRUE))) +
  labs(x = 1585, y = "Letters sent")

# Letters from Sunday
letters %>% filter(wday(date) == 1)

## Combining data for correspondents and months ##

# Per correspondent by month
correspondent_month <- letters %>% 
  mutate(month = month(date, label = TRUE)) %>% 
  group_by(writer, month) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Per correspondent by month barchart
ggplot(data = correspondent_month) +
  geom_bar(aes(x = month, y = count, fill = writer), stat = "identity") +
  labs(x = 1585, y = "Letters sent", fill = "Correspondents")

# Per correspondent by month line chart
ggplot(data = correspondent_month, aes(x = month, y = count, color = writer)) + 
  geom_point(size = 3) + 
  geom_line(aes(group = writer)) + 
  labs(x = 1585, y = "Letters sent", color = "Correspondents")