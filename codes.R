library(tidytext)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(forcats)
library(stringr)
library(readr)

#load data
my.dir <- "/Users/chenzhou/LaTeX_files/IST719/Data/"
profiles <- read.csv(paste0(my.dir, "profiles.csv")
                     , header = TRUE
                     , stringsAsFactors = TRUE)

View(profiles)
colnames(profiles)
colors <- c("#FE4365", "#83AF9B", "#C8C8A9","#FC9D9A", "#F9CDAD")

#The relation between user gender and user age


p <- ggplot(profiles, aes(x = profiles$sex
                          , y = profiles$age
                          , fill = profiles$sex)) +
  geom_violin() +
  scale_fill_manual("Result", values = colors) 

options(scipen = 99)

#distribution of body type
profiles2 <- profiles[!profiles$body_type == -1,]
mytable <- table(profiles2$body_type)
pie(mytable, col = (colors))

#height
library(okcupiddata)
data(profiles)

#View(profiles)
require(mosaic)
favstats(height, data=profiles)
require(dplyr)
profiles.subset <- filter(profiles, height>=55 & height <=80)
histogram(~height | sex, width=1, layout=c(1,2), xlab="Height in inches"
          , data=profiles.subset
          , col = colors)


#distribution of income over education
profiles1 <- profiles[!profiles$income >= 200000,]
profiles1 <- profiles1[!profiles1$income == -1,]
M <- tapply(profiles1$income
            , profiles1$education
            , mean)

df <- as.data.frame.table(M)
df %>%
  mutate(Var1 = fct_reorder(Var1, Freq)) %>%
  ggplot(aes(x = Var1, y = Freq)) + 
    geom_bar(stat = "identity", fill = "#B6BA99", alpha=1, width=.4) +
    coord_flip()

#self description
text_df <- data_frame(text = profiles$essay0)
text_df <- tibble(text = profiles$essay0)
text_df <- tokenize(profiles$essay0)
text_df <- text_df %>% 
  mutate_all(as.character) %>% 
  unnest_tokens(word, text)

text_df <- data_frame(line = 1, text = profiles$essay0)
unigrams <- text_df %>%
  unnest_tokens(word, text)

unigrams <- unigrams %>%
  anti_join(stop_words)

mystopwords <- data_frame(word = c("br", "href", "ilink"))
unigrams <- anti_join(unigrams, mystopwords, by = "word")
library(wordcloud)

unigrams %>%
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100, colors = colors))

  