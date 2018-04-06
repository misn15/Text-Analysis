library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(scales)
library(tidyr)


## Read in text file
Clinton <- read.delim("clinton first state of the union.txt", header=TRUE, stringsAsFactors=FALSE)
Nixon <- read.delim("nixon first state of the union.txt", header=TRUE, stringsAsFactors=FALSE)

## Create a tibble with text
clint_df <- Clinton %>% 
  tbl_df()

nix_df <- Nixon %>%
  tbl_df()

## Create columns 
clint_clean <- setNames(clint_df, "text") %>%
  tibble::rowid_to_column("linenumber") 

nix_clean <- setNames(nix_df, "text") %>%
  tibble::rowid_to_column("linenumber") 

## Make each word a different row
tidy_clint <- clint_clean %>% unnest_tokens(word, text)

tidy_nix <- nix_clean %>% unnest_tokens(word, text)

## Delete stop words
data(stop_words)

clint_stop <- tidy_clint %>%
  anti_join(stop_words)

nix_stop <- tidy_nix %>%
  anti_join(stop_words)

## Count the number of times words appear
clint_stop %>%
  count(word, sort = TRUE) 

nix_stop %>%
  count(word, sort = TRUE)

## Graph the count of each word

nix_stop %>%
  count(word, sort = TRUE) %>%
  filter(n > 11) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

clint_stop %>%
  count(word, sort = TRUE) %>%
  filter(n > 18) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Get frequencies of top words and graph 

clint_freq <- mutate(clint_stop) %>%
  count(word) %>%
  mutate(proportion = n / sum(n)) %>%
  filter(proportion > 0.008)

nix_freq <- mutate(nix_stop) %>%
  count(word) %>%
  mutate(proportion = n / sum(n)) %>%
  filter(n > 11)

ggplot(clint_freq, aes(y= proportion, x = word)) +
  geom_col() +
  coord_flip() 

ggplot(nix_freq, aes(y= proportion, x = word)) +
  geom_col() +
  coord_flip() 


#total_freq <- bind_rows(mutate(nix_freq, author = "Clinton"),
#                       mutate(clint_freq, author = "Nixon")) 

# Sentiment analysis

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

clint_stop %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

nix_stop %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


## Compare two speeches
frequency <- bind_rows(mutate(clint_stop, author = "Clinton"),
                       mutate(nix_stop, author = "Nixon")) %>% 
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n)

clinton_sentiment <- clint_stop %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, index = linenumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
