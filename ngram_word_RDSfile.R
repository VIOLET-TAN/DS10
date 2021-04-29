# LOAD PACKAGES

suppressPackageStartupMessages({
  library(tidytext)
  library(tidyverse)
  library(stringr)
  library(knitr)
})

#LOAD DATA

#' English Repository Files
blogs_file   <- "./final/en_US/en_US.blogs.txt"
news_file    <- "./final/en_US/en_US.news.txt"
twitter_file <- "./final/en_US/en_US.twitter.txt"  
```
#READ DATA FROM FILES LISTED ABOVE

blogs   <- readLines(blogs_file, skipNul = TRUE)
news    <- readLines(news_file,  skipNul = TRUE)
twitter <- readLines(twitter_file, skipNul = TRUE)

#CREATE DATA FRAMES FOR ABOVE DATA

blogs   <- data_frame(text = blogs)
news    <- data_frame(text = news)
twitter <- data_frame(text = twitter)

#DATA SAMPLING

set.seed(1000000)
sample_portion <- 0.10

blogs_sample <- blogs %>%
  sample_n(., nrow(blogs)*sample_portion)
news_sample <- news %>%
  sample_n(., nrow(news)*sample_portion)
twitter_sample <- twitter %>%
  sample_n(., nrow(twitter)*sample_portion)

#CREATE NEAT REPO

repo_sample <- bind_rows(mutate(blogs_sample, source = "blogs"),
                         mutate(news_sample,  source = "news"),
                         mutate(twitter_sample, source = "twitter")) 
repo_sample$source <- as.factor(repo_sample$source)

#REMOVE FILES

rm(list = c("blogs", "blogs_file", "blogs_sample","news", "news_file",     
            "news_sample", "sample_portion", "twitter","twitter_file", 
            "twitter_sample"))

# Create filters: 
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  

#' Clean sample data with filters above
clean_sample <-  repo_sample %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>% 
  mutate(text = iconv(text, "ASCII//TRANSLIT"))
rm(list = c("repo_sample"))

# CREATE N_GRAM REPOS


# 1. Bigrams
bigram_repo <- clean_sample  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# 2. Trigrams
trigram_repo <- clean_sample  %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

# 3. Quadgrams
quadgram_repo <- clean_sample  %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4)

# 4. Pentagrams
pentagram_repo <- clean_sample  %>%
  unnest_tokens(pentagram, text, token = "ngrams", n = 5)

# 5. Hexatgrams
hexagram_repo <- clean_sample  %>%
  unnest_tokens(hexagram, text, token = "ngrams", n = 6)


# FILTERING NGRAMS TO REDUCE SIZE

#' Bigrams
bigram_cover <- bigram_repo %>%
  count(bigram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("bigram_repo"))

#' Trigrams
trigram_cover <- trigram_repo %>%
  count(trigram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("trigram_repo"))

#' Quadgrams
quadgram_cover <- quadgram_repo %>%
  count(quadgram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("quadgram_repo"))

# Pentagrams
pentagram_cover <- pentagram_repo %>%
  count(pentagram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("pentagram_repo"))

#' Hexagrams
hexagram_cover <- hexagram_repo %>%
  count(hexagram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("hexagram_repo"))

# NGRAM DISTRIBUTION PLOT

disty <- data_frame(ngram = c(rep("bigrams",  nrow(bigram_cover)),
                              rep("trigrams",  nrow(trigram_cover)),
                              rep("quadgrams", nrow(quadgram_cover)),
                              rep("pentagrams", nrow(pentagram_cover)),
                              rep("hexagrams",  nrow(hexagram_cover))),
                    number = c(bigram_cover$n,  trigram_cover$n, 
                               quadgram_cover$n, pentagram_cover$n,
                               hexagram_cover$n))
disty
disty$ngram <- as.factor(disty$ngram)
ggplot(data = disty, aes(y = number, x = reorder(ngram, -number))) +
  geom_boxplot() + scale_y_log10() +
  xlab("ngram")
#ggsave("./ngram_boxplot.png")

hexagram_cover %>%
  top_n(15, n) %>%
  mutate(hexagram = reorder(hexagram, n)) %>%
  ggplot(aes(hexagram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Hexagrams")
#ggsave("./hexagrams.png")

pentagram_cover %>%
  top_n(15, n) %>%
  mutate(pentagram = reorder(pentagram, n)) %>%
  ggplot(aes(pentagram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Pentagrams")
#ggsave("./pentagrams.png")

quadgram_cover %>%
  top_n(15, n) %>%
  mutate(quadgram = reorder(quadgram, n)) %>%
  ggplot(aes(quadgram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Quadgrams")
#ggsave("./quadgrams.png")

trigram_cover %>%
  top_n(15, n) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Trigrams")
#ggsave("./trigrams.png")

bigram_cover %>%
  top_n(15, n) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Bigrams")
# ggsave("./bigrams.png")

# SEPARATE NGRAM WORDS

bi_words <- bigram_cover %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bi_words

tri_words <- trigram_cover %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
tri_words

quad_words <- quadgram_cover %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")
quad_words

penta_words <- pentagram_cover %>%
  separate(pentagram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")
penta_words

hexa_words <- hexagram_cover %>%
  separate(hexagram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")
hexa_words


# SAVE WORD FILES FOR SHINY APP

saveRDS(bi_words, "./bi_words.rds")
saveRDS(tri_words, "./tri_words.rds")
saveRDS(quad_words,"./quad_words.rds")
saveRDS(penta_words,"./penta_words.rds")
saveRDS(hexa_words,"./hexa_words.rds")
#' 
#' -------------
#'  
# Session info 
sessionInfo()

