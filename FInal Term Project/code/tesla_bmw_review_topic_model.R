library(rvest)
library(dplyr)
library(tm)
library(topicmodels)
library(ggplot2)
library(tidytext)

# Tesla Reviews
tesla_url <- "https://www.trustpilot.com/review/tesla.com"
tesla_webpage <- read_html(tesla_url)

tesla_reviews <- tesla_webpage %>%
  html_elements(".typography_color-black__wpn7m") %>%
  html_text()

tesla_corpus <- Corpus(VectorSource(tesla_reviews))

tesla_corpus <- tm_map(tesla_corpus, content_transformer(tolower))

tesla_corpus <- tm_map(tesla_corpus, removePunctuation)
tesla_corpus <- tm_map(tesla_corpus, removeNumbers)
tesla_corpus <- tm_map(tesla_corpus, removeWords, stopwords("en"))
tesla_corpus <- tm_map(tesla_corpus, stripWhitespace)

tesla_dtm <- DocumentTermMatrix(tesla_corpus)

tesla_tfidf <- weightTfIdf(tesla_dtm)

tesla_tfidf_matrix <- as.matrix(tesla_tfidf)

head(tesla_tfidf_matrix[1:5, 1:5])

tesla_num_topics <- 15

tesla_lda_model <- LDA(tesla_dtm, k = tesla_num_topics, control = list(seed = 1234))

tesla_term_probs <- tidy(tesla_lda_model)

print(tesla_term_probs)

tesla_top_terms <- tesla_term_probs %>%
  group_by(topic) %>%
  arrange(desc(beta)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic))

ggplot(tesla_top_terms, aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Word Probabilities by Topic (Tesla)",
       x = "Keywords/Terms",
       y = "Probability (Beta)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))
