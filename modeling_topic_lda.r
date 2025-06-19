library(tm)
library(topicmodels)
library(tidytext)
library(broom)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)
library(slam)
library(reshape2)
library(wordcloud)
library(RColorBrewer)


df <- read_csv("/Users/abdullahalmaruf/Downloads/Topic Modeling/cleaned_text_arxiv_preprocessed.csv")


corpus <- VCorpus(VectorSource(df$cleaned_text))


dtm <- DocumentTermMatrix(corpus, control = list(
  wordLengths = c(3, Inf),
  bounds = list(global = c(100, nrow(df) * 0.9))
))
dtm


row_totals <- slam::row_sums(dtm)
dtm <- dtm[row_totals > 0, ]


k <- 10  
lda_model <- LDA(dtm, k = k, method = "Gibbs",
                 control = list(seed = 1234, burnin = 1000, iter = 2000, thin = 100))


topic_labels <- c(
  "Algorithms & Graph Theory",
  "User Studies & HCI",
  "NLP & Large Language Models",
  "Network Systems & Performance",
  "Machine Learning",
  "Deep Learning & Neural Networks",
  "Scientific Research & Applications",
  "Control Systems & Robotics",
  "Mathematical Optimization",
  "Computer Vision & Multimedia"
)


top_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta) %>%
  mutate(topic_label = topic_labels[topic])

top_terms


top_terms %>%
  mutate(term = reorder_within(term, beta, topic_label)) %>%
  ggplot(aes(term, beta, fill = factor(topic_label))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_label, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms per Topic",
       x = "Term", y = "Beta")


write_csv(top_terms, "lda_topic_terms1.csv")


doc_topics <- tidy(lda_model, matrix = "gamma") %>%
  mutate(topic_label = topic_labels[topic])

write_csv(doc_topics, "lda_document_topics_labeled1.csv")


pdf("wordclouds_all_topics1.pdf", width = 10, height = 8)


term_matrix <- as.matrix(dtm)
term_freq <- colSums(term_matrix)
term_freq <- sort(term_freq, decreasing = TRUE)

set.seed(1234)
wordcloud(
  words = names(term_freq),
  freq = term_freq,
  min.freq = 10,
  max.words = 200,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2"),
  scale = c(4, 0.5)
)
title("Word Cloud - Entire Corpus")


full_beta <- tidy(lda_model, matrix = "beta")


filtered_beta <- full_beta %>%
  filter(beta > 0.0001)


pdf("topic_wordclouds_all_terms.pdf")
for (i in 1:10) {
  topic_data <- filtered_beta %>% filter(topic == i)
  
  wordcloud(
    words = topic_data$term,
    freq = topic_data$beta,
    max.words = 500,  
    random.order = FALSE,
    scale = c(4, 0.5),
    colors = brewer.pal(8, "Dark2")
  )
  title(main = topic_labels[i])
}
dev.off()


saveRDS(lda_model, "lda_model.rds")
saveRDS(dtm, "dtm.rds")
saveRDS(corpus, "corpus.rds")


rm(corpus, dtm)
gc()
