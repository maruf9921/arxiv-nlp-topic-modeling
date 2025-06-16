

library(dplyr)
library(stringr)
library(textclean)     
library(textstem)      
library(tm)            
library(tokenizers)    

df <- read.csv("arxiv_cs_complete(2020-2025).csv") 

df <- df %>%
  mutate(
    abstract_clean = abstract %>%
      str_to_lower() %>%                       
      replace_contraction() %>%                
      str_replace_all("[^a-z\\s]", " ") %>%    
      str_replace_all("\\s+", " ") %>%         
      str_trim()
  )

df$tokens <- tokenize_words(df$abstract_clean)

stop_words <- stopwords("en")
df$tokens <- lapply(df$tokens, function(words) {
  words[!words %in% stop_words]
})

df$tokens <- lapply(df$tokens, lemmatize_words)

corpus <- sapply(df$tokens, paste, collapse = " ")
df$corpus <- corpus
write.csv(corpus, "token_text_arxiv_preprocessed.csv", row.names = FALSE)


head(df[, c("abstract", "corpus")])
