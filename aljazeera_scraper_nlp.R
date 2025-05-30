# install.packages(c("rvest", "stringr", "httr", "dplyr", "tm", "textstem", "hunspell", "qdapRegex", "textclean", "tokenizers", "stopwords", "SnowballC", "tidytext", "ggplot2", "wordcloud", "igraph", "ggraph", "tidyr"))


# ----------------- Load Libraries -----------------
library(rvest)           # For web scraping
library(stringr)         # String manipulation
library(httr)            # HTTP requests
library(dplyr)           # Data manipulation
library(tm)              # Text mining
library(textstem)        # Lemmatization
library(hunspell)        # Spell check (optional, not used here)
library(qdapRegex)       # Regex cleaning
library(textclean)       # Text cleaning utilities
library(tokenizers)      # Tokenization
library(stopwords)       # Stopword removal
library(SnowballC)       # Stemming
library(tidytext)
library(ggplot2)
library(wordcloud)
library(igraph)
library(ggraph)
library(tidyr)
library(reshape2)
library(topicmodels)



# ----------------- Step 1: Define Web Scraping Functions -----------------
scrape_category_aljazeera <- function(category_url, category_name, max_articles = 100, max_pages = 100) {
  base_url <- "https://www.aljazeera.com"
  all_articles <- list()
  page_num <- 1
  
  while (length(all_articles) < max_articles && page_num <= max_pages) {
    url <- paste0(category_url, "?page=", page_num)
    cat("Reading:", url, "\n")
    
    html <- tryCatch(read_html(url), error = function(e) NULL)
    if (is.null(html)) break
    
    links <- html %>%
      html_nodes("a.u-clickable-card__link") %>%
      html_attr("href") %>%
      unique()
    
    if (length(links) == 0) break
    links <- paste0(base_url, links)
    
    for (link in links) {
      if (length(all_articles) >= max_articles) break
      
      article_page <- tryCatch(read_html(link), error = function(e) NULL)
      if (is.null(article_page)) next
      
      title <- article_page %>% html_node("h1") %>% html_text(trim = TRUE)
      
      date <- article_page %>%
        html_node("div.date-simple span.screen-reader-text") %>%
        html_text(trim = TRUE) %>%
        str_remove("Published On ")
      
      author_node <- article_page %>% html_node("div.article-author-name")
      author <- if (!is.null(author_node)) {
        tryCatch({
          a_text <- html_node(author_node, "a.author-link") %>% html_text(trim = TRUE)
          if (!is.na(a_text)) {
            a_text
          } else {
            html_node(author_node, "span.article-author-name-item") %>% html_text(trim = TRUE)
          }
        }, error = function(e) NA)
      } else {
        NA
      }
      
      content <- article_page %>%
        html_nodes("p") %>%
        html_text(trim = TRUE) %>%
        paste(collapse = " ")
      
      if (!is.na(title) && nchar(content) > 50 &&
          !(link %in% sapply(all_articles, function(x) x$url)) &&
          !(title %in% sapply(all_articles, function(x) x$title)) &&
          !(content %in% sapply(all_articles, function(x) x$content))) {
        
        article <- data.frame(
          date = date,
          title = title,
          author = author,
          content = content,
          category = category_name,
          url = link,
          stringsAsFactors = FALSE
        )
        all_articles[[length(all_articles) + 1]] <- article
      }
    }
    
    page_num <- page_num + 1
    Sys.sleep(1)
  }
  
  return(bind_rows(all_articles))
}

# ----------------- Step 2: Scrape Articles by Category -----------------

# Define categories
categories <- list(
  news = "https://www.aljazeera.com/news",
  features = "https://www.aljazeera.com/features",
  economy = "https://www.aljazeera.com/economy",
  human_rights = "https://www.aljazeera.com/human-rights",
  sports = "https://www.aljazeera.com/sports",
  science_tech = "https://www.aljazeera.com/tag/science-and-technology",
  opinions = "https://www.aljazeera.com/opinions"
)

# Scrape all categories
all_articles <- list()

for (cat_name in names(categories)) {
  cat("Processing category:", cat_name, "\n")
  df_cat <- scrape_category_aljazeera(categories[[cat_name]], cat_name, max_articles = 100, max_pages = 100)
  all_articles[[cat_name]] <- df_cat
  cat("Completed category:", cat_name, "\n\n")
}

# ----------------- Step 3: Combine and Remove Duplicates -----------------

# Combine all
df <- bind_rows(all_articles)

# Remove duplicates by url, title, content
df <- df %>% distinct(url, title, content, .keep_all = TRUE)

# Save to CSV
write.csv(df, "D:/University/Semester 8/Data Science/Final/Project/initial_content.csv", row.names = FALSE)
# View dimensions
dim(df)

# ----------------- Step 3: Create Text Corpus -----------------

corpus_title <- VCorpus(VectorSource(df$title))
corpus_content <- VCorpus(VectorSource(df$content))
head(corpus_content)



# ----------------- Step 4: Clean Text -----------------

clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^a-z\\s]", " ", x)))
  return(corpus)
}

clean_corpus_title <- clean_corpus(corpus_title)
clean_corpus_content <- clean_corpus(corpus_content)

df$cleaned_title <- sapply(clean_corpus_title, content)
df$cleaned_content <- sapply(clean_corpus_content, content)

print(df$cleaned_content)


# ----------------- Step 5: Tokenization -----------------

tokenize_text_rowwise <- function(text_vector) {
  lapply(text_vector, function(text) {
    tokens <- tokenize_words(text)[[1]]
    tokens <- tokens[!tokens %in% stopwords("en")]
    return(tokens)
  })
}

title_tokens_list <- tokenize_text_rowwise(df$cleaned_title)
content_tokens_list <- tokenize_text_rowwise(df$cleaned_content)


print(content_tokens_list)

# ----------------- Step 6: Stemming -----------------

stem_tokens_rowwise <- function(token_list) {
  lapply(token_list, function(tokens) wordStem(tokens, language = "en"))
}


stemmed_title_list <- stem_tokens_rowwise(title_tokens_list)
stemmed_content_list <- stem_tokens_rowwise(content_tokens_list)

print(stemmed_content_list)


# ----------------- Step 7: Lemmatization -----------------

lemmatize_text_rowwise <- function(token_list) {
  lapply(token_list, lemmatize_words)
}

lemmatized_title_list <- lemmatize_text_rowwise(title_tokens_list)
lemmatized_content_list <- lemmatize_text_rowwise(content_tokens_list)

df$lemmatized_title <- sapply(lemmatized_title_list, paste, collapse = " ")
df$lemmatized_content <- sapply(lemmatized_content_list, paste, collapse = " ")


print(df$lemmatized_content)


write.csv(df$lemmatized_content, "D:/University/Semester 8/Data Science/Final/Project/clean_corpus.csv", row.names = FALSE)


# Step 3: Create Corpus from processed_description
corpus <- Corpus(VectorSource(df$lemmatized_content))

# Step 4: Clean and preprocess the text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Step 5: Create Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.95)  # Remove sparse terms


freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq, 10)






# Step 6: Apply LDA for Topic Modeling
k <- 7  # Number of topics
lda_model <- LDA(dtm, k = k, control = list(seed = 1234))

# Step 7: Examine the Topics
# Step 7.1: Get the most probable words per topic
topics <- tidy(lda_model, matrix = "beta")
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta)

print(top_terms)



topic_probs <- posterior(lda_model)$topics
head(topic_probs, 5)

top_topic <- apply(topic_probs, 1, which.max)
head(top_topic, 10)  

topic_terms <- posterior(lda_model)$terms
head(topic_terms[, 1:5])  



# Step 7.2: Get the topic proportions per document
doc_topics <- tidy(lda_model, matrix = "gamma")
head(doc_topics)



# Tidy the LDA model (get beta matrix)
topics <- tidy(lda_model, matrix = "beta")

# Get top 10 terms per topic
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Plot using just topic numbers (no labels)
ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ paste("Topic", topic), scales = "free_y") +  
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "LDA Topics",
    subtitle = "Top 10 terms per topic",
    x = "Term",
    y = "Probability (Beta)"
  ) +
  theme_minimal()

# Save to CSV





# =======================
# Step 9: Word Cloud
# =======================
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs)

set.seed(123)
wordcloud(words = word_freqs_df$word,
          freq = word_freqs_df$freq,
          min.freq = 2,
          max.words = 200,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))



#----------------------Topic modeling evaluation-----------------------------
# Top 20 frequent words
top_words <- word_freqs_df %>%
  top_n(20, freq) %>%
  arrange(desc(freq))

# Plot
ggplot(top_words, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Most Frequent Words",
       x = "Words", y = "Frequency") +
  theme_minimal()


# Find associations with a specific word, e.g., "palestine"
findAssocs(tdm, terms = "palestine", corlimit = 0.2)

df <- df %>%
  mutate(word_count = str_count(content, "\\w+"))

ggplot(df, aes(x = word_count)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Article Word Counts",
       x = "Word Count",
       y = "Number of Articles") +
  theme_minimal()



ggplot(df, aes(x = category)) +
  geom_bar(fill = "darkorange") +
  labs(title = "Number of Articles by Category",
       x = "Category",
       y = "Count of Articles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
