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



# ----------------- Step 1: Define Web Scraping Functions -----------------

# Function to get article links from Al Jazeera category pages
get_article_links <- function(base_url, total_articles = 100, total_pages = 100) {
  links <- c()
  page_num <- 1
  
  while (length(links) < total_articles && page_num <= total_pages) {
    cat("Scraping page", page_num, "from", base_url, "\n")
    page_url <- paste0(base_url, "?page=", page_num)
    page <- tryCatch(read_html(page_url), error = function(e) NULL)
    if (is.null(page)) break
    
    new_links <- page %>%
      html_nodes("a.u-clickable-card__link") %>%
      html_attr("href") %>%
      unique()
    
    if (length(new_links) == 0) break
    
    new_links <- paste0("https://www.aljazeera.com", new_links)
    links <- unique(c(links, new_links))
    
    page_num <- page_num + 1
    Sys.sleep(1)  # Be polite to the server
  }
  
  return(links[1:min(total_articles, length(links))])
}

# Function to extract article data
scrape_article <- function(url, category) {
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(NULL)
  
  title <- page %>% html_node("h1") %>% html_text(trim = TRUE)
  
  date <- page %>%
    html_node("div.date-simple span.screen-reader-text") %>%
    html_text(trim = TRUE) %>%
    str_remove("Published On ")
  
  author_node <- page %>% html_node("div.article-author-name")
  if (!is.na(html_node(author_node, "a.author-link") %>% html_text(trim = TRUE))) {
    author <- html_node(author_node, "a.author-link") %>% html_text(trim = TRUE)
  } else {
    author <- html_node(author_node, "span.article-author-name-item") %>% html_text(trim = TRUE)
  }
  
  content <- page %>%
    html_nodes("p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = " ")
  
  data.frame(
    category = category,
    title = title,
    date = date,
    author = author,
    content = content,
    url = url,
    stringsAsFactors = FALSE
  )
}




# ----------------- Step 2: Scrape Articles by Category -----------------

categories <- list(
  news = "https://www.aljazeera.com/news",
  features = "https://www.aljazeera.com/features",
  economy = "https://www.aljazeera.com/economy",
  human_rights = "https://www.aljazeera.com/human-rights",
  sports = "https://www.aljazeera.com/sports",
  science_tech = "https://www.aljazeera.com/tag/science-and-technology",
  opinions = "https://www.aljazeera.com/opinions"
)

all_articles <- list()

for (category in names(categories)) {
  cat("Processing category:", category, "\n")
  links <- get_article_links(categories[[category]], total_articles = 100, total_pages = 1)
  articles <- lapply(links, function(link) scrape_article(link, category))
  all_articles[[category]] <- bind_rows(articles)
  cat("Completed category:", category, "\n\n")
}

# Combine all category data
df <- bind_rows(all_articles)

# Save raw scraped data
write.csv(df, "D:/University/Semester 8/Data Science/Final/Project/IDS_SecD_G3.csv", row.names = FALSE)
print("All articles saved to CSV.")





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


# ----------------- Step 8: Save Cleaned Data -----------------
write.csv(df$lemmatized_content, "D:/University/Semester 8/Data Science/Final/Project/IDS_SecD_G3_Corpus.csv", row.names = FALSE)
cat("Final cleaned corpus saved to 'IDS_SecD_G3_Final_Corpus.csv'\n")
