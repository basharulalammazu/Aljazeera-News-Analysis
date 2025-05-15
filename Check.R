library(rvest)
library(stringr)
library(httr)
library(dplyr)
library(tm)  # For text cleaning functions
library(textstem)             # Load


# Function to collect article URLs
get_article_links <- function(base_url, total_articles = 100) {
  links <- c()
  page_num <- 1
  
  while (length(links) < total_articles && page_num <= 100) {  # Cap page_num at 100
    cat("Scraping page", page_num, "from", base_url, "\n")
    page_url <- paste0(base_url, "?page=", page_num)
    page <- tryCatch(read_html(page_url), error = function(e) NULL)
    if (is.null(page)) break
    
    new_links <- page %>%
      html_nodes("a.u-clickable-card__link") %>%
      html_attr("href") %>%
      unique()
    
    if (length(new_links) == 0) break  # Stop if no more new links found
    
    new_links <- paste0("https://www.aljazeera.com", new_links)
    links <- unique(c(links, new_links))
    
    page_num <- page_num + 1
    Sys.sleep(1)
  }
  
  return(links[1:min(total_articles, length(links))])  # Avoid out-of-bounds error
}


# Function to scrape article data
scrape_article <- function(url, category) {
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(NULL)
  
  title <- page %>% html_node("h1") %>% html_text(trim = TRUE)
  
  # Extract date
  date <- page %>%
    html_node("div.date-simple span.screen-reader-text") %>%
    html_text(trim = TRUE) %>%
    str_remove("Published On ")
  
  # Extract author (handle both <a> and plain span cases)
  author_node <- page %>% html_node("div.article-author-name")
  if (!is.na(html_node(author_node, "a.author-link") %>% html_text(trim = TRUE))) {
    author <- html_node(author_node, "a.author-link") %>% html_text(trim = TRUE)
  } else {
    author <- html_node(author_node, "span.article-author-name-item") %>% html_text(trim = TRUE)
  }
  
  # Extract content from all <p> tags
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

# Store results
all_articles <- list()

# Scrape each category
for (category in names(categories)) {
  cat("Processing category:", category, "\n")
  links <- get_article_links(categories[[category]], total_articles = 100)
  articles <- lapply(links, function(link) scrape_article(link, category))
  all_articles[[category]] <- bind_rows(articles)
  cat("Completed category:", category, "\n\n")
}

# Merge and save
final_df <- bind_rows(all_articles)
write.csv(final_df, "D:/University/Semester 8/Data Science/Final/Project/aljazeera_all_categories_articles.csv", row.names = FALSE)
print("All articles saved to 'aljazeera_all_categories_articles.csv'\n")

print("All ")
print(final_df)
dim(final_df)


# Define the text cleaning function
Text_cleaning <- function(df) {
  clean_text <- function(text) {
    text <- tolower(text)                          # Convert to lowercase
    text <- removeNumbers(text)                    # Remove numbers
    text <- removePunctuation(text)                # Remove punctuation
    text <- gsub("[^a-z\\s]", " ", text)            # Keep only letters and spaces
    text <- stripWhitespace(text)                  # Remove extra whitespace
    return(text)
  }
  
  df_cleaned <- df %>%
    mutate(
      title = sapply(title, clean_text),
      content = sapply(content, clean_text)
    )
  
  return(df_cleaned)
}

head(articles)
head(final_df, 3)
clean_df <- Text_cleaning(final_df)
head(clean_df, 3)






# Load the required libraries
library(tokenizers)
library(stopwords)

# Define the tokenize function with stopword removal
tokenize <- function(text_vector) {
  combined_text <- paste(text_vector, collapse = " ")         # Combine all text
  tokens <- tokenize_words(combined_text)                     # Tokenize using tokenizers package
  tokens <- unlist(tokens)                                    # Flatten the list
  tokens <- tokens[!tokens %in% stopwords("en")]              # Remove stopwords
  return(tokens)
}


# Tokenize titles without stopwords
title_tokens <- tokenize(final_df$title)
print("Tokenized Titles (No Stopwords):\n")
print(title_tokens)

# Tokenize cleaned content without stopwords
content_tokens <- tokenize(clean_df$content)
print("Tokenized Content (No Stopwords):\n")
print(content_tokens)



# Stemming
stemming<- function(text_vector){
  return(stemDocument(text_vector))
}

print("Stemmed Titles:\n")
stemmed_title <- stemming(title_tokens)
print(stemmed_title)


print("Stemmed Content:\n")
stemmed_content <- stemming(content_tokens)
print(stemmed_content)





# Lemmatization
lemmatization <- function(text_vector){
  return(lemmatize_words(text_vector))
}


lemmatization_title <- lemmatization(stemmed_title)
print("\nLemmatized Title Data (First 50 words):\n")
print(head(lemmatization_title, 50))


lemmatization_content <- lemmatization(stemmed_content)
print("\nLemmatized Content Data (First 50 words):\n")
print(head(lemmatization_content, 50))

