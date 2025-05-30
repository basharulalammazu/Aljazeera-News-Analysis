# Aljazeera-News-Analysis

This project is a comprehensive R-based pipeline for scraping, cleaning, and analyzing news articles from Al Jazeera across multiple categories. It performs web scraping, text preprocessing, tokenization, stemming, lemmatization, document-term matrix creation, topic modeling (using LDA), and data visualization (e.g., word clouds and topic plots).

---

## 📦 Packages Required

To run this project, install the following R packages:

```r
install.packages(c(
  "rvest", "stringr", "httr", "dplyr", "tm", "textstem", "hunspell", 
  "qdapRegex", "textclean", "tokenizers", "stopwords", "SnowballC", 
  "tidytext", "ggplot2", "wordcloud", "igraph", "ggraph", "tidyr", "reshape2", "topicmodels"
))
````

---

## 🕸️ Web Scraping

* Function: `scrape_category_aljazeera()`
* Purpose: Scrapes articles from defined Al Jazeera categories (News, Features, Economy, Human Rights, Sports, Science & Tech, Opinions).
* Saves results to: `initial_content.csv`

---

## 🧹 Text Preprocessing

* Creates text corpus from scraped titles and contents.
* Cleans text:

  * Lowercasing
  * Removing numbers, punctuation, and special characters
  * Removing stopwords

---

## 🧩 Tokenization, Stemming & Lemmatization

* Tokenization using `tokenizers`
* Stemming using `SnowballC::wordStem()`
* Lemmatization using `textstem::lemmatize_words()`
* Final cleaned content saved to: `final_corpus.csv`

---

## 🧠 Topic Modeling (LDA)

* Creates Document-Term Matrix
* Reduces sparsity
* Applies LDA to find latent topics (default: `k = 7`)
* Extracts:

  * Top terms per topic
  * Document-topic distributions

---

## 📊 Visualization

* **Topic-Term Bar Plot**:

  * Visualizes top 10 words per topic with `ggplot2`
* **Word Cloud**:

  * Displays most frequent terms using `wordcloud`

---

## 📁 Output Files

* `initial_content.csv`: Raw scraped articles
* `final_corpus.csv`: Preprocessed, tokenized, and lemmatized content
* Topic model outputs: Word probabilities, document-topic matrices (optionally saved)

---

## 🧠 Future Enhancements

* Sentiment analysis
* Network graph of topic correlations
* Named entity recognition (NER)
* Automated daily scraping scheduler

---

## 📌 Author

**Basharul - Alam - Mazu**
GitHub: [basharulalammazu](https://github.com/basharulalammazu)
Website: [basharulalammazu.github.io](https://basharulalammazu.github.io)

---

## 📝 License

This project is for educational and academic purposes only.
