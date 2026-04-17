
links <- reddit_urls( search_terms   = "ice", page_threshold = 10, sort_by = 'new')

find_subreddits("ice")

temp_df <-find_thread_urls(keywords = "ice", sort_by = "top", subreddit = "conservatives", period = "day" )

posts <- get_thread_content(temp_df$url)

threads <- as.data.frame(posts$threads)
comments <- as.data.frame(posts$comments)

comments_subset <- comments[1:100,]

first_url <- posts$threads$url[1]

comments_subset$created_at <- paste(comments_subset$date, comments_subset$timestamp, sep=" ")

find_thread_urls(keywords = "ice", subreddit="conservatives", sort_by="comments", period="day")

#SICCS and reddit data

#text analysis basics
links <- reddit_urls( search_terms   = "ice", page_threshold = 10, sort_by = 'new')

find_subreddits("ice")

temp_df <-find_thread_urls(keywords = "ice", sort_by = "top", subreddit = "conservatives", period = "day" )

posts <- get_thread_content(temp_df$url)

threads <- as.data.frame(posts$threads)

comments <- as.data.frame(posts$comments)

comments_subset <- comments[1:100,]

first_url <- posts$threads$url[1]

comments_subset$created_at <- paste(comments_subset$date, comments_subset$timestamp, sep=" ")

find_thread_urls(keywords = "ice", subreddit="conservatives", sort_by="comments", period="day")

reddit_corpus <- Corpus(VectorSource(as.vector(comments_subset$comment)))

reddit_corpus


tidy_reddit_corpus <- comments_subset %>%
  select(created_at,comment) %>%
  unnest_tokens("word", comment)
head(tidy_reddit_corpus)


tidy_reddit_corpus %>%
  count(word) %>%
  arrange(desc(n))

tidy_reddit_corpus_cleaned <- tm_map(reddit_corpus, removeWords, stopwords("english"))
#cleaned bc removed words and stopwords

data("stop_words")
anti_join_reddit_corpus <- tidy_reddit_corpus%>%
  anti_join(stop_words)

anti_join_tidy_reddit_corpus %>%
  count(word) %>%
  arrange(desc(n))

reddit_corpus_tm_map<- tm_map(reddit_corpus, content_transformer(removeNumbers))
reddit_corpus_tm_map


tidy_reddit_corpus_grep <- tidy_reddit_corpus[-grep("\\b\\d+\\b", tidy_reddit_corpus$word),]
tidy_reddit_corpus_grep

tidy_reddit_corpus_gsub <- gsub("\\s+","",tidy_reddit_corpus$word)
tidy_reddit_corpus_gsub


reddit_corpus_stemming <- tm_map(reddit_corpus, content_transformer(stemDocument), language = "english")

reddit_corpus_stemming

tidy_reddit_corpus_mutate<- tidy_reddit_corpus %>%
  mutate_at("word", funs(wordStem ((.), language = "en")))
tidy_reddit_corpus_mutate


reddit_DTM <- DocumentTermMatrix(reddit_corpus, control = list(wordLengths = c(2, Inf)))

inspect(reddit_DTM[1:5,3:8])

#creating a DTM in TidyText
tidy_reddit_DTM <- tidy_reddit_corpus %>%
  count(created_at, word) %>%
  cast_dtm(created_at,word,n)

tidy_reddit_DTM
