data("stop_words")

top_words <- tidy_reddit_corpus %>%
  anti_join(stop_words) %>%
  filter(!(word=="https"|
             word=="t.co"|
             word== "t.co"|
             word== "amp")) %>%
  count(word) %>%
  arrange(desc(n))

top_words

top_words %>%
  slice(1:20) %>%
  ggplot(aes(x=reorder(word, -n), y=n, fill=word))+
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size =13)) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  ylab("Frequency") +
  xlab("") +
  ggtitle("Most Frequent Words in r/Conservatives") +
  guides(fill=FALSE)

#says column "text" doesn't exist
reddit_tfidf<- tidy_reddit_corpus %>%
  select(created_at,text) %>%
  unnest_tokens("word",text) %>%
  anti_join(stop_words) %>%
  count(word, created_at) %>%
  bind_tf_(word, created_at, n)

mynumber<-2+2
top_tfidf<- tidy_reddit_corpus %>%
  arrange(desc(reddit_tfidf))


top_tfidf$word[1]



conservative_dictionary <- c("ice", "illegals", "law", "federal", "criminals")

head(conservative_reddit$text, 10)

conservative_reddit<-ice_in_conservative[str_detect(ice_in_conservative$text, paste(conservative_dictionary, collapse="|")),]


head(conservative_reddit$text, 2)

head(get_sentiments("bing"))
