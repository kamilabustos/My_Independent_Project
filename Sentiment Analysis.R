ice_in_conservative$created_at <- paste(ice_in_conservative$date, ice_in_conservative$timestamp, sep=" ")

tidy_ice<- ice_in_conservative %>%
  select(created_at,text) %>%
  unnest_tokens("word", text)

conservative_sentiments <- tidy_ice%>%
  inner_join(get_sentiments("bing")) %>%
  count(created_at, sentiment)

head(conservative_sentiments)

tidy_ice$date<-as.Date(tidy_ice$created_at,
                       format="%Y-%m-%d %x")

ice_sentiment_plot <-
  tidy_ice %>%
  inner_join(get_sentiments("bing")) %>%
  filter(sentiment=="negative") %>%
  count(date, sentiment)

# IGNORE THIS FOR NOW
#ggplot(ice_sentiment_plot, aes(x=date, y=n))+
geom_line(color="red", size=.5)+
  theme_minimal()+
  theme(axis.text.x =
          element_text(angle = 60, hjust = 1, size = 13))+
  theme(plot.title =
          element_text(hjust = 0.5, size = 18))+
  ylab("Number of Negative Words")+
  xlab("")+
  ggtitle("Negative Sentiment in R/Conservative")+
  theme(aspect.ratio=0.25)
# IGNORE THIS FOR NOW


ice_sentiment_plot$date


# added this bc tidy_ice has dates bc it's created_at etc AND those dates are NA so I removed all the dates that are NA with this line of code
cleaned_ice_sentiment_plot <- ice_sentiment_plot %>% filter(!is.na(date))

# The plot has no points plotted on bc ice sentiment plot has no dates ie) ice_sentiment_plot$dates says 0
ggplot(ice_sentiment_plot, aes(x=date, y=n))+
  geom_line(color="red", size=.5)+
  theme_minimal()+
  theme(axis.text.x =
          element_text(angle = 60, hjust = 1, size = 13))+
  theme(plot.title =
          element_text(hjust = 0.5, size = 18))+
  ylab("Number of Negative Words")+
  xlab("")+
  ggtitle("Negative Sentiment in R/Conservative")+
  theme(aspect.ratio=0.25)
#when i changed ice sentiment plot back to the OG aka not the cleaned ice sent plot, it gives me an error even though tidy ice which makes up the sentiment plot has dates
