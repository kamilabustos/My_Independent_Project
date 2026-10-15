install.packages("topicmodels")


data("AssociatedPress")

inspect(AssociatedPress[1:5, 1:5])

AP_topic_model <- LDA(AssociatedPress, k=10, control = list(seed = 321))


Ap_topics <- tidy(AP_topic_model, matrix = "beta")

ap_top_terms <- AP_topics %>%
  group_by(topic) %>%
  top_n (10, beta) %>%
  ungroup () %>%
  arrange (topic, -beta)


ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  mutate (topic = paste("Topic #", topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  labs (title = "Topic Model of AP News Articles", caption = "Top Terms by Topic (betas)") +
  ylab("") +
  xlab("")+
  coord_flip()

google_doc_id <-"1LcX-JnpGB0lU1iDnXnxB6WFqBywUKpew"

poliblogs <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_doc_id, stringAsFactors = FALSE))


processed <- textProcessor(poliblogs$documents, metadata = poliblogs)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

First_STM <- stm(documents = out$documents, vocab =out$vocab, K =10, prevalence = ~ rating +s(day), max.em.its = 75, data = out$meta, init.type = "Spectral", verbose = FALSE )

plot(First_STM)

findThoughts(First_STM, texts = poliblogs$documents, n = 2, topics =3)

findingk <- searchK(out$documents, out$vocab, K = c(10:30), prevalence = ~ rating + s(day), data = meta, verbose = FALSE)

plot(findingk)

load("./findingk.Rda")
plot(findingk)


predict_topics<-estimateEffect(formula = 1:10 ~ rating + s(day), stmobj = First_STM, metadata = out$meta, uncertainty = "Global")


plot(predict_topics, covariate = "rating", topics = c(3, 5, 9),
     model = First_STM, method = "difference",
     cov.value1 = "Liberal", cov.value2 = "Conservative",
     xlab = "More Conservative ... More Liberal",
     main = "Effect of Liberal vs. Conservative",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('Topic 3', 'Topic 5','Topic 9'))


plot(predict_topics, "day", method = "continuous", topics = 3,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Time (2008)")
monthseq <- seq(from = as.Date("2008-01-01"),
                to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
     labels = monthnames)

immigration_dictionary <- c("illegal", "ICE", "border", "Trump")


conservative_reddit <- tidy_reddit_corpus %>% filter(str_detect(text, immigration_dictionary))

head(conservative_reddit$text, 2)
