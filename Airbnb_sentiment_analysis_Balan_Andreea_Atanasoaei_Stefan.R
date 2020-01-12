library(readr)
library(tidyverse)
library(tidytext)
library(stringr)
library(wordcloud)
library(DT)
library(dplyr)
library(leaflet)


listing <- read.csv('listings.csv')
review <- read.csv('reviews.csv')

listing <- arrange(listing,listing$id)

#Getting prices and neighbourhood in reviews table
review <- review %>% left_join(listing, by=c("listing_id"="id"))

#Cleaning the data
colSums(is.na(review))
review <- filter(review,!is.na(comments))

#We are now plotting Airbnb homestays based on their latitude and longitude and representing pricey 
#listings (with price per night greater than $130 - upper quantile range) with white circles to help us 
#identify pricey locations of Boston.

review$comments <- as.character(review$comments)

#Breaking customer reviews in words 
neighbourhood_words <- review %>% 
  select(comments, neighbourhood) %>% 
  unnest_tokens(word, comments) %>% 
  filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))

nrc <- sentiments %>% filter(lexicon == "nrc") %>% select(word, sentiment)

prop_tot_words <- neighbourhood_words %>% group_by(neighbourhood) %>% mutate(total_words = n()) %>% ungroup() %>% distinct(neighbourhood, total_words) %>%  arrange(desc(total_words)) %>% top_n(10)

#Identify 5 most reviewed properties
most_reviews_5 <- review %>%
  group_by(neighbourhood) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(5)

prop_tot_words <- most_reviews_5 %>% left_join(prop_tot_words)

#count words associated with each type of sentiment in 5 most reviewed properties
by_prop_sentiment <- neighbourhood_words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, neighbourhood) %>%
  ungroup() %>%
  inner_join(prop_tot_words,by = "neighbourhood") %>%
  group_by(neighbourhood, sentiment) %>%
  mutate(prop = round(count / total_words * 100, digits=1)) %>%
  ungroup()

#Plotting GGPlot for showing sentiments
ggplot(data = by_prop_sentiment) +
  geom_bar(mapping = aes(x = neighbourhood,
                         y = prop),
           stat = "identity", fill = "blue") +
  facet_wrap( ~ sentiment) +
  labs(title = "Sentiment Analysis in 5 Most Reviewed Neighbourhoods",
       x ="Neighbourhood", y="Proportion \n (sentiment word count / total word count)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#In this section, we are making a wordcloud with word size based on the frequency of that word 
#appearing in the reviews

listings_words <- review %>%
  select(id, comments) %>%
  unnest_tokens(word, comments) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

cloud <- as.data.frame(listings_words %>% 
                         group_by(word) %>%
                         summarise(no_rows = length(word)))

#building the word cloud
wordcloud(words = cloud$word, freq = cloud$no_rows, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))


#Now we identify 20 most frequently appearing words that customers associate Airbnb Chicago properties
#with.

#We need to use the unnest_tokens function to obtain one-row-per-term-per-listing-description
listings_words <- review %>%
  select(id, comments, price) %>%
  unnest_tokens(word, comments) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

#plot the graph
common_listings <- listings_words %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  top_n(n = 20, wt = count) %>%
  ggplot() +
  geom_bar(mapping = aes(x=reorder(word, count),
                         y=count),
           stat="identity", fill = "light grey") +
  coord_flip() +
  labs(title="Top 20 words described in Reviews",
       x="Word count", y="Words") +
  theme_minimal()

print(common_listings)


#We are now plotting Airbnb homestays based on their latitude and longitude and representing pricey
#listings with white circles to help us 
#identify pricey locations of Boston.

review$price<- as.numeric(review$price)
review %>% filter(price>286) %>% leaflet::leaflet() %>% 
  leaflet::addProviderTiles("CartoDB.DarkMatter") %>%
  leaflet::addCircleMarkers(~longitude, ~latitude, radius = 2, color = "white", fillOpacity = 0.3)






