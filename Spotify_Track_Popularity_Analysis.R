########################################

### SPOTIFY TRACK POPULARITY PROJECT
### MADE BY SYLVAIN PIHET, MSC BUSINESS ANALYTICS AT IMPERIAL COLLEGE LONDON

########################################

#load libraries
library(dplyr)
library(quanteda)
library(ggrepel)
library(textclean)
library(tidyverse)
library(glmnet)
library(sentimentr)
library(spacyr) # a new one - for grammar parsing
library(politeness)
library(stringr)
library(stm) # new one... for topic models
library(wordcloud)
library(igraph)

source("vectorFunctions.R") # a new one!
source("TAB_dfm.R")
source("kendall_acc.R")

load("wfFile.RData")
#loading the vec file
vecFile<-data.table::fread("crawl-300d-2M.vec",
                           quote="",header=F,col.names = c("word",paste0("vec",1:300)))

#load data
data<-read.csv("song_audio_spotify_temp.csv")

#################______DATA EXPLORATION_______#######################

# Number of NA values per column
na_count <- sapply(data, function(x) sum(is.na(x)))
print(na_count)

# Type of data in each column
data_types <- sapply(data, class)
print(data_types)

# Unique records in a specific column 'lyrics_sliced'
unique_lyrics <- length(unique(data$lyrics_sliced))
print(unique_lyrics)


# Calculate mean of numeric columns and round them
numeric_means <- sapply(data[, !names(data) %in% c("Unnamed..0","duration_ms")], function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA)
numeric_means_rounded <- round(numeric_means, 2)

# Create a data frame for plotting
mean_data <- data.frame(
  Column = names(numeric_means_rounded),
  Mean = numeric_means_rounded,
  stringsAsFactors = FALSE
)

# Filter out NA values to avoid plotting errors
mean_data <- mean_data[!is.na(mean_data$Mean), ]

# Generate a bar plot
ggplot(mean_data, aes(x = Column, y = Mean, fill = Column)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mean of Numeric Columns (Excluding 'duration_ms')", x = "Column", y = "Mean Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# The amount of unique 'track_name'
unique_track_name <- length(unique(data$track_name))
print(unique_track_name)

# The amount of unique 'track_artist'
unique_track_artist <- length(unique(data$track_artist))
print(unique_track_artist)


#relationship between music features and popularity
data_clean <- data %>%
  select(-c(Unnamed..0, lyrics, track_name_length)) %>%
  mutate(duration_sec = duration_ms / 1000) %>%
  mutate(word_count = str_count(lyrics_sliced, pattern = "\\S+"))%>%
  select(-duration_ms)

# Assuming 'data_clean' is your data frame and 'lyrics' is the column with text
data_clean <- data_clean %>%
  filter(language == "en") %>%  # Keeps only rows where the language is English
  filter(!(lyrics_sliced == "" | grepl("^\\s*$", lyrics_sliced))) %>%
  mutate(lyrics_sliced = str_replace_all(lyrics_sliced, "\\b(Verse|Intro|Outro|Lyrics|Chorus|Guitar|guitar|bridge|Bridge)\\b", "")) %>%  # Removes specific words
  mutate(lyrics_sliced = str_remove(lyrics_sliced, "\\s*Embed$"))   # Removes 'Embed' if it's the last word in the string

# View the modified data frame
print(head(data_clean))
dim(data_clean)

#average song length
mean(data_clean$duration_sec)

#average lyric word count
mean(data_clean$word_count)


# Calculate correlation matrix but focus only on 'track_popularity'
correlation_matrix <- cor(data_clean[, sapply(data_clean, is.numeric)], use = "complete.obs")
popularity_correlation <- correlation_matrix['track_popularity', ]
popularity_correlation <- popularity_correlation[-which(names(popularity_correlation) == "track_popularity")]
sorted_correlation <- sort(popularity_correlation, decreasing = TRUE)


#create a dataframe for correlation
correlation_df <- data.frame(
  Feature = names(sorted_correlation),
  Correlation = as.numeric(sorted_correlation)
)

# Sort the dataframe by the Correlation value in descending order
correlation_df <- correlation_df[order(correlation_df$Correlation, decreasing = FALSE), ]
correlation_df$Feature <- factor(correlation_df$Feature, levels = correlation_df$Feature)


# Bar plot of feature correlations with track popularity
ggplot(correlation_df, aes(x = Feature, y = Correlation, fill = Feature)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Correlation of Music Features with Track Popularity",
       x = "Feature", y = "Correlation") +
  coord_flip() +
  theme(
    legend.position = "none",  # Removes the legend
    plot.background = element_rect(fill = "#191414", color = "#191414"),  # Set the plot background color to black
    panel.background = element_rect(fill = "#191414", color = "#191414"),  # Set the panel background color to black
    text = element_text(color = "#FFFFFF"),  # Set text color to white for all text elements
    axis.title = element_text(color = "#FFFFFF"),  # Set axis titles to white
    axis.text = element_text(color = "#FFFFFF"),  # Set axis text to white
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )

#popularity distribution
# Create a histogram of track popularity
ggplot(data_clean, aes(x = track_popularity)) +
  geom_histogram(bins = 30, fill = "#1DB954", color = "white") + # You can adjust the number of bins based on your data range
  labs(title = "Distribution of Track Popularity",
       x = "Track Popularity",
       y = "Frequency") +
  theme(
    legend.position = "none",  # Removes the legend
    plot.background = element_rect(fill = "#191414", color = "#191414"),  # Set the plot background color to black
    panel.background = element_rect(fill = "#191414", color = "#191414"),  # Set the panel background color to black
    text = element_text(color = "#FFFFFF"),  # Set text color to white for all text elements
    axis.title = element_text(color = "#FFFFFF"),  # Set axis titles to white
    axis.text = element_text(color = "#FFFFFF"),  # Set axis text to white
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )



#################______TEXT ANALYSIS BENCHMARK_______#######################

benchmark_sample <- data_clean %>%
  mutate(text_wdct=str_count(lyrics_sliced,"[[:alpha:]]+"),
         sentiment=sentiment_by(lyrics_sliced)$ave_sentiment)

saveRDS(benchmark_sample, file="benchmark_sample.rds")


#accuracy for model with word count (benchmark1)
acc_wdct<-kendall_acc(data_clean$track_popularity,
                      benchmark_sample$text_wdct); acc_wdct
saveRDS(acc_wdct, file="acc_wdct.rds")


#accuracy for sentiment analysis(benchmark2)
acc_sentiment<-kendall_acc(data_clean$track_popularity,
                           benchmark_sample$sentiment); acc_sentiment
saveRDS(acc_sentiment, file="acc_sentiment.rds")




#################______N grams and Embeddings______#######################

set.seed(2022)

#number of rows in the data set
nrow(data_clean)
nrow(data_clean)*0.80

# split into train and test
train_split=sample(1:nrow(data_clean),18738)

song_train<-data_clean%>%
  slice(train_split)

song_test<-data_clean%>%
  slice(-train_split)


############   MODEL 1 : N GRAM with lemmatization

#TAB function modification to omit word stemming
TAB_dfm_no_stemming<-function(text,
                              ngrams=1,
                              stop.words=TRUE,
                              min.prop=.01){
  # First, we check our input is correct
  if(!is.character(text)){  
    stop("Must input character vector")
  }
  custom_stopwords <- "" # add a vector c(...) if you want to stem a series of words 
  #uses stop.words arugment to adjust what is dropped
  drop_list <- if(stop.words) c(stopwords("en"), custom_stopwords) else custom_stopwords 
  # quanteda pipeline
  text_data<-text %>%
    replace_contraction() %>%
    tokens(remove_numbers=TRUE,
           remove_punct = TRUE) %>%
    #tokens_wordstem() %>%.  # prevent word stemming as lemmantization instead
    tokens_select(pattern = drop_list, 
                  selection = "remove") %>%
    tokens_ngrams(ngrams) %>%
    dfm() %>%
    dfm_trim(min_docfreq = min.prop,docfreq_type="prop")
  return(text_data)
}


#lemmatization- training set
rev_tiny_song<-spacy_parse(song_train$lyrics_sliced,
                           nounphrase = T,
                           lemma = T,
                           dependency = T,
                           pos = T,
                           tag=T)

saveRDS(rev_tiny_song, file="rev_tiny_song.rds")
#rev_tiny_song<-readRDS(file="rev_tiny_song.rds")


# Recreate documents from the lemmas
song_all_train_lemma = rev_tiny_song %>%
  group_by(doc_id) %>%
  summarize(text=paste(lemma, collapse=" ")) %>%
  mutate(doc_id=as.numeric(str_replace_all(doc_id,"text",""))) %>%
  arrange(doc_id)

saveRDS(song_all_train_lemma, file="song_all_train_lemma.rds")
#lemma_docs_songs<-readRDS(file="lemma_docs_songs.rds")


#TAB function for training set
dfm_song_train<-TAB_dfm_no_stemming(song_all_train_lemma$text,
                        ngrams=1:2) %>%
  convert(to="matrix")

saveRDS(dfm_song_train, file = "dfm_song_train.rds")
dfm_song_train<-readRDS("dfm_song_train.rds")

#lemmatization- test set
rev_tiny_song_test<-spacy_parse(song_test$lyrics_sliced,
                           nounphrase = T,
                           lemma = T,
                           dependency = T,
                           pos = T,
                           tag=T)

saveRDS(rev_tiny_song_test, file="rev_tiny_song_test.rds")


# Recreate documents from the lemmas
song_all_test_lemma = rev_tiny_song_test %>%
  group_by(doc_id) %>%
  summarize(text=paste(lemma, collapse=" ")) %>%
  mutate(doc_id=as.numeric(str_replace_all(doc_id,"text",""))) %>%
  arrange(doc_id)

saveRDS(song_all_test_lemma, file="song_all_test_lemma.rds")


#TAB function for test set
dfm_song_test<-TAB_dfm_no_stemming(song_all_test_lemma$text,
                       ngrams=1:2) %>%
  dfm_match(colnames(dfm_song_train)) %>%
  convert(to="matrix")

#saveRDS(dfm_song_test, file = "dfm_song_test.rds")
#dfm_song_test<-readRDS("dfm_song_test.rds")

# train the model with a lasso regression 
lasso_ngram_model_1<-glmnet::cv.glmnet(x=dfm_song_train,
                                       y=song_train$track_popularity)

saveRDS(lasso_ngram_model_1, file = "lasso_ngram_model_1.rds")
#lasso_ngram_model_1<-readRDS("lasso_ngram_model_1.rds")

#predict track popularity with the test set
test_predict_ngram_1<-predict(lasso_ngram_model_1, newx = dfm_song_test,
                              s="lambda.min")

#calculate accuracy and overall performance of the model
# answer: 62.23 60.84 63.62
acc_ngram_1<-kendall_acc(test_predict_ngram_1,song_test$track_popularity); acc_ngram_1

saveRDS(acc_ngram_1, file = "acc_ngram_1.rds")


#plot coefficients
# plot lasso_ngram_model_1
plotDat_ngram<-lasso_ngram_model_1 %>%
  coef() %>%
  drop() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ngram") %>%
  rename(score=".") %>%
  filter(score!=0 & ngram!="(Intercept)" & !is.na(score))  %>%
  # add ngram frequencies for plotting
  left_join(data.frame(ngram=colnames(dfm_song_train),
                       freq=colMeans(dfm_song_train)))

plotDat_ngram %>%
  mutate_at(vars(score,freq),~round(.,3))


#plot with x limits from 0 to 1
plotDat_ngram %>%
  ggplot(aes(x=score, y=freq, label=ngram, color=score)) +
  scale_color_gradient(low="white", high="#1DB954") +
  geom_vline(xintercept=0, color = "white") +
  geom_point(color="green1") +  # Setting point color to the specified green
  geom_label_repel(max.overlaps = 15, force = 6, fill = "#212121") +  # Set label text color to white
  scale_y_continuous(trans="log2", breaks=c(.01,.05,.1,.2,.5,1,2,5)) +
  scale_x_continuous(limits = c(-1, 1)) +  # Set x-axis limits
  theme_bw() +
  labs(x="Coefficient in Model", y="Frequency") +
  theme(
    legend.position = "none",
    axis.title=element_text(size=20, color="#FFFFFF"),  # Set axis titles to white
    axis.text=element_text(size=16, color="#FFFFFF"),  # Set axis text to white
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    plot.background = element_rect(fill = "#191414"),  # Set the plot background color to black #212121
    panel.background = element_rect(fill = "#191414"),  # Set the panel background color to black #212121
    panel.grid.major = element_line(color = "#424242"),  # Optional: Adjust major grid lines
    panel.grid.minor = element_line(color = "#616161"),  # Optional: Adjust minor grid lines
    plot.title = element_text(color="#FFFFFF")  # Set plot title to white
  )



###########  MODEL 2 : N-GRAMS with stemming only

#load TAB
source("TAB_dfm.R")


#TAB function for training set
dfm_song_train_normal<-TAB_dfm(song_train$lyrics_sliced,
                                    ngrams=1:2) %>%
  convert(to="matrix")

saveRDS(dfm_song_train_normal, file = "dfm_song_train_normal.rds")


#dfm_song_train<-readRDS("dfm_song_train.rds")

#TAB function for test set
dfm_song_test_normal<-TAB_dfm(song_test$lyrics_sliced,
                       ngrams=1:2) %>%
  dfm_match(colnames(dfm_song_train_normal)) %>%
  convert(to="matrix")

saveRDS(dfm_song_test_normal, file = "dfm_song_test_normal.rds")


# train the model with a lasso regression 
lasso_ngram_model_normal<-glmnet::cv.glmnet(x=dfm_song_train_normal,
                                       y=song_train$track_popularity)

saveRDS(lasso_ngram_model_normal, file = "lasso_ngram_model_normal.rds")


#predict EPS_actual with the test set
test_predict_ngram_normal<-predict(lasso_ngram_model_normal, newx = dfm_song_test_normal,
                              s="lambda.min")

#calculate accuracy and overall performance of the model
# answer: lambda.min 62.92 61.53  64.3
acc_ngram_normal<-kendall_acc(test_predict_ngram_normal,song_test$track_popularity); acc_ngram_normal

saveRDS(acc_ngram_normal, file = "acc_ngram_normal.rds")


#plot coefficients
# plot lasso_ngram_model_1
plotDat_ngram_normal<-lasso_ngram_model_normal %>%
  coef() %>%
  drop() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ngram") %>%
  rename(score=".") %>%
  filter(score!=0 & ngram!="(Intercept)" & !is.na(score))  %>%
  # add ngram frequencies for plotting
  left_join(data.frame(ngram=colnames(dfm_song_train_normal),
                       freq=colMeans(dfm_song_train_normal)))

plotDat_ngram_normal %>%
  mutate_at(vars(score,freq),~round(.,3))


plotDat_ngram_normal %>%
  ggplot(aes(x=score, y=freq, label=ngram, color=score)) +
  scale_color_gradient(low="white", high="#1DB954") +
  geom_vline(xintercept=0) +
  geom_point(color="green1") +  # Setting point color to the specified green
  geom_label_repel(max.overlaps = 15, force = 6, fill = "#212121") +  # Set label text color to white
  scale_y_continuous(trans="log2", breaks=c(.01,.05,.1,.2,.5,1,2,5)) +
  theme_bw() +
  labs(x="Coefficient in Model", y="Frequency") +
  theme(
    legend.position = "none",
    axis.title=element_text(size=20, color="#FFFFFF"),  # Set axis titles to white
    axis.text=element_text(size=16, color="#FFFFFF"),  # Set axis text to white
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    plot.background = element_rect(fill = "#191414"),  # Set the plot background color to black #212121
    panel.background = element_rect(fill = "#191414"),  # Set the panel background color to black #212121
    panel.grid.major = element_line(color = "#424242"),  # Optional: Adjust major grid lines
    panel.grid.minor = element_line(color = "#616161"),  # Optional: Adjust minor grid lines
    plot.title = element_text(color="#FFFFFF")  # Set plot title to white
  )






#MODEL 3 : EMBEDDING

# project data to embedding space for training set 
vdat_song_train<-vecCheck(song_train$lyrics_sliced,
                          vecFile,
                          wfFile,
                          PCAtrim=1)

#saveRDS(vdat_song_train, file = "vdat_song_train.rds")
#vdat_earn_train<-readRDS(file="vdat_earn_train.rds")

# project data to embedding space for test set 
vdat_song_test<-vecCheck(song_test$lyrics_sliced,
                         vecFile,
                         wfFile,
                         PCAtrim=1)

#saveRDS(vdat_song_test, file = "vdat_song_test.rds")
#vdat_song_test<-readRDS(file="vdat_song_test.rds")


#train model using word2vec embeddings
lasso_embed_model_2<-glmnet::cv.glmnet(x=vdat_song_train,
                                       y=song_train$track_popularity)

saveRDS(lasso_embed_model_2, file = "lasso_embed_model_2.rds")
#lasso_embed_model_2<- readRDS(file="lasso_embed_model_2.rds")

#predict using word2vec embeddings with the test set
test_predict_embed<-predict(lasso_embed_model_2,
                            newx = vdat_song_test,
                            s="lambda.min")

#calculate accuracy and overall performance of the model
acc_embed<-kendall_acc(test_predict_embed,song_test$track_popularity);acc_embed
saveRDS(acc_embed, file = "acc_embed.rds")





#MODEL 4 : EMBEDDING + N-grams

#combine ngram and embedding features into one set
combined_x_train_model_3=cbind(vdat_song_train,dfm_song_train_normal)
combined_x_test_model_3=cbind(vdat_song_test,dfm_song_test_normal)

#train the model with embeddings and n grams using a lasso regression
lasso_combined_model_3<-glmnet::cv.glmnet(x=combined_x_train_model_3,
                                          y=song_train$track_popularity)

#store and back up the model
saveRDS(lasso_combined_model_3, file = "lasso_combined_model_3.rds")


#predict popularity using the lasso model with the 2 features (embeddings and ngrams)
test_combined_model_3<-predict(lasso_combined_model_3,
                               newx = combined_x_test_model_3,
                               s="lambda.min")

#calculate accuracy and overall performance of the model
acc_combined<-kendall_acc(test_combined_model_3,song_test$track_popularity);acc_combined






#MODEL 5 : N-grams lemmatization combined with song features

#combine ngram and song features into one set
combined_x_train_model_4_songfeatures=cbind(dfm_song_train_normal,
                                       song_train$loudness,
                                       song_train$danceability,
                                       song_train$energy,
                                       song_train$word_count,
                                       song_train$instrumentalness,
                                       song_train$acousticness,
                                       song_train$mode)

#combine ngram and song features into one for test set
combined_x_test_model_4_songfeatures=cbind(dfm_song_test_normal,
                                           song_test$loudness,
                                           song_test$danceability,
                                           song_test$energy,
                                           song_test$word_count,
                                           song_test$instrumentalness,
                                           song_test$acousticness,
                                           song_test$mode)


#train the model using a lasso regression
lasso_combined_model_4_songfeatures<-glmnet::cv.glmnet(x=combined_x_train_model_4_songfeatures,
                                          y=song_train$track_popularity)

#store and back up the model
saveRDS(lasso_combined_model_4_songfeatures, file = "lasso_combined_model_4_songfeatures.rds")


#predict popularity using the lasso model with the 2 features (embeddings and ngrams)
test_combined_model_4_songfeatures<-predict(lasso_combined_model_4_songfeatures,
                               newx = combined_x_test_model_4_songfeatures,
                               s="lambda.min")

#calculate accuracy and overall performance of the model
acc_combined_4_songfeatures<-kendall_acc(test_combined_model_4_songfeatures,song_test$track_popularity);acc_combined_4_songfeatures



#MODEL 6 : Lasso with song features only

# Convert 'key' to a factor and then to dummy variables for the test set
song_train <- song_train %>%
  mutate(key = as.factor(key),
         explicit = as.integer(explicit == "True"))  # Convert 'explicit' to binary

# Select features for the model and create dummy variables for 'key'
song_features_train <- song_train %>%
  select(explicit, danceability, energy, key, loudness,
         mode, speechiness, acousticness, instrumentalness, liveness,
         valence, tempo, duration_sec, word_count) %>%
  mutate(key = as.numeric(key)) %>%  # Convert 'key' to numeric if it's factor level indices
  model.matrix(~ . -1, data = .)  # The '-1' removes the intercept


# Convert 'key' to a factor and then to dummy variables
song_test <- song_test %>%
  mutate(key = as.factor(key),
         explicit = as.integer(explicit == "True"))  # Convert 'explicit' to binary

# Select features for the model and create dummy variables for 'key' for the test set
song_features_test <- song_test %>%
  select(explicit, danceability, energy, key, loudness,
         mode, speechiness, acousticness, instrumentalness, liveness,
         valence, tempo, duration_sec, word_count) %>%
  mutate(key = as.numeric(key)) %>%  # Convert 'key' to numeric if it's factor level indices
  model.matrix(~ . -1, data = .)  # The '-1' removes the intercept



#train the model using a lasso regression
lasso_songfeatures<-glmnet::cv.glmnet(x=song_features_train,
                                      y=song_train$track_popularity)


#predict popularity using the lasso model
predict_songfeatures<-predict(lasso_songfeatures,
                              newx = song_features_test,
                              s="lambda.min")


#calculate accuracy and overall performance of the model
acc_songfeatures<-kendall_acc(predict_songfeatures,song_test$track_popularity);acc_songfeatures




#################______Topic analysis______#######################
source("TAB_dfm.R")
source("TAB_dfm_stop_words.R")
source("kendall_acc.R")

lyrics = read.csv("song_audio_spotify_temp.csv")

##### Data Processing for both topics and sentiment analysis

# Clean data
lyrics =  lyrics %>% 
  mutate(lyrics_sliced = str_replace_all(lyrics_sliced, "\\b (Verse|Intro|Outro|Lyrics|Chorus| Guitar|guitar/bridge|Bridge)\\b","")) %>%
  mutate(lyrics_sliced = str_remove(lyrics_sliced, "\\s*Embed$")) # Removes 'Embed' if it's the last word in the string

# Filter dataset on language
lyrics_s = lyrics %>% filter(language == "en")

# Create categories for the popularity score
lyrics_s = lyrics_s %>% mutate(quintile = ntile(track_popularity, 5))

lyrics_s = lyrics_s %>%
  mutate(quintile_label = case_when(
    quintile == 1 ~ "Last 20%",
    quintile == 2 ~ "20-40%",
    quintile == 3 ~ "40-60%",
    quintile == 4 ~ "60-80%",
    quintile == 5 ~ "Top 20%"))

lyrics_s = lyrics_s %>% select(-quintile) %>% rename(quintile = quintile_label)

# Add contextual features
lyrics_s = lyrics_s %>% mutate(wdct=str_count(lyrics_sliced,"[[:alpha:]]+"),
                               wdct_density = wdct/(duration_ms/1000))
# Set up training and validation splits
set.seed(2025)
train_split = sample(1:nrow(lyrics_s),18738) # 80% of data

lyrics_s_train = lyrics_s[train_split,]
lyrics_s_test = lyrics_s[-train_split,]

##### Topics model

# Create DFM matrix and build the model
lyrics_s_dfm_train = TAB_dfm(lyrics_s_train$lyrics_sliced, ngrams=1)
lyrics_s_topic_model = stm(lyrics_s_dfm_train,K=15)

# Rename the topics
topicNum = lyrics_s_topic_model$settings$dim$K
topicNames[9] = "Heart-Broken:"
topicNames[10] = "Moving On:"
topicNames[12] = "Throw Back:"
topicNames[8] = "Family:"
topicNames[15] = "Daily Life:"
topicNames[2] = "Coursing:"
topicNames[6] = "Playlist:"
topicNames[7] = "Religion:"
topicNames[14] = "Romance:"
topicNames[3] = "Night Out:"
topicNames[5] = "Nature:"
topicNames[11] = "Love"
topicNames[13] = "Party Girls:"
topicNames[4] = "Dance:"
topicNames[1] = "Rebelious beats:"

# Most common topics, and most common words from each topic
plot(lyrics_s_topic_model,type="summary",n = 10,xlim=c(0,.4),labeltype = "frex",
     topic.names = topicNames) 

# Cloud representation of top topic "Heart-Broken"
cloud(lyrics_s_topic_model,9)

# Topic proportion as feature to predict popularity
lyrics_s_topic_prop_train = lyrics_s_topic_model$theta
dim(lyrics_s_topic_prop_train)
colnames(lyrics_s_topic_prop_train) = topicNames

# Build regression model
lyrics_s_train_regression = lyrics_s_train %>% slice(1:nrow(lyrics_s_topic_prop_train))

lyrics_s_topic_prop_stm = glmnet::cv.glmnet(x=lyrics_s_topic_prop_train, y=lyrics_s_train_regression$track_popularity)
plot(lyrics_s_topic_prop_stm)

# DFM matrix of test set
lyrics_s_dfm_test = TAB_dfm(lyrics_s_test$lyrics_sliced, ngrams=1)

# Evaluate topic model on test set
lyrics_s_topic_prop_test = fitNewDocuments(lyrics_s_topic_model,
                                           lyrics_s_dfm_test %>%
                                             convert(to="stm") %>%
                                             `$`(documents))

test_stm_predict = predict(lyrics_s_topic_prop_stm, newx = lyrics_s_topic_prop_test$theta, s="lambda.min")[,1]

# Estimate accuracy of the model
lyrics_s_test_regression = lyrics_s_test %>% slice(1:length(test_stm_predict))
acc_topic_prop_stm = kendall_acc(lyrics_s_test_regression$track_popularity,test_stm_predict)
acc_topic_prop_stm

#################______Sentiment analysis______#######################

# Add sentiment vector
lyrics_s_test = lyrics_s_test %>%
  mutate(sentiment=sentiment_by(lyrics_sliced)$ave_sentiment)

# Calculate accuracy score
sentiment_acc = kendall_acc(lyrics_s_test$sentiment, lyrics_s_test$track_popularity)
sentiment_acc

# Only keep songs with most significant sentiment coefficients
lyrics_s_test_sentiment_popular_category = lyrics_s_test %>%
  group_by(quintile) %>% 
  summarize(mean_sentiment = mean(sentiment), sd_sentiment=sd(sentiment), mean_popular = mean(track_popularity)) %>%
  arrange(desc(quintile))

# Create sentiment coefficient plot
ggplot(lyrics_s_test_sentiment_popular_category, aes(x = reorder(quintile, mean_sentiment), y = mean_sentiment)) +
  geom_bar(stat = "identity", fill = "#1DB954") +
  coord_flip() +  # To make the plot horizontal
  labs(x = "Quintiles of popularity score", y = "Sentiment Score") +
  ggtitle("Sentiment-based Popularity") +
  theme_classic() + 
  theme(
    plot.background = element_rect(fill="#191414", color="#191414"),
    panel.background = element_rect(fill = "#191414", color="white"),
    text = element_text(color="white"),
    axis.title = element_text(color="white"),
    axis.text = element_text(color="white"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(color="white", hjust = 0.5, size = 20))

#ggsave("Sentiment_based Popularity_Track_Ranking.png", dpi=300, width = 15, height = 10)

#################______Deep-Dive into top 3 topics identified______#######################

##### Ngrams analysis on subset of songs from top 3 topics 

# Define the indices of the topics of interest
topics_of_interest = c(9, 10, 12)

# Identify the index of the topic with maximum proportion for each document
max_topic_indices = apply(lyrics_s_topic_prop_train, 1, which.max)

# Check if the maximum topic index for each document is one of the topics of interest
indices_of_interest = which(max_topic_indices %in% topics_of_interest)

# Extract the lyrics belonging to the topics of interest
songs_of_interest = lyrics_s_train[indices_of_interest, ]

# Ngram model to break down the lyrics of the songs of interest
songs_of_interest_dfm = TAB_dfm_stop_words(songs_of_interest$lyrics_sliced, ngrams=1:2)
lasso_songs_dfm = glmnet::cv.glmnet(x=songs_of_interest_dfm, y=songs_of_interest$track_popularity)
plot(lasso_songs_dfm)

# Coefficient plot for the song of interest
lasso_songs_dfm_min_lambda = coef(lasso_songs_dfm, s = "lambda.min")

Plot_Songs_Top_Topics = lasso_songs_dfm_min_lambda %>%
  drop() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ngram") %>%
  rename(score=".") %>%
  filter(score!=0 & ngram!="(Intercept)" & !is.na(score))  %>%
  # add ngram frequencies for plotting
  left_join(data.frame(ngram=colnames(songs_of_interest_dfm),
                       freq=colMeans(songs_of_interest_dfm)))

Plot_Songs_Top_Topics %>%
  mutate_at(vars(score,freq),~round(.,3))

Plot_Songs_Top_Topics %>%
  ggplot(aes(x=score,y=freq,label=ngram,color=score)) +
  scale_color_gradient2(low="#FF9933", mid="#FFFFFF", high="#CC33FF", midpoint=0, limits=c(-1.1, 0.7))+
  geom_vline(xintercept=0, color="white")+
  geom_point() +
  geom_label_repel(max.overlaps = 22,force = 6, fill = "#191414")+  
  scale_y_continuous(trans="log2", breaks=c(.01,.05,.1,.2,.5,1,2,5))+
  scale_x_continuous(limits = c(-1.1,0.7), breaks = seq(-1.1,0.7,.2))+
  theme_bw() +
  labs(x="Coefficient in Model",y="Uses per Opening Speech")+
  theme(legend.position = "none",
        axis.title=element_text(size=18, color="white"),
        axis.text=element_text(size=16, color="white"),
        plot.background = element_rect(fill="#191414", color="#191414"),
        panel.background = element_rect(fill = "#191414"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "white"))

#ggsave("Deep_Dive_Analysis_Top_Topic.png", dpi=200, width = 20, height = 10)


#################______Impact of audio features on topics model______#######################

##### Regression combining topic models and audio features

# Combine influential audio features with topic proportions 
combined_features_topics = cbind(lyrics_s_topic_prop_train,
                                 lyrics_s_train$danceability,
                                 lyrics_s_train$energy,
                                 lyrics_s_train$wdct,
                                 lyrics_s_train$loudness,
                                 lyrics_s_train$track_popularity)

# Scale data for same importance
combined_features_topics_scaled = as.data.frame(scale(combined_features_topics))

# Split between predictors and target variable
x_features_topics_scaled = as.matrix(combined_features_topics_scaled[, -20]) # Features
y_features_topics_scaled = combined_features_topics_scaled$V20                  # Target variable

# Lasso regression model
lasso_features_topics = cv.glmnet(x_features_topics_scaled, y_features_topics_scaled, alpha = 1)

# Predict popularity on test set with the combined features-topics model
combined_features_topics_test = cbind(lyrics_s_topic_prop_test$theta,
                                      lyrics_s_test$danceability,
                                      lyrics_s_test$energy,
                                      lyrics_s_test$wdct,
                                      lyrics_s_test$loudness)

combined_features_topics_test_scaled = as.data.frame(scale(combined_features_topics_test))
predict_features_topics = predict(lasso_features_topics, s = "lambda.min", newx = as.matrix(combined_features_topics_test_scaled))

# Calculate the accuracy of the combined features-topics model
lyrics_s_test_regression2 = lyrics_s_test %>% slice(1:nrow(predict_features_topics))
acc_features_topics = kendall_acc(predict_features_topics, lyrics_s_test_regression2$track_popularity)
acc_features_topics

##### Accuracy plot of topic only, sentiment only and topics combined with audio features
# Plot accuracy of the 2 models
bind_rows(acc_topic_prop_stm %>%
            mutate(field="Topic Model"),
          sentiment_acc %>%
            mutate(field="Sentiment Model"),
          acc_features_topics %>%
            mutate(field="Combined Features-Topics")) %>%
  ggplot(aes(x=field,color=field,
             y=acc,ymin=lower,ymax=upper)) +
  geom_point() +
  geom_errorbar(width=.4) +
  scale_y_continuous(breaks = seq(45, 75, 1)) + 
  theme_bw() +
  labs(x="Prediction Model",y="Accuracy Score") +
  geom_hline(yintercept = 50) +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        panel.grid=element_blank(),
        legend.position="none") +
  theme(
    plot.background = element_rect(fill="#191414", color="#191414"),
    panel.background = element_rect(fill = "#191414", color="white"),
    text = element_text(color="white"),
    axis.title = element_text(color="white"),
    axis.text = element_text(color="white"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(color="white", hjust = 0.5, size = 20))

#ggsave("Comparison_Topic_Sentiment_Combined_Models.png", width = 10, height = 8)

#################______All Models comparison______#######################

#####
# temporarily hard coding acc topic modelling
acc_topic<-data.frame(
  acc=58.69,
  lower=57.28,
  upper=60.1)

#####

accuracy_data <- bind_rows(
  acc_ngram_1 %>% mutate(field="N-gram lemmatization"),
  acc_ngram_normal %>% mutate(field="N-gram stemming"),
  acc_embed %>% mutate(field="Embedding"),
  acc_combined %>% mutate(field="Embedding + N-gram"),
  acc_combined_4_songfeatures %>% mutate(field="N-gram + song features"),
  acc_wdct %>% mutate(field="Benchmark word count"),
  acc_sentiment %>% mutate(field="Benchmark sentiment"),
  acc_songfeatures %>% mutate(field="Song features"),
  acc_topic %>% mutate(field="Topic Modelling + song features")
) %>%
  mutate(field = reorder(field, acc, FUN = median))  # Order by median accuracy

# Plot the ordered accuracy scores
ggplot(accuracy_data, aes(x = field, color = field, y = acc, ymin = lower, ymax = upper)) +
  geom_point() +
  geom_errorbar(width = 0.3, size = 1.2) +  # Increase thickness of error bars
  theme_bw() +
  labs(x = "Models", y = "Accuracy (%)") +
  geom_hline(yintercept = 50, color="#FFFFFF")+
  theme(
    legend.position = "none",
    axis.title = element_text(size = 14),  # Reduce size of axis titles
    axis.text = element_text(size = 11, color = "#FFFFFF"),  # Set axis text to white and reduce size
    axis.text.y = element_text(size=12),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  # Rotate x-axis labels
    plot.background = element_rect(fill = "#191414"),  # Set the plot background color to dark
    panel.background = element_rect(fill = "#191414"),  # Set the panel background color to dark
    panel.grid.major = element_line(color = "#424242"),  # Adjust major grid lines
    panel.grid.minor = element_line(color = "#616161"),  # Adjust minor grid lines
    plot.title = element_text(color = "#FFFFFF")  # Set plot title to white
  )


# Calculate the absolute errors between the predictions and the actual values
song_test$predicted_popularity <- test_combined_model_4_songfeatures
song_test$abs_error <- abs(song_test$predicted_popularity - song_test$track_popularity)

# Arrange by absolute error to find the best and worst predictions
song_test <- song_test %>%
  arrange(abs_error)

# Order the song_test dataframe by absolute error
song_test <- song_test %>%
  arrange(abs_error)

# Extract the example with the smallest prediction error
good_prediction_example <- song_test[1,]
good_prediction_lyrics <- good_prediction_example$lyrics_sliced
good_prediction_actual_popularity <- good_prediction_example$track_popularity
good_prediction_predicted_popularity <- good_prediction_example$predicted_popularity

# Extract the example with the largest prediction error
bad_prediction_example <- song_test[3771,]
bad_prediction_lyrics <- bad_prediction_example$lyrics_sliced
bad_prediction_actual_popularity <- bad_prediction_example$track_popularity
bad_prediction_predicted_popularity <- bad_prediction_example$predicted_popularity

# Print the examples with the actual and predicted popularity
cat("Good Prediction Example:\n")
cat("Song name:", good_prediction_example$track_name, "\n")
cat("Song artist:", good_prediction_example$track_artist, "\n")
cat("Lyrics Sliced:", good_prediction_lyrics, "\n")
cat("Actual Popularity:", good_prediction_actual_popularity, "\n")
cat("Predicted Popularity:", good_prediction_predicted_popularity, "\n\n")

cat("Bad Prediction Example:\n")
cat("Song name:", bad_prediction_example$track_name, "\n")
cat("Song artist:", bad_prediction_example$track_artist, "\n")
cat("Lyrics Sliced:", bad_prediction_lyrics, "\n")
cat("Actual Popularity:", bad_prediction_actual_popularity, "\n")
cat("Predicted Popularity:", bad_prediction_predicted_popularity, "\n")


#################______Filtering for top artists______####################### (didn't use this since the output was not insightful)
#remove space at the beginning of the name
data_clean_artist <- data_clean %>%
  mutate(track_artist = tolower(str_trim(track_artist)))

# Calculating mean popularity for each artist
artist_popularity <- data_clean_artist %>%
  group_by(track_artist) %>%
  summarise(total_popularity = sum(track_popularity, na.rm = TRUE))

# Selecting top 2300 artists by total popularity
top_artists <- artist_popularity %>%
  arrange(desc(total_popularity)) %>%
  slice_head(n = round(nrow(artist_popularity) * 0.30))

# Filtering data for top artists and collapsing lyrics
top_artist_lyrics <- data_clean_artist %>%
  filter(track_artist %in% top_artists$track_artist) %>%
  group_by(track_artist) %>%
  summarise(all_lyrics = paste(lyrics_sliced, collapse = " "))

# Adding the total popularity to the top_artist_lyrics dataframe
top_artist_lyrics <- top_artist_lyrics %>%
  left_join(artist_popularity, by = "track_artist")

# Adding the total popularity to the top_artist_lyrics dataframe
top_artist_lyrics_wordcount <- top_artist_lyrics %>%
  mutate(word_count = str_count(all_lyrics, "\\S+"))

#plot the popularity distribution again
ggplot(top_artist_lyrics_wordcount, aes(x = total_popularity)) +
  geom_histogram(bins = 30, fill = "#1DB954", color = "white") + # You can adjust the number of bins based on your data range
  labs(title = "Distribution of Track Popularity",
       x = "Track Popularity",
       y = "Frequency") +
  theme(
    legend.position = "none",  # Removes the legend
    plot.background = element_rect(fill = "#191414", color = "#191414"),  # Set the plot background color to black
    panel.background = element_rect(fill = "#191414", color = "#191414"),  # Set the panel background color to black
    text = element_text(color = "#FFFFFF"),  # Set text color to white for all text elements
    axis.title = element_text(color = "#FFFFFF"),  # Set axis titles to white
    axis.text = element_text(color = "#FFFFFF"),  # Set axis text to white
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )


#lemmatization- training set
rev_top_artists<-spacy_parse(top_artist_lyrics$all_lyrics,
                           nounphrase = T,
                           lemma = T,
                           dependency = T,
                           pos = T,
                           tag=T)

#saveRDS(rev_top_artists, file="rev_top_artists.rds")
#rev_top_artists<-readRDS("rev_top_artists.rds")

# Recreate documents from the lemmas, excluding specific words
lemma_docs_songs_top_artists <- rev_top_artists %>%
  group_by(doc_id) %>%
  summarize(text = paste(lemma, collapse = " ")) %>%
  mutate(doc_id = as.numeric(str_replace_all(doc_id, "text", ""))) %>%
  arrange(doc_id)

#saveRDS(lemma_docs_songs_top_artists, file="lemma_docs_songs_top_artists.rds")
#lemma_docs_songs_top_artists<-readRDS("lemma_docs_songs.rds")

#TAB function for training set
dfm_song_train_top_artists<-TAB_dfm_no_stemming(lemma_docs_songs_top_artists$text,
                        ngrams=1:2) %>%
  convert(to="matrix")

#saveRDS(dfm_song_train_top_artists, file = "dfm_song_train_top_artists.rds")
#dfm_song_train_top_artists<-readRDS("dfm_song_train_top_artists.rds")

# train the model with a lasso regression 
lasso_ngram_model_top_artists<-glmnet::cv.glmnet(x=dfm_song_train_top_artists,
                                       y=top_artist_lyrics$total_popularity)

#saveRDS(lasso_ngram_model_top_artists, file = "lasso_ngram_model_top_artists.rds")
lasso_ngram_model_top_artists<-readRDS("lasso_ngram_model_top_artists.rds")

#plot coefficients
# plot lasso_ngram_model_1
plotDat_ngram_top_artists<-lasso_ngram_model_top_artists %>%
  coef() %>%
  drop() %>%
  as.data.frame() %>%
  rownames_to_column(var = "ngram") %>%
  rename(score=".") %>%
  filter(score!=0 & ngram!="(Intercept)" & !is.na(score))  %>%
  # add ngram frequencies for plotting
  left_join(data.frame(ngram=colnames(dfm_song_train_top_artists),
                       freq=colMeans(dfm_song_train_top_artists)))

plotDat_ngram_top_artists %>%
  mutate_at(vars(score,freq),~round(.,3))


plotDat_ngram_top_artists %>%
  ggplot(aes(x=score, y=freq, label=ngram, color=score)) +
  scale_color_gradient(low="white", high="#1DB954") +
  geom_vline(xintercept=0, color = "white") +
  geom_point(color="green1") +  # Setting point color to the specified green
  geom_label_repel(max.overlaps = 30, force = 6, fill = "#191414") +  # Set label text color to white
  scale_y_continuous(trans="log2", breaks=c(.01,.05,.1,.2,.5,1,2,5)) +
  theme_bw() +
  labs(x="Coefficient in Model", y="Frequency") +
  theme(
    legend.position = "none",
    axis.title=element_text(size=20, color="#FFFFFF"),  # Set axis titles to white
    axis.text=element_text(size=16, color="#FFFFFF"),  # Set axis text to white
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    plot.background = element_rect(fill = "#191414"),  # Set the plot background color to black #212121
    panel.background = element_rect(fill = "#191414"),  # Set the panel background color to black #212121
    panel.grid.major = element_line(color = "#424242"),  # Optional: Adjust major grid lines
    panel.grid.minor = element_line(color = "#616161"),  # Optional: Adjust minor grid lines
    plot.title = element_text(color="#FFFFFF")  # Set plot title to white
  )





