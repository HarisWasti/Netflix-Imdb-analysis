library(ggplot2)
library(dplyr)
library(readr)

clean_Netflix_imdb_data <- read_csv("C:/Users/haris/OneDrive/Desktop/Data viz coursework/clean_Netflix_imdb_data.csv")

#filter votes that are more than a 1000
netflix_1000 <- clean_Netflix_imdb_data %>%
  filter(imdb_votes >= 1000)

#set the imdb rating in descending order
rating_desc <- netflix_1000 %>%
  arrange(desc(imdb_score))

#cutting top/bottom 10 and 100 for graphs
top_10_movies_show <- head(rating_desc, 10)
bottom_10_movies_show <- tail(rating_desc, 10)
top_100_movies_show <- head(rating_desc, 100)
bottom_100_movies_show<- tail(rating_desc, 100)

#joining top and bottom 10
top_and_bottom <- rbind(top_10_movies_show, bottom_10_movies_show)

#joining top/bottom 100
top_100_and_bottom_100 <- rbind(top_100_movies_show, bottom_100_movies_show)

#top 10 movies/shows
top_10_movies_show %>%
  ggplot(aes(x = reorder(title, imdb_score), y = imdb_score)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme_bw() +
  labs(title = "Top 10 Movies/shows on Netflix based on IMDb Score",
       x = "Movie/show Title",
       y = "IMDb Score") +
  coord_flip()

#top 10 and bottom 10 with movie or show coloured
top_and_bottom %>%
  ggplot(aes(x = reorder(title, imdb_score), y = imdb_score - 5, fill = type)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  scale_fill_manual(values = c("MOVIE" = "blue", "SHOW" = "brown")) +
  labs(title = "Top 10 Best and Worst Movies/Shows",
       x = "Title",
       y = "Average") +
  theme_bw()

#top 100 and bottom 100 with movie or show coloured
top_100_and_bottom_100 %>%
  ggplot(aes(x = reorder(title, imdb_score), y = imdb_score - 5, fill = type)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  scale_fill_manual(values = c("MOVIE" = "blue", "SHOW" = "brown")) +
  labs(title = "Top 100 Best and Worst Movies/Shows",
       x = "Title",
       y = "Average") +
  theme_bw()

#distribution of ratings for movies and shows
clean_Netflix_imdb_data %>%
  ggplot(aes(y = imdb_score)) +
  geom_boxplot() +
  facet_wrap(~type) +
  labs(title = "IMDb Rating Distribution by Type",
       x = "Type",
       y = "IMDb Rating") +
  theme_bw()

#distribution for ratings for each age certification
clean_Netflix_imdb_data %>%
  ggplot(aes(x = age_certification, y = imdb_score, fill = type)) +
  geom_boxplot() +
  facet_wrap(~age_certification, scales = "free") +
  labs(title = "IMDb Rating Distribution by Age Certification",
       x = "Age Certification",
       y = "IMDb Rating") +
  theme_bw()

#Distribution of votes
clean_Netflix_imdb_data %>%
  ggplot( aes(x = imdb_votes, fill = type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_bw() +
  labs(title = 'distribution of imdb votes' ,
       fill="type")

#Distribution of votes for age certification
clean_Netflix_imdb_data %>%
  ggplot(aes(x = age_certification, y = imdb_votes, fill = type)) +
  geom_boxplot() +
  facet_wrap(~age_certification, scales = "free") +
  labs(title = "IMDb vote Distribution by Age Certification",
       x = "Age Certification",
       y = "IMDb vote") +
  theme_bw()

#filtering numeric for correlation matrix
numeric_columns <- clean_Netflix_imdb_data %>% select_if(is.numeric)
#remove extra column
numeric_columns <- numeric_columns %>% select(-...1)
#correlation matrix
correlation_matrix <- cor(numeric_columns)
#make correlation a data frame
cor_df <- as.data.frame(as.table(correlation_matrix))

#heatmap for correlation
cor_df %>%
  ggplot(aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "coral", midpoint = 0) +
  labs(title = "Correlation Heatmap") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#imdb score over the years
clean_Netflix_imdb_data %>%
  group_by(release_year, type) %>%
  summarise(avg_imdb_score = mean(imdb_score)) %>%
  ggplot(aes(x = release_year, y = avg_imdb_score, color = type)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, aes(group = type)) +
  labs(title = "Average IMDb Score Over the Years",
       x = "Year",
       y = "Average IMDb Score") +
  theme_bw()

#average popularity over the years
clean_Netflix_imdb_data %>%
  group_by(release_year, type) %>%
  summarise(avg_imdb_votes = mean(imdb_votes)) %>%
  ggplot(aes(x = release_year, y = avg_imdb_votes, color = type)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = type), linetype = 'solid') +
  labs(title = "Average popularity Over the Years",
       x = "Year",
       y = "Average IMDb votes") +
  theme_bw()

#average popularity past 2010
clean_Netflix_imdb_data %>%
  filter(release_year > 2010) %>%
  group_by(release_year, type) %>%
  summarise(avg_imdb_votes = mean(imdb_votes)) %>%
  ggplot(aes(x = release_year, y = avg_imdb_votes, color = type)) +
  geom_line() +
  geom_point() +
  labs(title = "Average popularity Over the Years (after 2010)",
       x = "Year",
       y = "Average IMDb Votes") +
  theme_bw()

#popularity compared to run time
clean_Netflix_imdb_data %>%
  ggplot(aes(x = runtime, y = imdb_votes, color = type)) +
  geom_point() +
  facet_wrap(~type, scales = "free_y") +
  labs(title = "Runtime compared to votes",
       x = "Runtime (min)",
       y = "IMDb Votes") +
  theme_bw()

# Filter out No Certification
no_certification_data <- clean_Netflix_imdb_data %>%
  filter(age_certification != "No certification")

# Calculate total imdb votes and average imdb score for each age certification
avg_age_certification_data <- no_certification_data %>%
  group_by(age_certification) %>%
  summarise(total_imdb_votes = sum(imdb_votes),
            avg_imdb_score = mean(imdb_score))

#bubble chart for age certification with votes and score
ggplot(avg_age_certification_data, aes(x = age_certification, y = avg_imdb_score, size = total_imdb_votes)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(name = "Total IMDb Votes") +
  labs(title = "IMDb Scores/votes by Age Certification",
       x = "Age Certification",
       y = "Average IMDb Score") +
  theme_bw()





