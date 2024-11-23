# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(rvest)      # For web scraping
options(warn = -1)

# Read the CSV file into songs_data dataframe
songs_data <- read.csv("spotify-2023.csv")

#################################################
# Data Preprocessing
#################################################

# Convert specific columns to numeric
songs_data[, c(9)] <- as.numeric(songs_data[, c(9)])
songs_data[, c(12)] <- as.numeric(gsub(",", "", songs_data[, c(12)]))
songs_data[, c(14)] <- as.numeric(gsub(",", "", songs_data[, c(14)]))

# Fill empty strings in 'key' column with 'C'
songs_data$key[songs_data$key == ""] <- "C"

# To check for Null values in Columns
colSums(is.na(songs_data))
# Remove rows with NA values in 'streams' and 'in_shazam_charts' columns
songs_data <- songs_data %>% filter(!is.na(streams))
songs_data <- songs_data %>% filter(!is.na(in_shazam_charts))
# To check for Null values after filtering
colSums(is.na(songs_data))

######################   Plots   ######################

###################### Histogram  #####################
# Define bins for histogram
bins <- seq(2000, 4e9, length.out = 200)

# Create histogram of 'streams' column
hist(songs_data$streams, breaks = bins, main = "Histogram of number of streams", xlab = "Number of Streams", col = "#81cdc6")
# Add vertical lines for quartiles and median
abline(v = quantile(songs_data$streams, probs = c(0.25)), col = 'red', lwd = 2)
abline(v = quantile(songs_data$streams, probs = c(0.75)), col = 'orange', lwd = 2)
abline(v = median(songs_data$streams), col = 'blue', lwd = 2)
# Add legend
legend("topright", legend = c("Median", "1st quartile", "3rd quartile"), fill = c("blue", "red", "orange"))

#####################   Boxplot  #####################
# Create boxplot of 'streams' column
boxplot(songs_data$streams, horizontal = TRUE, col = '#d6b4fc', xlab = "Number of Streams", main = "Boxplot of Number of streams")

#####################  Scatter plots  #####################
# Scatter plot of 'streams' against 'danceability'
#################################################
songs_data %>% ggplot(aes(y = streams, x = danceability_.)) +
  geom_point(aes(col = key, shape = mode)) +
  labs(title = "Scatter plot of number of Streams vs Dancebility", y = "Number of streams", x = "Dancebility %")

#################################################
# Scatter plot of 'streams' against 'valence'
#################################################
songs_data %>% ggplot(aes(y = streams, x = valence_.)) +
  geom_point(aes(col = key, shape = mode)) +
  labs(title = "Scatter plot of number of Streams vs Valence ", y = "Number of streams", x = "Valence %")

#################################################
# Scatter plot of 'streams' against 'energy'
#################################################
songs_data %>% ggplot(aes(y = streams, x = energy_.)) +
  geom_point(aes(col = key, shape = mode)) +
  labs(title = "Scatter plot of number of Streams vs Energy", y = "Number of streams", x = "Energy %")

#################################################
# Scatter plot of 'streams' against 'acousticness'
#################################################
songs_data %>% ggplot(aes(y = streams, x = acousticness_.)) +
  geom_point(aes(col = key, shape = mode)) +
  labs(title = "Scatter plot of number of Streams vs Acousticness", y = "Number of streams", x = "Acousticness %")

#################################################
# Scatter plot of 'streams' against 'instrumentalness'
#################################################
songs_data %>% ggplot(aes(y = streams, x = instrumentalness_.)) +
  geom_point(aes(col = key, shape = mode)) +
  labs(title = "Scatter plot of number of Streams vs Instrumentalness", y = "Number of streams", x = "Instrumentalness %")

#################################################
# Scatter plot of 'streams' against 'liveness'
#################################################
songs_data %>% ggplot(aes(y = streams, x = liveness_.)) +
  geom_point(aes(col = key, shape = mode)) +
  labs(title = "Scatter plot of number of Streams vs Liviness", y = "Number of streams", x = "Liviness %")

#################################################
# Scatter plot of 'streams' against 'speechiness'
#################################################
songs_data %>% ggplot(aes(y = streams, x = speechiness_.)) +
  geom_point(aes(col = key, shape = mode)) +
  labs(title = "Scatter plot of number of Streams vs Speechiness", y = "Number of streams", x = "Speechiness %")

#################################################
# Scatter plot of 'streams' against 'bpm'
#################################################
songs_data %>% ggplot(aes(y = streams, x = bpm)) +
  geom_point(aes(col = key, shape = mode)) +
  labs(title = "Scatter plot of number of Streams vs BPM", y = "Number of streams", x = "BPM")

#################################################
# Scatter plot of 'streams' against 'danceability' faceted by 'key'
#################################################
songs_data %>% ggplot(aes(y = streams, x = danceability_.)) +
  geom_point(aes(col = mode)) +
  facet_wrap(~key) +
  labs(title = "Scatter plot of number of Streams vs Dancebility ", y = "Number of streams", x = "Dancebility %")

#################################################
# Scatter plot of 'streams' against 'valence' faceted by 'key'
#################################################
songs_data %>% ggplot(aes(y = streams, x = valence_.)) +
  geom_point(aes(col = mode)) +
  facet_wrap(~key) +
  labs(title = "Scatter plot of number of Streams vs Valence ", y = "Number of streams", x = "Valence %")

#################################################
# Scatter plot of 'streams' against 'in_spotify_playlists'
#################################################
songs_data %>% ggplot(aes(y = streams, x = in_spotify_playlists)) +
  geom_point(aes(col = key, shape = mode)) +
  labs(title = "Scatter plot of number of Streams vs In Spotify Playlists", y = "Number of streams", x = "In Spotify Playlists")

#################################################
# Scatter plot of 'streams' against 'in_deezer_playlists'
#################################################
songs_data %>% ggplot(aes(y = streams, x = in_deezer_playlists)) +
  geom_point(aes(col = key, shape = mode)) +
  labs(title = "Scatter plot of number of Streams vs In Deezer Playlists", y = "Number of streams", x = "In Deezer Playlists")

