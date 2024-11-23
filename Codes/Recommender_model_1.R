################################################################################
#loading libraries
library(ggplot2)
library(dplyr)
library(stringi)

#loading data
data = read.csv("spotify-2023.csv")

#preparing X4 containing all the musical features, artist names and song names.
X1 = data %>% select(bpm,danceability_.:speechiness_.)
X1 = scale(X1) #Scaling all the numerical values.
X3 = data %>% select(track_name,artist.s._name)
X4 = cbind(X3, X1)

#Creating a new column "norms" that contains all the "Euclidean norms" of the musical features of the songs.
X1 = as.numeric(X1)
mat = matrix(X1,953,8)
norms = numeric(length = length(data$track_name))
for(i in 1:length(norms)){
  norms[i] = norm(mat[i,], "2")
}

X4['eculidean_dist'] = norms

## Model 1
#Building the recommender function based on nearest distance from the given song.
track_name = stri_encode(data$track_name, to = "ASCII")
# Function check song
check_song <- function(song_name, v) {
  # Convert both song_name and vector v to lowercase for case-insensitive comparison
  song_name <- tolower(song_name)
  v <- tolower(v)
  
  # Check if song_name is in the vector v or is a substring of any element in v
  match_index <- which(song_name == v)
  
  if (length(match_index) > 0) {
    return(match_index[1])  # Return only the first exact match
  } else {
    # Check if song_name is a sub-string of any element in v
    close_elements <- v[agrep(song_name, v, ignore.case = TRUE)]
    if (length(close_elements) > 0) {
      cat("Did you mean one of these songs?\n")
      print(close_elements)
    } else {
      cat("Song not found in the vector.\n")
    }
    return(FALSE)
  }
}

#check_song("mastermind", data$track_name)

#constructing the function recommender which recommends 5 songs based on the current song.
recommender = function(song_name){
  x = check_song(song_name, data$track_name)
  if(!is.numeric(x)){
    return(x)
  }
  
  dist = abs(X4$eculidean_dist - X4$eculidean_dist[x]) #calculate the distance between all the songs and song_name
  X4 = X4 %>% mutate('dist' = dist)
  new = X4 %>% arrange(dist)
  output = character(length = 0)
  # If there is common artist, placing that song at the top. 
  for(i in length(2:6)){
    if(any(strsplit(X4$artist.s._name[i],",")[[1]] %in% strsplit(X4$artist.s._name[x],",")[[1]])){
      output = c(new$track_name[i],output)
    }
  }
  output = c(output,new$track_name[2:6]) #Take the first 5 songs closest to song_name
  output = unique(output)
  return(output)
}

#######################################################################################################################
