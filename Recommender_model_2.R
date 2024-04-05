#######################################################################################################################
#loading data
data = read.csv("spotify-2023.csv")

#loading libraries
library(ggplot2)
library(dplyr)
library(factoextra)
library(stringi)

#Preparing data
X1 = data %>% select(bpm,danceability_.:speechiness_.)
X1 = scale(X1) #Scaling all the numerical values.
X3 = data %>% select(track_name,artist.s._name)
X4 = cbind(X3, X1)
X5 = X4[c(3:10)]

#calculating distance using dist() function
d = dist(X5, method = "euclidean")

#clustering using hierarchical clustering
hc <- hclust(d)
# EDA 

plot(hc, cex = 0.6) 
sub_grp <- cutree(hc, k = 7)
fviz_cluster(list(data = X5, cluster = sub_grp))

#clustering using k-means
fviz_cluster(clusters, X5)
fviz_nbclust(X5, kmeans, method = "wss")

clusters <- kmeans(X5, 7, nstart = 10)

#making a scatter plot by taking the first two principle components of the X5
pca = prcomp(X5)
dat = pca$x
typeof(dat)
dat = dat[,c(1,2)]

dat = as.data.frame(dat)
dat["clusters"] = clusters$cluster

#scatter plot
dat %>% ggplot(aes(PC1,PC2,color = factor(clusters)))+
  geom_point()


data$track_name = stri_encode(data$track_name, to = "ASCII")

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

# Model 2
recommender2 = function(song_name){
  x = check_song(song_name, data$track_name)
  if(!is.numeric(x)){
    return(x)
  }
  
  which_cluster = clusters$cluster[x]
  index = which(clusters$cluster == which_cluster)
  top_5 = sample(index, 5)
  
  output = data$track_name[top_5]
  
  return(output)
}


#recommender2("cruel summer")

########################################################################################################################
