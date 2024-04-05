#loading libraries
library(ggplot2)
library(dplyr)


#Load Data (will change according to directory structure)
data = read.csv("spotify-2023.csv")

#Creating a vector "unique_artists" with all artists in the data 
vec = character(length = 0)
split = strsplit(data$artist.s._name,",")
for(i in 1:953){
  vec = c(vec,split[[i]])
}
unique_artists = unique(vec)

#Preparing vectors artist, cl to create a contingency table. 
artist = character(length = 0)
cl = numeric(length = 0)
for(i in 1:length(unique_artists)){
  a = is_substring(unique_artists[i],data$artist.s._name)
  w = which(a == TRUE)
  cus = clusters$cluster[w]
  r = rep(unique_artists[i], length(w))
  artist = c(artist,r)
  cl = c(cl,cus)
}

#Since the size of artist and cl is quite large, we break them into 4 tables, and combine them to make a big table.
#This was done because the table function gave inaccuracies when we used the entire artist vector. 
table1 = table(artist[1:664], cl[1:664])
table2 = table(artist[664:998],cl[664:998])
table3 = table(artist[999:1697], cl[999:1697])
table4 = table(artist[2465:2617], cl[2465:2617])

TABLE = rbind(table1,table2,table3,table4)

#This function randomly selects 30 artists in the table and returns the chi-square test results. 
sampled_table = function(tab){
  l = length(tab)/7
  s = sample(1:l,100)
  new_table = tab[s,]
  chi_sq_test <- chisq.test(new_table)
  return(chi_sq_test)
}

print(sampled_table(TABLE))
#since the p-value is very low => the null hypothesis can be rejected.
#Artists' songs fall in the same cluster.

z = numeric(length = 1e3)
for(i in 1:1e3){
  result = sampled_table(TABLE)
  z[i] = result$p.value
}

mean(z,na.rm = TRUE)

