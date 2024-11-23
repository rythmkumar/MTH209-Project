##############
###Mosaic Plot
###############
data = read.csv("songs_complete_data.csv")
top100 <- factor(data$Top100)
genre <- factor(data$Genre)
categories <- sort(unique(genre))
gen.codes <- c("a", "cl", "c", "e", "j", "m", "p", "r", "reg", "rock")

# Convert genre to codes
genre <- gen.codes[match(genre, categories)]

# Create joint distribution table
tab <- table(top100, genre)
joint.dist <- prop.table(tab)

# Conditional distributions
cond.genre <- prop.table(tab, 1)
cond.top <- prop.table(tab, 2)


# Mosaic plot with custom colors, labels, and main title
mosaic(joint.dist, shade = TRUE, main = "Genre and Top100", col = my_colors,
           xlab = "Genre", ylab = "Top100", sub = "Joint Distribution")

###################
####Stacked Barplot
###################
library(ggplot2)
data = read.csv("songs_complete_data.csv")
top100 <- factor(data$Top100)
genre <- factor(data$Genre)
tab <- table(top100, genre)
joint.dist <- prop.table(tab)
cond.genre <- prop.table(tab, 1)
cond.top <- prop.table(tab, 2)



# Melt the joint distribution table for easier plotting
joint_df <- as.data.frame.table(joint.dist)
colnames(joint_df) <- c("Top100", "Genre", "Proportion")

# Melt the conditional genre distribution for plotting
cond_genre_df <- as.data.frame.table(cond.genre)
colnames(cond_genre_df) <- c("Top100", "Genre", "Proportion")

# Melt the conditional top distribution for plotting
cond_top_df <- as.data.frame.table(cond.top)
colnames(cond_top_df) <- c("Top100", "Genre", "Proportion")



# Barplots of conditional distributions using ggplot2 stacked on top of each other
ggplot(cond_genre_df, aes(x = Genre, y = Proportion, fill = Top100)) +
  geom_bar(stat = "identity", position = "stack") +  # Change position to "stack"
  labs(title = "Genre wise Top 100 songs", y = "Proportion", x = "Genre", fill = "Top100",
       legend = c("No", "Yes"))

