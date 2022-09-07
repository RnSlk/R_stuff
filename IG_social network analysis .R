#

#########################
# Business understanding: Goal is to analyze any given Instagram page. 
#Objectives are to find the most commented posts, the most common word in the post, 
#the social network graph of the user, and the most influential people in the network.
#########################
#1
# Extract source vertex and target vertex from the tweet data frame
rtwt_df <- twts_trvl[, c("screen_name" , "retweet_screen_name" )]

# View the data frame
head(rtwt_df)

# Remove rows with missing values
rtwt_df_new <- rtwt_df[complete.cases(rtwt_df), ]

# Create a matrix
rtwt_matrx <- as.matrix(rtwt_df_new)
head(rtwt_matrx)

# Convert the matrix to a retweet network
nw_rtweet <- graph_from_edgelist(el = rtwt_matrx, directed = TRUE)

# View the retweet network
print.igraph(nw_rtweet)

####################
#2 Network centrality measures: degree centrality und betweenness
####################
############
#degree 
library(igraph)
#outdegree
out_deg <- degree(nw_rtweet, 
                      "OutfitAww", #user
                      mode = c("out"))
#wert bedeutet dann wie oft retweetet z.B 20


#in-degree
in_deg <- degree(nw_rtweet, 
                  "OutfitAww", #user
                  mode = c("in"))
#diese werden am meisten retweetet 

out_degree_sort <- nw_rtweet %>%
  degree(mode = c("out")) %>%
  arrange(desc())
#oder sort(decreasing=TRUE)

############
#betweenness
#viele infos gehen durch diese leute tweeten und retweeten viel und werdne viel geretweetet
betwn <- nw_rtweet %>%
  betweenness(directed=TRUE) %>%
  sort(decreasing=TRUE) %>%
  round()


####################
#3. Visualizing 
####################
set.seed(1234)
plot.igraph(nw_rtweet)

#create a variable for out_degree & amplify it
out_deg <- degree(nw_rtweet,
                  mode = c("out"))

vert_size <- (out_deg*2) +10 #linear transformation 

#plot again
set.seed(1234)
plot(nw_rtweet, asp = 10/11, 
     vertex.size = vert_size, vertex.color = "lightblue",
     edge.arrow.size = 0.5,
     edge.color = "grey",
     vertex.label.cex = 0.8,
     vertex.label.color = "black")

#add followers count
#import df
#followers <- read.csv("...")
# Create a column and categorize follower counts above and below 500
followers$follow <- ifelse(followers$followers_count > 500, "1", "0")
head(followers)

# Assign the new column as vertex attribute to the retweet network
V(nw_rtweet)$followers <- followers$follow
vertex_attr(nw_rtweet)

# Set the vertex colors based on follower count and create a plot
sub_color <- c("lightgreen", "tomato")
plot(nw_rtweet, asp = 9/12,
     vertex.size = vert_size, edge.arrow.size = 0.5,
     vertex.label.cex = 0.8,
     vertex.color = sub_color[as.factor(vertex_attr(nw_rtweet, "followers"))],
     vertex.label.color = "black", vertex.frame.color = "grey")

#die jetzt grün und groß sind sind wichtigsten, haben viele Follower und tweeten viel 

####################
#3. Geomapping twitter data  
####################



