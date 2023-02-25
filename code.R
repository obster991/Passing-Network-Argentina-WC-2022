# Libraries ---------------------------------------------------------------

library(igraph)
install.packages("ggplot2")
library(ggplot2)
library(scales)
install.packages("gridExtra")
library(gridExtra)
library(grid)
install.packages("formattable")
library(formattable)

# Data loading ------------------------------------------------------------

data_path = "C:/Users/andre/Desktop/progetto_ddcs/data/"

# Load the data and reorder the columns
first_half = read.csv(file=paste(data_path, "first_half.csv", sep=''), sep=',')
first_half = first_half[c(2,4,1,3,5)]

second_half = read.csv(file=paste(data_path, "second_half.csv", sep=""), sep = ";")
second_half = second_half[,-c(6,7,8)]
second_half = second_half[,c(2,3,1,4,5)]

et_first_half = read.csv(file=paste(data_path, "suppl_fir_half.csv", sep=""), sep = ",")
et_first_half = et_first_half[,c(2,4,1,3,5)]
et_first_half$minute = et_first_half$minute + 90

et_second_half = read.csv(file=paste(data_path, "suppl_sec_half.csv", sep=""), sep = ";")
et_second_half = et_second_half[,c(2,3,1,4,5)]
et_second_half$minute = et_second_half$minute + 90

# First half: 266 passes
# Second half: 172 passes
# First half extra time: 79 passes
# Second half extra time: 55 passes

# Create a dataframe for the full match
match = rbind(first_half, second_half, et_first_half, et_second_half)

# Graph visualization -----------------------------------------------------

# Colors
color_att = "red"
color_def = "blue"
color_cen = "orange"
color_por = "black"

# Arrays of players
att = c("10", "11", "9", "8", "21", "22")
def = c("19", "13", "26", "3", "4", "6")
cen = c("24", "7", "20", "5")
por = c("23")

# Function to preprocess the data in order to create the graph
create_graph_data = function(data) {
  # Create the graph and compute the degree of each node
  data$weight = 1
  g = graph_from_data_frame(data, directed=TRUE)
  deg = degree(g)
  
  # Aggregate the edges between the same nodes
  agg_data = aggregate(weight ~ data$player1 + data$player2,
                        data=data, FUN=sum)
  graph = graph_from_data_frame(agg_data, directed=TRUE)
  
  # Nodes' degree
  degrees = c()
  for (node in V(graph)$name) {
    degrees = c(degrees, deg[as.character(node)])
  }
  
  return(list("agg_data" = agg_data, "graph" = graph, "degrees" = degrees))
}

# Function to color the graph according to the player position
graph_colors = function(agg_data) {
  agg_data$color = 1
  for (i in 1:dim(agg_data)[1]) {
    if (agg_data[i,1] %in% att) {
      agg_data$color[i] = color_att
    }else if(agg_data[i,1] %in% def){
      agg_data$color[i] = color_def
    }else if(agg_data[i,1] %in% cen){
      agg_data$color[i] = color_cen
    }else{
      agg_data$color[i] = color_por
    }
  }
  return(agg_data)
}

### Full match
g = create_graph_data(match)

# Take a look at the nodes and assign labels
V(g$graph)
V(g$graph)$labels = c("Paredes", "Acuna", "Alvarez", "Di Maria", "Otamendi",
                      "MacAllister", "Enzo Fernandez", "De Paul", "Messi",
                      "Romero", "Montiel", "L.Martinez", "E.Martinez", "Molina",
                      "Tagliafico", "Dybala")
V(g$graph)$colors = c(color_cen, color_att, color_att, color_att, color_def,
                      color_cen, color_cen, color_cen, color_att,
                      color_def, color_def, color_att, color_por, color_def,
                      color_def, color_att)

g$agg_data = graph_colors(g$agg_data)

# Dynamic plot
tkplot(g$graph,
       vertex.size=g$degrees*250/sum(g$degrees), #normalized
       edge.width=g$agg_data$weight*250/sum(g$agg_data$weight),
       vertex.label=V(g$graph)$labels,
       vertex.color=V(g$graph)$colors,
       vertex.label.font=2, 
       edge.label.font=1, 
       edge.label.cex=1, 
       vertex.label.cex=1,
       edge.arrow.size=0.5,
       edge.curved=0.3,
       edge.color=g$agg_data$color)

### First half
g1 = create_graph_data(first_half)

# Take a look at the nodes and assign labels
V(g1$graph)
V(g1$graph)$labels = c("Di Maria", "Otamendi", "MacAllister", "Enzo Fernandez",
                       "Alvarez", "Messi", "Romero", "Molina", "Tagliafico",
                       "De Paul", "E.Martinez")
V(g1$graph)$colors = c(color_att, color_def, color_cen, color_cen,
                       color_att, color_att, color_def, color_def, color_def,
                       color_cen, color_por)

g1$agg_data = graph_colors(g1$agg_data)

# Dynamic plot
tkplot(g1$graph,
       vertex.size=g1$degrees*0.7,
       edge.width=g1$agg_data$weight*0.7,
       vertex.label=V(g1$graph)$labels,
       vertex.color=V(g1$graph)$colors,
       vertex.label.font=2, 
       edge.label.font=1, 
       edge.label.cex=1, 
       vertex.label.cex=1,
       edge.arrow.size=0.5,
       edge.curved=0.3,
       edge.color=g1$agg_data$color)

### Second half
g2 = create_graph_data(second_half)

# Take a look at the nodes and assign labels
V(g2$graph)
V(g2$graph)$labels = c("Acuna", "Alvarez", "Di Maria", "Otamendi", "MacAllister",
                       "Enzo Fernandez", "De Paul", "Montiel", "Messi", "Romero",
                       "E.Martinez", "Molina", "Tagliafico")
V(g2$graph)$colors = c(color_att, color_att, color_att, color_def, color_cen,
                       color_cen, color_cen, color_def, color_att, color_def,
                       color_por, color_def, color_def)

g2$agg_data = graph_colors(g2$agg_data)

# Dynamic plot
tkplot(g2$graph,
       vertex.size=g2$degrees*0.7,
       edge.width=g2$agg_data$weight*0.7,
       vertex.label=V(g2$graph)$labels,
       vertex.color=V(g2$graph)$colors,
       vertex.label.font=2, 
       edge.label.font=1, 
       edge.label.cex=1, 
       vertex.label.cex=1,
       edge.arrow.size=0.5,
       edge.curved=0.3,
       edge.color=g2$agg_data$color)

### Extra-time first half
g3 = create_graph_data(et_first_half)

# Take a look at the nodes and assign labels
V(g3$graph)
V(g3$graph)$labels = c("Acuna", "Otamendi", "Romero", "Enzo Fernandez", "Alvarez",
                       "Messi", "MacAllister", "E.Martinez", "De Paul", "Montiel",
                       "Paredes", "Tagliafico", "L.Martinez")
V(g3$graph)$colors = c(color_att, color_def, color_def, color_cen, color_att,
                       color_att, color_cen, color_por, color_cen, color_def,
                       color_cen, color_def, color_att)

g3$agg_data = graph_colors(g3$agg_data)

# Dynamic plot
tkplot(g3$graph,
       vertex.size=g3$degrees*0.7,
       edge.width=g3$agg_data$weight*0.7,
       vertex.label=V(g3$graph)$labels,
       vertex.color=V(g3$graph)$colors,
       vertex.label.font=2, 
       edge.label.font=1, 
       edge.label.cex=1, 
       vertex.label.cex=1,
       edge.arrow.size=0.5,
       edge.curved=0.3,
       edge.color=g3$agg_data$color)

### Extra-time second half
g4 = create_graph_data(et_second_half)

# Take a look at the nodes and assign labels
V(g4$graph)
V(g4$graph)$labels = c("Paredes", "Otamendi", "Messi", "Romero", "Enzo Fernandez",
                       "Montiel", "MacAllister", "L.Martinez", "Tagliafico", "Acuna",
                       "E.Martinez", "Dybala")
V(g4$graph)$colors = c(color_cen, color_def, color_att, color_def, color_cen,
                       color_def, color_cen, color_att, color_def, color_att,
                       color_por, color_att)

g4$agg_data = graph_colors(g4$agg_data)

# Dynamic plot
tkplot(g4$graph,
       vertex.size=g4$degrees*0.7,
       edge.width=g4$agg_data$weight*0.7,
       vertex.label=V(g4$graph)$labels,
       vertex.color=V(g4$graph)$colors,
       vertex.label.font=2, 
       edge.label.font=1, 
       edge.label.cex=1, 
       vertex.label.cex=1,
       edge.arrow.size=0.5,
       edge.curved=0.3,
       edge.color=g4$agg_data$color)


# Players' metrics --------------------------------------------------------

# Clustering coefficient, betweenness centrality, closeness centrality
# of each player
summary_players_metrics = function(gra){
  player_matrix = matrix(NA, nrow=length(V(gra)), ncol=4)
  for(i in 1:length(V(gra))) {
    player_matrix[i, 1] = round(transitivity(gra, type="local"), 3)[i]
    player_matrix[i, 2] = round(betweenness(gra, v=V(gra)[i], directed=TRUE, normalized = T), 3)
    player_matrix[i, 3] = round(closeness(gra, vids=V(gra)[i], mode="in", normalized = T), 3)
    player_matrix[i, 4] = round(closeness(gra, vids=V(gra)[i], mode="out", normalized = T), 3)
  }
  rownames(player_matrix) = V(gra)$labels
  colnames(player_matrix) = c("Clustering coefficient",
                              "Betweenness centrality",
                              "Closeness centrality - in",
                              "Closeness centrality - out")
  return(player_matrix)
}

# Metrics on original graph with non-aggregated edges
original_graph = graph_from_data_frame(match)
V(original_graph)$labels = c("Molina", "De Paul", "Romero", "Otamendi", "Tagliafico", "Di Maria", "Mac Allister", 
                             "Messi", "Enzo Fernandez", "Alvarez", "E.Martinez", "Acuna", "Montiel", "Paredes",
                             "L.Martinez", "Dybala")
summary_metrics = summary_players_metrics(original_graph)
formattable(as.data.frame(summary_metrics), list("Clustering coefficient" = color_tile("orange", "white"),
                                                 "Betweenness centrality" = color_tile("white", "orange"),
                                                 "Closeness centrality - in" = color_tile("white", "orange"),
                                                 "Closeness centrality - out" = color_tile("white", "orange")))


# Zone-player graph -------------------------------------------------------

# Function to create a graph where the nodes are the zone-player tuples
zone_player_graph = function(data) {
  # Paste together player and zone
  zp_data = data.frame(paste(data$player1, data$zone1), 
                       paste(data$player2, data$zone2), data$minute)
  colnames(zp_data) = c("player1", "player2", "minute")
  # Use the previous function
  g_zp = create_graph_data(zp_data)
  return(g_zp)
}

g_zp = zone_player_graph(match)
g1_zp = zone_player_graph(first_half)
g2_zp = zone_player_graph(second_half)
g3_zp = zone_player_graph(et_first_half)
g4_zp = zone_player_graph(et_second_half)

### Degree distribution

# Function to plot the degree distribution
degree_distr_plot = function(g, g_mode) {
  
  # Compute the degrees
  G.degrees = degree(g, mode = g_mode)
  G.degree.histogram = as.data.frame(table(G.degrees))
  G.degree.histogram$G.degrees = as.numeric(levels(G.degree.histogram$G.degrees))
  
  if(g_mode == "in"){
    lab = "In-Degree"
  }else{
    lab = "Out-Degree"
  }
  
  # Histogram
  hist = ggplot(as.data.frame(G.degrees), aes(x=G.degrees)) + 
    geom_histogram(color="darkblue", fill="lightblue", position = position_dodge(0.7), size = 0.8, alpha = 0.8, binwidth = 1)+
    scale_x_continuous(breaks= pretty_breaks()) + 
    scale_y_continuous(breaks= pretty_breaks(), limits = c(0,30)) +
    xlab(lab) +
    ylab("Frequency") +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
          legend.title = element_blank(), 
          legend.position="bottom",
          legend.text=element_text(size=9, face = "bold"),
          axis.text = element_text(size = 15, face = "bold"),
          axis.title = element_text(size = 15, face = "bold"),
          aspect.ratio=1,
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
  
  # Points
  p = ggplot(G.degree.histogram, aes(x = G.degrees, y = Freq)) +
    geom_point(size = 3, color="darkblue", alpha=0.5) +
    scale_y_continuous(breaks= pretty_breaks(), limits = c(0,30)) +
    xlab(lab) +
    ylab("Frequency") +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
          legend.title = element_blank(), 
          legend.position="bottom",
          legend.text=element_text(size=9, face = "bold"),
          axis.text = element_text(size = 15, face = "bold"),
          axis.title = element_text(size = 15, face = "bold"),
          aspect.ratio=1,
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))

  return(list("hist" = hist, "p"=p))
}

h1 = degree_distr_plot(g_zp$graph, "in")$hist
p1 = degree_distr_plot(g_zp$graph, "in")$p
h2 = degree_distr_plot(g_zp$graph, "out")$hist
p2 = degree_distr_plot(g_zp$graph, "out")$p
#grid.arrange(h1, p1, h2, p2, top=textGrob("Histogram and Degree distribution of entire match", 
                                          #gp=gpar(fontsize=22,font=2),
                                          #vjust = c(0.3,1)))
grid.arrange(h1, p1, h2, p2)

### Strength distribution
s = ggplot(as.data.frame(table(g_zp$agg_data$weight)), aes(x=Var1, y=Freq)) +
  geom_point(size = 3, color="darkblue", alpha=0.5) +
  xlab("Weight") +
  ylab("Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
        legend.title = element_blank(), 
        legend.position="bottom",
        legend.text=element_text(size=9, face = "bold"),
        axis.text = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        aspect.ratio=1,
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
s


## 15 minutes time-window analysis

metric_every_15_min = function(df) {
  minutes = unique(df$minute)
  mat_clu = matrix(NA, ncol = 5, nrow = length(minutes))
  
  for (i in 1:length(minutes)) {
    window = minutes[i]-15
    sta = 0
    if (window < 0){
      cut_first_half = df[which(df$minute >= 0 & df$minute <= minutes[i]),]
      sta = 0
    }else{
      cut_first_half = df[which(df$minute >= window & df$minute <= minutes[i]),]
      sta = window
    }
    cut_first_half$weight = 1
    edge_list <- aggregate(weight ~ cut_first_half$player1 + cut_first_half$player2,
                           data=cut_first_half, 
                           FUN=sum)
    g_clu = graph_from_data_frame(edge_list, directed=TRUE)
    mat_clu[i,1] = transitivity(g_clu, type = "average")
    mat_clu[i,2] = sta
    mat_clu[i,3] = minutes[i]
    mat_clu[i,4] = mean(degree(g_clu))
    mat_clu[i,5] = sd(degree(g_clu))
  }
  
  p_clu_coeff <- ggplot(as.data.frame(mat_clu), aes(mat_clu[,3], mat_clu[,1])) + 
    geom_line(color = "blue", lwd = 1.5) + 
    geom_point(color = "orange", lwd=2) +
    geom_vline(xintercept = 23, color="red", lwd=0.9) +
    geom_vline(xintercept = 36, color="red", lwd=0.9) +
    scale_x_continuous(breaks= pretty_breaks()) + 
    scale_y_continuous(breaks= pretty_breaks()) +
    theme_classic() +
    xlab("Minute") + 
    ylab("Clustering coefficient") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
          legend.title = element_blank(), 
          legend.position="bottom",
          legend.text=element_text(size=9, face = "bold"),
          axis.text = element_text(size = 15, face = "bold"),
          axis.title = element_text(size = 15, face = "bold"),
          aspect.ratio=1,
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
  
  p_mean_degree <- ggplot(as.data.frame(mat_clu), aes(mat_clu[,3], mat_clu[,4])) + 
    geom_line(aes(colour = "Mean"), lwd = 1.5) + 
    geom_line(aes(y=mat_clu[,4]+mat_clu[,5],colour="Mean +/- Std"), lwd=0.8) +
    geom_line(aes(y=mat_clu[,4]-mat_clu[,5], colour="Mean +/- Std"), lwd=0.8) +
    geom_ribbon(aes(x = mat_clu[,3],ymin = mat_clu[,4]-mat_clu[,5],ymax = mat_clu[,4]+mat_clu[,5]),fill = "green", alpha=0.1)+
    geom_point(color = "orange", lwd=2) +
    scale_x_continuous(breaks= pretty_breaks()) + 
    scale_y_continuous(breaks= pretty_breaks()) +
    scale_color_manual(values=c("Mean"="blue", "Mean +/- Std"="darkgreen"), 
                      name="Legend") +
    theme_classic() +
    xlab("Minute") + 
    ylab("Mean degree") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
          legend.title = element_blank(), 
          legend.position="right",
          legend.text=element_text(size=9, face = "bold"),
          axis.text = element_text(size = 15, face = "bold"),
          axis.title = element_text(size = 15, face = "bold"),
          aspect.ratio=1,
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
  
  return(list("p_clu_coeff"=p_clu_coeff, "p_mean_degree"=p_mean_degree))
}

# On all the match

plot = metric_every_15_min(match)
p1 = plot$p_clu_coeff +
  geom_vline(xintercept = 23, color="green", lwd=0.9) +
  geom_vline(xintercept = 36, color="green", lwd=0.9) +
  geom_vline(xintercept = 80, color="red", lwd=0.9) +
  geom_vline(xintercept = 81, color="red", lwd=0.9) +
  geom_vline(xintercept = 108, color="green", lwd=0.9) +
  geom_vline(xintercept = 118, color="red", lwd=0.9) +
  annotate("text", x = 21.5, y = 0.15, label = "1-0", angle="90", fontface="bold", size=5, color="green", vjust=.1) +
  annotate("text", x = 34.5, y = 0.15, label = "2-0", angle="90", fontface="bold", size=5, color="green", vjust=.1) +
  annotate("text", x = 78.5, y = 0.15, label = "2-1", angle="90", fontface="bold", size=5, color="red", vjust=-0.3) +
  annotate("text", x = 84.5, y = 0.15, label = "2-2", angle="90", fontface="bold", size=5, color="red", vjust=0.3) +
  annotate("text", x = 106.5, y = 0.15, label = "3-2", angle="90", fontface="bold", size=5, color="green", vjust=.1) +
  annotate("text", x = 116.5, y = 0.15, label = "3-3", angle="90", fontface="bold", size=5, color="red", vjust=.1)

p1
ggsave("clust_coeff_15_min.jpg", dpi = 1000)

p2 = plot$p_mean_degree +
  geom_vline(xintercept = 23, color="green", lwd=0.9) +
  geom_vline(xintercept = 36, color="green", lwd=0.9) +
  geom_vline(xintercept = 80, color="red", lwd=0.9) +
  geom_vline(xintercept = 81, color="red", lwd=0.9) +
  geom_vline(xintercept = 108, color="green", lwd=0.9) +
  geom_vline(xintercept = 118, color="red", lwd=0.9) +
  annotate("text", x = 21.5, y = 15, label = "1-0", angle="90", fontface="bold", size=5, color="green", vjust=.1) +
  annotate("text", x = 34.5, y = 15, label = "2-0", angle="90", fontface="bold", size=5, color="green", vjust=.1) +
  annotate("text", x = 78.5, y = 13, label = "2-1", angle="90", fontface="bold", size=5, color="red", vjust=-0.3) +
  annotate("text", x = 84.5, y = 13, label = "2-2", angle="90", fontface="bold", size=5, color="red", vjust=0.3) +
  annotate("text", x = 106.5, y = 3, label = "3-2", angle="90", fontface="bold", size=5, color="green", vjust=.1) +
  annotate("text", x = 116.5, y = 15, label = "3-3", angle="90", fontface="bold", size=5, color="red", vjust=.1)

p2
ggsave("mean_degree_15_min.jpg", dpi = 1000)

# Zone of the player with the highest degree (every 15 min) (paper Spagna)

highest_degree_zone_15_min = function(df) {
  df = data.frame(paste(df$player1, df$zone1), 
                  paste(df$player2, df$zone2), df$minute)
  colnames(df) = c("player1", "player2", "minute")
  minutes = unique(df$minute)
  mat_clu = matrix(NA, ncol = 2, nrow = length(minutes))
  
  for (i in 1:length(minutes)) {
    window = minutes[i]-15
    sta = 0
    if (window < 0){
      cut_first_half = df[which(df$minute >= 0 & df$minute <= minutes[i]),]
      sta = 0
    }else{
      cut_first_half = df[which(df$minute >= window & df$minute <= minutes[i]),]
      sta = window
    }
    cut_first_half$weight = 1
    edge_list <- aggregate(weight ~ cut_first_half$player1 + cut_first_half$player2,
                           data=cut_first_half, 
                           FUN=sum)
    g_clu = graph_from_data_frame(edge_list, directed=TRUE)
    d = degree(g_clu)[which(degree(g_clu) == max(degree(g_clu)))][1]
    d = substr(names(d), start=nchar(names(d)), stop=nchar(names(d)))
    mat_clu[i,1] = as.numeric(d)
    mat_clu[i,2] = minutes[i]
  }
  
  p_15_degree <- ggplot(as.data.frame(mat_clu), aes(mat_clu[,2], mat_clu[,1])) + 
    geom_point(color = "orange", size=3) +
    geom_segment( aes(x=as.vector(mat_clu[,2]), xend=as.vector(mat_clu[,2]), y=0, yend=as.vector(mat_clu[,1])))+
    scale_x_continuous(breaks= pretty_breaks()) + 
    scale_y_continuous(breaks= pretty_breaks(n=9)) +
    annotate("text", x = 0, y = 9, label = "") +
    theme_classic() +
    xlab("Minute") + 
    ylab("Zone") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15), 
          legend.title = element_blank(), 
          legend.position="bottom",
          legend.text=element_text(size=9, face = "bold"),
          axis.text = element_text(size = 15, face = "bold"),
          axis.title = element_text(size = 15, face = "bold"),
          aspect.ratio=1,
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
  return(p_15_degree)
}

p_15_degree = highest_degree_zone_15_min(match)
p_15_degree + 
  geom_vline(xintercept = 23, color="green", lwd=0.9) +
  geom_vline(xintercept = 36, color="green", lwd=0.9) +
  geom_vline(xintercept = 80, color="red", lwd=0.9) +
  geom_vline(xintercept = 81, color="red", lwd=0.9) +
  geom_vline(xintercept = 108, color="green", lwd=0.9) +
  geom_vline(xintercept = 118, color="red", lwd=0.9) +
  annotate("text", x = 21.5, y = 8.5, label = "1-0", angle="90", fontface="bold", size=5, color="green", vjust=.1) +
  annotate("text", x = 34.5, y = 8.5, label = "2-0", angle="90", fontface="bold", size=5, color="green", vjust=.1) +
  annotate("text", x = 78.5, y = 7.5, label = "2-1", angle="90", fontface="bold", size=5, color="red", vjust=-0.05) +
  annotate("text", x = 84.5, y = 8.5, label = "2-2", angle="90", fontface="bold", size=5, color="red", vjust=0.4) +
  annotate("text", x = 106.5, y = 8.5, label = "3-2", angle="90", fontface="bold", size=5, color="green", vjust=.1) +
  annotate("text", x = 116.5, y = 8.5, label = "3-3", angle="90", fontface="bold", size=5, color="red", vjust=.1)

ggsave("zone_player_high_deg.jpg", dpi = 1000)
  
# Graph with the zones as nodes -------------------------------------------

# Data manipulation
match_z = match[,c(4,5,1,2,3)]
first_half_z = first_half[,c(4,5,1,2,3)]
second_half_z = second_half[,c(4,5,1,2,3)]
et_first_half_z = et_first_half[,c(4,5,1,2,3)]
et_second_half_z = et_second_half[,c(4,5,1,2,3)]

zone_graph = function(data) {
  # Create the graph and compute the degree of each zone
  z = graph_from_data_frame(data, directed=TRUE)
  deg_z = degree(z)
  
  # Aggregate the edges between the same nodes
  data$weight = 1
  agg_data = aggregate(weight ~ data$zone1 + data$zone2,
                       data=data, FUN=sum)
  graph = graph_from_data_frame(agg_data, directed=TRUE)
  
  # Nodes' degree
  degrees = c()
  for (node in V(graph)$name) {
    degrees = c(degrees, deg_z[as.character(node)])
  }
  return(list("graph" = graph, "agg_data" = agg_data, "degrees" = degrees))
}

# Plots

# Full match
z = zone_graph(match_z)

# Check the nodes and assign the labels
V(z$graph)
V(z$graph)$labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9")
V(z$graph)$colors = c("black", "blue", "blue", "blue", "yellow", "red", "yellow", "red", "red")

tkplot(z$graph,
       vertex.size=z$degrees*250/sum(z$degrees),
       vertex.color=V(z$graph)$colors,
       edge.width=z$agg_data$weight*250/sum(z$agg_data$weight),
       vertex.label=V(z$graph)$labels,
       vertex.label.font=2,
       vertex.label.color="black",
       vertex.label.cex=2,
       edge.label.font=1, 
       edge.label.cex=1,
       vertex.label.size=5,
       edge.arrow.size=0.5,
       edge.curved =0.3)


# Community detection -----------------------------------------------------

comunity_detection = function(gra) {
  clp_edge_betw = cluster_edge_betweenness(gra)
  LOC = layout_components(gra)
  plot(clp_edge_betw, gra,layout=LOC, 
       vertex.label="",
       vertex.size=8,
       edge.arrow.size = 0.01, main="Edge-betweenness")
  
  clp_label_prop = cluster_label_prop(gra)
  plot(clp_label_prop, gra,
       vertex.label="",
       vertex.size=8,
       edge.arrow.size = 0.01, main="Label-propagating")
  
  # louvain works only with undirected graphs
  
  clp_walktrap_prop = cluster_walktrap(gra)
  plot(clp_walktrap_prop, gra,
       vertex.label="",
       vertex.size=8,
       edge.arrow.size = 0.01, main="Walktrap")
  
  clp_infomap_prop = cluster_infomap(gra)
  plot(clp_infomap_prop, gra,
       vertex.label="",
       vertex.size=8,
       edge.arrow.size = 0.01, main="Infomap")
  
  return(list("clp_edge_betw"=clp_edge_betw,
              "clp_label_prop"=clp_label_prop,
              "clp_walktrap_prop"=clp_walktrap_prop,
              "clp_infomap_prop"=clp_infomap_prop))
}

out = comunity_detection(g$graph)

length(unique(out$clp_edge_betw$membership))
length(unique(out$clp_label_prop$membership))
length(unique(out$clp_walktrap_prop$membership))
length(unique(out$clp_infomap_prop$membership))

out$clp_edge_betw$membership
out$clp_edge_betw$names

out$clp_edge_betw$names[which(out$clp_edge_betw$membership != 1)]

out$clp_walktrap_prop$names[which(out$clp_walktrap_prop$membership == 5)]

modularity(out$clp_edge_betw)
modularity(out$clp_label_prop)
modularity(out$clp_walktrap_prop)
modularity(out$clp_infomap_prop)



