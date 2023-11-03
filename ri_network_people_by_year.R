library(ggplot2)
library(plotly)
library(ggrepel)
library(igraph)
library(viridis)
library(visNetwork)

load("~/Documents/Prosopography/ManMax/Data/Regesta_Imperii/RIXII_people_graph_gc.RData")
# SUBGRAPHS DECADES

timewindows <- sort(as.numeric(unique(E(gp_giant)$date)))
DGlist <- list()

#extract a subgraph for each decade
for(i in 1:length(timewindows)){
  d = timewindows[i]
  print(d)
  id_links_todel <- E(gp_giant)[E(gp_giant)$date!=d] 
  d_g <- delete_edges(gp_giant, id_links_todel)
  V(d_g)$deg <- degree(d_g)
  V(d_g)$btw <- betweenness(d_g)
  V(d_g)$membership <- clusters(d_g)$membership
  d_gc <- induced_subgraph(d_g, V(d_g)[V(d_g)$membership==which.max(clusters(d_g)$csize)])
  E(d_gc)$w <- 1
  #d_sg <- igraph::simplify(d_gc,remove.multiple = TRUE, 
  #                        edge.attr.comb = list(w=function(x) sum(x),
  #                                             date="ignore",words="ignore",year="ignore"))
  #summary(d_sg)
  
  hubs_nodes <- head(V(d_gc)[order(V(d_gc)$deg,decreasing = TRUE)],20)
  hubs <- data.frame(hubs_nodes$name,hubs_nodes$person,hubs_nodes$deg)
  colnames(hubs)=c("LemmaID","Person","Degree")
  V(d_gc)$hub <- 0
  V(d_gc)[hubs_nodes]$hub <-1
  
  
  bridges_nodes <- head(V(d_gc)[order(V(d_gc)$btw,decreasing = TRUE)],20)
  bridges <- data.frame(bridges_nodes$name,bridges_nodes$person,bridges_nodes$btw)
  colnames(bridges)=c("LemmaID","Person","Btw")
  V(d_gc)$bridge <- 0
  V(d_gc)[bridges_nodes]$bridge <-1
  
  write.csv(hubs, file=paste0("~/Documents/Prosopography/ManMax/Data/Regesta_Imperii/hubs_",d,".csv"))
  write.csv(bridges, file=paste0("~/Documents/Prosopography/ManMax/Data/Regesta_Imperii/bridges_",d,".csv"))

  summary(d_gc)
  
  visdata <- toVisNetworkData(d_gc,idToLabel = FALSE)
  visdata$nodes <- data.frame(id = V(d_gc)$name, title = V(d_gc)$person)
  
  visNetwork(nodes = visdata$nodes, edges = visdata$edges)%>%
    visOptions(highlightNearest = TRUE,clickToUse = F,nodesIdSelection = TRUE) %>%
    visIgraphLayout(layout = "layout_nicely") %>%
    visNodes(label = V(d_gc)$name, title = V(d_gc)$person,color = "forestgreen", size=3)%>%
    visEdges(color = list(color = "grey", highlight = "black")) %>%
    visInteraction(hover = TRUE)

  

  
  plot(d_gc, 
       main=d,
       #main=paste0(d,"-",d+9),
       layout= layout_nicely(d_gc),
       #vertex.size=0.5,
       vertex.size= ifelse(V(d_gc)$hub, 5+0.01*V(d_gc)$deg, 1),
       vertex.frame.color =NA,
       vertex.color = adjustcolor("orange", alpha.f = 0.7),
       #vertex.label= NA,
       vertex.label= ifelse(V(d_gc)$deg>400, V(d_gc)$name, NA),
       vertex.label.color="black",
       vertex.label.family="Helvetica",                  # Font family of the label (e.g.“Times”, “Helvetica”)
       vertex.label.font=1,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
       vertex.label.cex=0.5,                           # Font size (multiplication factor, device-dependent)
       vertex.label.dist=0.5,
       edge.width = 0.1,
       edge.curved=0.2, 
       edge.color=adjustcolor("grey", alpha.f = 0.2)
       #edge.color= ifelse(E(_giant)$weigth>100,adjustcolor("grey", alpha.f = 0.5),adjustcolor("grey", alpha.f = 0.35))
  )
  
  
  
  d_gc$decade = d
  DGlist[[i]] <- d_gc
}

common_nodes <- V(DGlist[[1]])$name
for(i in 2:length(timewindows)){
  print(timewindows[i])
  print(vcount(DGlist[[i]]))
  common_nodes <- intersect(common_nodes,V(DGlist[[i]])$name)
  num_common_nodes <- length(common_nodes)
  print("*****")
}


btw_deg <- data.frame(V(gp_giant)[intersect(hubs_nodes,bridges_nodes)]$name,
                      V(gp_giant)[intersect(hubs_nodes,bridges_nodes)]$person,
                      V(gp_giant)[intersect(hubs_nodes,bridges_nodes)]$btw,
                      V(gp_giant)[intersect(hubs_nodes,bridges_nodes)]$deg)
colnames(btw_deg)=c("LemmaID","Person","Betweenness","Degree")

p <- ggplot(btw_deg,aes(x=Degree,y=Betweenness)) +
  geom_point(size=1, color="forestgreen") +
  geom_text_repel(label = btw_deg$LemmaID,  size=3.5)
plot(p)
plotly::ggplotly(p)


ggplot(btw_deg, aes(X, Y, fill= Z)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  theme_ipsum()



