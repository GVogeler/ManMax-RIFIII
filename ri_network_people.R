library(igraph)
library(dplyr)
library(lubridate)
library(colorspace)
library(ggplot2)
library(ggrepel)

load("~/data/dati_df_noChmelSuppl.Rdata")

e_doc <- unique(union(e_df$from,e_df$to)) #19122 people
length(doc_df$doc_id)
length(unique(doc_df$doc_id)) #document 24399, 15294 without CS
length(e_df$doc)
length(unique(e_df$doc))

dup_doc <- doc_df[duplicated(doc_df$doc_id),]$doc_id 
prova <- doc_df[doc_df$doc_id %in% dup_doc,]
for(dup in 1:length(dup_doc)){
  print(doc_df[doc_df$doc_id==dup_doc[dup],]$identifier[[1]])
  
}


#examples of duplicates
length(doc_df[doc_df$doc_id=="0-489",]$identifier)###ACHTUNG! duplicated in "Chmel n.*" with different places associated 
length(doc_df[doc_df$doc_id=="0-347",]$identifier)###ACHTUNG! duplicated in "Chmel n.*" and "[RI XIII] Suppl. 1 n. *"

doc_to_remove <- intersect(dup_doc,e_df$doc_id)
edges_to_remove <- e_df[e_df$doc_id %in% doc_to_remove,] 
#removing
e2_df<- e_df[!(e_df$doc_id %in% doc_to_remove),] #edges without duplicates

#select docs who are in the data from csv AND in the edges
docp_df <- doc_df[doc_df$doc_id %in% e2_df$doc_id,]

docp_df[is.na(docp_df$doc_id)]

graph_df <- merge(x=e2_df,y=docp_df,by="doc_id") #,all.x=TRUE)
gp_df <- graph_df[,c(2,3,1,4,5,6,7,8,9,10,11,12)]


#check doc_id!=NA
length(gp_df[is.na(gp_df$doc_id),]$doc_id) #0

gp <- graph_from_data_frame(gp_df,directed=FALSE)
summary(gp)

#Customize nodes
people_df <- subset(df_index,select=c("lemma_id","people")) %>% distinct()
people_df <- people_df[people_df$lemma_id %in% V(gp)$name,]
people_df <- people_df[match(V(gp)$name,people_df$lemma_id),] 
V(gp)$person <- people_df$people

all(V(gp)$name == people_df$lemma_id)


V(gp)$deg <- degree(gp)
V(gp)$btw <- betweenness(gp)

plot(table(V(gp)$deg),log="x")
length(V(gp)[V(gp)$deg==1])

#select the 5 biggest hubs (high degree) and flag them as "vip"
vip_nodes <- head(V(gp)[order(V(gp)$deg,decreasing = TRUE)],5)
V(gp)$vip <- 0
V(gp)[vip_nodes]$vip <-1

print("highest degree users:")
vip_nodes <- head(V(gp)[order(V(gp)$deg,decreasing = TRUE)],20)
hubs <- data.frame(vip_nodes$person,vip_nodes$deg)
colnames(hubs)=c("Person","Degree")

print("bridges users:")
bridges_nodes <- head(V(gp)[order(V(gp)$btw,decreasing = TRUE)],20)
bridges <- data.frame(bridges_nodes$person,bridges_nodes$btw)
colnames(bridges)=c("Person","Btw")




#Edges measures
length(E(gp)[E(gp)$start_date!=E(gp)$end_date])  #3971

diff_dates <- E(gp)[E(gp)$start_date!=E(gp)$end_date]
start <- as.Date(E(gp)[diff_dates]$start_date)
end <- as.Date(E(gp)[diff_dates]$end_date)

years <- year(start)
plot(table(years),ylab="documents")



hcl_palettes("sequential (multi-hue)", n = 5, plot = TRUE)
palette_edges <- rev(sequential_hcl(5, "BluGrn"))
E(gp)$date <- year(as.Date(E(gp)$start_date))

E(gp)$decade <-  case_when(
  E(gp)$date < 1450 ~  "1440-1449",
  E(gp)$date < 1460 ~  "1450-1459",
  E(gp)$date < 1470 ~  "1460-1469",
  E(gp)$date < 1480 ~  "1470-1479",
  E(gp)$date < 1500 ~  "1480-1493"
)

E(gp)$dyear <-  case_when(
  E(gp)$date < 1450 ~  1440,
  E(gp)$date < 1460 ~  1450,
  E(gp)$date < 1470 ~  1460,
  E(gp)$date < 1480 ~  1470,
  E(gp)$date < 1500 ~  1480
)

E(gp)$color <-  case_when(
    E(gp)$date < 1450 ~  palette_edges[1],
    E(gp)$date < 1460 ~  palette_edges[2],
    E(gp)$date < 1470 ~  palette_edges[3],
    E(gp)$date < 1480 ~  palette_edges[4],
    E(gp)$date < 1500 ~  palette_edges[5]
  )


layout_base <- layout_with_fr(gp,
                              dim=2,
                              niter=5000, 
                              start.temp = sqrt(vcount(gp)), 
                              grid="nogrid", 
                              weights=NA)


plot(gp, 
     #layout=layout_base,
     layout= layout_nicely(gp),
     vertex.size=0.5,
     #vertex.size= ifelse(V(gp)$deg>10000, 2+0.001*V(gp)$deg, 0.5),
     vertex.frame.color =NA,
     vertex.color = adjustcolor("orange", alpha.f = 0.5),
     #vertex.label= NA,
     #vertex.label= ifelse(V(gp)$deg>2000, V(gp)$name, NA),
     edge.width = 0.1,
     edge.curved=0.2, 
     edge.color=adjustcolor(E(gp)$color, alpha.f = 0.2)
     #edge.color= ifelse(E(gmh)$weigth>100,adjustcolor("grey", alpha.f = 0.5),adjustcolor("grey", alpha.f = 0.35))
)


V(gp)$component <- clusters(gp)$membership
gp_giant <- delete.vertices(gp, V(gp)[V(gp)$component!=1])
V(gp_giant)$deg = degree(gp_giant)
V(gp_giant)$btw <- betweenness(gp_giant)

plot(table(V(gp_giant)$deg), log="x")
length(V(gp_giant)[V(gp_giant)$deg>10])
summary(gp_giant)




hubs_nodes <- head(V(gp_giant)[order(V(gp_giant)$deg,decreasing = TRUE)],50)
hubs <- data.frame(hubs_nodes$name,hubs_nodes$person,hubs_nodes$deg)
colnames(hubs)=c("LemmaID","Person","Degree")
V(gp_giant)$hub <- 0
V(gp_giant)[hubs_nodes]$hub <-1


bridges_nodes <- head(V(gp_giant)[order(V(gp_giant)$btw,decreasing = TRUE)],50)
bridges <- data.frame(bridges_nodes$name,bridges_nodes$person,bridges_nodes$btw)
colnames(bridges)=c("LemmaID","Person","Btw")
V(gp_giant)$bridge <- 0
V(gp_giant)[bridges_nodes]$bridge <-1


plot(gp_giant, 
     layout= layout_nicely(gp_giant),
     #vertex.size=0.5,
     vertex.size= ifelse(V(gp_giant)$hub, 2, 0.5),
     vertex.frame.color =NA,
     vertex.color = ifelse(V(gp_giant)$hub, adjustcolor("red", alpha.f = 0.8), adjustcolor("yellow", alpha.f = 0.2)),
     vertex.label= NA,
     #vertex.label= ifelse(V(gp_giant)$hub, V(gp_giant)$name, NA),
     vertex.label.color="black",
     vertex.label.family="Helvetica",                  
     vertex.label.font=1,                          
     vertex.label.cex=2,                          
     vertex.label.dist=0.1,
     edge.width = 0.1,
     edge.curved=0.2, 
     edge.color=adjustcolor(E(gp_giant)$color, alpha.f = 0.2)
     #edge.color= ifelse(E(_giant)$weigth>100,adjustcolor("grey", alpha.f = 0.5),adjustcolor("grey", alpha.f = 0.35))
)


print("highest degree users:")
highestdeg_users <- V(sg)[V(sg)$deg %in% head(sort(V(sg)$deg, decreasing = TRUE),30)]
highestdegusers <- head(highestdeg_users[order(highestdeg_users$deg,decreasing = TRUE)],30)
print(highestdeg_users$person)

print("bridges users:")
V(sg)$btw<- betweenness(gp_giant)
bridges <-V(gp_giant)[V(gp_giant)$btw %in% head(sort(V(gp_giant)$btw, decreasing = TRUE),30)]
bridges  <- head(bridges [order(bridges$btw,decreasing = TRUE)],30)
print(bridges$person)

write.csv(hubs, file="~/Results/hubs_giant.csv")
write.csv(bridges, file="~/Results/bridges_giant.csv")


save(gp_giant,file = "~/Graph/RIXII_people_graph_gc.RData")
