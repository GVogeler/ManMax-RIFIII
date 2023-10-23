library(xml2)
library(XML)
library(tidyr)
library(stringr)

mypath= #write path

xml_url="https://gitlab.rlp.net/adwmainz/regesta-imperii/lab/regesta-imperii-data/-/raw/main/data/indexes/RI_013.xml"
data_xml <- read_xml(xml_url)

lemma<-xml_find_all(data_xml, xpath = ".//lemma")
df <- tibble::tibble(
  lemma_id = xml_attr(lemma, "id"),
  typ = xml_attr(lemma, "typ"),
  label = lemma %>% xml_find_first("./label") %>% xml_text(),
  type = lemma %>% xml_find_first("./idno") %>% xml_attr("type"),
  type_value = lemma %>% xml_find_first("./idno") %>% xml_text(),
  mentions = lemma %>% xml_find_first("./numbers") %>% xml_text()
)

n <- length(df$lemma_id)


df_index = data.frame(doc=character(0),lemma_id=character(0),people=character(0),stringsAsFactors=F) 


for(i in 1:n){
  if(!is.na(df[i,]$typ)){ 
      if(df[i,]$typ=="person"){ 
        if(!is.na(df[i,]$mentions)){
          m <- strsplit(df[i,]$mentions, "#")[[1]][-1]
        for(j in 1:length(m)){
            row <- c(m[j],df[i,]$lemma_id,df[i,]$label)
            if(length(df_index$doc)==0){
              df_index[1,] <- row
            }else{
              df_index <- rbind(df_index,row)
            }
           }
        }
      }
  }
}

df_index <- df_index[order(df_index$doc,decreasing = FALSE),]

e_df = data.frame(from=character(0),to=character(0),doc=character(0),stringsAsFactors=F)
k=1

i=1
while (i<length(df_index$doc)){
  if(i%%1000==0){print(i)}
  doc <- df_index[i,]$doc
  j=1
  while(df_index[i,]$doc==df_index[i+j,]$doc){
    j <- j+1
  }
  if(j>1){
    for(p1 in seq(i,i+j-2,1)){
      person1 <- as.character(df_index[p1,]$lemma_id)
      for(p2 in seq(p1+1,i+j-1,1)){
        person2 <- as.character(df_index[p2,]$lemma_id)
        new_edge <- c(person1,person2,doc)
        e_df[k,]$from <- person1
        e_df[k,] <- new_edge
        k <- k+1
      }
    }
  }
  i=i+j
}



ri=paste0(mypath,"RI_13alles.csv")
data <- read.csv(ri,header = TRUE, sep = '\t')


####without Chmel and Suppl
ri=paste0(mypath,"RI_13alles.csv")
data <- read.csv(ri,header = TRUE, sep = '\t')


n_data <- length(data$identifier)
doc_id <- vector("character", length=n_data)

for(d in 1:n_data) {
  d_id <- as.character(data[d,]$identifier)
  if(data[d,]$urn!="" && !(str_detect(d_id,"Chmel") || str_detect(d_id," Suppl.")) ){
    d_urn <- data[d,]$urn
    docs <- strsplit(as.character(d_urn),"_")
    band <- docs[[1]][5]
    regest <- docs[[1]][8]
    doc_id[d] <- paste0(band,"-",regest)
    if(d%%10000==0) {print(doc_id[d])}
  }else{
    doc_id[d]=NA
  }
}


data$doc_id <- doc_id

doc_df <- subset.data.frame(data, select = c(doc_id,identifier,locality_string,start_date,end_date,summary))
doc_df <- doc_df[!is.na(doc_df$doc_id),]

filename = paste0(mypath,"dati_dfnoChmelSuppl.Rdata")
save(df_index, df, e_df, doc_df,file=filename )

