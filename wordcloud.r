#####################################################################
####### Make publication wordcloud ##################################
#####################################################################


#load libraries
require(scholar)
require(tibble) 
require(tm)
require(tidytext)
require(tidyverse)
require(wordcloud2)
require(randomcoloR) 

#Create function
GoogleScholarWordCloud<-function(IDnumber){
  
  #Get publications
  Mypublications<-tibble(get_publications(IDnumber))
  
  #Add abstracts
  Mypublications$abstract<-rep("initialize", nrow(Mypublications))
  for (i in 1:nrow(Mypublications)) {
    Abstracts <- get_publication_abstract(id = IDnumber, 
                                          pub_id = Mypublications$pubid[i])
    if (length(Abstracts)>1) {Abstracts<-paste(Abstracts, collapse=" ") }

    ifelse(length(Abstracts) == 0, NA, Mypublications$abstract[i] <- Abstracts) 
  }
  
  #Create Corpus
  MyAbstracts.corpus<-tm::VCorpus(tm::VectorSource(Mypublications$abstract))
  
  #Create TDM from corpus
  MyAbstracts.TDM<-tm::TermDocumentMatrix(MyAbstracts.corpus,
                                      control = 
                                        list(removePunctuation = TRUE,  
                                             stopwords = TRUE,
                                             tolower = TRUE,
                                             stemming = F,
                                             removeNumbers = TRUE))
  #Tidy TDM
  MyAbstracts.TDM<-tidytext::tidy(MyAbstracts.TDM) %>% 
    group_by(term) %>% 
    summarise(count,count = sum(count)) %>% 
    unique()%>%  
    ungroup() %>%
    arrange(desc(count))    
  
  #retain top 100 most frequently used words
  MyAbstracts.TDM<-head(MyAbstracts.TDM, 100) 
  
  #create wordcloud
  wordcloud<-wordcloud2(MyAbstracts.TDM,
             fontWeight = "bold",  #bold all items
             rotate = 0,           #max rotation of words 
             size=.75,             #size of wordcloud
             fontFamily = "Times", #font of wordcloud   
             color = randomColor(nrow(MyAbstracts.TDM), #create color palette
                                 hue = "random", 
                                 luminosity = "dark")) 
  
  return(wordcloud)
}

##################################################################
### Run the function we just created, using the assigned name ###
##################################################################


p<-GoogleScholarWordCloud("Z4Tl1S4AAAAJ")
p


#library(htmlwidgets)
#library(webshot)
#htmlwidgets::saveWidget(p, "wordcloud.html", selfcontained=FALSE)
#webshot::webshot("wordcloud.html", "assets/wordcloud.png")

