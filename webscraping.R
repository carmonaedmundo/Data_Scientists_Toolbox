# load packages
library(RCurl)
library(XML)
library(dplyr)
#install.packages("wordcloud",dependencies = T)
library(wordcloud)

get_google_page_urls <- function(u) {
  # read in page contents
  html <- getURL(u)
  
  # parse HTML into tree structure
  doc <- htmlParse(html)
  
  # extract url nodes using XPath. Originally I had used "//a[@href][@class='l']" until the google code change.
  
  encabezados <- xpathApply(doc, "//h3//a[@href]", xmlValue)
  attrs <- xpathApply(doc, "//h3//a[@href]", xmlAttrs)
  nodoPaps <- xpathApply(doc, "//h3//a[@href]", xmlParent)

  notas<-sapply(nodoPaps, function(x){
    paps<- xmlParent(x)
    xmlValue(xmlChildren(paps)[[3]])
  })
  # extract urls
  encabezado <- sapply(encabezados, function(x) x[[1]])
  nota <- sapply(notas, function(x) x[[1]])
  links <- sapply(attrs, function(x) x[[1]])
  #links <- grep("http://", links, fixed = TRUE, value=TRUE)
  informacion <- as.data.frame(cbind(encabezado, nota, links))
  row.names(informacion)<- NULL
  # free doc from memory
  free(doc)
  return(informacion)
}

palabra <- "panama+papers+mexico"
u<- paste0("https://www.google.com.mx/search?aq=f&gcx=w&sourceid=chrome&ie=UTF-8&q=",palabra,"&tbm=nws")
u2<- paste0("https://www.google.com.mx/search?aq=f&gcx=w&sourceid=chrome&ie=UTF-8&q=",palabra,"&tbm=nws&start=10")
u3<- paste0("https://www.google.com.mx/search?aq=f&gcx=w&sourceid=chrome&ie=UTF-8&q=",palabra,"&tbm=nws&start=20")
u4<- paste0("https://www.google.com.mx/search?aq=f&gcx=w&sourceid=chrome&ie=UTF-8&q=",palabra,"&tbm=nws&start=30")
u5<- paste0("https://www.google.com.mx/search?aq=f&gcx=w&sourceid=chrome&ie=UTF-8&q=",palabra,"&tbm=nws&start=40")

datos <- get_google_page_urls(u)
datos2 <- get_google_page_urls(u2)
datos3 <- get_google_page_urls(u3)
datos4 <- get_google_page_urls(u4)
datos5 <- get_google_page_urls(u5)

datos<- rbind(datos,datos2, datos3,datos4,datos5)
rm(datos2)
rm(datos3)
rm(datos4)
rm(datos5)

datos$encabezado<- as.character(datos$encabezado)
datos$nota<- as.character(datos$nota)
texto <- c(datos$encabezado,datos$nota)

vocalesCon<-"áéíóú"
vocalesSin <-"aeiou"
palabra<-paste(palabra,chartr(vocalesCon, vocalesSin, palabra),sep = "+")

wordList = c("los","que","por","para","son","más","cuando","bajo","este","de","tiene","sobre",
             "las","con","the","una","como","del","ser","tras","la",
             unlist(strsplit(palabra, "+",fixed = T)))

texto<-gsub(x = texto, pattern = paste(wordList, collapse = "|"), replacement = "", ignore.case = TRUE)

wordcloud(texto, colors=brewer.pal(6,"Dark2"),random.order=FALSE)
