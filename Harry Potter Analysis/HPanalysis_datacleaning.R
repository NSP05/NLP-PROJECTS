library('stringr')
library("SnowballC") # for stemming
library("stopwords")
library("tm")
rm(list=ls())

## extracting all words b4 processing
##################################################
HPB1.loc<-"/Users/neena/OneDrive/Desktop/BANA/Portfolio/Harrypotteranlysis/harrypotterbook1"
HPB1 <- VCorpus(DirSource(HPB1.loc))
summary(HPB1)
inspect(HPB1[[1]])
HPB1[[1]]$meta 
class(HPB1)

tokenize_words(as.character(HPB1[[1]]))

## sentiment analysis; how 1 chapter is connected to another

###########EFFICIENT WAYS TO GET THE WORDS punctuations, nonwords 

## all words
Wordzz<-function(x){
  all_words(as.character(x))
}

Word.list<-function(x){
  unlist(tokenize_words(as.character(x)))
}

tw1<-tm_map(HPB1,content_transformer(Word.list))

inspect(tw1[[1]])
class(tw1[[1]])

###Punctuations
punts<-function(x){
  all_words(as.character(x),contains ="[[:punct:]]" )
}


###digits 
nums<-function(x){
  str_extract_all(x,"[[:digit:]]+")
}

table(unlist(lapply(HPB1,nums)))
##### specialchars extract
spcharz<-function(x){
  str_extract_all(x, "\\W")
  
}


#### all chars
allchars<-function(x){
  str_extract_all(x, boundary("character"))
  
}
#data stored in excelfiles
##0=b4 preprocessing 1=preprocessed

##words list 
words.freq0<-as.data.frame(table(unlist(lapply(HPB1,Wordzz))))
head(words.freq0);dim(words0);sum(words0$Freq)
word.list0<-as.data.frame(table(unlist(lapply(HPB1,Word.list))))
dim(word.list0);head(word.list0)
##punctuation list
punctuations0<-as.data.frame(table(unlist(lapply(HPB1,punts))))
head(punctuations0);dim(punctuations0);sum(punctuations0$Freq)

### numerics
allnums0<-as.data.frame(table(unlist(lapply(HPB1,nums))))

##### specialchars
specialchar0<-as.data.frame(table(unlist(lapply(HPB1,spcharz))))

##### All chars 
Allchar0<-as.data.frame(table(unlist(lapply(HPB1,allchars))))

num_detect<-function (x) {str_detect(as.character(x),"[[:digit:]]+")}
table(unlist(lapply(HPB1,num_detect)))

wb1<- loadWorkbook("HPB1datalayout.xlsx")
addWorksheet(wb1,sheetName="uncleaned words")
addWorksheet(wb1,sheetName="punctuations")
addWorksheet(wb1,sheetName="digits")
addWorksheet(wb1,sheetName="special chars")
addWorksheet(wb1,sheetName="allchars chars0")
addWorksheet(wb1,sheetName="word list0")

writeData(wb1, sheet ="uncleaned words",words.freq0)
writeData(wb1, sheet ="punctuations",punctuations0)
writeData(wb1, sheet ="digits",allnums0)
writeData(wb1, sheet ="special chars",specialchar0)
writeData(wb1, sheet ="allchars chars0",Allchar0)
writeData(wb1, sheet ="word list0",word.list0)


saveWorkbook(wb1,file="HPB1datalayout.xlsx", overwrite = TRUE)


#############################################################
#############################################################

##stage 1 cleaning 
#### 1)  convert to lowercase
#### 2)  remove some words
#### 3) change words

##stage 2 cleaning
####
#### 4) change the \t,\r,\n blank spaces 
#### 
#### 5) remove the digits 

#### removing nos and changing case.
inspect(HPB1[[1]])
stopwords_getsources()
stopwords_getlanguages("stopwords-iso")
stopwords_getlanguages("snowball")
stopwords_getlanguages(source = "smart")

stopwords()
length(stopwords::data_stopwords_stopwordsiso$en)
length(stopwords::data_stopwords_snowball$en)
stopwords::data_stopwords_nltk$en

############################ 
### functions to be used :
#Wordzz , Word.list ,punts, nums,allchars,

num_replace<-content_transformer(function(x){replace_number(as.character(x))})
cleanBook1<- tm_map(cleanBook1,num_replace)

blackwords<-unlist(lapply(HPB1,punts))
blackwords1<-tm::stopwords()
blackword2<-stopwords::data_stopwords_snowball$en
blackword3<-stopwords::data_stopwords_smart$en
blackwords4<-stopwords::data_stopwords_nltk$en
unwanted.words<-c(blackwords,blackwords1,blackword2,blackword3,blackwords4)


rinse_1 <- content_transformer(function(x) {
  clean(as.character(x))})

unwanted_extract <- content_transformer(function(x, pattern) {
  str_replace_all(x,pattern,"")})



clean.corpus1 <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords,unwanted.words )
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, num_replace)
  corpus <- tm_map(corpus, rinse_1)
  corpus<-tm_map(corpus,unwanted_extract, "¿")
  corpus<-tm_map(corpus,unwanted_extract, "»")
  corpus<-tm_map(corpus,unwanted_extract,"ï")
  return(corpus)
}


cleanBook1<-clean.corpus1(HPB1)

allchars1<-table(unlist(lapply(cleanBook1,allchars)))

word.list11<-as.data.frame(table(unlist(lapply(cleanBook1,Word.list))))
head(word.list11);dim(word.list11)


addWorksheet(wb1,sheetName="cleaned words1")
writeData(wb1, sheet ="cleaned words1",word.list11)
saveWorkbook(wb1,file="HPB1datalayout.xlsx", overwrite = TRUE)


pattern <- c("aaaaaaaaaargh","aaah","aargh","\\<yeh",
          "yehve","\\<yer","yeah","yvonne","youknow","can't've","myst'ry","mm","mmm","nah","ooh"	,"oooooh","oooooooh",
          "abou","tryin","askin","aren","wasn","buyin","flyin",
          "frightenin","geroff","gettin","gulpin","gonna","gotta",
          "haaa","makin","haaaaaa","interestinglooking","jus","knowin","lecturin","lookin","meanin",
          "meddlin","readin","tellin","waitin","watchin","wizardin","aff","ah","aha","atta","bleaaargh",
          "blimey","cafru","comin","concen","conk","dumbledorein","emeraldgreen",
          "\\er\\","finchfletchley","\\fir","\\ha\\","halfhalf","hufflepuffis","lotta","mentionin","middecember","nof","nothin","nott","oy",
          "oyt","\\sa\\","\\sn\\","\\sp\\","\\st\\","\\upf\\","wow","ye" )

replacement <- c("","","","you","you have","your","yes","you have","you know", "",
              "mystery","","","","","","","about","trying","asking",'are not',
              "was not","buying","flying","frightening","get off" ,"getting","gulping","going to","got to","",
              "making","","interesting looking","just","knowing","lecturing", "looking","meaning","meddling","reading",
               "telling","waiting","watching","wizarding","","","","","","",
              "","comming","","","dumbledore in","emerald green","",
              "finch fletchley","fir ","ha ","half half","hufflepuff is","",
              "mentioning","mid december","","nothing","not","oy ",
              "","","","","","","wow","")



word.transformer<-content_transformer(function(x,pattern,replacement){multigsub(pattern,replacement,as.character(x))})

cleanBook1<- tm_map(cleanBook1,word.transformer, pattern, replacement)

cleanBook1<-tm_map(cleanBook1,stemDocument)
strip.space<- content_transformer(function(x){trimws(as.character(x))})
cleanBook1<-tm_map(cleanBook1,strip.space)


word.list21<-as.data.frame(table(unlist(lapply(cleanBook1,Word.list))))
head(word.list21);dim(word.list21);sum(word.list21$Freq)


addWorksheet(wb1,sheetName="cleaned words2")
writeData(wb1, sheet ="cleaned words2",word.list21)
saveWorkbook(wb1,file="HPB1datalayout.xlsx", overwrite = TRUE)
