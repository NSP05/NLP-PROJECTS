#my working directory.I have an blank excel file "data4project.xlsx" created in my work space
# at the very beginning, The raw data was saved in that file.


#setwd("C:/Users/neena/OneDrive/Desktop/BANA/Portfolio/Tweetanalysis")

library("dplyr");library("rtweet")
library("maps");library(RColorBrewer)
library(tidyverse);library(ggrepel);library(openxlsx)
library(grid);library(stringr);library(readr)
library(wordcloud);library(readxl);library(qdap)
library(tokenizers);library(tm);library(stopwords)
library(RWeka);library(SnowballC);library(reshape2);library(igraph)
library(ggthemes);library(rematch2);library(openxlsx);library(Unicode)

#Date::NOV13, 2020
#######creating the twitter API and tryout how things work
API_KEY<-"luB6xgS9kBWG3l9xx76Xtiykw"
APP_NAME<- "ISMG 6470_NSP"
API_SECRET_KEY<-"QbwWJTFPyscXrNumD1zMlfjbgAdExOdZEAD0xYAZRVp6FQUAGr"
BEARERtkn<-"AAAAAAAAAAAAAAAAAAAAABaCJwEAAAAArPs%2BM0QjTU2HKk2eHGiP%2FVjlhdY%3Dji1AIgYCnuqQACg7cZ5ghEQzzeoBVSJkLHoh1wZ90au3cjsx1d"
access_token<-"1043172204626604032-23zTJHKdTj9x6Rajz51rSXTL7miqmV"
access_token_secret <-"2sY72ACvDSwaQS0OFZXR55xoPtVFycOPSAsvexlErEuvm"

create_token(
  app = APP_NAME,
  consumer_key = API_KEY,
  consumer_secret = API_SECRET_KEY,
  access_token = access_token,
  access_secret = access_token_secret)

??post_tweet()
post_tweet("Look, i'm tweeting from R in my #rstats")


#nOV 15
####################################################################
                  #DATA COLLECTION
####################################################################
##PART 1 Collecting Tweets regarding women in technology

women.keywords<-c("#WomenInStem", "#WomenInTech","#GirlsWhoCode")
womenpower <- search_tweets2(women.keywords,n = 3000)
search
## inspecting the data
dim(womenpower);head(womenpower);head(womenpower$text)

## saving the raw data
wb1<-loadWorkbook("data4project.xlsx")
addWorksheet(wb1,sheetName = "Rawdata")
writeData(wb1,"Rawdata",womenpower)
saveWorkbook(wb1,"data4project.xlsx",overwrite = TRUE)

###########################################################

######### PART 2: DATA CLEANING : stage 1
## filter by land="en" 
## user_id	created_at	screen_name	text	source	
##is_quote	is_retweet	favorite_count	retweet_count	
##lang	retweet_text	retweet_created_at	retweet_source	retweet_favorite_count	
##retweet_retweet_count	retweet_user_id	retweet_screen_name	retweet_followers_count	retweet_friends_count	
##retweet_location	retweet_description	country	country_code	location	followers_count	friends_count	favourites_count


WD.raw<- read_excel("data4project.xlsx", sheet = "Rawdata")
head(WD.raw)


#### reducing the dimension and saving the sample data
head(WD.raw$lang);unique(WD.raw$lang)
WD.sample<- WD.raw %>% filter(WD.raw$lang=="en") %>%
            select(user_id,	created_at,	screen_name,	text,	source,	
                   is_quote,	is_retweet,	favorite_count,	retweet_count,
                   retweet_text,	retweet_created_at,	retweet_source,	retweet_favorite_count,	
                   retweet_retweet_count,	retweet_user_id,	retweet_screen_name,retweet_followers_count,	retweet_friends_count
                   ,retweet_location,location,	retweet_description,location,	followers_count,	friends_count,favourites_count) %>%
           mutate(tweet.location=location,
                  retweet.location=retweet_location)

addWorksheet(wb1,sheetName = "sampledata")
writeData(wb1,"sampledata",WD.sample)
saveWorkbook(wb1,"data4project.xlsx",overwrite = TRUE)

########### the sample data was saved as an excel file,2 coloums:
########### location and retweet_location,that shows the country source of the 
### tweet was duplicated. They are cleaned and recoded in JMP in the format :Location,Country.
######It was easy to split these cols in Excel,So we have 4 cols signifying
####the location of the user: Tweet.Location,Tweet.Country,and retweet.location,retweet.Country.
## the subsiquent redundant cols were deleted.
Sys.setlocale('LC_ALL','C')

##Nov 19
#################################################################################3
                       #Analysis PART 1: BASIC STRUCTURE OF THE DATA
#################################################################################3

WD.sample.set<- read_excel("data4project.xlsx", sheet = "sampledata")
head(WD.sample.set$text);dim(WD.sample.set)


### The top contributers
Top.contributers<-WD.sample.set %>%
  count(screen_name, sort = TRUE) %>%
  top_n(15) %>%
  ggplot(aes(x =screen_name , y = n)) +
  geom_col(color="black", fill="pink") +
  coord_flip() +
  labs(x = "Users",
       y = "Count",
       title = "The Top contributers of #Womeninstem tweets")

Top.contributers

######proportion of quotes in the tweets
quotes<-WD.sample.set %>%
  group_by(is_quote) %>%
  summarize(n_distinct(screen_name))

ggplot(quotes, aes(x="", y=`n_distinct(screen_name)`, fill=is_quote)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label =`n_distinct(screen_name)`) , position = position_stack(vjust=0.5))+
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_brewer(palette="Blues")

#whether the given tweets are retweet or not
isretweet<-WD.sample.set %>%
  group_by(is_retweet) %>%
  summarize(n_distinct(screen_name))

ggplot(isretweet, aes(x="", y=`n_distinct(screen_name)`, fill=is_retweet)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label =`n_distinct(screen_name)`) , position = position_stack(vjust=0.5))+
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_brewer(palette="Pastel2")


tweetsource<-WD.sample.set %>%
  count(source, sort = TRUE) %>%
  top_n(15) %>%
  na.omit()%>%
  ggplot(aes(x =source, y = n)) +
  geom_col(color="black", fill="#FDAE6B") +
  coord_flip() +
  labs(x = "Source of the tweet",
       y = "Count",
       title = "The major source of  #Womeninstem tweets")





#############################################################################################################
               #Analysis part2 : Sentiment Polarity Analysis of Tweets
#############################################################################################################

###Tweet analysis, stage 1: cleaning of tweets
Tweets<- WD.sample.set$text %>% 
        str_remove_all("#.*")%>%
        str_remove_all("https.*")%>%
        str_remove_all("@.*")%>%
        str_remove_all("<U+[[:alnum:]].*")%>%
        tm::stripWhitespace()%>%
        tm::removePunctuation()%>%
       as.vector() 

## preparing vector for polarity-sentiment analysis
Tweet.vector<- Tweets%>%
      tm::removeNumbers()%>%
      str_to_lower()

# removing the emoticons     
Tweets.vector<-iconv(Tweet.vector, "latin1", "ASCII", sub="")
 
################# Frequent words in the tweets
Freq.terms<-freq_terms(Tweet.vector,stopwords = Top100Words,top = 10)

wordcloud(Freq.terms$WORD,Freq.terms$FREQ,scale=c(3,.5),vfont=c("serif","plain"),colors='blue')
######################

##Polarity-Sentiment analysis of Tweets
Tweet.pol.data<- polarity(Tweets.vector, polarity.frame = key.pol,amplifier.weight = 0.93,rm.incomplete = TRUE,n.before = 2,
                          n.after = 2)
Tweet.pol.data
Tweets.polarity<-Tweet.pol.data$all
# removing NAN values
Tweets.polarity<-Tweets.polarity[complete.cases(Tweets.polarity$polarity),]


## tweets with 0 polarity was removed and visualized
Tweet.ext.sentiment<-Tweets.polarity[!Tweets.polarity$polarity==0,]

### visualization
display.brewer.pal(n = 8, name = 'GnBu');brewer.pal(n = 8, name ='GnBu')
#"#F7FCF0" "#E0F3DB" "#CCEBC5" "#A8DDB5" "#7BCCC4"
#"#4EB3D3" "#2B8CBE" "#08589E"

Tweet.polarity.graph<- ggplot(Tweet.ext.sentiment, aes(x=scale(polarity), y=..density..)) + 
  theme_gdocs() + geom_histogram(binwidth=.11, fill="#08589E", colour='black', size=.4) + 
  geom_density(size=.60)+ggtitle("The sentiment polarity across the tweets")

Tweet.polarity.graph

####################################################################################
  
            #Analysis part3 : Sentiment Polarity Analysis of Retweet Discription

####################################################################################
Retweet.discription<-WD.sample.set$retweet_description %>% 
  str_remove_all("#.*")%>%
  str_remove_all("https.*")%>%
  str_remove_all("@.*")%>%
  replace_symbol()%>%
  str_replace_all("-","none")%>%
  tm::stripWhitespace()%>%
  tm::removePunctuation()%>%
  tm::removeNumbers()%>%
  as.vector()

## preparing vector for polarity-sentiment analysis
Retweet.discribtion.vector<- Retweet.discription%>%
  tm::removeNumbers()%>%
  str_to_lower()

# removing the emoticons     
Retweet.discribtion.vector<-iconv(Retweet.discribtion.vector, "latin1", "ASCII", sub="")

##Polarity-Sentiment analysis of Retweets
Retweet.dis.pol.data<- polarity(Retweet.discribtion.vector, polarity.frame = key.pol,amplifier.weight = 0.93,rm.incomplete = TRUE,n.before = 2,
                                n.after = 2)
Retweets.dis.polarity<-Retweet.dis.pol.data$all

# removing NAN values
Retweets.polarity<-Retweets.dis.polarity[complete.cases(Retweets.dis.polarity$polarity),]

### Removing tweets with 0 polarity values
Retweet.ext.sentiment<-Retweets.polarity[!Retweets.polarity$polarity==0,]

Reweets.dis.polarity.graph<- ggplot(Retweet.ext.sentiment, aes(x=scale(polarity), y=..density..)) + 
  theme_gdocs() + geom_histogram(binwidth=.15, fill="#CCEBC5", colour='black', size=.4) + 
  geom_density(size=.60)+ggtitle("The sentiment polarity across the Retweets discription")

Reweets.dis.polarity.graph


###############################################################################################
                    # Analysis Part 4: Sentiment Word Cloud of the Tweets
###############################################################################################


neutral.tweets<-Tweets.polarity$text.var %>%
  subset(Tweets.polarity$polarity==0)%>%
  tokenize_words()%>%as.vector()


positive.tweets<- Tweets.polarity$text.var %>%
  subset(Tweets.polarity$polarity>0)%>%
  tokenize_words()%>%as.vector()

negative.tweets<-Tweets.polarity$text.var %>%
  subset(Tweets.polarity$polarity<0)%>%
  tokenize_words()%>%as.vector()

###### combining positive, negative and neutral tweets

all.tweets<-data.frame(cbind(unlist(positive.tweets),unlist(neutral.tweets),unlist(negative.tweets)))
colnames(all.tweets)<-c("positive tweets","neutral tweets","negative tweets")
head(all.tweets)

#building a corpus
tweet.corpus<-VCorpus(VectorSource(all.tweets))
inspect(tweet.corpus)

##Term Document Matrix

tweet.tdm<-TermDocumentMatrix(tweet.corpus, control=list(weighting=weightTfIdf, tm::stopwords(kind = "en"),tm::stopwords(kind = "smart")))
tweet.tdm.m<-as.matrix(removeSparseTerms(tweet.tdm,0.95))       

colnames(tweet.tdm.m) <- c('positive','neutral', 'negative')

par(mfrow=c(0.5,0.5), mar=c(1,1.5,1.5,1.5))
wordcloud::comparison.cloud(tweet.tdm.m,scale=c(2,.25),max.words=200,random.order=FALSE,title.size=1,
                            title.colors=c("green","#737373","red"),
                           colors=c('darkgreen',"#525252", "#F16913"))

??comparison.cloud();brewer.pal(n = 8, name ='Greys');brewer.pal(n = 8, name ='Oranges')

###############################################################################################
                           # Analysis Part 5: Emoji/Emoticon Analysis of the Tweets
###############################################################################################

### two datasets on emojis were used: emojis and emDict.The former shows 
### a vareity of emojis like man, man scientist, woman, girl etc. where as the latter is crisp.
#### two datasets were initially used for comparison.It was found out that
#### emdict effectively summarizes the emojis present in the tweets.So it was to
### used to analyse the general range of emojis used, 
### emojis dataset a greater varitey of emojis, so for visualizing 
## the use of women scitist, girl, man etc it was used.

data("emojis");data("emoticon")
head(emoticon$emoticon);head(emojis$code)
head(emoticon);tail(emoticon);dim(emoticon)
head(emojis);dim(emojis) 
emojis$code;

# data on emojis.
emdict<- read.csv2("emDict.csv")

emoji.frequency<- vapply(emdict$Description,
                                  regexpr,FUN.VALUE = integer(length(Tweets)),
                         WD.sample.set$text, useBytes = T )

emoji.counts <- colSums(emoji.frequency>-1)
emoji.counts
######converting to a dataframe
Tweet.emoji.df<-emoji.counts%>%reshape2::melt()
colnames(Tweet.emoji.df)<-c("Tweet.emojis")
Tweet.emoji.df<- cbind(Emoji.Discription = rownames(Tweet.emoji.df), Tweet.emoji.df)
rownames(Tweet.emoji.df) <- 1:nrow(Tweet.emoji.df)
Tweet.emoji.df<-Tweet.emoji.df[!Tweet.emoji.df$Tweet.emojis==0,]
Tweet.emoji.df<-Tweet.emoji.df[order(-Tweet.emoji.df$Tweet.emojis),]
head(Tweet.emoji.df)

### if the code for the graph didnt run at the first time,please run 
## this code and rerun the Tweet.emoji.plot function.
dev.off()

### top 6 recurring emojis
Tweet.emoji.plot<- ggplot(Tweet.emoji.df[1:6,],aes(Tweet.emojis,Emoji.Discription, fill=Emoji.Discription))+
  theme_gdocs() + geom_bar(stat="identity")+scale_fill_brewer(palette="Pastel1")+ggtitle(label="The common emojis  in tweets")

Tweet.emoji.plot

##women in data science related emojis

emoji.frequency2<- vapply(emojis$description,
                         regexpr,FUN.VALUE = integer(length(Tweets)),
                         WD.sample.set$text, useBytes = T )

emojiz <- colSums(emoji.frequency2>-1)
emojiz

womenEmoji<-emojiz%>%reshape2::melt()
colnames(womenEmoji)<-c("Tweet.emojis")
womenEmoji<- cbind(Emoji.Discription = rownames(womenEmoji),womenEmoji)
rownames(womenEmoji) <- 1:nrow(womenEmoji)
womenEmoji<-womenEmoji[!womenEmoji$Tweet.emojis==0,]
womenEmoji<-womenEmoji[order(-womenEmoji$Tweet.emojis),]

w.index=c("woman","girl","information","woman scientist","man scientist","man","boy")
Women.rep<-womenEmoji[womenEmoji$Emoji.Discription %in% w.index,]

Womenpwr.emoji.plot<- ggplot(Women.rep,aes(Tweet.emojis,Emoji.Discription, fill=Emoji.Discription))+
  theme_gdocs() + geom_bar(stat="identity")+scale_fill_brewer(palette="Accent")+
  ggtitle(label="The Common #womeninstem specific emojis in the tweets")+geom_text(aes(label=Tweet.emojis))
Womenpwr.emoji.plot

########### emoticons in the Tweets

emoticon.frequency<- vapply(emoticon$meaning,
                         regexpr,FUN.VALUE = integer(length(Tweets)),
                         Tweets, useBytes = T )
emoticon.counts <- colSums(emoticon.frequency>-1)

Emoticon.tweets<- reshape2::melt(as.matrix(emoticon.counts))
Emoticon.tweets["Var2"]<-emoticon$emoticon
colnames(Emoticon.tweets)<- c("Discription","Emoticon","Tweet.Frequency")
Emoticon.tweets<-Emoticon.tweets[!Emoticon.tweets$Tweet.Frequency==0,]

Twt.emoticon.plot<- ggplot(Emoticon.tweets,aes(Tweet.Frequency,Emoticon , fill=Emoticon))+
theme_gdocs() + geom_bar(stat="identity")+scale_fill_brewer(palette="Paired")+
  ggtitle(label="The Common Emoticons in the tweets")+geom_text(aes(label=Tweet.Frequency))

Twt.emoticon.plot



###############################################################################################
# Analysis Part 6: Tracing the Tweet Flow
###############################################################################################

WD.sample.set$Tweet.Country<-recode(WD.sample.set$Tweet.Country,
                                    "New Mexico.USA" = 'USA',
                                    'United Kingdom' = 'UK')
Social.circle<-WD.sample.set %>% 
  select(user_id,	screen_name,favorite_count,	retweet_count,retweet_favorite_count,	
         retweet_retweet_count,retweet_screen_name,retweet_followers_count,	
         retweet_friends_count,followers_count,	friends_count,
         favourites_count,Tweet.location,Tweet.Country,retweet.location,retweet.Country)%>%
  mutate(Tweet.location=as.factor(Tweet.location),
         Tweet.Country=as.factor(Tweet.Country),
         retweet.location=as.factor(retweet.location),
         retweet.Country=as.factor(retweet.Country),
         screen_name=as.factor( screen_name),
         retweet_screen_name=as.factor(retweet_screen_name))
         
######### Calculating Tweet popularity and Tweet spread.
tweetExt<- Social.circle%>%  select(screen_name,favorite_count,	retweet_count,retweet_favorite_count,	
                                    retweet_retweet_count,retweet_screen_name,retweet_followers_count,	
                                    retweet_friends_count,followers_count,	friends_count,
                                    favourites_count)%>% 
  mutate(tweetpopularity=favorite_count+(0.67*retweet_count)+(1.25*retweet_favorite_count)+(1.74*retweet_retweet_count),
         tweetspread=scale((mean(followers_count+friends_count+favourites_count))+ 2.5*((retweet_followers_count+retweet_favorite_count+retweet_friends_count)/3)))%>%
  select(screen_name,retweet_screen_name,tweetpopularity,tweetspread)



Total.tweet.ext<-aggregate(
  x = tweetExt[c("tweetpopularity","tweetspread")],
  by = tweetExt[c("screen_name","retweet_screen_name")],
  FUN = sum, na.rm = TRUE)


Total.tweet.pop <-Total.tweet.ext%>% 
  group_by(screen_name)%>%
  summarize(total.pop= sum(tweetpopularity,na.rm=TRUE))%>%
  arrange(-total.pop)

Popularityplot<- ggplot(head(Total.tweet.pop, n=15),aes(screen_name,total.pop,fill=screen_name))+
  theme_gdocs() + geom_bar(stat="identity",colour="dodgerblue", fill="white")+
  coord_flip()
Popularityplot
#Popularityplot2<-ggplot(tail(Total.tweet.pop, n=10),aes(screen_name,total.pop,fill=screen_name))+
#theme_gdocs() + geom_bar(stat="identity",colour="dodgerblue", fill="white")+
#coord_flip()

Total.twt.spread<-Total.tweet.ext%>% 
  group_by(screen_name)%>%
  summarize(total.spread= sum(tweetspread,na.rm=TRUE))%>%
  arrange(-total.spread)

tweetcirculation<- ggplot(head(Total.twt.spread, n=17),aes(screen_name,total.spread,fill=screen_name))+
  theme_gdocs() + geom_bar(stat="identity",colour="red", fill="white")+
  coord_flip()
tweetcirculation
###############################################################################################
# Analysis Part 7: Who retweets whom: the Spread of tweets of prominent contributer
###############################################################################################


Total.tweet.ext$tweetspread
tweet.poster<- Total.tweet.ext%>% select(screen_name,retweet_screen_name,tweetspread)%>%
  filter(screen_name =='femtech_')%>%arrange(-tweetspread)%>%head(n=15)%>%
  na.omit(unique())
g <- graph.data.frame(tweet.poster[,c(1:2)], directed = T)
g <- set_edge_attr(g, "weight", value= tweet.poster$tweetspread)

ecount(g) # edges (connections)
vcount(g) # vertices (nodes)
diameter(g) # network diameter
farthest.nodes(g) # show the farthest nodes
par(mar=c(1,1,1,1))
plot(g)
title("Tweets from the User account femtech_: Who retweets whom")



#########################################################################################

            # Analysis:8                  ###Geospatial Visualization
#########################################################################################
#########THE MAJOR ORIGIN OF THE TWEETS:
## the tweets seems to be orginating from USA,Canada,India,Australia 
#########################################################################################

# Analysis:8                  ###Geospatial Visualization
maps::worldMapEnv;map_data("world")
head(Social.circle)
class(Social.circle$user_id)

tweet_country<- Social.circle %>% group_by(Tweet.Country)%>%
  summarize(no_tweets = n_distinct(as.numeric(user_id)))%>%arrange(-no_tweets)
tweet_country<-tweet_country[!tweet_country$Tweet.Country=="NA",]


map.world <- map_data("world")

#joining the data
map.world_joined <- left_join(map.world,tweet_country, by = c('region' = 'Tweet.Country'))
head(map.world_joined)
#CREATE FLAG
map.world_joined <-  map.world_joined %>% mutate(fill_flg = ifelse(is.na(no_tweets),F,T))
head(map.world_joined);


theme_map <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}

tweet.loc.points11<-ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group,fill = fill_flg))+
  scale_fill_manual(values = c("#CCCCCC","#e60000")) +
  labs(title = 'The geo-spatial origin of the Tweets'
       ,subtitle = "#womeninstem,#girlswhocode") +theme_map() 

tweet.loc.points11

#############
## country wise spread of tweets

tweet.concent<-ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat,group = group,fill = no_tweets))+
  labs(title = 'No of Tweets: Country Specific Classification'
       ,subtitle = "#womeninstem,#girlswhocode")  

tweet.concent

############################################ THE END ##############################################

