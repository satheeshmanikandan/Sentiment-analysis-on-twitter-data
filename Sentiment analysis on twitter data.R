##bayes algorith with xfinity tweets


#Step 1: Load the necessary packages
# required pakacges
library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

#method 1: using twitter api data
#twitter authetication 
#consumer_key    <- 'XXXXXXXXXXXXXXXXXXXXXXXX'
#consumer_secret <- 'YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY'
#access_token    <- 'ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ'
#access_secret   <- 'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW'
#setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
#setup_twitter_oauth("XXXXXXXXXXXXXXXXXXXXXXXX","YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY","3278708340-gmwMICQtptZsitnkdFAtUeqP47QMJ1zmuzA0NMe","05YB4TygRbVTK9xH4nrT73hFWrJt3Qq7Jsj5xrxnxGeu6")

#Step 2: Lets collect some tweets containing the term "starbucks" 
# harvest some tweets
#some_tweets = searchTwitter("comcast", n=1500, lang="en")


#method 2: using user data
comcast<-read.csv("D:\\Projects\\comcast\\twitter data\\Xfinity twitter data\\Overall xfinity data\\overall_Xfinity_twitter_data_dec_2014-dec_2015.csv")
head(comcast)
some_tweets=comcast$twitter_comment


# get the text
#some_txt = sapply(some_tweets, function(x) x$getText())
some_txt = some_tweets

#Step 3: Prepare the text for sentiment analysis
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
head(some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
#note: while removing two spaces replace it with a single space(in replace text area) 
some_txt = gsub("[ \t]{2,}", " ", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
length(some_txt)


# define "tolower error handling" function 
#try.error = function(x)
#{
# create missing value
# y = NA
# tryCatch error
#try_error = tryCatch(tolower(x), error=function(e) e)
# if not an error
#if (!inherits(try_error, "error"))
#y = tolower(x)
# result
#return(y)
#}
# lower case using try.error with sapply 
#some_txt = sapply(some_txt, try.error)
length(some_txt)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL
length(some_txt)

#Step 4: Perform Sentiment Analysis
# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
#alternative method 
#class_emo1 = classify_emotion(some_txt, algorithm="voter")


# get emotion best fit
emotion = class_emo[,7]
table(emotion)
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"
head(class_pol)
# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
#alternative method 
#class_pol1 = classify_polarity(some_txt, algorithm="voter")


# get polarity best fit
polarity = class_pol[,4]
table(polarity)
polarity_tweets<-data.frame(comcast$twitter_comment,polarity)
head(polarity_tweets)
write.csv(polarity_tweets,"D:\\polarity_tweets.csv")
#Step 5: Create data frame with the results and obtain some general statistics
# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
head(sent_df)
table(emotion,polarity)



#Step 6: Lets do some plots of the obtained results
#plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2") +
labs(x="emotion categories", y="number of tweets") +
opts(title = "Sentiment Analysis of Tweets about comcast\n(classification by emotion)",
     plot.title = theme_text(size=12))