require(RCurl)
require(XML)
require(stringr)
require(ggplot2)

sentimentScore <- function(sentences,vNegTerms, negTerms, posTerms, vPosTerms)
{
  
      #final_scores <- matrix('', 0, 5)
      
      end_score<-data.frame(X1=NA,vNegTerms_Count=NA,negTerms_Count=NA,posTerms_Count=NA,vPosTerms_Count=NA)
      i=0;scores<-0
      for(i in 1:10)
      {
        final_scores<-data.frame()
        
        #final_scores<-data.frame(X1=NA,vNegTerms_Count=NA,negTerms_Count=NA,posTerms_Count=NA,vPosTerms_Count=NA)
        scores <-as.data.frame(lapply(sentences[i], function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
          
          initial_sentence <- sentence
          initial_sentence<-gsub("*\\n", '', sentence)
          #initial_sentence<-gsub("newrow", '', sentence)
          
          #remove unnecessary characters and split up by word 
          sentence <- gsub('[[:punct:]]', '', sentence)
          sentence <- gsub('[[:cntrl:]]', '', sentence)
          sentence <- gsub('\\d+', '', sentence)
          sentence <- gsub("*\\n", '', sentence)
          sentence <- tolower(sentence)
          wordList <- str_split(sentence, '\\s+')
          words <- unlist(wordList)
          #build vector with matches between sentence and each category
          vPosMatches <- match(words, vPosTerms)
          posMatches <- match(words, posTerms)
          vNegMatches <- match(words, vNegTerms)
          negMatches <- match(words, negTerms)
          #sum up number of words in each category
          vPosMatches <- sum(!is.na(vPosMatches))
          posMatches <- sum(!is.na(posMatches))
          vNegMatches <- sum(!is.na(vNegMatches))
          negMatches <- sum(!is.na(negMatches))
          score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
          #add row to scores table
          newrow <- c(initial_sentence, score)
          final_scores <- rbind(final_scores, newrow)
          
          return(final_scores)
          
        }, vNegTerms, negTerms, posTerms, vPosTerms))
        
        colnames(scores)<-c("X1","vNegTerms_Count","negTerms_Count","posTerms_Count","vPosTerms_Count")
        end_score<-rbind(end_score,scores)
        
      }
  
  
  return(end_score)
}

########
#sentimetal analysis outside loop loading data part

#load up word polarity list and format it
afinn_list <- read.delim(file='C:/Users/ramesh/Documents/AFINN/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

# categorize words as very negative to very positive and add some movie-specific words
# wordlist updation taken from abromberg sentiment_analysis repository
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1], "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious")
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")




########

#### flipkart 18000-25000 page URL
## To remove the limit change the link given in the URL to another link.
## The link should belong to flipkart mobile section


URL <-"http://www.flipkart.com/mobiles/pr?p[]=facets.price_range%255B%255D%3DRs.%2B18001%2B-%2BRs.%2B25000&sid=tyy,4io&otracker=ch_vn_mobile_filter_Price_Rs.%2018001%20-%20Rs.%2025000#jumpTo=300|40"

sub_mainpage <- getURL(URL,verbose=T, header=F) 
sub_mainpagecount <- readLines(tc <- textConnection(sub_mainpage)); close(tc)  

sub_maintree <- htmlTreeParse(sub_mainpagecount, error=function(...){}, useInternalNodes = T)



#mobile1 has the mobile names
#mobile_link has the further mobile links
mobile1 <- xpathSApply(sub_maintree, "//*/div[@class='pu-title fk-font-13']", xmlValue)
# removing new ling character and leading and trailing blank spaces
mobile1 <- gsub("*\\n|^\\s+|\\s+$", '', mobile1)

###########
#subsetting 
mobile1<-mobile1[1:2]
###########

mobile1_link<-xpathSApply(sub_maintree, "//*/a[@class='fk-display-block']", xmlGetAttr,"href")

mobile1_link <- paste("http://www.flipkart.com",mobile1_link,sep="")

######################################################
#NOTE: PICKING ONLY 1 mobile1_link OR ELSE LAPTOP MIGHT HANG..
#      A for/while LOOP CAN BE PUT UP : DONE
######################################################

Mobile_Scores<-data.frame(vNegTerms_Count=NA,negTerms_Count=NA,posTerms_Count=NA,vPosTerms_Count=NA)

i=0;
for(i in 1:2)
{
  mobile_tmp<-mobile1_link[i]

  #opening connection to flipkart
  mobilepage <- getURL(mobile_tmp,verbose=T, header=F) 
  mobilepagecount <- readLines(tc <- textConnection(mobilepage)); close(tc)  
  
  mobiletree <- htmlTreeParse(mobilepagecount, error=function(...){}, useInternalNodes = T)
  
  # Url to the review page being extracted
  review_url <- xpathSApply(mobiletree, "//*/a[@class='fk-color-divider-dark fk-underline']", xmlGetAttr,"href")
  
  # Getting 2 urls so choose the 1st 1
  review_url<-review_url[1]
  review_url <- paste("http://www.flipkart.com",review_url,"&rating=1,2,3,4,5&reviewers=all&type=top&sort=most_helpful&start=",sep="")
  
  #### review_url has the review url not with a starting number that can be given through j
  ####
  
  ########################################
    
  # Review Sentiment analysis
  
  Results<-data.frame(X1=NA,vNegTerms_Count=NA,negTerms_Count=NA,posTerms_Count=NA,vPosTerms_Count=NA)
  Results_temp<-data.frame(vNegTerms_Count=NA,negTerms_Count=NA,posTerms_Count=NA,vPosTerms_Count=NA)
  
  j=0
  for(j in seq(0, 20, by = 10))
  {
    URL <-paste(review_url,j,sep="")
    j<-j+10
    #Initial URL to check the code functionality
    #URL <-paste("http://www.flipkart.com/moto-x-16-gb/product-reviews/ITMDWGFFRGC885QT?pid=MOBDSGU27YGJAZNB&rating=1,2,3,4,5&reviewers=all&type=top&sort=most_helpful&start=10")
    
    #opening connection to flipkart
    webpage <- getURL(URL,verbose=T, header=F) 
    webpagecount <- readLines(tc <- textConnection(webpage)); close(tc)  
    
    webtree <- htmlTreeParse(webpagecount, error=function(...){}, useInternalNodes = T)
    
    
    #Review heading
    record2 <- xpathSApply(webtree, "//*/div[@class='line fk-font-normal bmargin5 dark-gray']", xmlValue)
    
    #review
    record1 <- xpathSApply(webtree, "//*/span[@class='review-text']", xmlValue)
    len<-length(record1)
    
    
    #Reviewers star rating
    #stars
    #<div class="rating" style="width: 20%;">
    #  *****        </div>
    # NOTE: width tells how many stars are given by the rater
    
    #star rating
    ##########record3 <- xpathSApply(webtree, "//*/div[@class='rating']", xmlValue)
    
    #record4 has the width parameter
    ##########record4<-xpathSApply(webtree, "//*/div[@class='rating']", xmlGetAttr,"style")
    
    #review page moves to next set of reviews after a set of 10 reviews 
    #http://www.flipkart.com/moto-x-16-gb/product-reviews/
    #  ITMDWGFFRGC885QT?pid=MOBDSGU27YGJAZNB&rating=1,2,3,4,5&
    #  reviewers=all&type=top&sort=most_recent&start=20
    #NOTE: start in the end of the given url is the starting review number
    
    
    #totavgrating: the total average of all the rating given in reviews
    ##########totavgrating <- as.numeric(xpathSApply(webtree, "//*/div[@class='pp-big-star']", xmlValue))
    
    
    #totraters: total number of raters who rated the product
    ##########totraters <- xpathSApply(webtree, "//*/span[@class='fk-font-11']", xmlValue)
    
    
    #totstarraters: Number of raters who gave 5/4/3/2/1 rating
    ##########totstarraters <- xpathSApply(webtree, "//*/ul[@class='rating-histogram rating-histogram-with-innertext']", xmlValue)
    
    
    #sentiment analysis
    # Data frame with Results of sentimental score
    
    Records <-sentimentScore(record1, vNegTerms, negTerms, posTerms, vPosTerms)
    #Records <-Records[-1,]
    Results<-rbind(Results,Records)
    Records<-NA
    
  }
  
  Results<-Results[complete.cases(Results),]
  Results_temp<-Results[,-1]
  row.names(Results_temp)<-1:nrow(Results_temp)
  
  ##############
  #naive way to convert the classes of columns
  
  Results_temp$vNegTerms_Count<-as.numeric(Results_temp$vNegTerms_Count)
  Results_temp$negTerms_Count<-as.numeric(Results_temp$negTerms_Count)
  Results_temp$posTerms_Count<-as.numeric(Results_temp$posTerms_Count)
  Results_temp$vPosTerms_Count<-as.numeric(Results_temp$vPosTerms_Count)
  
  ##############
  
  Mobile_Scores<-rbind(Mobile_Scores,colSums(Results_temp))
}

Mobile_Scores<-Mobile_Scores[-1,]
row.names(Mobile_Scores)<-1:nrow(Mobile_Scores)
Mobile_Scores<-cbind(mobile1,Mobile_Scores)

#Plots
#Plot1 
#NOTE: Mobile_Scores is being updated

Mobile_Scores$vNegTerms_Count<- (-1)* Mobile_Scores$vNegTerms_Count
Mobile_Scores$negTerms_Count<- (-1)* Mobile_Scores$negTerms_Count

Mobile_Scores$sums<-rowSums(Mobile_Scores[,-1])
Mobile_Scores$pos<-Mobile_Scores$sums>0

ggplot(Mobile_Scores, aes(x=mobile1, y=sums, fill=pos)) +  
  geom_bar(stat="identity", position="identity")
