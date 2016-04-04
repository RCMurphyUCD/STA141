install.packages("RCurl")
install.packages("XML")
library(RCurl)
library(XML)
library(stringr)

install.packages("RSelenium")
library(RSelenium)
#empty data frame to be used later
emptydf = data.frame(ID=as.factor(factor()),
                     Date = as.factor(factor()),
                     Tags=as.character(character()), 
                     Title=as.factor(factor()), 
                     URL = as.factor(factor()),
                     Views = as.numeric(numeric()),
                     Vote = as.numeric(numeric()),
                     Answers= as.numeric(numeric()),
                     Username = as.character(character()),
                     Reputation = as.numeric(numeric()),
                     stringsAsFactors=FALSE) 


ScrapeMyPages = function(emptydf) #function takes in an empty data frame and returns a new data frame with all
{#desired columns
  doc<-htmlParse(remDr$getPageSource()[[1]])
  
  thedf = FindThese(doc)#uses the FindThese function to get all the information eeded
  emptydf = rbind(emptydf,thedf)

  return(emptydf)

}


FindThese = function(doc) #this is the function that actually processes the webpage and gets the values in a data frame
{
  GetTitle <- xpathApply(doc,  "//body//div//h3//a[contains(@href, 'questions')]")
  Title = sapply(1:length(GetTitle), function(x) xmlValue(GetTitle[[x]]))
  GetUser = xpathApply(doc,  "//body//div[@class='user-details']")
  GetID = xpathApply(doc, "//body//div[contains(@id,'question-summary-')]/@id")
  
  ID = sapply(1:length(GetID), function(x) strsplit(GetID[[x]], "-")[[1]][3])
  
  UserName = sapply(1:length(GetUser), function(x) gsub("  ", "",strsplit(xmlValue(GetUser[[x]]), "\n")[[1]][2]))
  DateX = xpathApply(doc,  "//body//div[@class='user-action-time']/span//@title")
  Date = sapply(1:length(DateX), function(x) as.character(DateX[[x]]))
    #sometimes, if the user is anonymous, then the Reputation is non existant, so this is to counter that case
    #checking if the level above of RepX has reputation in it
  RepX = xpathApply(doc,  "//body//div[@class='user-details']//span[@dir='ltr']/text()")
  if (length(RepX)!=length(Date))
  {
    ans = vector("list",length(Date))
    els = getNodeSet(doc, "//body//div[@class='user-details']")
    k = 0
    for (i in 1:length(Date))
    {
      k = k+1
      if(length(grep("reputation", toString.XMLNode(els[[i]])))==0)
      {
      RepX[[i]] = NA
      k = k-1
      }
      else
      {
        ans[[i]] = RepX[[k]]
      }
    }
    
    Reputation = sapply(1:length(ans), function(x) NullChecker(ans[[x]]))
    Reputation = ifelse(Reputation==(-100), NA, Reputation)
  }
  if (length(RepX)==length(Date)){
    Reputation = sapply(1:length(RepX), function(x) NullChecker(RepX[[x]]))
    Reputation = ifelse(Reputation==(-100), NA, Reputation)
  }
  TagsX = xpathApply(doc,  "//body//div[contains(@class, 'tags')]")
  
  Tags = sapply(1:(length(TagsX)-1), function(x) gsub("  ", "", strsplit(xmlValue(TagsX[[x]]), "\n")[[1]][2]))
  
  ViewsX = xpathApply(doc,  "//body//div[contains(@title, 'views')]/@title")
  Views = sapply(1:length(ViewsX), function(x) as.character(ViewsX[[x]]))
  VoteX = xpathApply(doc, "//body//div[@class='votes']//span//strong/text()")
  Vote = sapply(1:length(VoteX), function(x) as.numeric(gsub("[[:punct:]]","", xmlValue(VoteX[[x]]))))
  AnswersX = xpathApply(doc,"//body//div[contains(@class,'status')]/strong/text()")
  Answers= sapply(1:length(AnswersX), function(x) as.numeric(xmlValue(AnswersX[[x]])))
  
  URLX = xpathApply(doc,  "//body//div[@class='summary']/h3/a/@href")
  URL = sapply(1:length(URLX), function(x) as.character(URLX[[x]]))
  
  newdf = data.frame(ID, Date, Tags, Title, URL, Views, Vote, Answers, UserName, Reputation)
  return(newdf)
}

NullChecker = function(x) #Was having roblems dealing with trying to use xmlValue with sapply
{ #because sometimes the value was a logical and it couldn't process... so I had to make an if else case
  if(is.null(x))
  {
    return (-100)
  }
  if(is.logical(x))
  {
    return (-100)
  }
  else
  {
    return(xmlValue(x))
  }
}
newurl = "http://stackoverflow.com/questions/tagged/r"
library(RSelenium)
checkForServer()
startServer()
remDr = remoteDriver$new()
remDr$open(silent=TRUE)

TimeToScrape = function(URL, numPages, emptydf, remDr) #Scraping function that requires the URL to be scraped
{  #the Number of pages to be scraped, an empty data frame store the values, and a remote driver to operate
 
remDr$navigate(URL)
tt = ScrapeMyPages(emptydf) #create initial data frame

for(i in 2:numPages) #loop over number of pages
{
  searchID = '//span[@class="page-numbers next"]'
  remDr$findElement(value=searchID)
  webElem = remDr$findElement(value=searchID)
  webElem$clickElement()
  tt = ScrapeMyPages(tt)
}
return(tt)
}


tt = TimeToScrape(newurl, 4560, emptydf, remDr) #work with above half of the pages
dim(tt) #I get 82350 rows, 10 columns
tt[sample(1:nrow(tt),10),] #sample of 10 to use as proven output
#Part 3
#1
splitted = split(rQAs, rQAs$type)
answers = splitted[[1]] #only answers
questions = splitted[[3]] #only questions
zz = data.frame(table(answers$user))
hist(zz$Freq, breaks = 750, xlim = c(0,35), main = "Histogram of Answers by Users", ylab = "Answered Question Freq", xlab = "Users")

#2

splitandcount = function(x)
{
  Splitted=strsplit(as.character(x), " ")[[1]]
  return(Splitted)
}
trial = sapply(1:nrow(tt), function(x) splitandcount(tt$Tags[x]))
ListofTags = unlist(trial)
MostCommonTags = sort(table(ListofTags), decreasing = TRUE)[1:10]
MostCommonTags
#3
HowManyMentionGGplot = table(grepl("ggplot", row.names(questions), ignore.case= TRUE))
HowManyMentionGGplot
#4
HowManyMentionWebScraping = table(grepl("xml|html|web scraping", questions$text, ignore.case=TRUE))
HowManyMentionWebScraping

#5
splitrownames = sapply(1:nrow(rQAs), function(x) strsplit((row.names(rQAs)[x]), "-")[[1]])
wordsintitle = unlist(splitrownames)
functionsintitle = wordsintitle[wordsintitle %in% ls(baseenv())]
unique(functionsintitle)

#6
library(stringr)
newtext = gsub("[^[:alnum:] ]","", answers$text)
newtext2 = sapply(1:nrow(answers), function(x) splitandcount(newtext[x]))
newtext3 = unlist(newtext2)
newtext4 = newtext3[newtext3%in%ls(baseenv())]
write.csv(unique(newtext4), file ="uniquefunc.csv")
