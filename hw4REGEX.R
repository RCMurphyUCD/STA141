library(stringr)
load(url("http://eeyore.ucdavis.edu/stat141/Data/vehicles.rda"))

#1 (PRICE)
price = str_match(vposts$body, "\\$([0-9]*,?[0-9]+)")
#Match if string begins with $ (\\$ bc $ is a special character)
#followed by any number of numbers (0-9) with an optional , followed by numbers until you reach a non-number.
#Get these values
removecomma = gsub(",", "", price[,2]) #Remove commas
bodyprice = as.numeric(removecomma) #Set as numeric and compare
vposts$bodyprice = bodyprice
table(bodyprice == vposts$price) #Check results
#Problems with matching price
#str_match grabs the very first item that looks like the pattern (if they just had a $5 before the 
#real price, it would be thrown off). 
#Also, many of the bodies don't include the price --
#They actually include a different $ amount, such as an advertisement's $300 per month payment, for ex.
#If price wasn't actually there to begin with, the str_match will take this advertisement's $ amount as the mistaken price


#2 (VIN)
VIN = str_match(vposts$body, "[A-NPR-Z0-9]{17}")
#Some potential problems
#First, I assume that all VINs in the body are 17 characters long and only have uppercase letters
#For older cars, the VIN may not actually be 17 numbers
#Also, Stock was not separated from the VIN which means that if a VIN was actually 16 characters, the S from stock
#would be interpreted as part of the VIN mistakenly
table(!is.na(VIN))
#8611 TRUEs represent the amount of VINs extracted from the body
vposts$VIN = VIN


#3 (PHONE)
phone = str_match(vposts$body, "(([(0-9)]){5}.[(0-9)]{3}.[(0-9)]{4}|[(0-9)]{3}[-][(0-9)]{3}[-][(0-9)]{4}")
#Has 5 characters of (, 0-9, and ), followed by a space, and then 0-9 3 times.
#Or has 3 numbers, a -, 3 numbers, another -, and then 4 numbers
phone = phone[1,]
vposts$phone = phone

#4 (EMAILS)
email = str_match(vposts$body ," [A-z]+([@][A-z]+\\.[A-z]{3})")[,1]
vposts$email = email
#any number of letters followed by a @ followed by any number of letters then a period and then 3 letters


#5 (YEAR)
bodyyear = str_match(vposts$body, "1[9][4-9][0-9]|2[0][0-1][0-9]")
table(as.character(vposts$year) == bodyyear)
#high % of years extracted were equal to the year column in the data frame


#6 (MODEL)
Title = vposts$title

getTitle = function(splitt, listonum) #take in a vector of strings and the number where the pattern was matched
{
  if(!is.na(listonum)) #if the pattern matched was not NA, return the string + next string element together
  { # "Chevy" "MovieTheater" return "Chevy MovieTheater"
    return(paste(splitt[as.numeric(listonum):as.numeric(listonum+1)],collapse = " "))
  }
  else
  {
    return(NA) #else return NA
  }
}

GetModel = function(Title, pattern) #take in a Title and a Pattern
{
  splitdat = strsplit(Title, " ") #split the Title on spaces (ex "2012" "Chevrolet" "MovieTheater")
  listonum = sapply(1:length(Title), function(x) agrep(pattern, splitdat[[x]], ignore.case = TRUE)[1]) 
  #return a vector of numbers where each number corresponds to the match (ex above would be 2)
  together = sapply(1:length(Title), function(x) getTitle(splitdat[[x]], listonum[x]))
  #return the match with the following string ("Chevrolet MovieTheater")
}

MakerNames = names(sort(table(vposts$maker), decreasing = TRUE))[1:33]
biggerthanlife = sapply(1:length(MakerNames), function(x){GetModel(Title, MakerNames[x])}) #matrix/dataframe of each maker side by side
smalllife = tolower(biggerthanlife)
trialrun = sapply(1:ncol(smalllife), function(x) gsub("[^[:alnum:]///' ]", "",smalllife[,x])) #remove non space/non-alphanumeric characters
testrun = sapply(1:ncol(trialrun), function(x) {ifelse(grepl(paste("\\<",MakerNames[x],"\\>", sep = ""), trialrun[,x]), trialrun[,x], NA)})
#if the row value doesn't contain the vehicle maker, set to NA (example for sale should be removed from fords)
#also, it must have ford in it alone (remove oxford ___ for example)

GetNamesF = function(getnames, column) #takes in getnames from CommonChange (top values that have > 12 )
{ #and a single observation from column... a bit misleading on the argument name
  #if the value isn't NA, then go ahead and find the closest value in the getnames vector and return the closest one
  if(!is.na(column))
  {
    max.dist = list(cost = 3, ins = 0, del = 1, sub = 0)
    getnames[agrepl(column, getnames, max.distance = max.dist)][1]
  }
  else #if it is NA originally, keep at NA
  {
    return(NA)
  }
}

CommonChange = function(column)
{ 
  sortedtable = sort(table(column), decreasing = TRUE) #sorts a table with decreasing order
  greaterthan15 = sortedtable[sortedtable>12] #takes values that have more than 12 counts
  getnames = names(greaterthan15) #gets the names of these values
  
  estimated = sapply(1:length(column), function(x) GetNamesF(getnames, column[x])) 
  #return the new changes
  return(estimated)
}

NEWDF = sapply(1:ncol(testrun), function(x) CommonChange(testrun[,x])) #6 complete
names(vposts)



#### MODEL
vposts$age = 2016-vposts$year
Ford_Chev = vposts[which(NEWDF[,1]=="ford explorer"|NEWDF[,2]=="chevrolet malibu"),] 
#new data frame without NAs in maker column of volvos and toyotas

removeoutliers = boxplot(Ford_Chev$odometer) #remove odometer outliers
newod = Ford_Chev[Ford_Chev$odometer < removeoutliers$stats[5,],]
removeoutliers = boxplot(newod$price)
newprice = newod[newod$price < removeoutliers$stats[5,],] #remove price outliers
DiffDF = split(newprice, newprice$maker) #split the data on maker (we only have two different makers/models)

#Price vs Odometer
plot(price~odometer, data = DiffDF[[1]], main = "Chev Malibu vs Ford Explorer (Price vs Odometer)")
regress = lm(price~odometer, data = DiffDF[[1]])
abline(regress, col = "red")
points(price~odometer, data = DiffDF[[2]], col = "green")
regress = lm(price~odometer, data = DiffDF[[2]])
abline(regress, col = "blue")
legend("topright", legend = c("ChevM Points", "ChevM Linear", "FordE Points", "FordE Linear"), 
       pch = c(1,32,1,32),
       lwd=c(NA,2.5,NA,2.5),
       col = c("black","red", "green", "blue"))

#Price vs Age
plot(price~age, data = DiffDF[[1]], main = "ChevM vs FordE (Price vs Age)")
points(price~age, data = DiffDF[[2]], col = "green")

model = lm(price~poly(age,2), data = DiffDF[[1]])
age = DiffDF[[1]]$age
predicted.intervals <- predict(model,data.frame(x=age),interval='confidence',
                               level=0.99)

points(age,predicted.intervals[,1],col='red',pch = 4, lwd=3) #plotting poly regression for Chev

model = lm(price~poly(age,2), data = DiffDF[[2]])
age = DiffDF[[2]]$age
predicted.intervals <- predict(model,data.frame(x=age),interval='confidence',
                               level=0.99)

points(age,predicted.intervals[,1],col='blue',pch = 4, lwd=3) #plotting poly reg for Ford
legend("topright", legend = c("ChevM", "ChevM Poly2", "FordE Points", "FordE Poly2"), 
       pch = c(1,4,1,4),
       col = c("black","red", "green", "blue"))
library(rpart)
install.packages("rpart.plot") #Use regression tree (rpart)
library(rpart.plot)

#Price vs Condition
tree = rpart(price~condition, data = DiffDF[[1]])
rpart.plot(tree, main = "Rpart of Price vs Condition for Chev Malibu")
tree = rpart(price~condition, data = DiffDF[[2]])
rpart.plot(tree, main = "Rpart of Price vs Condition for Ford Explorer")

#Price vs City
tree = rpart(price~city, data = DiffDF[[1]])
rpart.plot(tree, main = "ChevM Price vs City Rpart")
ok = data.frame(predict(tree), DiffDF[[1]]$price)
tree = rpart(price~city, data = DiffDF[[2]])
rpart.plot(tree, main = "FordE Price vs City Rpart")