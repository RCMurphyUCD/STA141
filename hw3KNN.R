Train = read.csv("/Users/chrismurphy/Downloads/digitsTrain.csv")
distance = dist(Train[,2:ncol(Train)], method = "euclidean") 
disteuc = as.matrix(distance)
distmat=  as.matrix(dist(Train[,2:ncol(Train)], method = "manhattan"))
#so to do this assignment, I first started with the smallest building blocks
#I started with a function to get a vector of the nearest neighbor of a single column of a matrix
getNbors = function(x, n){ #this function takes in a distance matrix (x) and a column index (n) 
  return(as.numeric(names(sort(x[,n])))) #it sorts this column of values and returns a vector of indeces of 
  #the nearest neighbors
}

GetPred = function(data, test_neighbors) #the next function I takes in a neighbors vector (created from
{#getNbors ) and the data set for
  #distance matrix and spits out what the actual prediction is value is
  Neighbor_values = data$label[test_neighbors] #creates a vector of labels for all the nearest neighbors 
  return(as.integer(names(which.max(table(Neighbor_values)))))#returns the most common label as the prediction
  
}
GetColAvg = function(data)
{
  splitlab = split(data, data$label)#split data on label
  ColumnAverage = sapply(1:10, function(x) colMeans(splitlab[[x]]))#get the column average of each data frame
  return(t(ColumnAverage)) #transpose so label/pixel are columns not rows
}

KNN = function(distance, data, folds=5, k, method = "Normal", type = "euclidean") #KNN function which takes in a distance matrix, the data, 
{  #the folds (set default as 5 for 20/80 train test split), and k (the number of neighbors)
  #method is whether or not we're comparing against averages or normal KNN
  #type is only specificied if method is Average
  ID = sample(1:folds, nrow(data), replace = TRUE)#basically I'm doing the sampling inside the function instead of out
  #we create a vector of ID's (1:5, sampled randomly nrow(data) times) (our case 5000)
  #so we've in essence randomized ~1000 1's, ~1000 2's...etc in this vector of 5000 

  
  Compare = data.frame(data[,1]) #these are the labels (Actual values)
  Compare$Predictions = 500 #initializing a new data frame column because I'm going to be changing individual
  #values within in, not in a straightforward order. (ex replacing index 5, 10, 72,400,25, 62... etc)
  #wanted an artificially high number to make it easy for me to check if I got 5000 predicted values
  
  if (method == "Average") #if else case(do the average comparison if method == Average, else do normal KNN)
  {
    for(i in 1:folds)#my idea for cross validation was to pull out all the observations who correspond to ID i and use that 
    {#as the test set and the remaining data as the training
    
      index = i == ID #trues/falses vector where if i == ID, then the values is a TRUE, otherwise a FALSE
      
      NewTrain = data[!index,] #training data (all the rows that aren't index) 
      NewTest = data[index,] #all the rows that are index
      Avg = data.frame(GetColAvg(NewTrain)) #column averages ( this is a data frame of 10 rows, 785 columns)
      #each row is a unique label (1,2,3,4,5,6,7,8,9) and the average pixel 
      num = Avg$label #want the label values (1,2,3,4,5,6,7,8,9)
      Distm = as.matrix(dist(rbind(NewTest[,2:length(NewTest)],Avg[,2:length(Avg)]), method = type)) #distance
      #matrix of test data to averages
      NewDistm = tail(Distm, 10) 
      NewDistm = NewDistm[,1:(ncol(NewDistm)-10)]#only want distance from test to average and not the test to test stuff
      #so I removed all but the last 10 rows and removed the last 10 columns 
      row.names(NewDistm) = num #setting the rownames to be the labels 1,2,3,4,5,6,7,8,9
      Neighbors = sapply(1:ncol(NewDistm), function(x) getNbors(NewDistm,x))[1,]#get the first neighbor (which is the label average it is closest to)
      Compare[index,]$Predictions=Neighbors #set predictions
    }
  }
  else
  {
  for (i in 1:folds)#my idea for cross validation was to pull out all the observations who correspond to ID i and use that 
  {#as the test set and the remaining data as the training
    index = i == ID #trues/falses vector where if i == ID, then the values is a TRUE, otherwise a FALSE
    NewDist = distance[!index,index] #separate distance matrix into training/test (all that are false are the training and all that were true are the testing)
    #this creates a roughly 4000 by 1000 matrix (since I sampled above to get train/test, dimentions are not perfect 1000/4000 but close enough (ie 3963/1037))
    Neighbors = sapply(1:ncol(NewDist), function(x) getNbors(NewDist,x))[1:(k),] #
    if (k == 1)
    {
      Neighbors = t(as.matrix(Neighbors)) #the odd case where it turns neighbors into a vector
    }
    predictions = sapply(1:ncol(Neighbors), function(x) GetPred(data, Neighbors[,x])) #get predictions
    Compare[index,]$Predictions = predictions #set predictions at correct indeces
    
  }
  }
  
  names(Compare) = c("Actual", "Predicted") #rename columns
  return(Compare) #return the data frame of actual and predicted
}



###### # # # # # # # # #  # # # # # # # # # # # # # # # # #


listy = lapply(1:10, function(x) KNN(disteuc, Train, folds = 5, k = x)) #actual/predicted values for euclidean
matty = lapply(1:10, function(x) KNN(distmat, Train, folds = 5, k = x)) #actual/predicted values for mat
for(i in 1:10)#this for loop runs through all the k for euc and mat dist, choose the best k and distance
{ 
  togetha = paste("For Euc K = ", i )
  print(togetha)
  print(table(listy[[i]]$Predicted == listy[[i]]$Actual))
  togetha = paste("For Mat K = ", i )
  print(togetha)
  print(table(matty[[i]]$Predicted == matty[[i]]$Actual))
}
#which one is the best performed the best? whichever k did best, put that in where Y/Z are for mat/euc to make confusion matrix
#interpret this for part 1
#go through misclassified values and do the image thing duncan gave


table(listy[[Y]][,1],listy[[Y]][,2])
table(matty[[Z]][,1], matty[[Z]][,2])

getImage =
  function(vals)
  {
    matrix(as.integer(vals), 28, 28, byrow = TRUE)
  }

draw = function(vals, colors = rgb((255:0)/255, (255:0)/255, (255:0)/255), ...)
{
  if(!is.matrix(vals))
    vals = getImage(vals)
  
  m = t(vals)  # transpose the image
  m = m[,nrow(m):1]  # turn up-side-down
  
  image(m, col = colors, ..., xaxt = "n", yaxt = "n")
}
#once you choose best k and distance,  go through some of the values where the Actual did not equal the predicted
#once you find some, instead of "REPLACE" put that row number and run this
Images = getImage(Train[REPLACE,2:length(Train)])
draw(Images)
#run this for part 2

part2 = KNN(distuc, Train, folds = 5, k = 1, method = "Average")
table(part2[,1]==part2[,2])
#how did this compare?


