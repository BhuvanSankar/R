# Code from the practical in wk 4.
# For more information about any function in the script,
# type '?functionName' to get the documentation.




testVector = c(0,1,0,0,1,0) #vector to calculate entropy for

#' (This is how function comments are usually written)
#' Calculate entropy of a vector
entropyCalculator = function( inputVector ){

  uniqueValues = unique( inputVector ) #Get all different values in vector
  entropy = 0
  
  for( currentValue in uniqueValues ) {
    currentProportion = mean( inputVector == currentValue ) #Poportion equal to current value
    entropyComponent = - currentProportion * log2(currentProportion)
    entropy = entropy + entropyComponent
  }

  return( entropy )
}

entropyCalculator( testVector ) #Print the entropy of this test vector



# Part 2

#Set where we want ot work for the session. All filenames are now relative to this.
# (usually I put this at the top of the file)
setwd("~/../Desktop/INFS Tutes")

surveyFull = read.csv("survey.csv")

#Create new survey data frame with only subset of full survey.
survey = data.frame(
  Sex = surveyFull$Sex,
  LeftHanded = surveyFull$Handed == "Left",
  Residence = surveyFull$Residence,
  
  Tall = as.numeric( surveyFull$Height ) > mean(as.numeric( surveyFull$Height ),
                                                na.rm = TRUE), # Aggregate functions 'fall over' with NA's,
                                                               # unless you explicitly remove/ignore them
  
  Kiss = surveyFull$Kiss == "Yes" #We are going to use this column as our output
  
)

survey = survey[ complete.cases(survey) , ] #only use rows which have no missing values (NA's)


entropyCalculator(survey$Kiss) #eg, how much entropy is in our current table output


#' How much entropy we have if we split the survey according to the answers in a column.
#' Note that this function uses 'survey', which it reads in from the outer definition.
#' (You might prefer to have survey as an additional function parameter).
#' It also assumes "Kiss" is our desied column.
entropySplit = function(columnNum) {
  
  values = unique( survey[, columnNum] )
  
  #vapply maps elements of parameter 1, according to the specified function.
  #We specify the expected return type to be a "numeric" (/double/real) vector of length 1.
  entropyComponents = vapply(values, function(currValue){
    
    AnsIndicies = survey[, columnNum] == currValue
    AnsTable = survey[AnsIndicies , ]
    
    entropyCalculator( AnsTable$Kiss ) * nrow(AnsTable)/nrow(survey)
  }, numeric(1))
  
  sum(entropyComponents) # This is the short form of a return statement
                         # - whatever is evaluated last can 'become' the return value
}


for(colIndex in 1:(ncol(survey)-1) ) {
  cat( sprintf("Entropy of split %d - %s : %f\n", colIndex,
               colnames(survey)[colIndex],
               entropySplit(colIndex) )
  )
}


# Not done in class:
questionIndicies = 1:(ncol(survey)-1)

qEntropies = sapply( questionIndicies, entropySplit ) #Apply entropy calculator to elements of questionIndicies.
#( sapply doesn't ask for return type - see help on ?apply/sapply/vapply )

names(qEntropies) = colnames(survey)[questionIndicies] #Name the elements in the vector

qEntropies

which.min(qEntropies) #Gets the index of the minimum value
sprintf("Best split question is %s", colnames(survey)[which.min(qEntropies)])