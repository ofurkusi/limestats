combineQuestions <- function(datasets, questions, colnames, as.question=FALSE) {
  # Validate the data submitted by the user
  questionCount <- c(length(datasets), length(questions), length(colnames))
  if (length(unique(questionCount)) != 1) {
    stop("The number of datasets, questions and column names are not equal")
  }
  
  # TODO: Compare questiontexts, e.g.
  #questionTexts <- c(
  #  attributes(datasets[,1])$variable.labels[which(colnames(data)==newCol)]
  #)
  # Use as question text the question text of the first variable
  questionText <- attributes(datasets[[1]])$variable.labels[which(colnames(datasets[[1]])==questions[1])]
  
  # Isolate the question text and labels
  m <- regexpr("\\[(.*?)\\]", questionText, perl=TRUE)
  questionLabel <- regmatches(questionText, m)
  questionLabel <- str_sub(questionLabel, 2, -2)
  
  m <- regexpr("][\\s\\S]*$", questionText, perl=TRUE)
  questionText <<- regmatches(questionText, m)
  questionText <<- str_sub(questionText, 3)
  
  # Find maximum number of rows
  # i.e. the number of rows of the "longest" dataset
  nRows <- 0
  for (i in seq(from=1, to=length(datasets))) {
    nRows <- max(nRows, length(datasets[[i]][[questions[i]]]))
  }

  # Combine the data into a data frame
  combinedData <- data.frame(rep(NA, nRows))
  for (i in seq(from=1, to=length(datasets))) {
    combinedData[i] <- as.factor(c(datasets[[i]][[questions[i]]], rep(NA, nRows-length(datasets[[i]][[questions[i]]]) ) ))
    #combinedData[i] <- factor(combinedData[i], levels=levels(datasets[[i]][[questions[i]]]))
    levels(combinedData[[i]]) <- levels(datasets[[i]][[questions[i]]])
  }
  
  # Generate column names, i.e. question ID's as "Q_1 ... Q_n"
  colnames(combinedData) <- paste("Q", seq(from=1, to=length(datasets)), sep="_")
  
  attributes(combinedData)$variable.labels <- paste("[", colnames, "] ", questionLabel, sep="")
  
  if (as.question==TRUE) {
    return(Question$new(data=combinedData, question="Q", type="questionset"))
  } else {
    return(combinedData)    
  }

}
