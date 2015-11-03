getQuestionList <- function(data, chapters){
  # Make a data frame to hold the results
  questionStats <- data.frame(Chapter=numeric(0), Question=numeric(0), Subquestion= numeric(0))
  
  # Analyze and structure the questions
  for (column in 1:length(data)) {
    questionData <- unlist(as.vector(strsplit(names(data[column]), split="_")))
    
    questionChapter <- questionData[1]
    questionNumber  <- as.numeric(questionData[2])
    subquestionNumber  <- questionData[3]
    
    # Check if this question belongs to any chapter
    if (is.element(questionChapter, names(chapters))) {
      questionStats[nrow(questionStats)+1,] <- c(questionChapter, questionNumber, subquestionNumber)
    }
  }
  
  questionStats$Question <- as.numeric(questionStats$Question)
  
  # Return the list, properly ordered
  return(questionStats[with(questionStats, order(Chapter, Question)), ])
}
