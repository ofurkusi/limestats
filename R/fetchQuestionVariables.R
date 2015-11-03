fetchQuestionVariables <- function(data, question) {
  regex <- paste("^", question, "(_[a-zA-Z0-9]{1,}){0,}$", sep="")
  variables <- grep(regex, colnames(data))
  #variables <- grep("^Q2_1(_[a-zA-Z0-9]{1,}){0,}$", colnames(data))
  
  thisQuestion <- subset(data, select=variables)
  
  # Fetch question labels
  questionLabels <- attributes(data)$variable.labels[variables]
  m <- regexpr("\\[(.*?)\\]", questionLabels, attr)
  questionLabels <- regmatches(questionLabels, m)
  questionLabels <- str_sub(questionLabels, 2, -2)
  
  # Set question labels
  colnames(thisQuestion) <- questionLabels
  
  return(thisQuestion)
}
