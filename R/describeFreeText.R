# Open question, text field
# Dump the answers
# Depends on packages: Hmisc
# TODO: add the option to either sort or randomize the answers, by having
# the answers in the same order as other questions, they may become
# traceable
describeFreeText <- function(data, latex=TRUE) {
  if (is.data.frame(data)) {
    data <- data[,1]
  }

  if (!is.character(data)) {
    stop("Free text variables must be defined as character")
  }

  # Remove na's
  data <- data[!is.na(data)]
  
  if (latex) {
    for (i in 1:length(data)) {
      text <- paste("\\begin{quote}", latexTranslate(data[i]), "\\end{quote}\n", sep="")
      cat(text)
    }
    #return (TRUE)
  } else {
    return (data)
  }
  
}