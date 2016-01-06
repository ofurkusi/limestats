describeQuestionSet <- function(data, digits = 0, latex=TRUE, caption="", label="", display="percentages") {

  # Check if the levels match for all the variables
  for (i in 1:ncol(data)) {
    if (!is.factor(data[,i])) {
      # This variable is not a factor at all!
      # Handle questions with open text fields ("other")
      #nameOfLastColumn <-colnames(thisQuestion[ncol(thisQuestion)])
      #if (str_sub(nameOfLastColumn, -5) == "other") {
      
      data[i] <- replace(data[i], !is.na(data[i]), 1)
      data[i] <- replace(data[i], is.na(data[i]), 0)
      data[,i] <- factor(data[,i], levels=c(1,0),labels=levels(data[,1]))
    #} else if (all (levels(data[,i])==mylevels)) {
      # ERROR! Mismatch in levels!
    }
  }
  

  # Check what type of value to display
  if (display=="percentages") {
    stats <- sapply(data, FUN=function(x) {
      c(prop.table(table(x))*100, n=sum(!is.na(x)))    
    })
    displaySign <- "\\mathrm{\\%}"
  } else if (display=="counts") {
    stats <- sapply(data, FUN=function(x) {
      c(table(x), n=sum(!is.na(x)))    
    })
    displaySign <- "n"
    
  } else {
    # unsupported display type, e.g. both
    stop("The value you requested cannot be displayed.")
  }
  
  rownames(stats)[nrow(stats)] <- gettext("All answers", domain="R-limestats")

  stats <- t(stats)


  # Output LaTeX table
  if (latex) {
    latexTable <- xtable(stats, caption = caption,
                         label = label, digits=c(0,rep(digits, ncol(stats)-1),0))
    #hlines <- c(-1,0,1,nrow(table))
    headerValues <- c("\\textrm{}",
                      rep(paste0("\\parbox[b]{0.4in}{\\centering$", displaySign, "$}"), ncol(stats)-1 ),
                      "\\parbox[b]{0.4in}{\\centering$n$}")
    headerValues <- paste(gsub(", "," & ",toString(headerValues)), "\\\\")
    align(latexTable) <- c("p{3in}", rep("r", times=ncol(stats)))
    #align(latexTable) <- c("p{3in}", rep(">{\\centering}p{0.4in}", times=ncol(stats)-1),"r")
    names(latexTable) <- paste("\\parbox[b]{0.4in}{\\vspace{5pt}\\centering ", names(latexTable),"}", sep="")
    
    # Use this if you don't want to display "All answers" in the heading
    #names(latexTable) <- c(names(latexTable)[1:length(names(latexTable))-1],"")
    
    print(latexTable, booktabs = TRUE, floating = TRUE, type = "latex", table.placement="H",
          add.to.row=list(pos=list(0), command=c(headerValues)),
          rotate.colnames=FALSE, tabular.environment="tabular", scalebox=0.9,
          width="\\textwidth", sanitize.text.function = function(x){x}) #hline.after = hlines, 
  } else {
    return(stats)
  }
  
  #attributes(data)$variable.labels[which( colnames(data)=="Q6_1_1" )]
  
  #attributes(data)$variable.labels
  
  #summary(thisQuestion)
  #table(thisQuestion$Q6_1_2, useNA="always")
  #tabulate(thisQuestion)
  #attributes(thisQuestion)
  #attributes(data$Q6_1_1)
  #levels(thisQuestion[,5])
  #getQuestionIds(currentQuestion)
  #print("multi")  
  
}
