crosstabCategorical <- function(varx, vary, digits = 1, latex=TRUE, useNA="ifany",
                                caption="", label="", chisq=TRUE, percentages=TRUE,
                                ignore.levels.x=NULL, ignore.levels.y=NULL,
                                transpose=FALSE, filter=NULL, ...) {
  
  txtAll     <- gettext("All", domain="R-limestats") #"Alls"
  txtSum     <- gettext("Total", domain="R-limestats") #"Samtals"
  txtNA      <- gettext("No answer", domain="R-limestats") #"Ekkert valið"
  
  if (inherits(varx,"Question")) {
    varx <- varx$getQuestionResponses(filter=filter)
  }
  if (inherits(vary,"Question")) {
    vary <- vary$getQuestionResponses(filter=filter)
  }
  if (is.data.frame(varx)) {
    varx <- varx[,1]
  }
  if (is.data.frame(vary)) {
    vary <- vary[,1]
  }
  
  # If the user specifies a vector of levels to ignore, then remove them from
  # the data
  if (!is.null(ignore.levels.x)) {
    varx <- remove_levels(varx, ignore.levels.x)
  }
  if (!is.null(ignore.levels.y)) {
    vary <- remove_levels(vary, ignore.levels.y)
  }  
  
  frequencies  <- table(varx, vary, useNA=useNA)
  #chiSqTable <- prop.table(frequencies)
  
  # Perform the ChiSquared test if requested
  if (chisq) {
    chiSqResults <- chisq.test(frequencies)
  }

  # Add a "total" column
  frequencies  <- cbind(frequencies, table(varx, useNA=useNA, dnn=c(txtSum)))
  
  # Check if the table should display percentages or counts
  if (percentages) {
    finalTable <- prop.table(frequencies,2) # get proportions where each column adds up to 100%
  } else {
    finalTable <- frequencies
  }
  
  #proportions <- frequencies
  #sum.row <- margin.table(proportions, 2)
  #finalTable <- rbind(proportions, sum.row)
  sum.row    <- margin.table(finalTable, 2)
  finalTable <- rbind(finalTable, Total=sum.row)
  
  # Name the NA columns and rows something else than "NA"
  rownames(finalTable)[is.na(rownames(finalTable))] <- txtNA
  colnames(finalTable)[is.na(colnames(finalTable))] <- txtNA
  
  if (colnames(finalTable)[ncol(finalTable)] == "") {
    colnames(finalTable)[ncol(finalTable)] <- txtAll
  }
  
  # Use translated text for the name of the "sum" row
  rownames(finalTable)[nrow(finalTable)] <- txtSum
  
  if (transpose==TRUE) {
    finalTable <- t(finalTable)
  }
  
  if (latex) {
    # define number of digits for each column
    tableDigits <- c(0, rep(digits, ncol(finalTable)))
    
    # if values are in percentages, multiply by 100
    if (percentages==TRUE) {
      finalTable <- finalTable*100
    }
    
    headerValues <- c("\\textrm{}",
                      rep("\\parbox[b]{0.4in}{\\centering$\\mathrm{\\%}$}", ncol(finalTable) ))
    headerValues <- paste(gsub(", "," & ",toString(headerValues)), "\\\\ \n")
    topRule <- "\\toprule \n"
    midRule <- "\\midrule \n"
    bottomRule <- "\\bottomrule \n"
    
    #chiSqResults <- chisq.test(frequencies.1)
    #print (chiSqResults)
    if (chisq) {
      chiSqRow <- paste("$\\chi$-squared = " , round(chiSqResults$statistic, 4),
                        ", df = " , round(chiSqResults$parameter,0),
                        ", p-value = ", round(chiSqResults$p.value,4), sep="")
      chiSqRow <- paste("\\\\\n \\multicolumn{", ncol(finalTable)+1,"}{r}{\\textit{", chiSqRow, "}}\\\\", sep="")
    } else {
      chiSqRow <- paste("")
    }
    
    latexTable <- xtable(finalTable, caption=caption, label=label, digits=tableDigits)
    align(latexTable) <- c("l", rep("r", times=ncol(finalTable)))
    names(latexTable) <- paste("\\parbox[b]{0.4in}{\\vspace{5pt}\\centering ", names(latexTable),"}", sep="")    
    print(latexTable, floating = TRUE, type = "latex",
          table.placement="H", booktabs = TRUE, hline.after=NULL,
          add.to.row=list(pos=list(-1, 0, 0, nrow(finalTable)-1,  nrow(finalTable), nrow(finalTable)),
                          command=c(topRule, headerValues, midRule, midRule, bottomRule, chiSqRow)),
          tabular.environment="tabular", scalebox=0.9, zero.print = ".", rotate.colnames=FALSE,
          #width="\\textwidth",
          sanitize.text.function = function(x){x}) #hline.after = hlines, 
  } else {
    return(finalTable)
  }
}
