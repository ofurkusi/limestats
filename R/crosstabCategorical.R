crosstabCategorical <- function(varx, vary, digits = 1, latex=TRUE, useNA="ifany", caption="", label="", chisq=TRUE, percentages=TRUE,...) {
  txtAll     <- "Alls"
  txtSum     <- "Samtals"
  txtNA      <- "Ekkert valið"
  
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
  finalTable <- rbind(finalTable, Samtals=sum.row)
  
  # Name the NA columns and rows something else than "NA"
  rownames(finalTable)[is.na(rownames(finalTable))] <- txtNA
  colnames(finalTable)[is.na(colnames(finalTable))] <- txtNA
  
  if (colnames(finalTable)[ncol(finalTable)] == "") {
    colnames(finalTable)[ncol(finalTable)] <- txtAll
  }
  
  if (latex) {
    tableDigits <- c(0, rep(digits, ncol(finalTable))) # define number of digits for each column
    finalTable <- finalTable*100 # values are in percentages, multiply by 100
    
    headerValues <- c("\\textrm{}", rep("$\\mathrm{\\%}$", ncol(finalTable) ))
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
    align(latexTable) <- c("X", rep("r", times=ncol(finalTable)))
    print(latexTable, floating = TRUE, type = "latex",
          table.placement="H", booktabs = TRUE, hline.after=NULL,
          add.to.row=list(pos=list(-1, 0, 0, nrow(finalTable)-1,  nrow(finalTable), nrow(finalTable)),
                          command=c(topRule, headerValues, midRule, midRule, bottomRule, chiSqRow)),
          tabular.environment="tabularx", scalebox=0.9, zero.print = ".", rotate.colnames=FALSE,
          width="\\textwidth", sanitize.text.function = function(x){x}) #hline.after = hlines, 
  }
  
  return(finalTable)
}
