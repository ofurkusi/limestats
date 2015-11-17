crosstabQuestionSet <- function(question, by, latex=FALSE, digits=1, caption="", label="", ...) {
  if (!match(by,colnames(question$data))) {
    stop(paste("The crosstab question '", by, "' was not found", by=""))
  }
  
  # find the crosstab levels
  crosstabLevels <- levels(question$data[,eval(by)])
  
  if (is.null(crosstabLevels)) {
    stop("The crosstab question did not contain any factor levels.")
  }
  
  total <- as.data.frame(question$describe(latex=FALSE)[,1])
  #print(total)
  
  for (i in crosstabLevels) {
    tbl <- as.data.frame(question$describe(filter=list(question=by, values=i), latex=FALSE)[,1])
    total <- cbind(total, tbl)#
    
  }
  
  # set correct column names
  colnames(total) <- c("All", crosstabLevels)
  
  #rearrange the columns, put the total as the last column
  finalTable <- cbind(total[seq(from=2, to=length(crosstabLevels)+1)], total[1])
  
  if (latex) {
    tableDigits <- c(0, rep(digits, ncol(finalTable))) # define number of digits for each column
    #finalTable <- finalTable*100 # values are in percentages, multiply by 100
    
    headerValues <- c("\\textrm{}", rep("$\\mathrm{\\%}$", ncol(finalTable) ))
    headerValues <- paste(gsub(", "," & ",toString(headerValues)), "\\\\ \n")
    topRule <- "\\toprule \n"
    midRule <- "\\midrule \n"
    bottomRule <- "\\bottomrule \n"
    
    latexTable <- xtable(finalTable, caption=caption, label=label, digits=tableDigits)
    align(latexTable) <- c("l", rep("r", times=ncol(finalTable)))
    print(latexTable, floating = TRUE, type = "latex",
          table.placement="H", booktabs = TRUE, hline.after=NULL,
          add.to.row=list(pos=list(-1, 0, 0, nrow(finalTable)),
                          command=c(topRule, headerValues, midRule, bottomRule)),
          #tabular.environment="tabularx", width="\\textwidth",
          scalebox=0.9, zero.print = ".", rotate.colnames=FALSE,
          sanitize.text.function = function(x){x}) #hline.after = hlines, 
  } else {
    return(total)
  }
}
