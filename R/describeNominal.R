describeNominal <- function(data, digits = 1, latex=TRUE, caption="", label="", questiondescriptions=NULL) {

  # Check if this is a group of nominal questions or just a single one
  if (typeof(data) != "list") {
    data <- data.frame(q1=data)
  }
  
  # make a list of non-numeric columns in the submitted data
  columnTypesNumeric <- sapply(data, is.numeric)

  if (any(columnTypesNumeric == FALSE)) {
    stop(paste("Data must be numeric in order to be described as nominal. Invalid columns: ",
               paste(names(columnTypesNumeric[columnTypesNumeric==FALSE]), collapse= ", ")))
  }

  
  stats <- t(sapply(data, FUN=function(x) {
    c(Valid  = sum( !is.na( x ) ),
      Min    = min(x, na.rm=TRUE),
      Median = median(x, na.rm=TRUE),
      Mean   = mean(x, na.rm=TRUE),
      Max    = max(x, na.rm=TRUE),
      Stdev  = sd(x, na.rm=TRUE),
      NAs    = sum( is.na( x ) ) )
  }))

  if (!is.null(questiondescriptions)) {
    rownames(stats) <- questiondescriptions
  }
  

  # Output LaTeX table
  if (latex==TRUE) {
    colNames <- colnames(stats)

    # Set question labels
    colnames(stats) <- c("$n$", "Min", "$\\tilde{x}$", "$\\bar{x}$", "Max", "$s$", "\\#na")
    
    latexTable <- xtable(stats, caption = caption, label = label, digits=c(0,0,0,0,digits,0,digits,0))
    #hlines <- c(-1,0,1,nrow(table))
    align(latexTable) <- c("X", rep("r", times=ncol(stats)))
    print(latexTable, booktabs = TRUE, floating = TRUE, type = "latex", table.placement="H",
          rotate.colnames=FALSE, tabular.environment="tabularx", scalebox=0.9,
          width="\\textwidth", sanitize.text.function = function(x){x}) #hline.after = hlines, 

    colnames(stats) <- colNames
  } else {
    return(stats)
  }

}
