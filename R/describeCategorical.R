describeCategorical <- function(data, digits = 1, latex=TRUE, caption="", label="") {

  if (is.data.frame(data)) {
    data <- data[,1]
  }
  
  # Check if this variable is a factor
  if (!is.factor(data)) {
    stop("Categorical variables must be defined as factors")
  }
  
  # Text labels
  txtValid   <- "Gild svör"
  txtInvalid <- "Ógild svör"
  txtAll     <- "Alls"
  txtSum     <- "Samtals"
  txtNA      <- "Ekkert valið"

  showNA     <- TRUE
  emptyCell  <- NA

  # Get the number of different valid options
  levelCount <- length(levels(data))

  propTable <- as.vector(prop.table(table(data))*100)

  #Check if there are any #NA's
  if (!any(is.na(data))) {
    txtInvalid <- NULL
    txtNA <- NULL
    showNA <- FALSE
    emptyCell  <- NULL
  }
  
  stats <- data.frame(
    Type = c(txtValid, rep("", times=levelCount-1), txtInvalid),
    Labels = c(levels(data), txtNA),
    Frequency = as.vector(table(data, useNA="ifany")),
    PercentTotal = as.vector(as.vector(prop.table(table(data, useNA="ifany")))*100)
    #Percent = as.vector(format(prop.table(table(data))*100, digits=1, decimal.mark="."))
  )
  
  #return(stats)
  
  #Get the sum of all frequencies and percents, valid or invalid
  totalRowFinal <- data.frame(t(c(txtAll, txtSum, sum(stats$Frequency), sum(stats$PercentTotal), NA, NA)))
  
  #Get data used to make the total row
  sumData <- colSums(stats[1:levelCount,3:4])

  #Add valid percent
  stats <- within(stats, PercentValid <- c(propTable, emptyCell))
  
  #Add CumulativePercent
  stats <- within(stats, CumulativePercent <- c(cumsum(propTable), emptyCell))
  
  #Add total row
  totalRow <- data.frame(t(c("", txtSum, sumData, sum(propTable), NA)))
  colnames(totalRow) <- colnames(stats)
  stats <- rbind(stats, totalRow)
  #stats <- data.frame(prop.table(table(data[spurning])), as.vector(table(data[spurning])))
  
  #return(stats)
  #Rearrange the rows
  if (showNA) {
    stats <- rbind(stats[1:levelCount,], stats[nrow(stats),], stats[nrow(stats)-1,])
  } else {
    #stats <- rbind(stats[1:levelCount,], stats[nrow(stats),], stats[nrow(stats)-1,])
  }
  
  #Add final total row
  colnames(totalRowFinal) <- colnames(stats)
  stats <- rbind(stats, totalRowFinal)
  
  stats$Frequency <- as.numeric(stats$Frequency)
  stats$PercentTotal <- as.numeric(stats$PercentTotal)
  stats$PercentValid <- as.numeric(stats$PercentValid)
  stats$CumulativePercent <- as.numeric(stats$CumulativePercent)
  
  if (latex==TRUE) {
    colNames <- colnames(stats)
    colnames(stats) <- c("", "\\textrm{Valkostur}", "$\\mathrm{n}$", "$\\mathrm{\\%}$", "$\\mathrm{\\%}$", "$\\mathrm{\\sum \\%}$")
  
    midRule  <- "\\midrule \n"
    cmidRule <- "\\cmidrule{2-6} \n"
    
    tableDigits <- c(0, 0, 0,0, digits, digits, digits) # define number of digits for each column
    
    latexTable <- xtable(stats, caption=caption, label=label, digits=tableDigits)
    align(latexTable) <- c("l", "l", "l", "r", "r", "r", "r")
    
    if (showNA) {
      posList <- list(nrow(stats)-3,nrow(stats)-2)      
    } else {
      posList <- list(nrow(stats)-2,nrow(stats)-1)
    }
    
    print(latexTable, booktabs = TRUE, floating = TRUE, type = "latex", table.placement="H",
          add.to.row=list(pos=posList, command=c(cmidRule,midRule)),
          #tabular.environment="tabularx", width="\\textwidth",
          #rotate.colnames=TRUE,  zero.print = ".",
          scalebox=0.9, include.rownames=FALSE,
          sanitize.text.function = function(x){x}) #hline.after = hlines, 
  
    colnames(stats) <- colNames
  } else {
    return(stats)
  }
  
}
