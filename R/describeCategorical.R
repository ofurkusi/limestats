describeCategorical <- function(data, digits = 1, latex=TRUE, caption="", label="",
                                cumulative=TRUE, validonly=FALSE, showNA=TRUE) {

  if (is.data.frame(data)) {
    data <- data[,1]
  }
  
  # Check if this variable is a factor
  if (!is.factor(data)) {
    stop("Categorical variables must be defined as factors")
  }

  # Text labels
  txtValid   <- gettext("Valid answers",   domain="R-limestats")
  txtInvalid <- gettext("Invalid answers", domain="R-limestats")
  txtAll     <- gettext("All answers",     domain="R-limestats")
  txtSum     <- gettext("Total",           domain="R-limestats")
  txtNA      <- gettext("No answer",       domain="R-limestats")
  txtOption  <- gettext("Option",          domain="R-limestats")

  emptyCell  <- NA

  # Get the number of different valid options
  levelCount <- length(levels(data))

  propTable <- as.vector(prop.table(table(data))*100)

  #Check if there are any #NA's
#  if (!any(is.na(data))) {
#    txtInvalid <- NULL
#    txtNA <- NULL
#    showNA <- FALSE
#    emptyCell  <- NULL
#  }
  
  stats <- data.frame(
    Type = c(txtValid, rep("", times=levelCount-1), txtInvalid),
    Labels = c(levels(data), txtNA),
    Frequency = as.vector(table(data, useNA="always")),
    PercentTotal = as.vector(as.vector(prop.table(table(data, useNA="always")))*100)
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
  #} else {
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
    # NOTE: lenght of align and digits is 1+ncol if table is dataframe, however that does not apply to head
    columnParts <- data.frame(
      head = c(NA,
               "",
               paste("\\textrm{", txtOption, "}", sep=""),
               "$\\mathrm{n}$",
               "$\\mathrm{\\%}$",
               "$\\mathrm{\\%}$",
               "$\\mathrm{\\sum \\%}$"),
      align = c("l", "l", "l", "r", "r", "r", "r"),
      digits = c(0, 0, 0,0, digits, digits, digits),
      midrule = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
      seq = c(NA, seq(from=1, to=6))
    )
    rownames(columnParts) <- c("rowname", "Type", "Labels", "Frequency", "PercentTotal", "PercentValid", "CumulativePercent")
  
    # Remove rows that are not to be displayed
    rowsToShow    <- c(seq(from=1, to=levelCount), (levelCount+1)*showNA, levelCount+2, (levelCount+3)*showNA)
    stats <- stats[rowsToShow,]
    
    # Remove column parts that are not being used
    columnsToShow <- c(TRUE, showNA, TRUE, TRUE, showNA, TRUE, cumulative)
    columnParts <- columnParts[columnsToShow, ]
    #columnParts <- columnParts[setdiff(rownames(columnParts),columnsToHide),]
    
    # Remove columns from the "stats" table that are not to be displayed
    stats <- stats[columnParts$seq[!is.na(columnParts$seq)]]
    
    #colNames <- colnames(stats)
    colnames(stats) <- columnParts$head[!is.na(columnParts$head)]
    
    midRule  <- "\\midrule \n"
    if(showNA) {
      cmidRule <- sprintf("\\cmidrule{%d-%d} \n", table(columnParts$midrule)[1], table(columnParts$midrule)[1]+table(columnParts$midrule)[2]-1)
    } else {
      cmidRule <- NULL
    }
    
    latexTable <- xtable(stats, digits=columnParts$digits, caption=caption, label=label)
    align(latexTable) <- as.character(columnParts$align)

    if (showNA) {
      posList <- list(nrow(stats)-3,nrow(stats)-2)
    } else {
      posList <- list(nrow(stats)-1)
    }
    
    
    print(latexTable, booktabs = TRUE, floating = TRUE, type = "latex", table.placement="H",
          add.to.row=list(pos=posList, command=c(cmidRule,midRule)),
          #tabular.environment="tabularx", width="\\textwidth",
          #rotate.colnames=TRUE,  zero.print = ".",
          scalebox=0.9, include.rownames=FALSE,
          sanitize.text.function = function(x){x}) #hline.after = hlines, 
  
    #colnames(stats) <- colNames
  } else {
    return(stats)
  }
  
}
