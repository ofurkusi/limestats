chartify <- function(question, filter=NULL, levels=NULL, wrap.x=NULL,
                     wrap.legend=NULL, legend.title="Category",
                     xlab="Question", ylab="Percentage of respondents",
                     ...) {

  # fetch the question data to be charted and if applicable
  # emove data that is not needed
  chartData <- question$describe(latex=FALSE, filter=filter, ...)
  
  # remove the "n" column, i.e. only leave the proportions
  chartData <- chartData[,seq(from=1,to=ncol(chartData)-1,by=1)]
  
  # melt the data from wide to normal form
  chartData <- melt(chartData, measure=seq(from=1,to=ncol(chartData),by=1))
  
  # set correct column names, typically:
  # question text | question option, e.g. likert scale | prop. of respondents
  colnames(chartData) <- c(xlab, legend.title, "values")
  
  # if custom levels are not defined, fetch the
  # question levels from the data automatically
  if (is.null(levels)) {
    levels <- question$getQuestionLevels()
  }
  
  # after melting, column 1 contains the question categories
  chartData[[1]] <- as.factor(chartData[[1]])
  chartData[[1]] <- factor(chartData[[1]],
                           levels=rev(colnames(question$getQuestionResponses())) )
  
  if (!is.null(wrap.legend)) {
    chartData[[2]] <- str_wrap(chartData[[2]], width=wrap.legend)
    levels <- str_wrap(levels, width=wrap.legend)
  }

  # after melting, column 2 contains the question response levels
  chartData[[2]] <- as.factor(chartData[[2]])
  # Ensure correct factor level ordering
  chartData[[2]] <- factor(chartData[[2]], levels=levels)
  
  chart <- ggplot(chartData, aes_string(x=xlab, y="values", fill=legend.title)) + 
    geom_bar(stat="identity") + coord_flip() + scale_fill_brewer(palette="RdBu") +
    labs(y=ylab)
  
  # note: this requires "str_wrap" from the "stringr" package
  if (!is.null(wrap.x)) {
    chart <- chart + 
             scale_x_discrete(labels = function(x) str_wrap(x, width = wrap.x))
  }
  

  return(chart)
}
