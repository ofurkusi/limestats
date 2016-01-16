#' Remove levels
#'
#' Removes specified levels from factors, replacing the values with NA and
#' removing the level from list of levels. This function is useful for ignoring
#' certain categories in categorical variables.
#' 
#' @param data A factor or a data frame containing one or more columns of factor
#'             variables
#' @param remove.levels A vector containing the levels that should be removed
remove_levels <- function(data, remove.levels) {
  
  if (length(remove.levels) < 1) {
    stop("No levels were specified for removal")
  }
  
  if (is.factor(data)) {
    data[data %in% remove.levels] <- NA       
    data <- factor(data,
                   levels=setdiff(levels(data), remove.levels))
  } else if (is.data.frame(data)) {
    
    for (col in 1:ncol(data)) { 
      if (!is.factor(data[,col])) {
        msg <- gettext("Column %s is not a factor", domain="R-limestats")
        warning(sprintf(msg, col))
      }
      
      if (length(data[data[,col] %in% remove.levels,col]) > 0) {
        data[data[,col] %in% remove.levels,col] <- NA       
      }
      # These mess stuff up as they also remove levels that I want to include
      # but may be unused for a particular question
      #data[,col] <- factor(data[,col])
      #data[,col] <- droplevels(data[,col])
      # using instead:
      data[,col] <- factor(data[,col],
                           levels=setdiff(levels(data[,col]), remove.levels))
    }    
  } else {
    msg <- gettext("Expected a data.frame or a factor but received a %s",
                   domain="R-limestats")
    stop(sprintf(msg, class(data)))
  }

  return(data)
}