
# Define question class
Question <- setRefClass("Question",
    fields = list(
      data = "data.frame",
      question = "character",
      columnnames = "character",
      columnids = "integer",
      type = "character",
      questionText = "character"
    ),
    methods = list(
      getQuestionResponses = function(filter=NULL) {
        # Use regex to filter all columns belonging to this question
        # e.g. if question is "q1" this will return columns "q1_SQ001", "q1_SQ002" etc.
        question_column_ids <- getQuestionColumns()
        
        if (is.null(filter)) {
          thisQuestion <- subset(data, select=question_column_ids)
        } else {
          thisQuestion <- data.frame(data[ data[[filter$question]] %in%  filter$values , question_column_ids ])
          colnames(thisQuestion) <- colnames(subset(data, select=question_column_ids))
          #thisQuestion <- subset(data, select=question_column_ids, [[filter$question]]="ekki")
        }
        
        # Find the question labels
        questionLabels <- attributes(data)$variable.labels[question_column_ids]
        
        # If this is a group of questions, then isolate the subquestion text from the
        # main question text
        if (length(question_column_ids) > 1) {
          m <- regexpr("\\[(.*?)\\]", questionLabels, attr)
          questionLabels <- regmatches(questionLabels, m)
          questionLabels <- str_sub(questionLabels, 2, -2)
        }
        
        # Replace columns names with the actual question asked
        colnames(thisQuestion) <- questionLabels
        
        return(thisQuestion) # returns a dataframe object
      },
      getQuestionColumns = function() {
        # Use regex to filter all columns belonging to this question
        # e.g. if question is "q1" this will return columns number of columns "q1_SQ001", "q1_SQ002" etc.
        # sometimes, categorical values have comments - I haven't figured out how to treat those so drop them!
        regex <- paste("^", question, "(_[a-zA-Z0-9]{1,}){0,}$", sep="")
        columnids   <<- grep(regex, colnames(data))
        
        if (type=="questionset") {
          commentregex <- paste("^", question, "(_[a-zA-Z0-9]{1,}){0,}(comment)$", sep="")
          commentids   <- grep(commentregex, colnames(data))
          columnids <<- setdiff(columnids, commentids)
        }
        
        columnnames <<- colnames(subset(data, select=columnids))
        
        
        return(columnids)
      },
      getQuestionText = function() {
        getQuestionColumns() # make sure 'columnnames' is populated
        questionText <<- attributes(data)$variable.labels[match(columnnames,names(data))][1]
        
        # Ef spurningin er með "undirspurningum" þarf að fjarlægja undirspurninguna framanaf
        if (str_sub(questionText, 1,1) == "[") {
          m <- regexpr("][\\s\\S]*$", questionText, perl=TRUE)
          questionText <<- regmatches(questionText, m)
          questionText <<- str_sub(questionText, 3)
        }
        
        return(questionText)
      },
      getQuestionLevels = function() {
        for(i in getQuestionColumns()) {
          if (exists("myLevels")) {
            if (all(myLevels != levels(data[,i]))) {
              stop("Question levels do not match")
            }
          }
          myLevels <- levels(data[,i])
        }
        return(myLevels)
      },
      
      # filter: only show responses where...
      # partition: make separate descriptions for every X of a variable
      describe = function(filter=NULL, partition=NULL,...) {
        #rs <- switch(type,
        switch(type,
               categorical = describeCategorical(getQuestionResponses(filter), ...),
               questionset = describeQuestionSet(getQuestionResponses(filter), ...),
               freetext    = describeFreeText(getQuestionResponses(filter), ...),
               nominal     = describeNominal(getQuestionResponses(filter), ...)
        )
        #if (is.null(rs)) {
        #  stop(paste("Cannot describe question. There is no question type '", type, "'", sep=""))
        #}
        #return(rs)
      },
      print.latex = function() {
        print("hello")
      }
    )
)
