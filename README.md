# limestats #

limestats is an [R](http://www.r-project.org) package with functions for automating analysis of survey data from LimeSurvey.


## Installing ##

limestats is not available on [CRAN] (https://github.com/ofurkusi/limestats). In order to use it you must install from this GitHub repository. The most simple method is by using `devtools`. First install and load `devtools`:

````{r}
install.packages("devtools")
library(devtools)
````
Once `devtools` has been loaded, `limestats` can be installed and loaded with:

````{r}
install_github("ofurkusi/limestats")
library(limestats)
````

## Using limestats ##

Begin by downloading your LimeSurvey data in [R format](https://manual.limesurvey.org/Exporting_results#R_Export) and load it, adjusting the filename as needed:
````{r}
source("survey_00000_R_syntax_file.R")
````
This should create a dataframe called `data` which contains all the responses and question metadata from the survey.

Next individually define your survey questions, e.g.

````{r}
myNominalQuestion <- Question$new(data=data, question="q_0001", type="nominal")
myCategoricalQuestion <- Question$new(data=data, question="q_0002", type="categorical")
myFreeTextQuestion <- Question$new(data=data, question="q_0003", type="freetext")
myQuestionSetQuestion <- Question$new(data=data, question="q_0004", type="questionset")
````

Finally generate output, e.g. for including in [Sweave](https://www.statistik.lmu.de/~leisch/Sweave/) or [Knitr](http://yihui.name/knitr/) reports, using

````{r}
myNominalQuestion$describe(latex=TRUE, 
    caption=myNominalQuestion$getQuestionText(),
    label='myNominalQuestion')
````


## License ##

This package is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License, version 3, as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but
without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.  See the GNU
General Public License for more details.

A copy of the GNU General Public License, version 3, is available at
<http://www.r-project.org/Licenses/GPL-3>
