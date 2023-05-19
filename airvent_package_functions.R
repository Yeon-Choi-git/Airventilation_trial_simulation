library(tidyverse)
library(lme4)
library(data.table)
library(lubridate)

### unction ###
expand_class_id <- function(x){
  output <- NULL
  for(i in 1:length(x)){
    output <- append(output, seq_len(x[i]))
  }
  return(output)
}


Sys.setlocale("LC_ALL","English")
num_weekdays <- Vectorize(function(a, b) sum(!weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday")))

