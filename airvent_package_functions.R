if (require("tidyverse") == FALSE) install.packages(tidyverse)
if (require("lme4") == FALSE) install.packages(lme4)
if (require("data.table") == FALSE) install.packages(data.table)
if (require("lubridate") == FALSE) install.packages(lubridate)
library(tidyverse)
library(lubridate)

Sys.setlocale("LC_ALL","English")

#################################
###         functions         ###
#################################
# expand counts into a sequence of numbers 
expand_class_id <- function(x){
  output <- NULL
  for(i in 1:length(x)){
    output <- append(output, seq_len(x[i]))
  }
  return(output)
}

# Count the number of weekdays between two dates
num_weekdays <- Vectorize(function(a, b) sum(!weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday")))

