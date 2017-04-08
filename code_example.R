# L. Gjeltema
# R Ladies
# 2017-04-08
# https://github.com/math-and-data/rtp_20170408_data_hackathon/

#---- libraries ----
if (!require("tidyverse")) {
  install.packages("tidyverse"); library("tidyverse")
}
if (!require("readxl")) {
  install.packages("readxl"); library("readxl")
}
#---- read in a file ----
# great commands for reading files: .csv files or other character-delimited files
#  base::read.csv() [no need to use this anymore, use readr package instead]
#  readr::read_csv() [optimized reading of files]
#  readr::read_delim() [optimized reading of files, field separator can be defined here]
# however, we have an .xls file, so we'll use the 'readxl' package to handle it.
?read_excel
list.files("data")
file_to_read <- "data/Table_13_Hate_Crime_Incidents_per_Bias_Motivation_and_Quarter_by_State_and_Agency_2015.xls" 

# we get the 2 lines of headers and collapse them to one
t13_2015.header1 <- file_to_read %>% read_excel(skip=4) # %>% colnames()
t13_2015.header2 <- file_to_read %>% read_excel(skip=5) # %>% colnames()
names(t13_2015.header1)
names(t13_2015.header2)
# 'apply' version:
tworow_headers <- data.frame(names(t13_2015.header1), names(t13_2015.header2)) %>% 
  apply(., 1, function(x) {paste(x,collapse=" ")})
# 'purrr' version:
tworow_headers <- map2_chr(names(t13_2015.header1),names(t13_2015.header2),paste, collapse="")
rm(t13_2015.header1, t13_2015.header2) # clean up

# now read in the proper data and assign the newly created header
t13_2015 <- file_to_read %>% read_excel(skip=6, col_names=FALSE, na="")
colnames(t13_2015) <- tworow_headers # assign the new header
# check rows that had warnings when reading them in:
t13_2015[1179, 12]
t13_2015[1985, 5]

head(t13_2015)
View(t13_2015)

#---- minor cleanup of the data -----


#---- we have clean data now, let's visualize it ----
