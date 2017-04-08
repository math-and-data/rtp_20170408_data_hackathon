# L. Gjeltema
# R Ladies
# 2017-04-08
# https://github.com/math-and-data/rtp_20170408_data_hackathon/

#---- libraries ----
if (!require("tidyverse")) {
  install.packages("tidyverse"); library("tidyverse")
}
if (!require("stringr")) {
  install.packages("stringr"); library("stringr")
}
if (!require("readxl")) {
  install.packages("readxl"); library("readxl")
}
if (!require("purrr")) {
  install.packages("purrr"); library("purrr")
}
if (!require("DT")) {
  install.packages("DT"); library("DT")
}
if (!require("ggplot2")) {
  install.packages("ggplot2"); library("ggplot2")
}
#---- read in a file ----
# great commands for reading files: .csv files or other character-delimited files
#  base::read.csv() [no need to use this anymore]
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
tworow_headers <- purrr::map2_chr(names(t13_2015.header1),names(t13_2015.header2),paste, collapse="")

# trim whitespace from the column names
tworow_headers <- tworow_headers %>% str_trim(side="both")
rm(t13_2015.header1, t13_2015.header2) # clean up

# now read in the proper data and assign the newly created header
t13_2015 <- file_to_read %>% read_excel(skip=6, col_names=FALSE, na="")
colnames(t13_2015) <- tworow_headers
# check rows that had warnings when reading them in:
t13_2015[1179, 12]
t13_2015[1985, 5]

head(t13_2015)
tail(t13_2015)
View(t13_2015)
#---- minor cleanup of the data -----
# remove rows with all NAs in them
# version 1:
t13_2015 <- t13_2015 %>% filter(!(rowSums(is.na(.)) == ncol(.)))
## version 2:
#t13_2015 <- t13_2015 %>% filter(rowSums(is.na(.)) < ncol(.))

# remove rows with footnotes
# Assumption: Footnote is only in 1st column ('State'), starts with a number.
# Show the lines that should be removed:
print("We will remove the following lines from the dataset, assuming they are footnotes.")
grep("^[0-9]", t13_2015$State, value=TRUE)
# remove those lines:
t13_2015 <- t13_2015 %>% filter(!grepl("^[0-9]", t13_2015$State))
# A website to check you regex formulas: regexr.com

# set the right data type
t13_2015 <- t13_2015 %>% 
  mutate(
    State         = as.factor(State),
    `Agency Type` = as.factor(`Agency Type`)
  );
summary(t13_2015)

#---- we have clean data now, let's visualize it ----
# reportings by state ----

total_by_state <- t13_2015 %>% filter(`Agency Type`=="Total")
# remove columns that are all NAs
total_by_state <- total_by_state[, colSums(is.na(total_by_state)) < nrow(total_by_state)]

# new column with totals number of incidents that were reported
total_by_state$total_reported <- rowSums(total_by_state %>% select(-State, -`Agency Type`))

total_by_state %>%
  ggplot(aes(x=total_reported, y=State)) + geom_point();

total_by_state %>%
  ggplot(aes(x=State, y=total_reported)) + 
  geom_col() + coord_flip();

total_by_state %>% 
  DT::datatable(rownames=FALSE) %>%
  formatStyle("total_reported",
              background = styleColorBar(total_by_state$total_reported, 'steelblue'),
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition ='center'
  )

# %of total in the different bias categories ----
#  homework
# population by state [Table 12] ----
#  homework
