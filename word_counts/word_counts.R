# Purpose of script: This script was developed by the Evans School Policy Analysis & Research Group (EPAR) to report how frequently groups of keywords appear in a group of documents, normalized to the length of each document. The initial use case was looking for discussion of specific areas (e.g. self help groups) by specifying keywords for each area (e.g. self help groups, shgs, women's empowerment collective, wecs).

# Date: 2 May 2019

# Inputs: 
# -- csv file that contains keywords and the "buckets"/groups they fit into, from hereon referenced as "dictionary csv" 
# -- directory of documents to conduct word counts on (doc and docx)

# Outputs: 
# -- csv file with the raw count of words within each bucket and the counts normalized to the number of characters in the document (proxy for length) 

################ USER INPUT ###################
#clear workspace
rm(list = ls())

# read all the documents; define path to text files 
source <- "R:/Project/EPAR/Working Files/374 - SHG Portfolio Review/Grant Analysis by EPAR/Machine Coding Part/word_counts_Thomas_request_20180124/Final_Narratives/"

# set work directory (location of dictionary csv and where result will be output)
setwd("R:/Project/EPAR/Working Files/RA Working Folders/Rohit/Thomas_Ask_0125/")

# set output file name
fname = 'words_counts_example_output'

###############################################

#getting packages installed/loaded
packages <- c("tm", "textreadr", "stringr", "dplyr","zip","openxlsx")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

#load packages
library("tm")
library("textreadr")
library("stringr")
library("dplyr")
library("openxlsx")

# read in dictionary csv
dict_in <- read.csv("dict_input_counts.csv")

# reading input doc files
mydocfiles <- list.files(path = source, pattern = "doc",  full.names = FALSE)

# removing temp docx files in the directory (if any) 
doc_files <- vector(mode = "character", length = 0)
for (i in 1:length(mydocfiles)){
  # name of the file read
  name_file <- mydocfiles[i]
  # does not contain tilde (~)
  if (grepl("~", name_file) == FALSE){
    doc_files <- append(doc_files, name_file)
  }
}

# updating the original file
mydocfiles <- doc_files

# initialize the dataframe to hold all text data read from documents
corpus_df <- data.frame(doc_id = character(),
                        text = character(), 
                        stringsAsFactors=FALSE)

# reading all the docx files
for (i in 1:length(mydocfiles)){
  
  # detecting the extension
  ext <- strsplit(mydocfiles[i], split = '.', fixed = TRUE)[[1]][2]
  
  # reading a single file
  if (ext == 'doc'){
    doc <- textreadr::read_doc(paste0(source, mydocfiles[i]))
  }
  else if (ext == 'docx'){
    doc <- textreadr::read_docx(paste0(source, mydocfiles[i]))
  }
  else {
    print ('Unknown file extension!')
  }
  
  # document name
  doc_name <- mydocfiles[i]
  print (doc_name)
  
  # extract all strings from the list with at least 5 seperate words
  # assumption is that a meaningful sentence will have at least 5 words
  
  # looping through the resultant character list
  # initialize the empty vector to hold all the eligible strings
  sentence_str <- ""
  for (j in 1:length(doc)){
    # which element
    elem <- doc[[j]]
    # splitting the character vector on spaces
    len_element <- length(strsplit(elem, "\\s+")[[1]])
    # appending all eligible strings with length > 5
    if (len_element > 5){
      sentence_str <- paste(sentence_str, elem)
    }
  }
  
  # appending to the corpus dataframe
  corpus_df <- rbind(corpus_df, data.frame(doc_id = doc_name, text = sentence_str))
}

# function to process the incoming text string from a document
clean_text <- function(string_txt){
  
  # converting to lower case
  res_str <- tolower(string_txt)
  
  # replacing hyphen with a space
  res_str <- gsub("-", " ", res_str)
  
  # replacing comma and semi-colon with nothing!
  res_str <- gsub(",", "", res_str)
  res_str <- gsub(";", "", res_str)
  
  # return the treated string
  return (res_str)
}

####### checking for a single text for now
# initializing the dataframe to hold all the results
res_df <- data.frame(doc_id = character(),
                    str_seq = character(),
                    count_str = numeric(),
                    doc_len = numeric(),
                    stringsAsFactors=FALSE)

# going through all the documents
for (i in 1:nrow(corpus_df)){
  
  elem_name <- as.character(corpus_df$doc_id[i])
  elem_doc <- corpus_df$text[i]
  elem_len <- length(strsplit(as.character(elem_doc), " ")[[1]])
  #elem_len <- stringr::str_length(elem_doc)
  elem_doc <- clean_text(elem_doc)
  
  # initializing a dataframe to store counts for all string sequences
  elem_df <- data.frame(str_seq = character(),
                        count_str = numeric(),
                        doc_len = numeric(),
                        stringsAsFactors=FALSE)
  
  # getting count of all string sequences
  for (curr_str in dict_in$words){
    occur_str <- stringr::str_count(elem_doc, curr_str)
    # adding to the dataframe
    elem_df <- rbind(elem_df, data.frame(str_seq = curr_str, count_str = occur_str, doc_len = elem_len))
  }
  
  # attaching the elem_name
  elem_df$doc_id <- elem_name
  elem_df <- elem_df[,c('doc_id','str_seq','count_str', 'doc_len')]
  
  # appending to the data frame
  res_df <- rbind(res_df, elem_df)
}

# merging the dataframe to get the buckets
res_df <- merge(res_df, dict_in, by.x = 'str_seq', by.y = 'words')

# getting the final columns
sel_col <- c('doc_id', 'bucket', 'count_str', 'doc_len')
res_df <- res_df[sel_col]

# aggregating at the bucket level
res_df <- res_df %>% 
  group_by(doc_id, bucket) %>% 
  summarize(total_count = sum(count_str), doc_len = mean(doc_len)) 

# creating a count normalized by document length
res_df$norm_count <- (res_df$total_count/res_df$doc_len)*100

# writing the xlsx
wb <- createWorkbook()
sheet_num = 1
addWorksheet(wb = wb, sheetName = "Detailed")
writeDataTable(wb = wb, sheet = sheet_num, x = res_df)

saveWorkbook(wb, paste0(fname,".xlsx"), overwrite = TRUE)
