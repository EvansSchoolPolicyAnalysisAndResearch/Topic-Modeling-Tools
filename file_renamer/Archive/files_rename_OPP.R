# Purpose of script: This script was developed by the Evans School Policy Analysis & Research Group (EPAR) to update the names of files in a directory based on the name of their parent folder. The script will recursively enter all folders within a folder as-needed. The initial use case was appending the grant number to the set of files shared for that grant.

# Date: 2 May 2019

# Inputs: N/A

# Outputs: N/A

############# define the folder where parent folder(s) whose name(s) will be used to rename subfiles reside ##########################
# clear workspace
rm(list = ls())

# setting working directory
setwd('//netid.washington.edu/wfs/EvansEPAR/Project/EPAR/Working Files/372 - EPAR Tools Development/_TOOLS_Main_Folder/file_rename_OPP/test')  

# provide regex pattern to identify relevant parent folders by their folder names 
pattern = "*OPP*"

###################################################################################

# current working directory
mydir <- getwd()

# gets name of all parent folders to copy name from
# name should contain a pattern OPP
subfolders <- grep(pattern, list.dirs(mydir, full.names = TRUE, recursive = FALSE), value = TRUE)

# defining the function to rename all files 
renameFunc <- function(z){
  setwd(z)
  # reading all the files within a folder
  list_files <- list.files(recursive = TRUE)
  append_name <- substr(z, nchar(mydir)+2, nchar(z))
  print (paste0('All files in the folder ',append_name,' being renamed...'))
  # going through all the files in a directory
  for (i in 1:length(list_files)){
    # treating a certain file
    curr_file <- list_files[i]
    # splitting the file path on backslash
    str_split <- strsplit(curr_file, split='/', fixed = TRUE)
    # length of the split
    len_split <- length(str_split[[1]])
    # last part is the file_name
    file_name <- str_split[[1]][len_split]
    # length of split > 1?
    if (len_split == 1){
      rel_path = ""
    }
    else{
      # relative path of the file
      rel_path <- str_split[[1]][1]
      if (len_split > 2){
        for (j in 2:(len_split-1)){
          path <- str_split[[1]][j]
          rel_path <- paste0(rel_path, "/", path) 
        }
      }
    }
    # new name assignment
    # if the prefix is not OPP
    prefix <- "OPP"
    first_3letters <- substr(file_name, 1, 3)
    if (first_3letters != prefix){
      new_name <- paste0(append_name, "_", file_name)
      if (rel_path == "")
      {
        # renaming the file
        file.rename(from = curr_file, to = new_name)
      }
      else {
        full_new_name <- paste0(rel_path , "/", new_name)
        # renaming the file
        file.rename(from = curr_file, to = full_new_name)
      }
    }
  }
}

# applying the renaming function
invisible(lapply(subfolders, renameFunc))