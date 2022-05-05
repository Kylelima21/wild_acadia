#Wild Acadia data cleaning project
#Schoodic Institute at Acadia National Park
#Kyle Lima, Spring 2022



#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(tidyverse)
require(tidyr)
require(dplyr)
require(googledrive)
require(googlesheets4)

select <- dplyr::select



#------------------------------------------------#
####           Download directory             ####
#------------------------------------------------#

#We first need to download the Wild Acadia directory on Google Drive

#You need to zip the entire directory and download that onto your project
#directory in order to produce this metadata



#------------------------------------------------#
####              Make metadata               ####
#------------------------------------------------#

#List of all names in the directory and subdirectories
meta.basis <- as.data.frame(dir(path = "wild_acadia_data_cleaning", full.names = TRUE, recursive = TRUE))


##Create file extension column
#Grab only the extension from the file list
meta.basis$type <- meta.basis[,1] %>% 
  str_replace(".*(\\.\\w*)$", "\\1")


##Create the file name column
#Grab only the file name from the file list
meta.basis$name <- meta.basis[,1] %>% 
  str_replace_all(".*\\/(.*\\.\\w*)$", "\\1")


# ##Add in the file size column
# #Get file info for each and only return file size
# meta.basis$size <- file.info(meta.basis[,1]) %>% select("size")
# 
# #Fix row name change caused by the last action
# row.names(meta.basis) <- 1:751


##Add empty columns for data source, source contact, and notes
#Create the empty column
meta.basis$description <- NA


##Final cleaning
#Fix column names
colnames(meta.basis) <- c('path', 'extension', 'filename', 'desciption')

#Fix the order of columns
meta.basis <- meta.basis %>% 
  select(filename, path, extension, everything())


##Filter for only data/processed
meta.final <- meta.basis[1:19,]



#------------------------------------------------#
####           Write out metadata             ####
#------------------------------------------------#

#Write out the final metadata product
write.csv(meta.final, "data/processed/processed_metadata.csv")


