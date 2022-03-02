#!/usr/bin/env Rscript --vanilla


##IMPORTANT
##In order to run this code you have to download zipped files from Google Drive and place in the directory 
#on your local computer.



#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(utils)
require(tidyverse)
require(dplyr)
require(googlesheets4)


select <- dplyr::select



#------------------------------------------------#
####              1 BHM Datasets              ####
#------------------------------------------------#

#Create the basis of our sheet with a list of all names in the directory and subdirectories
datasets_bhm <- as.data.frame(dir(path = "Datasets_BHM_2021", full.names = TRUE, recursive = TRUE))

##Create the file type
#Grab only the extension from the file list
datasets_bhm$type <- datasets_bhm[,1] %>% 
  str_replace(".*(\\.\\w*)$", "\\1")
#Fix those with no file extension
datasets_bhm$type <- datasets_bhm$type %>% 
  str_replace("Datasets_BHM_2021/USGS/USGS Gage WQ Data", "\\.txt")

##Create the file name
#Grab only the file name from the file list
datasets_bhm$name <- datasets_bhm[,1] %>% 
  str_replace_all(".*\\/(.*\\.\\w*)$", "\\1")
#Fix those with no file extension
datasets_bhm$name <- datasets_bhm$name %>% 
  str_replace("Datasets_BHM_2021/USGS/USGS Gage WQ Data", "\\USGS Gage WQ Data.txt")

##Add in the file size
#Get file info for each and only return file size
datasets_bhm$size <- file.info(datasets_bhm[,1]) %>% select("size")
#Fix row name change caused by the last action
row.names(datasets_bhm) <- 1:66

#Add in the data category
datasets_bhm$category <- ifelse(datasets_bhm$type!=".jpg", "hydrologic", NA)

#Add in the priority for which Schoodic Institute should focus their efforts
datasets_bhm$priority <- ifelse(datasets_bhm$type==".xlsx" | datasets_bhm$type==".csv", "high","low")

##Add empty columns for data source, source contact, and notes
#Create the three columns
bhm_empty_cols <- c('source', 'source.contact', 'notes')
datasets_bhm[ , bhm_empty_cols] <- NA

##Final cleaning for final BHM
#Fix column names
colnames(datasets_bhm) <- c('path', 'type', 'name', 'size', 'category', 'priority', 'source', 'source.contact', 'notes')
#Fix the order of columns
datasets_bhm <- datasets_bhm %>% 
  select(name, path, type, everything())




#------------------------------------------------#
####             2 GMW Datasets               ####
#------------------------------------------------#

#Create the basis of our sheet with a list of all names in the directory and subdirectories
datasets_gmw <- as.data.frame(dir(path = "Datasets_GMW", full.names = TRUE, recursive = TRUE))

##Create the file type
#Grab only the extension from the file list
datasets_gmw$type <- datasets_gmw[,1] %>% 
  str_replace(".*(\\.\\w*)$", "\\1")
#Fix those with no file extension
datasets_gmw$type <- datasets_gmw$type %>% 
  str_replace("\\Data.*", "na")

##Create the file name
#Grab only the file name from the file list
datasets_gmw$name <- datasets_gmw[,1] %>% 
  str_replace_all(".*\\/(.*\\.\\w*)$", "\\1")
#Fix those with no file extension
datasets_gmw$name <- datasets_gmw$name %>% 
  str_replace_all(".*\\/(.*)$", "\\1")

##Add in the file size
#Get file info for each and only return file size
datasets_gmw$size <- file.info(datasets_gmw[,1]) %>% select("size")
#Fix row name change caused by the last action
row.names(datasets_gmw) <- 1:923

#Add in the data category
datasets_gmw$category <- ifelse(datasets_gmw$type==".shp" | datasets_gmw$type==".shx" | datasets_gmw$type==".dbf"
                                | datasets_gmw$type==".prj", "spatial", NA)

#Add in the priority for which Schoodic Institute should focus their efforts
datasets_gmw$priority <- ifelse(datasets_gmw$type==".xlsx" | datasets_gmw$type==".csv", "high","low")

##Add empty columns for data source, source contact, and notes
#Create the three columns
gmw1_empty_cols <- c('source', 'source.contact', 'notes')
datasets_gmw[ , gmw1_empty_cols] <- NA

##Final cleaning for final BHM
#Fix column names
colnames(datasets_gmw) <- c('path', 'type', 'name', 'size', 'category', 'priority', 'source', 'source.contact', 'notes')
#Fix the order of columns
datasets_gmw <- datasets_gmw %>% 
  select(name, path, type, everything())




#------------------------------------------------#
####                3 GMW Data                ####
#------------------------------------------------#

#Create the basis of our sheet with a list of all names in the directory and subdirectories
data_gmw <- as.data.frame(dir(path = "Data_GMW", full.names = TRUE, recursive = TRUE))

##Create the file type
#Grab only the extension from the file list
data_gmw$type <- data_gmw[,1] %>% 
  str_replace(".*(\\.\\w*)$", "\\1")

##Create the file name
#Grab only the file name from the file list
data_gmw$name <- data_gmw[,1] %>% 
  str_replace_all(".*\\/(.*\\.\\w*)$", "\\1")

##Add in the file size
#Get file info for each and only return file size
data_gmw$size <- file.info(data_gmw[,1]) %>% select("size")
#Fix row name change caused by the last action
row.names(data_gmw) <- 1:310

#Add in the data category
data_gmw$category <- ifelse(data_gmw$type==".csv" | data_gmw$type==".xlsx", "geologic", NA)

#Add in the priority for which Schoodic Institute should focus their efforts
data_gmw$priority <- ifelse(data_gmw$type==".xlsx" | data_gmw$type==".csv", "high","low")

##Add empty columns for data source, source contact, and notes
#Create the three columns
gmw2_empty_cols <- c('source', 'source.contact', 'notes')
data_gmw[ , gmw2_empty_cols] <- NA

##Final cleaning for final BHM
#Fix column names
colnames(data_gmw) <- c('path', 'type', 'name', 'size', 'category', 'priority', 'source', 'source.contact', 'notes')
#Fix the order of columns
data_gmw <- data_gmw %>% 
  select(name, path, type, everything())




#------------------------------------------------#
####              4 WQ Datasets               ####
#------------------------------------------------#

#Create the basis of our sheet with a list of all names in the directory and subdirectories
datasets_wq <- as.data.frame(dir(path = "Datasets_WQ", full.names = TRUE, recursive = TRUE))

##Create the file type
#Grab only the extension from the file list
datasets_wq$type <- datasets_wq[,1] %>% 
  str_replace(".*(\\.\\w*)$", "\\1")
#Fix those with no file extension
datasets_wq$type <- datasets_wq$type %>% 
  str_replace("\\Data.*", "na")

##Create the file name
#Grab only the file name from the file list
datasets_wq$name <- datasets_wq[,1] %>% 
  str_replace_all(".*\\/(.*\\.\\w*)$", "\\1")
#Fix those with no file extension
datasets_wq$name <- datasets_wq$name %>% 
  str_replace_all(".*\\/(.*)$", "\\1")

##Add in the file size
#Get file info for each and only return file size
datasets_wq$size <- file.info(datasets_wq[,1]) %>% select("size")
#Fix row name change caused by the last action
row.names(datasets_wq) <- 1:324

#Add in the data category
datasets_wq$category <- ifelse(datasets_wq$type==".kmz", "spatial", NA)

#Add in the priority for which Schoodic Institute should focus their efforts
datasets_wq$priority <- ifelse(datasets_wq$type==".xlsx" | datasets_wq$type==".csv", "high","low")

##Add empty columns for data source, source contact, and notes
#Create the three columns
wq_empty_cols <- c('source', 'source.contact', 'notes')
datasets_wq[ , wq_empty_cols] <- NA

##Final cleaning for final wq
#Fix column names
colnames(datasets_wq) <- c('path', 'type', 'name', 'size', 'category', 'priority', 'source', 'source.contact', 'notes')
#Fix the order of columns
datasets_wq <- datasets_wq %>% 
  select(name, path, type, everything())




#------------------------------------------------#
####               5 HYD Data                 ####
#------------------------------------------------#

#Create the basis of our sheet with a list of all names in the directory and subdirectories
data_hyd <- as.data.frame(dir(path = "Data_HYD", full.names = TRUE, recursive = TRUE))

##Create the file type
#Grab only the extension from the file list
data_hyd$type <- data_hyd[,1] %>% 
  str_replace(".*(\\.\\w*)$", "\\1")
#Fix those with no file extension
data_hyd$type <- data_hyd$type %>% 
  str_replace("\\Data.*", "na")

##Create the file name
#Grab only the file name from the file list
data_hyd$name <- data_hyd[,1] %>% 
  str_replace_all(".*\\/(.*\\.\\w*)$", "\\1")
#Fix those with no file extension
data_hyd$name <- data_hyd$name %>% 
  str_replace_all(".*\\/(.*)$", "\\1")

##Add in the file size
#Get file info for each and only return file size
data_hyd$size <- file.info(data_hyd[,1]) %>% select("size")
#Fix row name change caused by the last action
row.names(data_hyd) <- 1:791

#Add in the data category
data_hyd$category <- ifelse(data_hyd$type==".xlsx" | data_hyd$type==".csv", "hydrologic", NA)

#Add in the priority for which Schoodic Institute should focus their efforts
data_hyd$priority <- ifelse(data_hyd$type==".xlsx" | data_hyd$type==".csv", "high","low")

##Add empty columns for data source, source contact, and notes
#Create the three columns
hyd_empty_cols <- c('source', 'source.contact', 'notes')
data_hyd[ , hyd_empty_cols] <- NA

##Final cleaning for final wq
#Fix column names
colnames(data_hyd) <- c('path', 'type', 'name', 'size', 'category', 'priority', 'source', 'source.contact', 'notes')
#Fix the order of columns
data_hyd <- data_hyd %>% 
  select(name, path, type, everything())




#------------------------------------------------#
####          Combine to create final         ####
#------------------------------------------------#

#Rbind all 5 of those directory-based files we made
wild_acadia_metadata <- rbind(datasets_bhm, datasets_gmw)
wild_acadia_metadata <- rbind(wild_acadia_metadata, data_gmw)
wild_acadia_metadata <- rbind(wild_acadia_metadata, datasets_wq)
wild_acadia_metadata <- rbind(wild_acadia_metadata, data_hyd)

#Fix weird list formatting thing that's stuck in the dataframe
wild_acadia_metadata <- as.data.frame(lapply(wild_acadia_metadata,c))

#Add units to size column
wild_acadia_metadata <- wild_acadia_metadata %>% rename('size (B)'='size')



#------------------------------------------------#
####         Write out final metadata         ####
#------------------------------------------------#

##Run this line to produce a google sheet
##Will need to let tidyverse access your account
#(ss <- gs4_create("wild_acadia_metadata", sheets = wild_acadia_metadata))

