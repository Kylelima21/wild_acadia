#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(utils)
require(tidyverse)
require(dplyr)
require(googledrive)
require(readxl)
require(purrr)

select <- dplyr::select



#------------------------------------------------#
####              Read in File                ####
#------------------------------------------------#

##In order to download files from google drive you will need an access token
#Type 0 in the console and follow the steps to authorize your account

#Download the file from google drive
drive_download('wild_acadia_metadata', path = 'data/wild_acadia_metadata.xlsx')

#Read in file
wa.meta <- read_excel('data/wild_acadia_metadata.xlsx')



#------------------------------------------------#
####       Accessing and Moving Files         ####
#------------------------------------------------#

#Check values of priority
unique(wa.meta$priority)

#Only take high and medium priority files
wa.meta.p <- wa.meta %>% 
  filter(priority!="ignore" & priority!="low")

#Create a file list to pass through our purrr loop
wa.files <- wa.meta.p %>% 
  select(path)

#Set google drive output location
drive.output <- "https://drive.google.com/drive/u/4/folders/1rUoHnUILQS0Gs1kw-faf3cyqixVWBQYQ"

#Make a function that will upload a file to google drive
upload <- function(x){
  drive_upload(x, path = paste0("wild_acadia/data/", x))
}

#Upload every file to google drive
map(wa.files, ~upload(.))


drive_upload(wa.files, path = paste0("wild_acadia/data/", x))




# ##Example using purrr to loop a function to all files in a folder
# #Create the list of file paths for every file in the directory of interest
# test <- list.files(path="data/test", pattern=".csv", all.files=TRUE,
#            full.names=TRUE)
# 
# #Create your function that performs the tasks you desire
# maybeso <- function(x){
#   file_1 <- read.csv(x, header = TRUE)
#   file_2 <- file_1 %>% 
#     select(-1)
#   write_csv(file_2, paste(x, '.csv', sep=''))
# }
# 
# #Use function map from package purrr to loop this function across all the files in the directory
# map(test, ~maybeso(.))




# #Get practice dataset
# data(package = "datasets")
# cars <- as.data.frame(mtcars)
# 
# #Create a function that adds each column in a dataframe (x) to each other column in the dataframe
# newfunc <- function(x){
#   mpg <- x + cars[, 1:(ncol(cars))] #Here you we are saying take the specified column (x) and add
#                                     #all across each columns in dataframe cars
#   return(mpg)
# }
# 
# #Add every row to every other row
# across <- as.data.frame(map(cars, ~newfunc(.)))



