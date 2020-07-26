#  Getting and Cleaning Data Course Project

#This code loads, cleans and formats a data set form UCI according to requeriments of Course Project
#The following goals are achieved:
#   [Line 59]. Merges the training and the test sets to create one data set.
#   [Line 30]. Extracts only the measurements on the mean and standard deviation for each measurement.
#   [Line 36]. Uses descriptive activity names to name the activities in the data set
#   [Line 28]. Appropriately labels the data set with descriptive variable names.
#   [Line 63]. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#The lines where each goal is achieved is showed in brackets [].

library(tidyverse)
library(stringr)

#Download and unzip data. Resulting folder is same as base_dir
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","data.zip")
unzip("data.zip")

#directory of files
base_dir="UCI HAR Dataset"


#reading data from test an training
data=lapply(c("test","train"),function(folder){
  
  #_______________________   Reading features...  ______________________________
  features=read_delim(file.path(base_dir,"features.txt"),delim = " ",col_names = F) #features names
  X=read_delim(file.path(base_dir,folder,paste0("X_",folder,".txt")),delim = " ",col_names = features$X2) %>%
    mutate_all(funs(as.numeric)) %>% #cast to numeric
    select(matches("mean|std")) #select only mean and std features
  
  #_______________________    Reading activities...   ___________________________
  activity_labels=read_delim(file.path(base_dir,"activity_labels.txt"),delim = " ",col_names = F) %>% #activity labels
    deframe() #transform to named vector
  y=read_delim(file.path(base_dir,folder,paste0("y_",folder,".txt")),delim = " ",col_names = F)%>%
    mutate(Activity=recode(X1,!!!activity_labels) ) %>% #Recode levels of activities into dexcriptive names
    select(Activity)
  
  #_______________________    Reading subjects...   ___________________________
  subject=read_delim(file.path(base_dir,folder,paste0("subject_",folder,".txt")),delim = " ",col_names = F) %>%
    rename(Subject=X1)
  
  #uncomment if necessary to load the Inertial Signals data (description of project is ambiguous)
  #files=list.files(path=file.path(base_dir,folder,"Inertial Signals/"),pattern = "*.txt")
  #data=lapply(files, function(f){
  #   read_delim(file.path(base_dir,folder,"Inertial Signals",f),delim = " ",col_names = F)%>%
  #     mutate_all(funs(as.numeric)) %>%
  #     rename_all(funs(str_replace_all(.,"X",sub(paste0(folder,".*"), "", f))))
  # }) %>%
  #   reduce(cbind)
  
  
  #_______________________    Bind data.frames...   ___________________________
  #Format: (Selected X features) -- (subject identifier) -- (activity labels)
  X %>% bind_cols(subject,y)
  
  #X %>% bind_cols(data,subject,y) #Uncomment if you want to include Inertial Signals data
  
}) %>% reduce(rbind) #Merge test and train data


#_______________________    Summarise average data...   ___________________________
average.data= data %>%
  group_by(Activity,Subject) %>% #Grouping variables by activity and subject
  summarise_all(funs(mean))
