#This script assumes that the input files(see input test and train files) exist in the same directory as the R script
#See codebook for additional details.  Numbers in (1) in comments refer to specific transformations described in codebook

#Source Needed libraries(1)
library(data.table)
library(dplyr)

#Input Test and Train files(2)
        #Test
data_test<-read.table("X_test.txt",sep="",fill=TRUE)
active_test<-read.table("y_test.txt",sep="",fill=TRUE)
subj_test<-read.table("subject_test.txt",sep="",fill=TRUE)

      #Train
data_train<-read.table("X_train.txt",sep="",fill=TRUE)
active_train<-read.table("y_train.txt",sep="",fill=TRUE)
subj_train<-read.table("subject_train.txt",sep="",fill=TRUE)

#We want to manage column names in feature.txt to help strip out unwanted columns from data tables
#and simplify labels on the resulting column labels

feature<-read.table("features.txt",sep="",fill=TRUE)                  #Read in features.txt, these are all labels

#We only want columns which represent std and Mean, so grep features for only std() and mean() and Mean(3)

pattern<-"std|mean|Mean"
col_numbs<-grep(pattern,feature[,2])                                            #Grep features which contain std() and avg(), acquire col. number

#We have a vector of the column numbers we want to keep
#Let's cleanup column headers to remove special characters (4,5)

col_names<-feature[col_numbs[1:86],2]                                           #Extracts the actual column names
col_names<-gsub("\\(\\)","",col_names)                                          #Remove () in column names
col_names<-gsub("\\(","__",col_names)                                           #Remove ( and ) replace with __
col_names<-gsub("\\)","__",col_names)
col_names<-gsub("-","_",col_names)                                              #Convert special char '-' to not-so-speccial char '_'
col_names<-as.character(col_names)                                              #Tweak col_names so I can use it to label data columns
addonnames<-c("Subject","Activity")                                             #Last mod to colunames to accomodate subject and activity columns
colnames<-c(addonnames,col_names)                                               #Colnames is a list of clean column labels ready to merge

#Using the col_numbers which correspond to the key std and avg column lables
#Lets pare down the test and train datasets to only those we need.(6)

data_test_std_means<-data_test[,col_numbs[1:86]]                                #Drop test data columns that are not std or means
data_train_std_means<-data_train[,col_numbs[1:86]]                              #Drop train data columns that are not std or means

#Lets start combining Test and Train Data, Subjects, and Activities, with some minor tweaks before we combine all togather (7,8)

data<-rbind(data_test_std_means,data_train_std_means)                           #Combine Test and Train data sets
subj<-rbind(subj_test,subj_train)                                               #Combine Test and Train for subjects
active.df<-rbind(active_test,active_train)                                      #Cobine Test and Train for Activities

#Convert Activity Numbers to text descriptions (9)

cycle<-nrow(active.df)
while(cycle > 0) {
  if(active.df[cycle,1] == "1") {Active<<-"Walking"}
  else if (active.df[cycle,1]== "2") {Active<-"Walking_Upstairs"}
  else if (active.df[cycle,1]== "3") {Active<-"Walking_Downstairs"}
  else if (active.df[cycle,1]== "4") {Active<-"Sitting"}
  else if (active.df[cycle,1]== "5") {Active<-"Standing"}
  else if (active.df[cycle,1]== "6") {Active<-"Laying"}
  else  {active.df<-"error"}
  active.df[cycle,1]<-Active[1]
  cycle<- cycle-1
}


#Combine Data sets, with subject and activity data(10)

data<-cbind(subj, active.df,data) 

#Add column names to the data dataframe (11)

setnames(data,colnames) 

#Create a data table from the data dataframe (12)

data.dt<-tbl_df(data)

#Order the data by Activity Number and subject, obtaining the means for each combination(13)

means<-data.dt %>% group_by(Activity,Subject) %>% summarise_each(funs(mean))

#Write the data out to a space delimited .txt file(14)

write.table(means, file="tidy_data_number_5.txt",row.name=FALSE,sep=" ")