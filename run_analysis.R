#Course: "Getting and Cleaning Data"
# Project R code
#Abu M. Nurullah
# 10/21/2015 revision : 10/23/2015
#----------------------------------

# Section: 1 merging of the two datasets #
##########################################
require(reshape2)
require(plyr)

#download and unzip the project data file
setwd("C:/temp/")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              destfile = "projectdata.zip" )
if(!file.exists("UCI HAR Dataset" )){
  unzip("projectdata.zip")
}

#set path to the working data directory
#--------------------------------------------
path = paste(getwd(), "/UCI HAR Dataset/", sep = "")
setwd(path)
print("..reading training  data sets....")
train.data = read.table("train/X_train.txt", sep = "")
train.label = read.table("train/y_train.txt", sep = " ")
train.subj = read.table("train/subject_train.txt", sep = " ")
train_df = cbind(train.data, "act_id" = train.label$V1, "subject_id" = train.subj$V1)

#read the test data similar to above
#--------------------------------------
print("..reading test data sets.... ")
test.data = read.table("test/X_test.txt", sep = "")
test.label = read.table("test/y_test.txt", sep = " ")
test.subj = read.table("test/subject_test.txt", sep = " ")
test_df =  cbind(test.data, "act_id" = test.label$V1, "subject_id"= test.subj$V1)

# combine the two data frames into a singel data frame
print("...combining both train and test data frames.......")
df_merged = rbind(train_df, test_df)

###########################################################
# Section: 2.  Extracts only the measurements on          #
# the mean and standard deviation for each measurement.   #
###########################################################

# extract the variable names from feature.txt
#---------------------------------------------
print("..reading feature labels...")
df_feature = read.table("features.txt", sep = "")
col_labels = c(as.vector(df_feature[,2]), "act_id", "subject_id")

#assign col-labels to combined data frame
colnames(df_merged) <- col_labels

#identifying position of varaibles in vector colnames for mean() and Std()
#---------------------------------------------------------------------------
v1 = agrep("-mean()-", col_labels)
v2 = agrep("-std()-", col_labels)
v = sort(c(v1, v2))

#creating  dataset with only the mean and Std measurement variables
#-------------------------------------------------------------------
print("..creating dataset with selected variables.....")
df_sel = df_merged[, v]
df_sel = cbind(df_sel, "activity" = df_merged$"act_id", "subjectid"= df_merged$"subject_id")

#get the column lables of the selected set and assign tidy labels
selnames = names(df_sel)
selnames = gsub("-mean", "Mean", selnames)
selnames = gsub("-std", "Std", selnames)
selnames =gsub('[-()]', '', selnames)
names(df_sel) <- selnames

#################################################
# Section 3.Uses descriptive activity names     #
# to name  the activities in the data set       #
#################################################
#read the activity label
#-------------------------
print("..assigning activity labels")
actlabel = read.table("activity_labels.txt", sep ="")

# converting the numeric activity values into a descriptive one
#---------------------------------------------------------------
df_sel$activity = factor(df_sel$activity, levels = actlabel[,1], labels = actlabel[,2])
df_sel$subjectid = as.factor(df_sel$subjectid)

#reshaping the df_sel data frame for concise summary
#----------------------------------------------------
print("reshaping data for concise presentation...")
df_melt = melt(df_sel, c("activity", "subjectid"))
df_final = dcast(df_melt, subjectid + activity ~variable, mean)

print("..writing final tidy data set...")
write.table(df_final, "tidy.txt", row.names = FALSE, quote = FALSE)
#finally cleanup objects created during the process......
rm(df_merged, df_deature, df_sel, df_final, df_melt, sel_names )
