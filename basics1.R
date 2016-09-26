setwd("~/Pythprog/Rprog")							# set wd in which the data 'directory' resides
basics <- function(directory, var1 = "sulfate", id = 1:5) {
    if(grep("basics", directory) == 1) { 
        directory <- ("./basics/") 
	}	
all_files <- as.character( list.files(directory) ) 
    file_paths <- paste(directory, all_files, sep="") 
    for(i in id) { 
        current_file <- read.csv(file_paths[i], header=T, sep=",") 

row.names(current_file) 								# writing row names to the console (also counting no of rows)
colnames(current_file) 								# writing variable or field names to the console
sum(current_file$var1, na.rm=TRUE) 					# summing "var 1" and removing missing values
mean(current_file$var1, na.rm=TRUE) 				# summing "var 1" and removing missing values
max(current_file$var1, na.rm=TRUE)					# write the max value from the "var 1" column
min(current_file$var1, na.rm=TRUE)					# write the min value from the "var 1" column

}
}

#setwd("~/Pythprog/Rprog/basics")							# set wd in which the data 'directory' resides
basic <- function(directory, field, id = 1:5)
{
  fls <- list.files(directory, full.names = TRUE)
  target <- lapply(fls[id], function(basic) read.csv(basic, header=TRUE))
  result <- lapply(target, function(basic) basic[,field])
  field_all <- unlst (results)
  mean(field_all, na.rm=TRUE)
  print(results)
}

# LOOK INTO OTHER VARIATIONS IN THE BELOW WEBSITE
# http://stackoverflow.com/questions/23640594/reading-multiple-files-and-calculating-mean-based-on-user-input
####################################################################
# THE FOLLOWING CODE READS *.CSV FILES IN A DIRECTORY AND PRODUCES STATISTICS
# AS LONG AS ALL THE CSV FILES HAVE THE SAME NO. AND TYPE OF VARIABLES
####################################################################

path = "C:/Users/akuppam/Documents/Pythprog/Rprog/basics"			# set path
#setwd("~/Pythprog/Rprog/basics")									# set wd in which the data 'directory' resides
fileList = list.files(path=path, pattern="\\.csv$", full.names=T)	# get all csv files in the specified path folder
all.files.data = lapply (fileList, read.csv, header=TRUE)			# lapply loops over a list (files, in this case) and evaluates the function (reading csv files, in this case)
	
DATA = do.call("rbind", all.files.data)		# rbind helps in appending rows together from different csv files
summary(DATA)								# get a summarr of all variables (names, sums, min, max, means)
#row.names(DATA) 							# DON'T WRITE ROW NAMES IF THEY ARE IN THE 100'S
colnames(DATA) 								# writing variable or field names to the console
sum(DATA$sulfate, na.rm=TRUE) 				# summing "sulfate" and removing missing values
mean(DATA$sulfate, na.rm=TRUE) 				# summing "sulfate" and removing missing values
max(DATA$sulfate, na.rm=TRUE)				# write the max value from the "sulfate" column
min(DATA$sulfate, na.rm=TRUE)				# write the min value from the "sulfate" column
