setwd("~/Pythprog/Rprog") 						# setting working directory or path
initial <- read.csv("basics.csv", header=TRUE) 	# reading csv file into 'initial' and indicating that the 1st row has variable or field names
row.names(initial) 								# writing row names to the console (also counting no of rows)
colnames(initial) 								# writing variable or field names to the console
sum(initial$var.1, na.rm=TRUE) 					# summing "var 1" and removing missing values
mean(initial$var.1, na.rm=TRUE) 				# summing "var 1" and removing missing values
max(initial$var.1, na.rm=TRUE)					# write the max value from the "var 1" column
min(initial$var.1, na.rm=TRUE)					# write the min value from the "var 1" column

initial$var.4 <- as.numeric(gsub(',', '', initial$var.4))	# reading or converting thousand separator (eg: 2,000) to numeric (eg: 2000)


#####################
> read.csv('t.csv', header=F)
     V1          V2          V3          V4
1 Sudan  15,276,000  14,098,000  13,509,000
2  Chad      209000      196000      190000

# if you want to convert them to numbers:
> df <- read.csv('t.csv', header=F, stringsAsFactor=F)
> df$V2 <- as.numeric(gsub(',', '', df$V2))
############################

x1 <- (initial$var.1 * initial$var.4)			# multiplying two columns of numbers
print (x1)										# writing resulting values into the console

initial$var.5 <- as.numeric(gsub(',', '', initial$var.5))
x2 <- (x1/initial$var.5)						# dividing the new variable by var 5
print (x2)										# writing resulting values into the console

initial2 <- initial[as.numeric(gsub(initial$duration))<999990] # selecting values less than 999990 where duration is the variable name
initial2 <- initial[initial$duration<=999990,]

sum(initial2$duration, na.rm=TRUE) 					# summing "duration" and removing missing values
mean(initial2$duration, na.rm=TRUE) 				# summing "duration" and removing missing values
max(initial2$duration, na.rm=TRUE)					# write the max value from the "duration" column
min(initial2$duration, na.rm=TRUE)					# write the min value from the "duration" column

func1 <- function (ind) {							# writing a function
for (i in seq(ind)) print ("hello") }				# using "seq" that generates numbers or repeats a function certain number of timesgreet <- func1(10)
greet <- func1(10)									# write out "hello" to the console 'ind' times or 10 times
func1(10)											# just coding func1(10) will also yield the same result (no need to use 'greet')

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
