# installing SparkR inside RStudio
# follow this link (http://www.r-bloggers.com/installing-and-starting-sparkr-locally-on-windows-os-and-rstudio/)
# do steps 1 and 2, skip 3 (vis cmd), and do step 4 (inside Rstudio as shown below)
# step 4.5 has all the following code

# C:\Users\akuppam\spark-1.6.1-bin-hadoop2.6

Sys.setenv(SPARK_HOME="C:/Users/akuppam/spark-1.6.1-bin-hadoop2.6")
.libPaths(c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), .libPaths()))
library(SparkR)

# Create a spark context and a SQL context 
sc <- sparkR.init(master = "local") 
sqlContext <- sparkRSQL.init(sc) 
 
df <- read.df(sqlContext, "cars.csv", source = "com.databricks.spark.csv", inferSchema = "true")


#create a sparkR DataFrame 
DF <- createDataFrame(sqlContext, faithful) 
head(DF) 
summary(DF)
View(DF)

# Create a simple local data.frame 
localDF <- data.frame(name=c("John", "Smith", "Sarah"), age=c(19, 23, 18)) 

# Convert local data frame to a SparkR DataFrame 
df <- createDataFrame(sqlContext, localDF) 
View(df)

# Print its schema 
printSchema(df) 
# root 
#  |-- name: string (nullable = true) 
#  |-- age: double (nullable = true) 
 

# Create a DataFrame from a JSON file 
path <- file.path(Sys.getenv("SPARK_HOME"), "examples/src/main/resources/people.json") 
peopleDF <- jsonFile(sqlContext, path) 
printSchema(peopleDF) 
 

# Register this DataFrame as a table. 
registerTempTable(peopleDF, "people") 
 

# SQL statements can be run by using the sql methods provided by sqlContext 
teenagers <- sql(sqlContext, "SELECT name FROM people WHERE age >= 13 AND age <= 19") 
 

# Call collect to get a local data.frame 
teenagersLocalDF <- collect(teenagers) 
 

# Print the teenagers in our dataset  
print(teenagersLocalDF) 
 

# Stop the SparkContext now 
sparkR.stop() 

# ******************************
# example in this lin (http://inbergict.nl/blog/?p=126)
# compares standard R vs SparkR
# *****************************

# ***********
# standard R
# ***********

library(sqldf)
library(ggplot2)
library(magrittr)

start.time <- Sys.time()
#df <- read.csv(file.path(Sys.getenv("SPARK_HOME"), "datasets/crimes.csv"))
df <- read.csv("~/Pythprog/SparkR/Crimes_-_2001_to_present.csv")
grouped_df <- sqldf("select Year, count(*) as count from df group by Year")
ggplot(data = grouped_df, aes(x= Year, y=count)) + geom_point() + geom_line(group=1) + ggtitle("Amount of crimes in Chicago per year")

end.time <- Sys.time()
end.time - start.time

# ================

start.time <- Sys.time()

Sys.setenv(SPARK_HOME="C:/Users/akuppam/spark-1.6.1-bin-hadoop2.6")
.libPaths(c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), .libPaths()))
library(SparkR)

# Create a spark context and a SQL context 
sc <- sparkR.init(master = "local") 
sqlContext <- sparkRSQL.init(sc) 
getwd()
#read the data into a Spark DataFrame (capitals)
DF <- read.df(sqlContext, file.path(Sys.getenv("SPARK_HOME"), "data/Crimes_-_2001_to_present.csv"), source = "csv", header="true")
pathdf <- file.path(Sys.getenv("SPARK_HOME"), "data/Crimes_-_2001_to_present.csv")
DF <- read.df(sqlContext, "C:/Users/akuppam/spark-1.6.1-bin-hadoop2.6/data/Crimes_-_2001_to_present.csv", source = "csv", header="true")

# ---------------
library(SparkR)

Sys.setenv('SPARKR_SUBMIT_ARGS'='"--packages" "com.databricks:spark-csv_2.10:1.4.0" "sparkr-shell"')

# ^^^^^^^^^^
Sys.setenv(SPARK_HOME="C:/Users/akuppam/spark-1.6.1-bin-hadoop2.6")
.libPaths(c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), .libPaths()))
library(SparkR)

Sys.setenv('SPARKR_SUBMIT_ARGS'='"--packages" "com.databricks:spark-csv_2.10:1.0.3" "sparkr-shell"')
library(SparkR)
library(magrittr)

# Initialize SparkContext and SQLContext
sc <- sparkR.init(master = "local") 
sqlContext <- sparkRSQL.init(sc) 

sc <- sparkR.init(master="local", sparkPackages="com.databricks:spark-csv_2.11:1.0.3")
sqlContext <- sparkRSQL.init(sc)


#flights <- read.df(sqlContext, "nycflights13.csv", "com.databricks.spark.csv", header="true")
#DF <- read.df(sqlContext, file.path(Sys.getenv("SPARK_HOME"), "data/Crimes_-_2001_to_present.csv"), source = "com.databricks.spark.csv", header="true")

DF <- read.df(sqlContext, "C:/Users/akuppam/spark-1.6.1-bin-hadoop2.6/data/Crimes_-_2001_to_present.csv", source = "com.databricks.spark.csv", header=FALSE)
#DF <- read.df(sqlContext, "data/Crimes_-_2001_to_present.csv", source = "csv", header="true")

#DF <- read.df(sqlContext, "data/Crimes_-_2001_to_present.csv", source = "com.databricks.spark.csv", header="true")

getwd()

library(rJava)
library(SparkR)

Sys.setenv('SPARKR_SUBMIT_ARGS'='"--packages" "com.databricks:spark-csv_2.11:1.2.0" "sparkr-shell"')

Sys.setenv(SPARK_MEM="1g")


# Create a spark context and a SQL context
sc <- sparkR.init(master = "local")

sqlContext <- sparkRSQL.init(sc)

# ^^^^^^^^^^

Sys.setenv('SPARK_HOME/bin/spark-shell --packages com.databricks:spark-csv_2.11:1.4.0')
sqlContext <- sparkRSQL.init(sc)
DF <- read.df(sqlContext, "C:/Users/akuppam/spark-1.6.1-bin-hadoop2.6/data/Crimes_-_2001_to_present.csv", source = "csv", inferSchema = "true")

df <- read.df(sqlContext, "cars.csv", source = "com.databricks.spark.csv", inferSchema = "true")

# --------------------

#df = sqlContext.load(path="C:/Users/akuppam/spark-1.6.1-bin-hadoop2.6/data/Crimes_-_2001_to_present.csv, source="com.databricks.spark.csv")

#DF <- read.csv(pathdf, header=TRUE)

# add the data to a temp table to be able to use SQL
registerTempTable(DF, "crimes")
# define spark DataFrame with amount of crimes per year
groupedDF <- sqlContext %>% sql("select Year, count(*) as count from crimes group by Year")

#transfer to R. Since Spark is lazy, actual work is performed now!
grouped_df <- collect(groupedDF)
# stop spark
sparkR.stop()

#plot data in R
ggplot(data = grouped_df, aes(x= Year, y=count)) + geom_point() + geom_line(group=1) + ggtitle("Amount of crimes in Chicago per year")

end.time <- Sys.time()
end.time - start.time

# $$$$$$$$$$$$$$$$$$$$$$

# C:\Users\akuppam\Documents\Pythprog\SparkR

library(SparkR)

Sys.setenv('SPARKR_SUBMIT_ARGS'='"--packages" "com.databricks:spark-csv_2.10:1.4.0" "sparkr-shell"')
sc <- sparkR.init(master = "local")
sqlContext <- sparkRSQL.init(sc)
setwd("C:/Users/akuppam/Documents/Pythprog/SparkR")

df <- read.df(sqlContext, "cars.csv", source = "com.databricks.spark.csv", inferSchema = "true")
df <- read.df(sqlContext, "cars.csv", source = "csv", header = "true")
df <- read.df(sqlContext, "cars.csv")

write.df(df, "newcars.csv", "com.databricks.spark.csv", "overwrite")

getwd()


