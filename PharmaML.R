# Amsterdam Workshop

setwd("/cloud/project")

if(!'data.table' %in% rownames(installed.packages())) install.packages('data.table')
if(!'psych' %in% rownames(installed.packages())) install.packages('psych')
if(!'ggplot2' %in% rownames(installed.packages())) install.packages('ggplot2')
if(!'tidyverse' %in% rownames(installed.packages())) install.packages('tidyverse')

files_in_dir = list.files()

if(!"cms.psv" %in% files_in_dir) download.file("https://github.com/xbsd/amsterdam/raw/master/cms.psv","cms.psv")
if(!"nhs201701.csv" %in% files_in_dir) download.file("https://github.com/xbsd/amsterdam/raw/master/nhs201701.csv","nhs201701.csv")
if(!"nhsbnft201701.csv" %in% files_in_dir) download.file("https://github.com/xbsd/amsterdam/raw/master/nhsbnft201701.csv","nhsbnft201701.csv")
if(!'nhschemsubs201701.csv' %in% files_in_dir) download.file("https://github.com/xbsd/amsterdam/raw/master/nhschemsubs201701.csv","nhschemsubs201701.csv")


library(data.table)


# Basics

print ("Hello Amsterdam!") # Prints a line

# Comments start with #
x <- 2   # This is where you would put your comment
x <- 1   # assigns a value of 1 to x
y <- 4   # assigns a value of 4 to y
x + y    # adds 1 to 4

# Use <- for assignment

x <- 10
x

y <- c(10,20,30) # If I want to assign multiple values
y

y[0] # Not valid, index starts at 1

# y is (10,20,30)

y[1] # 10
y[2] # 20
y[3] # 30


# Objects are created in R workspace
# List all objects created in your workspace …  > ls()
# Remove an object permanently … > rm()  e.g. rm(x)
y
rm(y)
y <- c(10,20,30)

#Error: object 'y' not found

# Saving your work
# Saving individual objects … > save(object_name,file=“name.R”)

# Saving the entire workspace  … > 
#   save.image()   It will save the workspace to .RData file in your current working directory
# save.image(file=“path_to_file”) it will save to the named file

ls()
y
save.image(file="test0")
rm(y)

y
load("test0")
y

# Call system commands
system("rm test0*")

# Saving the R script file … use appropriate save menu command in the Source Editor
# Retrieving your work 
# Open the script : Use  File  Open File


# Source(run) the script : source(“path_to_file”)	
# Load individual object : load(“path_to_file”)
# Load entire workspace from default location : load(“.RData”)
# Load workspace saved to a file : load(“path_to_file”)


# R packages can be found in online repositories
# Comprehensive R Archive Network is at … http://cran.r-project.org 
# Package (contains functions)
# Function (creates objects)

# NOTE : Basic functions are available by default with R install
# SASxport is a package which contains the following functions
# Read.xport is a function to import SAS Xport files(.xpt)
# lookup.xport is a function lookup .xpt file information
# Write.xport is a function to create SAS .xpt file

# 
# Installing packages … use the function install.packages()
# To install the cluster package … > install.packages(‘SASxport’)
# R will respond with the directory (or library) the package files are installed in and whether the package was installed successfully
install.packages()

# 
# Loading and unloading packages
# Before you can use a package you have to load it into R
# Use the library() function … > library(‘SASxport’) 
# You are now ready to use functions of this package
# To unload a package use the detach() function …  > detach(package:SASxport) 

# ------------

# R Data Types
# 
# Integer      
# Double
# Character
# Logical
# Complex
# Raw

a <- c(1L,2L,3L)
typeof(a)

b <- c(1,2,3)
b
b <- c(1:10)
b
typeof(b)

d <- c("a","b","c")
typeof(d)

e <- c(T,F)
typeof(e)

# Lists

li <- list(a,b,d,e)
li
li[[1]]
li[[4]]


li_named <- list(col1=a,col2=b,col3=d,col4=e)
li_named
li_named$col3

# Vector contains values of same data type
# Examples of vector are : 
#   Values of SAS dataset variable
# Values of a Oracle table column

# Functions to get Properties of a vector
# typeof()   	# what data type are the values in the vector

# length()    	# how many values are there in the vector
length(a)
length(li_named)

# names() 	# what are the names of individual vector elements
names(li_named)

# Functions to test data type of a vector
# is.integer(), is.double(), is.character(), is.logical()
is.numeric

# is.numeric() is true for both integer and double


### Vector Data Type Conversion
# Functions to convert from one data type to other
# as.integer(), as.double(), as.character(), as.logical()
# 
# Suppose you read an Excel column that has the following values, into a vector name : ev
# 10,20.3,abc,TRUE

f <- c(10,20.3,"abc",TRUE)
typeof(f)

# Coercion occurs in the following order:
#   logical >  integer > double > character


#### Data Frames

# A data frame is the most common way of storing data in R
# A data frame is a list of equal-length vectors
# It has a 2-dimensional structure like a matrix
# It shares properties of both the matrix and the list
# You can subset a data frame like a 1d structure (where it behaves like a list),
#                                  or a 2d structure (where it behaves like a matrix).
# Data frames are similar to Excel Files, SAS datasets and Oracle tables

d <- data.frame(id=c(1,2,3,4,5),state=c("ny","nj","ct","nc","vt"),capital=c("albany","trenton",
                                                                            "hartford","raleigh","montpelier"))

d2 <- data.frame(id=c(1,2,3,4,5),state=c("ny","nj","ct","nc","vt"),capital=c("albany","trenton",
                                                                             "hartford","raleigh","montpelier"), stringsAsFactors = FALSE)

# Some useful commands about data frames
str(d)	# gives overall structure of data frame
# similar to PROC CONTENTS in SAS

str(d2) # Characters instead of factors

names(d)
colnames(d) # gives column names
rownames(d) 	# gives row names
ncol(d)		# gives number of columns
nrow(d) 		# gives number of rows


# Six types of subsetting
# with positive integers
# with negative integers
# with a logical vector
# with nothing
# with zero
# with character vector

# This is the data.frame d2

d2
#    id state    capital
# 1  1    ny     albany
# 2  2    nj    trenton
# 3  3    ct   hartford
# 4  4    nc    raleigh
# 5  5    vt montpelier


# Subsetting with positive integers
d2[1,2] # Row Number 1, Column Number 2 => ny
d2[5,3] # montpelier
d2[3:5,3] # "hartford"   "raleigh"    "montpelier"

# Subsetting with negative integers
d2[-1,2] # Return everything 'but' - it is a negation, i.e., all state values except "ny"

# Subsetting with a logical vector
d2$state         # [1] "ny" "nj" "ct" "nc" "vt"
d2$state == "ny" # [1]  TRUE FALSE FALSE FALSE FALSE
d2[d2$state == "ny",]
d2[d2$state %in% c("ny","ct"),]

d2[d2$state %in% c("ny","ct"),"state"]
d2[d2$state %in% c("ny","ct"),c("id","state")]

# Subsetting with a character vector
d2[,c("state","capital")]
d2[,c(2,3)]

d2$state
d2$capital

######

# Reading Files Into R

# dframe1 <- read.csv(“filename”) # to read csv files with header record
# dframe1 <- data.frame(fread(“filename”)) # to read large csv files

# SAS xport files
# dframe2 <- read.xport(“filename”) # make sure you included SASxport package

# XLSX files
# dframe3 <- read.xlsx(“filename”,1) # This will read sheet 1; make sure xlsx package is included 
# 
# NOTE : Run str(read.csv), str(read.xport), str(read.xlsx) to know more about other options
# Run ?read.csv for some examples
# Search in google for more examples

?read.csv
??csv

# Other commands
str(iris) # Structure (The iris dataset is available within R)
summary(iris)

library(psych)
describe(iris)

# Plotting

plot(iris$Sepal.Length,iris$Sepal.Width)
library(ggplot2)
?qplot
qplot(Sepal.Length,Sepal.Width, data=iris, colour = Species) # Multivariate Visualisations

iris$Year <- sample(c("2015","2016"),nrow(iris), replace=T)
head(iris)

qplot(Sepal.Length,Sepal.Width, data=iris, colour = Species, facets = Species ~ Year) # Faceted Visualisations
qplot(Sepal.Length,Sepal.Width, data=iris, colour = Species, facets = Species ~ Year, geom=c("boxplot","jitter")) # Faceted Visualisations
qpl <- qplot(Sepal.Length,Sepal.Width, data=iris, colour = Species, facets = Species ~ Year) 
qpl + geom_smooth(method="lm") # Add a regression line


library(data.table) # Load the library data.table

# Openpayments Dataset - https://www.cms.gov/openpayments/

system ("ls -lh .") # 1.7 GB 
system ("head cms.psv")

system.time(cms <- fread("cms.psv", sep="|")) # Read 8.5 million rows, 16 secs

# Read 8543511 rows x 18 columns from 1.662GB file in 00:19.761 wall clock time (can be slowed down by any other open apps even if seemingly idle)
# Rereading 3 columns due to out-of-sample type exceptions.
# Reread 8543511 rows x 3 columns in 00:04.432
# user  system elapsed
# 39.968   0.436  24.215

set.seed(10)
cms_1000 <- cms[sample(nrow(cms),1000)]
cms_1000

fwrite(cms_1000,"cms_1000.psv",sep="|")

View(head(cms))
View(tail(cms))

(cms$Manufacturer_Name[1:10]) # Some are lowercase, others uppercase
toupper(cms$Manufacturer_Name[1:10])

# Remember assignments with <- ?
# Just like we can assign variables, we can use <- to substitute just about everything else

a <- (cms$Manufacturer_Name[1:10])
a
toupper(a)
b <- toupper(a)
b

cms$Manufacturer_Name2 <- (cms$Manufacturer_Name) # Adding a column to the cms data.table (~ data.frame)
cms
cms$Manufacturer_Name2 <- toupper(cms$Manufacturer_Name)
cms
cms$Manufacturer_Name2 <- NULL
cms


# With snippets/examples from -
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html

# Prints 100 by default
cms
getOption("datatable.print.nrows")
str(cms)

# a) What is data.table? (Already covered)
# b) General form - in what way is a data.table enhanced?

# In contrast to a data.frame, you can do a lot more than just subsetting rows and selecting 
# columns within the frame of a data.table, i.e., within [ ... ]. To understand it we will 
# have to first look at the general form of data.table syntax, as shown below:
#   
#   DT[i, j, by]
# 
# ##   R:      i                 j        by
# ## SQL:  where   select | update  group by
# Users who have a SQL background might perhaps immediately relate to this syntax.
# 
# The way to read it (out loud) is:
#   
#   Take DT, subset rows using i, then calculate j, grouped by by.

# Subset using rows in i in DT[i,j,by]

# Find all the doctors in Durham, NC who were paid by Pfizer in January 2015

head(cms$Date_Of_Payment)
typeof(cms$Date_Of_Payment)

temp <- cms$Date_Of_Payment[1:5]

library(lubridate)
year(temp)
month(temp)

cms[,Year:=year(Date_Of_Payment)]
cms[,Month:=month(Date_Of_Payment)]
cms[,Date:=day(Date_Of_Payment)]

cms[,Manufacturer_Name:=toupper(Manufacturer_Name)]
cms[,State:=toupper(State)]
cms[,City:=toupper(City)]

View(head(cms))
View(tail(cms))

# c) Subset rows in i

# – Sort cms first by column Manufacturer_Name in ascending order, and then by Product_1 in descending order:
ans <- cms[order(Manufacturer_Name, -Product_1)]
head(ans)

ans <- cms[order(-Manufacturer_Name, Product_1)]
head(ans)

# – Get the first two rows from cms
cms[1:2]


# d) Select column(s) in j
# – Select Total_Amount column, but return it as a vector.

ans <- cms[, Total_Amount]
head(ans)


# – Select Total_Amount column, but return as a data.table instead.

ans <- cms[, list(Total_Amount)]
head(ans)

# We wrap the variables (column names) within list(), which ensures that a data.table is returned.
# In case of a single column name, not wrapping with list() returns a vector instead, as seen in the previous example.
# 
# data.table also allows using .() to wrap columns with. It is an alias to list(); they both mean the same.
# Feel free to use whichever you prefer.
# 
# We will continue to use .() from here on.
# 
# data.tables (and data.frames) are internally lists as well, but with all its columns of equal length and with a 
# class attribute. Allowing j to return a list enables converting and returning a data.table very efficiently.
# 
# Tip:
#   
#   As long as j-expression returns a list, each element of the list will be converted to a column in the 
# resulting data.table. This makes j quite powerful, as we will see shortly.
# 
# – Select both Manufacturer_Name and Product_1 columns.
# 

ans <- cms[, .(Manufacturer_Name, Product_1)]
head(ans)


# – Select both Manufacturer_Name and Product_1 columns and rename them to Manufacturer and Product
# 
# Since .() is just an alias for list(), we can name columns as we would while creating a list.

ans <- cms[, .(Manufacturer = Manufacturer_Name, Product = Product_1)]
head(ans)

# e) Compute or do in j
# 
# – How many Single Payments (Total_Payment) were greater than $ 1000

ans <- cms[, sum((Total_Amount/Number_Of_Payments) > 1000)]
ans

# What’s happening here?
# 
# data.table’s j can handle more than just selecting columns - it can handle expressions, i.e., compute on columns.
# This shouldn’t be surprising, as columns can be referred to as if they are variables. Then we should be able to compute
# by calling functions on those variables. And that’s what precisely happens here.


# f) Subset in i and do in j
# 
# – Calculate the average payment and number of payments in Durham, NC
# 
ans <- cms[State == "NC" & City == "DURHAM", .(m_pmt = mean(Total_Amount), s_pmt = .N)]
head(ans)

# – How many payments have been made by Glaxo in NC in the month of January?

ans <- cms[State=="FL" & Manufacturer_Name %like% "*ASTRAZENECA PHARMACEUTICALS LP*" & Month == 1]
head(ans)
nrow(ans)
ans

ans <- cms[State=="FL" & Manufacturer_Name %like% "*ASTRAZENECA PHARMACEUTICALS LP*" & Month == 1, length(Number_Of_Payments)]
ans

# Special symbol .N:
#   
#   .N is a special in-built variable that holds the number of observations in the current group. It is particularly 
# useful when combined with by as we’ll see in the next section. In the absence of group by operations, it simply 
# returns the number of rows in the subset.

ans <- cms[State=="NC" & Manufacturer_Name %like% "*GLAXO*" & Month == 6, .N]
ans

# g) Great! But how can I refer to columns by names in j (like in a data.frame)?
# 
# You can refer to column names the data.frame way using with = FALSE.
# 
# – Select both Manufacturer_Name and Total_Amount columns the data.frame way.

ans <- cms[, c("Manufacturer_Name", "Total_Amount"), with = FALSE]
ans


# 2. Aggregations
# 
# We’ve already seen i and j from data.table’s general form in the previous section. In this section, we’ll 
# see how they can be combined together with by to perform operations by group. Let’s look at some examples.
# 
# a) Grouping using by
# 
# – How can we get the numer of payments corresponding to each Manufacturer_Name?

ans <- cms[, .(.N), by = .(Manufacturer_Name)]
ans
ans[order(-N)]

## or equivalently using a character vector in 'by'
ans <- cms[, .(.N), by = "Manufacturer_Name"]
ans

# When there’s only one column or expression to refer to in j and by, we can drop the .() notation. 
# This is purely for convenience. We could instead do:

ans <- cms[, .N, by = .(Manufacturer_Name)]

# – How can we get the total amount paid for each Manufacturer & Product pair for State NC?

ans <- cms[State == "NC", list(Net_Amount = sum(Total_Amount)), by = .(Manufacturer_Name,Product_1)]
ans
ans[order(-Net_Amount)]

# Manufacturer_Name   Product_1 Net_Amount
# 1:               JANSSEN PHARMACEUTICALS, INC    Invokana 1023351.97
# 2:               JANSSEN PHARMACEUTICALS, INC     Xarelto  957547.69
# 3:                               ABBVIE, INC.      Humira  830594.91
# 4:             ASTRAZENECA PHARMACEUTICALS LP    BYDUREON  486751.08
# 5:       NOVARTIS PHARMACEUTICALS CORPORATION     GILENYA  438794.53

# – How can we get the total amount paid for each Manufacturer?

ans <- cms[,list(Net_Amount = sum(Total_Amount)), by = .(Manufacturer_Name)]
ans
ans[order(-Net_Amount)]

# > ans[order(-Net_Amount)]
# Manufacturer_Name  Net_Amount
# 1:    JANSSEN PHARMACEUTICALS, INC 48128388.29
# 2:  ASTRAZENECA PHARMACEUTICALS LP 46599395.45
# 3:                    ABBVIE, INC. 37623223.78
# 4: MERCK SHARP & DOHME CORPORATION 30918563.63
# 5:                     PFIZER INC. 30911557.35

# Taking Janssen as the manufacturer, which Physician got the highest payment
ans <- cms[Manufacturer_Name=="JANSSEN PHARMACEUTICALS, INC", .(Net_Amount=sum(Total_Amount)), by=.(Physician_ID)]
ans
ans[order(-Net_Amount)]

# > ans[order(-Net_Amount)]
# Physician_ID Net_Amount
# 1:       159839  147729.38
# 2:       176230  138159.48

cms[Physician_ID %in% c(159839,176230)]

# https://www.doximity.com/pub/richard-aguilar-md/1

cms[Physician_ID %in% c(159839,176230)][Physician_ID==159839,.(Net_Amount=sum(Total_Amount)),by=.(Product_1)][order(-Net_Amount)]

# > cms[Physician_ID %in% c(159839,176230)][Physician_ID==159839,.(Net_Amount=sum(Total_Amount)),by=.(Product_1)][order(-Net_Amount)]
# Product_1 Net_Amount
# 1:        Invokana  147822.37
# 2:       JARDIANCE   81799.97
# 3:          Nesina   52418.04
# 4:       TRULICITY   32130.91

# Go to http://www.investor.jnj.com/releasedetail.cfm?releaseid=960525
# First Real-World Evidence Comparing an SGLT2 Inhibitor with DPP-4 Inhibitors Shows Adults with Type 2 Diabetes Achieve
# Greater Blood Glucose Control with INVOKANA® (canagliflozin)
# ... "This real world analysis complements findings from the pivotal trials that informed the approval of INVOKANA®,
# as it provides insights to physicians on how these medicines are performing post-approval for people living with type 2 
# diabetes," said study co-author Richard Aguilar, M.D.*, Medical Director of Diabetes Nation.

# – How can we get the average Payment and Number of Payments for each Manufacturer, Product pair for each month for State NC?

ans <- cms[State == "NC",
           .(MEANPMT=mean(Total_Amount), MEANNUM=mean(Number_Of_Payments)),
           by = .(Manufacturer_Name, Product_1, Month)]
ans

# Now what if we would like to order the result by those grouping columns Manufacturer_Name, Product_1 and Month?
# 
# b) keyby

# data.table retaining the original order of groups is intentional and by design. There are cases when preserving the 
# original order is essential. But at times we would like to automatically sort by the variables we grouped by.
# 
# – So how can we directly order by all the grouping variables?

ans <- cms[State == "NC",
           .(MEANPMT=mean(Total_Amount), MEANNUM=mean(Number_Of_Payments)),
           keyby = .(Manufacturer_Name, Product_1, Month)]
head(ans)

# c) Chaining
# 
# Let’s reconsider the task of getting the total number of payments for each Manufacturer_Name, Product_1 pair for State “NC”.
# 
ans <- cms[State == "NC", .N, by = .(Manufacturer_Name, Product_1)]
ans
# – How can we order ans using the column N in ascending order, and Manufacturer_Name in descending order?
# 
# We can store the intermediate result in a variable, and then use order(Manufacturer_Name, -Product_1) on that variable.
# It seems fairly straightforward.
# 
ans <- ans[order(N,-Manufacturer_Name)]
ans

# Perhaps a bit more interesting result would have been had by ordering just by N
ans <- ans[order(-N)]
ans

# We can chain this entire expression --
cms[State == "NC", .N, by = .(Manufacturer_Name, Product_1)][order(-N)]


# To find all physicians in Durham who received compensation (in cash/kind) in 2015

dt1 <- cms[State == "NC" & City == "DURHAM"]
head(dt1)

dt1[,list(Physician_ID, First_Name, Last_Name, Address_Line1, Manufacturer_Name)] # Extract just a few fields

# Which manufacturers made the most number of payments
dt1[,.N,Manufacturer_Name]
dt1[,.N,Manufacturer_Name][order(-N)] # Chaining
head(dt1[,.N,Manufacturer_Name][order(-N)],25) # Top 25

# Payments by Product
dt1[,.N,by=Product_1][order(-N)]

# By Manufacturer, Product
dt1[,.N,by=c("Manufacturer_Name","Product_1")][order(-N)] # Chaining
dt1[,.(.N, Net_Paid=sum(Total_Amount)),by=c("Manufacturer_Name","Product_1")][order(-N)] # Chaining

# > dt1[,.(.N, Net_Paid=sum(Total_Amount)),by=c("Manufacturer_Name","Product_1")][order(-N)] # Chaining
#                   Manufacturer_Name   Product_1   N  Net_Paid
# 1: Merck Sharp & Dohme Corporation Non-Product 241 162324.25
# 2:  Teva Pharmaceuticals USA, Inc.     ZECUITY 188 113921.99
# 3:    Janssen Pharmaceuticals, Inc     Xarelto 181  81982.56
# 4:                    AbbVie, Inc.      Humira 176  46693.74
# 5:                     Pfizer Inc.     ELIQUIS 169 162960.57


# d) Expressions in by

# – Can by accept expressions as well or just take columns?
# 
# Yes it does. As an example, if we would like to find out how many payments were > 1000 and City was not Durham in NC,
# started and arrived late etc…

ans <- cms[State=="NC", .N, .(TOTAMTMORETHAN1000 = Total_Amount>1000, CITYISDURHAM=City=="DURHAM")]
ans

# Or, just group-by on City without using the City==DURHAM expression
# i.e., which cities received the most payment where Payment Amount was > 1000
ans <- cms[State=="NC", .N, .(TOTAMTMORETHAN1000 = Total_Amount>1000, City)]
ans

ans[TOTAMTMORETHAN1000==TRUE][order(-N)]

# > ans[TOTAMTMORETHAN1000==TRUE][order(-N)]
# TOTAMTMORETHAN1000          City    N
# 1:               TRUE     CHARLOTTE 1209
# 2:               TRUE        DURHAM 1025
# 3:               TRUE       RALEIGH  788
# 4:               TRUE    GREENSBORO  518


# e) Multiple columns in j - .SD
# 
# – Do we have to compute mean() for each column individually?
# 
# It is of course not practical to have to type mean(myCol) for every column one by one. What if you had a 100 columns 
# to compute mean() of?
# 
# How can we do this efficiently? To get there, refresh on this tip - “As long as j-expression returns a list, each 
# element of the list will be converted to a column in the resulting data.table”. Suppose we can refer to the data 
# subset for each group as a variable while grouping, then we can loop through all the columns of that variable using 
# the already familiar base function lapply(). We don’t have to learn any new function.
# 
# Special symbol .SD:
#   
#   data.table provides a special symbol, called .SD. It stands for Subset of Data. It by itself is a data.table that 
# holds the data for the current group defined using by.
# 
# Recall that a data.table is internally a list as well with all its columns of equal length.
# 
# Let’s use the data.table cms from before to get a glimpse of what .SD looks like.
cms
cms[,print(.SD)]

# To compute on (multiple) columns, we can then simply use the base R function lapply().
cms[State=="NC", lapply(.SD, mean), by = Physician_ID, .SDcols=c("Total_Amount","Number_Of_Payments")]

# f) Subset .SD for each group:
#   
#   – How can we return the first two rows for each month?

ans <- cms[, head(.SD, 2), by = Month]
head(ans)

# > head(ans) # Month ==> 1, 2, etc
#    Month   month Date_Of_Payment Physician_ID First_Name Last_Name            Address_Line1        City State   Zip
# 1:     1 2015-01      2015-01-01       228847     ROBERT   FELDMAN           6141 SUNSET DR SOUTH MIAMI    FL 33143
# 2:     1 2015-01      2015-01-01       282119    WILLIAM        OH          1190 5TH AVENUE    NEW YORK    NY 10029
# 3:     2 2015-02      2015-02-01        57748     KENDRA     GRUBB  201 ABRAHAM FLEXNER WAY  LOUISVILLE    KY 40202
# 4:     2 2015-02      2015-02-01        85220      DAVID   GREGORY            415 E ASH AVE     DECATUR    IL 62526


# g) Why keep j so flexible?
# 
# So that we have a consistent syntax and keep using already existing (and familiar) base functions instead of learning
# new functions. To illustrate, let us use the data.table DT we created at the very beginning under What is a data.table?
# section.
# 
# – How can we concatenate columns Manufacturer_Name and Product_1 for each City in NC?

ans <- cms[State=="NC", .(val = paste(Manufacturer_Name,"--",Product_1,sep=" ")), by = City]
head(ans)
ans[val %like% "*PURDUE*"]

# If you wanted to save this updated information
?fwrite
#fwrite(cms,file="cms2.psv",sep="|")





################### Reshaping Data with data.table
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html

# Creating long and wide tables
cms <- fread("cms.psv", sep="|")

# 1. Default functionality
# 
# a) melting data.tables (wide to long)

cms

# - Convert cms to long form where each Product (eg., Product_1 and Product_2) is a separate observation.
# This will create a 17M row table!
# 
# We could accomplish this using melt() by specifying id.vars and measure.vars arguments as follows:

cms.m1 = melt(cms, id.vars = c("Manufacturer_Name", "Total_Amount"),                 
              measure.vars = c("Product_1", "Product_2"))


# measure.vars specify the set of columns we would like to COLLAPSE (or combine) together.
# 
# We can also specify column indices instead of names.
# 
# By default, variable column is of type factor. Set variable.factor argument to FALSE if you’d like to 
# return a character vector instead. variable.factor argument is only available in melt from data.table and 
# not in the reshape2 package.
# 
# By default, the molten columns are automatically named variable and value.
# 
# melt preserves column attributes in result.

# > cms.m1
#                  Manufacturer_Name Total_Amount  variable                         value
# 1: Merck Sharp & Dohme Corporation      2500.00 Product_1                      NUVARING
# 2:         SANOFI US SERVICES INC.      2200.00 Product_1                       JEVTANA
# 3: Regeneron Pharmaceuticals, Inc.       315.20 Product_1 EYLEA (aflibercept) Injection
# 4:         SANOFI-AVENTIS U.S. LLC      6500.00 Product_1                       ZALTRAP
# 5:         SANOFI-AVENTIS U.S. LLC      4515.00 Product_1                        AUVI-Q
# ---                                                                                     
# 17087018:            Egalet US Inc        12.33 Product_2                              
# 17087019:            Egalet US Inc        12.33 Product_2                              


# - Name the variable and value columns to Product_1_2 and Total_Payment respectively

cms.m1 = melt(cms, measure.vars = c("Product_1", "Product_2"),
              variable.name = "Product_1_2", value.name = "Product_Name")
cms.m1

# Notice that since I didn't specify id.vars - all the other columns were included

# By default, when one of id.vars or measure.vars is missing, the rest of the columns are automatically 
# assigned to the missing argument.
# 
# When neither id.vars nor measure.vars are specified, as mentioned under ?melt, all non-numeric, integer, 
# logical columns will be assigned to id.vars.
# 
# In addition, a warning message is issued highlighting the columns that are automatically considered to 
# be id.vars.


cms.m1 = melt(cms, id.vars = c("Manufacturer_Name", "Total_Amount"),                 
              measure.vars = c("Product_1", "Product_2"))
cms.m1

# b) Casting data.tables (long to wide)
# 
# In the previous section, we saw how to get from wide form to long form. Let’s see the 
# reverse operation in this section.
# 
# - How can we get back to the original data table cms from cms.m1

# That is, we’d like to collect all Product observations corresponding to each 
# Manufacturer_Name, Total_Amount together under the same row. We can accomplish it using 
# dcast as follows:

# This works fine
dcast.data.table(cms.m1[1:2], Manufacturer_Name + value ~ variable, value.var = "Total_Amount")

# This as well
dcast.data.table(cms.m1[1:5], Manufacturer_Name + value ~ variable, value.var = "Total_Amount")

# But, not this
dcast.data.table(cms.m1[1:9], Manufacturer_Name + value ~ variable, value.var = "Total_Amount")

cms.m1[1:9] # There are 2 entries for:

# 8:   Ipsen Biopharmaceuticals, Inc       3000.0 Product_1   Somatuline Depot 120mg-.5ml
# 9:   Ipsen Biopharmaceuticals, Inc       3000.0 Product_1   Somatuline Depot 120mg-.5ml

# dcast doesn't know what to enter in the value.var, you could use functions, eg.,
dcast.data.table(cms.m1[1:9], Manufacturer_Name + value ~ variable, 
                 value.var = "Total_Amount", fun.aggregate = sum)

# 2:   Ipsen Biopharmaceuticals, Inc   Somatuline Depot 120mg-.5ml    6000.0

# Or, get only unique rows:
dcast.data.table(unique(cms.m1), Manufacturer_Name + value ~ variable, 
                 value.var = "Total_Amount", fun.aggregate = sum)


# Or, if you indeed want the original back, just add a column with say, the row number
# Such that each row becomes unique

cms.m2 <- cms.m1[1:9][,Row:=seq(1:9)]
cms.m2

dcast.data.table(cms.m2, Manufacturer_Name + value + Row ~ variable, 
                 value.var = "Total_Amount")

# Manufacturer_Name                         value Row Product_1
# 1:                 Genentech, Inc.            NonCovered Product   7     370.0
# 2:   Ipsen Biopharmaceuticals, Inc   Somatuline Depot 120mg-.5ml   8    3000.0
# 3:   Ipsen Biopharmaceuticals, Inc   Somatuline Depot 120mg-.5ml   9    3000.0

# 3. Enhanced (new) functionality
# 
# a) Enhanced melt
# 
# Since we’d like for data.tables to perform this operation straightforward and efficient 
# using the same interface, we went ahead and implemented an additional functionality, 
# where we can melt to multiple columns simultaneously.
# 
# - melt multiple columns simultaneously

cms
prods <- paste("Product_",1:2,sep="")
cms.m2 <- melt(cms[8:10], id.vars = c("Manufacturer_Name"), measure.vars = list(prods,"State"),
               value.var = c("Manufacturer_Name","Specialty_Group"))
str(cms.m2)
cms.m2

# variable -> Factor 1 = Product_1, Factor 2 = Product_2, Factor 3 = State
# Multiple value columns generated simultaneously

# > cms.m2
#               Manufacturer_Name  variable                      value1 value2
# 1: Ipsen Biopharmaceuticals, Inc        1 Somatuline Depot 120mg-.5ml     FL
# 2: Ipsen Biopharmaceuticals, Inc        1 Somatuline Depot 120mg-.5ml     FL
# 3: Ipsen Biopharmaceuticals, Inc        1 Somatuline Depot 120mg-.5ml     FL
# 4: Ipsen Biopharmaceuticals, Inc        2                                 NA
# 5: Ipsen Biopharmaceuticals, Inc        2                                 NA
# 6: Ipsen Biopharmaceuticals, Inc        2                                 NA
# 7: Ipsen Biopharmaceuticals, Inc        3                          FL     NA
# 8: Ipsen Biopharmaceuticals, Inc        3                          FL     NA
# 9: Ipsen Biopharmaceuticals, Inc        3                          FL     NA

# b) Enhanced dcast
# 
# Okay great! We can now melt into multiple columns simultaneously. Now given the data set 
# DT.m2 as shown above, how can we get back to the same format as the original data we started 
# with?
# 
# If we use the current functionality of dcast, then we’d have to cast twice and bind the 
# results together. But that’s once again verbose, not straightforward and is also inefficient.
# 
# - Casting multiple value.vars simultaneously
# 
# We can now provide multiple value.var columns to dcast for data.tables directly so that the operations are taken care of internally and efficiently.

## new 'cast' functionality - multiple value.vars
cms.m1 = melt(cms[1:100], id.vars = c("Physician_ID", "First_Name","Last_Name","City","Total_Amount","Date_Of_Payment"),                 
              measure.vars = list(c("Product_1", "Product_2"),"State"),
              value.name = c("Product","State"))
cms.m1
#                        Product_1 Product_2 State
# 1:                      NUVARING              FL
# 2:                       JEVTANA              NY
# 3: EYLEA (aflibercept) Injection              MA
# [...]

dcast(cms.m1, Physician_ID + First_Name + Last_Name + City + Total_Amount + 
        Date_Of_Payment ~ variable, value.var = c("Product","State"))

# Finally, multiple aggregations are also possible
# Can be quite intensive, use carefully

cms.m1 <- dcast(cms, Manufacturer_Name ~ State,
                value.var = "Total_Amount", fun=list(max,mean), fill=0)

cms.m1[Manufacturer_Name %like% c("Astellas")] # Two columns for max and mean

# Next Section: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-keys-fast-subset.html

# Read NHS File
nhs <- fread("nhs201701.csv")
nhs

# Names have spaces
setnames(nhs,c("SHA","PCT","PRACTICE","BNF_CODE","BNF_NAME","ITEMS","NIC","ACT_COST","QUANTITY","PERIOD","V1"))

# Delete the last column
nhs[,V1:=NULL]

# Set BNF NAME to lower
nhs[,BNF_NAME:=tolower(BNF_NAME)]


# Load Bnft Practices File)
prac <- fread("nhsbnft201701.csv")
setnames(prac,c("PERIOD","PRACTICE","PRAC_NAME","PRAC_ADDR1","PRAC_ADDR2","TOWN","COUNTY","POSTCODE"))

# 1. Keys
# 
# a) What is a key?
# 
# In the “Introduction to data.table” vignette, we saw how to subset rows in i 
# using logical expressions, row numbers and using order(). In this section, we 
# will look at another way of subsetting incredibly fast - using keys.
# 

# Keys and their properties
# 
# We can set keys on multiple columns and the column can be of different types – 
# integer, numeric, character, factor, integer64 etc. list and complex types are not supported yet.
# 
# Uniqueness is not enforced, i.e., duplicate key values are allowed. Since rows are sorted by 
# key, any duplicates in the key columns will appear consecutively.
# 
# Setting a key does two things:
#   
# - physically reorders the rows of the data.table by the column(s) provided by reference, 
# always in increasing order.
# 
# - marks those columns as key columns by setting an attribute called sorted to the data.table.
# 
# Since the rows are reordered, a data.table can have at most one key because it can not be 
# sorted in more than one way.
# 

# b) Set, get and use keys on a data.table
# 
# – How can we set the column PRACTICE as key in the data.table nhs?

setkey(nhs, PRACTICE) # The PRACTICE (ID) is being set to a key
head(nhs)

## alternatively we can provide character vectors to the function 'setkeyv()'
# setkeyv(cms, "PRACTICE") # useful to program with

# Important
# set* and :=:
#   
#   In data.table, the := operator and all the set* (e.g., setkey, setorder, setnames etc..)
# functions are the only ones which modify the input object by reference.
# 
# Once you key a data.table by certain columns, you can subset by querying those key columns 
# using the .() notation in i. Recall that .() is an alias to list().

# – Use the key column origin to subset all rows where the origin airport matches “JFK”

nhs[.("Y04634")] # Retrieving Rows by 'key'
nhs[,.(PRACTICE)] # This is different

## alternatively
# nhs[J("Y04634")] # or
# nhs[list("Y04634")]

# Multiple
nhs[.("A81001","Y05693")]

# – How can we get the column(s) a data.table is keyed by?
# 
# Using the function key().

key(nhs)

# c) Keys and multiple columns
# 
# To refresh, keys are like supercharged row names. We can set key on multiple columns and they
# can be of multiple types.
# 
# – How can I set keys on both PRACTICE and BNF_NAME columns?

setkey(nhs, PRACTICE, BNF_NAME)
head(nhs)

## or alternatively
# setkeyv(nhs, c("PRACTICE", "BNF_NAME")) # provide a character vector of column names

key(nhs)

# [1] "PRACTICE" "BNF_NAME"

# It sorts the data.table first by the column origin and then by dest by reference.

# – Subset all rows using key columns where first key column PRACTICE matches “A81001” and second key column BNF_NAME matches “Aciclovir_Tab 400mg”

nhs[.("A81001","Aciclovir_Tab 400mg")]
nhs[.(unique(PRACTICE),"Aciclovir_Tab 400mg")] # unique(nhs$PRACTICE) = 9781, hence, shows 9781 rows

# 2) Combining keys with j and by
# 
# All we have seen so far is the same concept – obtaining row indices in i, but just using a 
# different method – using keys. It shouldn’t be surprising that we can do exactly the same things
# in j and by as seen from the previous vignettes. We will highlight this with a few examples.
# 
# a) Select in j
# 
# – Return ACT_COST column as a data.table corresponding to PRACTICE = "A81014" and BNF_NAME = cymbalta_cap g/r 60mg.

key(nhs)
# [1] "PRACTICE" "BNF_NAME"
prac[PRACTICE=="A81014"]
nhs[.("A81014", "cymbalta_cap g/r 60mg"), .(ACT_COST)]


# c) Compute or do in j
# 
# – Find the maximum NIC (Net Ingredient Cost) correspondong to BNF_NAME = cymbalta_cap g/r 60mg.

nhs[.("A81014", "cymbalta_cap g/r 60mg"), .(max(NIC))]

nhs[.(unique(PRACTICE), "cymbalta_cap g/r 60mg"), .(max(NIC))] # Since there are NAs !
nhs[.(unique(PRACTICE), "cymbalta_cap g/r 60mg"), .(max(NIC, na.rm=TRUE))] # Remove NAs

# e) Aggregation using by
# Let's set the key to only BNF_NAME

setkey(nhs, BNF_NAME)

# – Get the maximum ACT_COST delay for each month corresponding to BNF_NAME = "cymbalta_cap g/r 60mg". Order the result by PRACTICE

ans <- nhs["cymbalta_cap g/r 60mg", max(ACT_COST), keyby = PRACTICE]
ans


# Further Reading --> https://cran.r-project.org/web/packages/data.table/vignettes/datatable-keys-fast-subset.html

# Merging

chem <- fread("nhschemsubs201701.csv")
chem[,V3:=NULL]
setnames(chem,c("CHEM_SUB","CHEMNAME"))

nhs[,BNFCHAR:=nchar(BNF_CODE)]
getv <- function(x){if(x==15){9}else{4}}
endindex <- unlist(lapply(nhs$BNFCHAR,getv))
nhs$endindex <- endindex

nhs[,CHEM_SUB:=substr(BNF_CODE,0,endindex)]
nhs

setkey(nhs,CHEM_SUB)
setkey(chem,CHEM_SUB)

merged <- merge(nhs,chem,all.x=TRUE)
merged

merged[CHEMNAME %like% "*Glucose"]

# We can do a further merge to create a final combined dataset
# We have already merged nhs with chem
# We can also merge this new dataset with the Practice data

merged <- merge(merged, prac, by="PRACTICE", all.x=TRUE)
merged



