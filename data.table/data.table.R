library(fortunes)
#---------------------------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/data.table/index.html
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro-vignette.html

#---------------------------------------------------------------------------------------------
library(data.table)

dt <- data.table(mtcars)

class(dt)
#[1] "data.table" "data.frame"

dt[,mean(mpg)]   # You can't do this with a normal data frame
#[1] 20.09062

mtcars[,mean(mpg)]  # Such a thing will not work with regular data frames
#Error in mean(mpg) : object 'mpg' not found


#--------------------------------------------------------------------------------------

tapply(mtcars$mpg,mtcars$am,mean)
dt[,mean(mpg),by=am]
t(dt[,mean(mpg),by=am])

# We could even extend this to group by am and cyl
dt[,mean(mpg),by=.(am,cyl)]

# If we want to more clearly label the computed average
dt[,.(avg=mean(mpg)),by=.(am,cyl)]



address(dt[,.(avg=mean(mpg)),by=.(am,cyl)])


rank
#--------------------------------------------------------------------

library(data.table)
##intro

DF = data.frame(x=c("b","b","b","a","a"),v=rnorm(5))
DF

DT = data.table(x=c("b","b","b","a","a"),v=rnorm(5))
DT

# to convert
CARS = data.table(cars)
head(CARS)

#It is often useful to see a list of all data.tables in memory:
tables()

sapply(DT, class)
#-------------

#keys

cat(try(DT["b",],silent =TRUE))


setkey(DT,x)
DT
DT["b",]

grpsize = ceiling(1e7/26^2)
grpsize

tt=system.time( DF <- data.frame(
  x=rep(LETTERS,each=26*grpsize),
  y=rep(letters,each=grpsize),
  v=runif(grpsize*26^2),
  stringsAsFactors=FALSE)
)
tt

dim(DF)
(tt=system.time(ans1 <- DF[DF$x=="R" & DF$y=="h",]))

head(DF,3)
tail(DF,3)

DT = as.data.table(DF)
system.time(setkey(DT,x,y))


tt=system.time(ans1 <- DF[DF$x=="R" & DF$y=="h",])   # vector search
ss=system.time(ans2<-DT[list("R","h")])   #binary search
head(ans2,3)
dim(ans2)

identical(ans1$v,ans2$v)

system.time(ans1 <- DT[x=="R" & y=="h",]) # works but is using data.table badly
system.time(ans2 <- DF[DF$x=="R" & DF$y=="h",]) # the data.frame way

mapply(identical,ans1,ans2)


identical( DT[list("R","h"),],
           DT[.("R","h"),])         #.() # the alias for list



#2 Fast grouping
DT[,sum(v)]

DT[,sum(v),by=x]
#DT[,sum(v),by=.(x,y)]

(ttt=system.time(tt <- tapply(DT$v, DT$x, sum)))
(sss=system.time(ss <- DT[,sum(v),by=x]))
head(tt)
head(ss)
identical(as.vector(tt),ss$V1)
#--------------------------------------
ttt=system.time(tt <- tapply(DT$v,list(DT$x,DT$y),sum)); ttt

sss=system.time(ss <- DT[,sum(v),by="x,y"]); sss



#----------------------------------

# Efficient reshaping using data.tables

DT<-fread("https://raw.githubusercontent.com/wiki/Rdatatable/data.table/data/melt_default.csv")
DT
str(DT)

DT.m1 = melt(DT, id.vars = c("family_id", "age_mother"), 
             measure.vars = c("dob_child1", "dob_child2", "dob_child3"))
DT.m1

#Name the variable and value columns to child and dob respectively

DT.m1 = melt(DT, id.vars = c("family_id", "age_mother"),
             measure.vars = c("dob_child1", "dob_child2", "dob_child3"), 
             variable.name = "child", value.name = "dob")
DT.m1

#b) Casting data.tables (long to wide)
dcast(DT.m1, family_id + age_mother ~ child, value.var = "dob")

dcast(DT.m1, family_id ~ ., fun.agg = function(x) sum(!is.na(x)), value.var = "dob")


# 2. Limitations in current melt/dcast approaches

#DT<-fread("https://raw.githubusercontent.com/wiki/Rdatatable/data.table/data/melt_enhanced.csv")
DT<-fread("melt_enhanced.csv")

DT

colA = paste("dob_child",1:3,sep="")
colB = paste("gender_child",1:3,sep="")
DT.m2 = melt(DT, measure = list(colA, colB), value.name = c("dob", "gender"))
DT.m2
str(DT.m2)

# using patterns is also an elegant way
DT.m2 = melt(DT, measure = patterns("^dob", "^gender"), value.name = c("dob", "gender"))
DT.m2

## new 'cast' functionality - multiple value.vars
DT.c2 = dcast(DT.m2, family_id + age_mother ~ variable, value.var = c("dob", "gender"))
DT.c2

#--------NEW FEATURE - multiple value.var and multiple fun.aggregate---------------------------------------------------------------------
# NEW FEATURE - multiple value.var and multiple fun.aggregate
dt = data.table(x=sample(5,20,TRUE), y=sample(2,20,TRUE), 
                z=sample(letters[1:2], 20,TRUE), d1 = runif(20), d2=1L)
# multiple value.var
dcast(dt, x + y ~ z, fun=sum, value.var=c("d1","d2"))
# multiple fun.aggregate
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var="d1")
# multiple fun.agg and value.var (all combinations)
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var=c("d1", "d2"))
# multiple fun.agg and value.var (one-to-one)
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var=list("d1", "d2"))
#-----------------------------------------------------------------------------------------------
#####################################################################
#####################################################################

#V Reference semantics
library(data.table)
flights <- fread("flights14.csv")
head(flights,20)

#a) Add columns by reference
#– How can we add columns speed and total delay of each flight to flights data.table?

flights[, `:=`(speed = distance / (air_time/60), # speed in km/hr
               delay = arr_delay + dep_delay)]   # delay in minutes
head(flights)

#b) Update some rows of columns by reference - sub-assign by reference

#get all 'hours' 
# get all 'hours' in flights
flights[, sort(unique(hour))]

#Replace those rows where hour == 24 with the value 0
# subassign by reference
flights[hour == 24L, hour := 0L]
#see
flights[hour == 24L, hour := 0L][]
# check again for '24'
flights[, sort(unique(hour))]

#----------------------------
#c) Delete column by reference
#– Remove delay column
flights[, c("delay") := NULL]
## or using the functional form
flights[, `:=`(delay = NULL)]  #  recommended

#d) := along with grouping using by 
# – How can we add a new column which contains for each orig,dest pair the maximum speed?

flights[, max_speed := max(speed), by=.(origin, dest)]  

head(flights)

#e) Multiple columns and :=
#– How can we add two more columns computing max() of dep_delay and arr_delay for each month, using .SD?
###attention!!!
in_cols  = c("dep_delay", "arr_delay")
out_cols = c("max_dep_delay", "max_arr_delay")
flights[, c(out_cols) := lapply(.SD, max), by = month, .SDcols = in_cols]
head(flights)  

# http://stackoverflow.com/questions/32276887/use-of-lapply-sd-in-data-table-r
#Before moving on to the next section, let’s clean up the newly created columns speed, max_speed, max_dep_delay and max_arr_delay.

# RHS gets automatically recycled to length of LHS
flights[, c("speed", "max_speed", "max_dep_delay", "max_arr_delay") := NULL]
head(flights)


#3) := and copy()
#a) := for its side effect
foo <- function(DT) {
  DT[, speed := distance / (air_time/60)]
  DT[, .(max_speed = max(speed)), by=month]
}
ans = foo(flights)
head(flights)
head(ans)

#-----------------------------------
flights[, speed := NULL]
foo <- function(DT) {
  DT <- copy(DT)                             ## copy
  DT[, speed := distance / (air_time/60)]    ## doesn't affect 'flights'
  DT[, .(max_speed = max(speed)), by=month]
}
ans <- foo(flights)
head(flights)
head(ans)

#-----------------------------------------
DT = data.table(x=1, y=2)
DT_n = names(DT)
DT_n
# [1] "x" "y"

## add a new column by reference
DT[, z := 3]

## DT_n also gets updated
DT_n
# [1] "x" "y" "z"

## use `copy()`
DT_n = copy(names(DT))
DT[, w := 4]

## DT_n doesn't get updated
DT_n
# [1] "x" "y" "z"




#-----------------------------------------------------------------------
# Keys and fast binary search based subset
library(data.table)

flights <- fread("flights14.csv")
head(flights)
dim(flights)

set.seed(1L)
DF = data.frame(ID1 = sample(letters[1:2], 10, TRUE), 
                ID2 = sample(1:3, 10, TRUE),
                val = sample(10), 
                stringsAsFactors = FALSE,
                row.names = sample(LETTERS[1:10]))
DF

rownames(DF)
DF["C", ]

rownames(DF) = sample(LETTERS[1:5], 10, TRUE)

DT = as.data.table(DF)
DT


#b) Set, get and use keys on a data.table
#– How can we set the column origin as key in the data.table flights?

setkey(flights, origin)
head(flights,30)

## alternatively we can provide character vectors to the function 'setkeyv()'
# setkeyv(flights, "origin") # useful to program with
flights["JFK"]              ## same as flights[.("JFK")]
flights[c("JFK", "LGA")]    ## same as flights[.(c("JFK", "LGA"))]

#– How can we get the column(s) a data.table is keyed by?
#Using the function key().
key(flights)


#c) Keys and multiple columns
#To refresh, keys are like supercharged rownames. We can set key on multiple columns and they can be of multiple types.
#– How can I set keys on both origin and dest columns?

setkey(flights, origin, dest)
head(flights)

## or alternatively
# setkeyv(flights, c("origin", "dest")) # provide a character vector of column names
key(flights)
flights[.("JFK", "MIA")]
flights[.(c("JFK", "LGA"), "MIA")]

key(flights)
# [1] "origin" "dest"

flights[.("JFK")] ## or in this case simply flights["JFK"], for convenience

#------------------------------------------------------------------------
#Subset all rows where just the second key column dest matches “MIA”
flights[.(unique(origin), "MIA")]
#----------------------------------------------------------

#2) Combining keys with j and by


#a) Select in j
#– Return arr_delay column as a data.table corresponding to origin = "LGA" and dest = "TPA".
key(flights)
flights[.("LGA","TPA"),.(arr_delay)]

#b) Chaining
#– On the result obtained above, use chaining to order the column in decreasing order.
flights[.("LGA","TPA"),.(arr_delay)][order(-arr_delay)]

#flights[.("LGA","TPA"),.(c(carrier,arr_delay,flight))][order(-arr_delay)]

flights[.("LGA","TPA"),.(carrier,arr_delay,flight)][order(-arr_delay)]

#c) Compute or do in j
#– Find the maximum arrival delay correspondong to origin = "LGA" and dest = "TPA".

flights[.("LGA", "TPA"), max(arr_delay)]


#d) sub-assign by reference using := in j
#We have seen this example already in the Reference semantics vignette. 
#Let’s take a look at all the hours available in the flights data.table:
# get all 'hours' in flights
flights[, sort(unique(hour))]

setkey(flights, hour)
key(flights)
# [1] "hour"
flights[.(0), hour := 24L] 
flights[, sort(unique(hour))]
#flights[,""]
key(flights)
# NULL

#e) Aggregation using by
#Let’s set the key back to origin, dest first.
setkey(flights,origin,dest)
key(flights)

#– Get the maximum departure delay for each month corresponding to origin = "JFK". 
#Order the result by month
ans <- flights["JFK",max(dep_delay),keyby = month]
head(ans)
ans <- flights["JFK",max(dep_delay),keyby = .(month,day)]
head(ans)



