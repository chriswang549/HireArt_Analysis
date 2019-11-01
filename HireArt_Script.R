#3. Simple Spreadsheet Analysis
##The marketing ops team sent over this spreadsheet containing four years of data from a CRM system
#(please make a copy of the spreadsheet in order to manipulate the data): 
 # https://docs.google.com/spreadsheets/d/16hLtx8bBDe2GS1MOa3v9hY6Yhm4C30koLoCpiIJ5WDg/edit?usp=sharing. 

#The team wants to find the month they're likely to contact the most clients,
#so they can schedule a product upgrade announcement. Which month does the team tend to contact the greatest percentage of its clients?

#(In addition to providing an answer, please either include a public link to a spreadsheet showing your work,
#or describe your process for answering the question.)


library(mice)
library(lattice)
library(dplyr)
library(plyr)
#read data
data <- read.csv("HireArtData.csv")
#observe data
glimpse(data)
# No missing data clean data!
md.pattern(data) 
summary(data) 
#  4 unique account managers
length(unique(data$Account.manager))
# 35 unique client names
length(unique(data$Client.Name)) 
#704 unique date of contacts
length(unique(data$Date.of.Contact))

# We want to find the month most likely to contact most clients
#split our date of contact into year month day
dataNew <- data %>%
  dplyr::mutate(year = lubridate::year(Date.of.Contact), 
                month = lubridate::month(Date.of.Contact), 
                day = lubridate::day(Date.of.Contact))
# 2013 - 2017
# Jan - December 
length(unique(dataNew$month))
# 31 unique days
length(unique(dataNew$day))      
summary(dataNew)
glimpse(dataNew)

# October comes up most frequently 213 vs 2nd most of Sept 121
# if an error with summarize comes up the plyr and dyplr packages are working against each other
count(dataNew$month)
count(dataNew$Client.Name)
length(dataNew$Client.Name)
# creating a count dataframe changing numeric month to actual month abbreviations for plot lbel
count <- count(dataNew$month) 
  count$month<-  month.abb[count$x]


# we can see Oct frequency is the highest
# hit zoom on lot
  
barplot(count$freq,xlab = "month",ylab = "freq",col = unique(count$x),names.arg = count$month)

# needed for as.date function
library(zoo)

#should be coutns of year month 
# October seems to be where we are most likely to contact the most clients throughout the year except for 2017
countsYearMonth <- data.frame(with(dataNew, table(year, month))) 
countsYearMonth$date <- as.yearmon(paste(countsYearMonth$year, countsYearMonth$month), "%Y %m")
barplot(countsYearMonth$Freq,xlab = "month",ylab = "freq",col = (countsYearMonth$year),names.arg = countsYearMonth$month)
#click on the plot to add the legend
legend(locator(1), legend=unique(countsYearMonth$year),col=countsYearMonth$year, lty=1:2, cex=0.8)

#We see October has a peak of 60 clients in Oct 2013
#We saw that we had 35 unique clients -> multiple clients repeated
#add a column with 1's to count frequency
dataNew$Count <- 1
#we summed the count variable by client name , year and month
uniqueClients <- aggregate(dataNew$Count,by = list(dataNew$Client.Name,dataNew$year,dataNew$month),sum)



#same logic as above
# we sum the column with 1's by client name (group.1) , and group.3 (month) by unique client names
# aggregate function changes the name of columns by default
uniqueClients$count <- 1
test <- aggregate(uniqueClients$count,by = list(uniqueClients$Group.1,uniqueClients$Group.3),function(x) length(unique(x)))

# aggregate it so we can see unique clients per month
numUniqueClients <- aggregate(test$x,by = list(test$Group.2),sum)
# max unique clients is 35

#percent of total table
numUniqueClients$x <- (numUniqueClients$x / 35) * 100


# october has the most percentage of unique clients and clients(repeated) contacted
# I can add more visualization for percent total of each year, but from our barplto above it seems the results will be the same
# organization of code can be better (pipe operators , variable names)
