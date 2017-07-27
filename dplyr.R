### dplyr package ###
library(dplyr)


chicago <- read.csv(file.choose(), header = T)
dim(chicago)
str(chicago)


# exploration using dplyr
names(chicago)

subsetData <- select(chicago, city:dptp)
head(subsetData)

s1 <- select(chicago, -(city:dptp))
head(s1)

names(chicago)

s2 <- select(chicago, starts_with("d"))
head(s2)

s3 <- select(chicago, ends_with("2"))
names(s3)


chic.f <- filter(chicago, pm25tmean2 > 30)
str(chic.f)


summary(chic.f$pm25tmean2)

boxplot(chic.f$pm25tmean2, col=c("cyan")) # data is right skewed

# order rows by date (asc)
chicago <- arrange(chicago, date) 
head(chicago)


# order rows by date(desc)
chicago <- arrange(chicago, desc(date))
head(chicago)


# rename columns dptp to dewpoint and pm25tmean2 to pm25
chicago <- rename(chicago, dewpoint = dptp, pm25 = pm25tmean2)
head(chicago)


#data transformations
# we often want to detrend the data by subtracting the mean from the data. That way we can look at whether a given day's air pollution level is higher than or less than average (as opposed to looking at its absolute level). Here we create a pm25detrend variable that subtracts the mean from the pm25 variable.

chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
head(chicago)


#generate summary statistics
# in this air pollution dataset, you might want to know what the average annual level of PM2.5 is. So the stratum is the year.
#The general operation here is a combination of splitting a data frame into separate pieces defined by a variable or group of variables (group_by()), and then applying a summary function across those subsets (summarize()).
library(lubridate)
chicago <- mutate(chicago, year = year(ymd(date)))
head(chicago)


# Now we can create a separate data frame that splits the original data frame by year.
years <- group_by(chicago, year)
# Finally, we compute summary statistics for each year in the data frame with the summarize() function.
summarize(years, pm25 = mean(pm25, na.rm = TRUE),
          o3 = max(o3tmean2, na.rm = TRUE),
          no2 = median(no2tmean2, na.rm = TRUE))


### USING PIPING
df <- chicago %>%
  mutate(year = year(ymd(date))) %>%
  group_by(year) %>%
  summarize(pm25 = mean(pm25, na.rm = TRUE),
            o3 = max(o3tmean2, na.rm = TRUE),
            no2 = median(no2tmean2, na.rm = TRUE))

