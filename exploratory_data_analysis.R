library(readxl)
library(plyr)
library(plotly)
library(ggplot2)

#gets rid of scientific notation issue
options(scipen = 999)

#uploading the rlf data set
rlf_data <- read_excel("G:/Shared/Disaster_RECOVERY_Assistance/COVID-19/Ryan's Working Files/Research Project - RLFs/working_rlf_data_10262020.xlsx")

#importing the fed prime interest rate and deflator data set, courtesy of FRED
prime_df <- read_excel("G:/Shared/Disaster_RECOVERY_Assistance/COVID-19/Ryan's Working Files/Research Project - RLFs/prime_loan_rates.xls")
price_deflator <- read_excel("G:/Shared/Disaster_RECOVERY_Assistance/COVID-19/Ryan's Working Files/Research Project - RLFs/gdp_deflator.xls")

#setting date as time series variable
prime_df$date <- as.Date(prime_df$date)
price_deflator$date <- as.Date(price_deflator$date)

#sorting date
sorted <- prime_df[order(prime_df$date),]
deflator_sorted <- price_deflator[order(price_deflator$date),]

#data length
data.length <- length(sorted$date)
data.length <- length(deflator_sorted$date)

#date min and max
date.min <- sorted$date[1]
date.max <- sorted$date[data.length]
deflator.min <- deflator_sorted$date[1]
deflator.max <- deflator_sorted$date[data.length]

#gets a day by day for the time period
all.dates <- seq(date.min, date.max, by="day")
deflator.dates <- seq(deflator.min, deflator.max, by="day")

#gets all of those dates into a data frame
all.dates.frame <- data.frame(list(date=all.dates))
deflator.dates.frame <- data.frame(list(date=deflator.dates))

#merges the data sets
merged.data <- merge(all.dates.frame, sorted, all=T)
merged_deflator <- merge(deflator.dates.frame, deflator_sorted, all=T)

#final time series data set acquired by doing a data merge
prime_interest_timeseries <- na.locf(merged.data)
deflator_timeseries <- na.locf(merged_deflator)

#inputting prime loan rates as a column, matching on closing date
merged_rlf_meta <- join(rlf_data, prime_interest_timeseries, by= "date")
merged_rlf <- join(merged_rlf_meta, deflator_timeseries, by = "date")

#cleaning up the names a little more
names(merged_rlf)[names(merged_rlf) == "date"] <- "loan_close_date"

#decimalizing interest rates to match up
merged_rlf$interest <- merged_rlf$interest_rate*100

#performing a rate density function on the nominal interest rate
rate_density <- density(merged_rlf$prime_loan_rate, na.rm = T)
plot(rate_density, main="RLF Interest Rate Density Plot")
polygon(rate_density, col="light blue", borders="blue")

#calculating rate density on the difference between the rate and the prime rate
merged_rlf$rate_difference <- (merged_rlf$interest - merged_rlf$prime_loan_rate)

ggplot(merged_rlf, aes(rate_difference)) + 
  geom_density(fill="light blue", color="blue", alpha=0.8) +
  xlab("Loan Interest Rate - Prime Interest Rate")

summary(merged_rlf$rate_difference)

#density plot of loan term
merged_rlf$loan_term <- as.numeric(as.character(merged_rlf$loan_term))
loan_term_density <- density(merged_rlf$loan_term, na.rm=T)
summary(merged_rlf$loan_term)
plot(loan_term_density, xlim=c(0,300), main="Loan Term Distribution", xlab="Loan Term (Months)")
polygon(loan_term_density, col="light blue")

#loan amounts, nominal usd
rlf_funding_density <- density(merged_rlf$rlf_funding, na.rm=T)
plot(rlf_funding_density, xlim=c(0,200000), main="RLF Funding Distribution, Nominal USD", xlab="USD (Unadjusted)")
polygon(rlf_funding_density, col="light blue")
summary(merged_rlf$rlf_funding)

#loan amounts, 2012 usd
summary(merged_rlf$deflated_rlf_funding)
ggplot(merged_rlf, aes(deflated_rlf_funding)) + 
  geom_density(fill="light blue", color="blue", alpha=0.8) +
  xlim(0,400000) +
  xlab("RLF Loan Amount (in 2012 USD)")

#grouping data out by creating a year variable
merged_rlf$year <- format(as.Date(merged_rlf$loan_close_date, "%d/%m/%Y"), "%Y")

#making a fancy graph of rlf yearly loans using ggplot variable
rlf_funds_by_year <- ggplot(merged_rlf, aes(x=year, y=rlf_funding)) + geom_bar(stat="identity")
rlf_funds_by_year + ggtitle("Sum of RLF Loans by Year") +
  xlab("Year") + ylab("USD (Nominal)")

#rlf yearly loans, adjusted for 2012 usd
merged_rlf$deflated_rlf_funding <- (merged_rlf$rlf_funding*100/merged_rlf$deflator)
rlf_deflated_funds_by_year <- ggplot(merged_rlf, aes(x=year, y=deflated_rlf_funding)) + geom_bar(stat="identity")
rlf_deflated_funds_by_year + ggtitle("Sum of RLF Loans by Year") +
  xlab("Year") + ylab("USD (in 2012 dollars)")

summary(merged_rlf$deflated_rlf_funding)

#analysis of percentages of loan status
merged_rlf$loan_status <- tolower(merged_rlf$loan_status)
loan_status <- table(merged_rlf$loan_status)
loan_status
prob_loan_status <- prop.table(loan_status)
prob_loan_status
