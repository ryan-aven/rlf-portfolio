# reading in the data
library(readxl)
merged_data <- read_excel("G:/Shared/Disaster_RECOVERY_Assistance/COVID-19/Ryan's Working Files/Research Project - RLFs/merged_data.xlsx")

#making a new data set with just loan status and year
loan_status <- data.frame(merged_data$year, merged_data$loan_status)
colnames(loan_status) <- c("year", "status")

#removing the big data set (to save space)
rm(merged_data)

#removing all current loans to calculate a historical rate of default
library(dplyr)
loan_status = filter(loan_status, status != "in default" & status != "NA" & status != "delinquent" & status != "current")

# turning loan status into a proportion table, grouped by year
loan_status2 <- table(loan_status)
prop.table(loan_status2)

#turning proportions into a data frame
final_loans <- as.data.frame.matrix(loan_status2)
final_loans$default_rate <- (final_loans$`written off`)*100/(final_loans$`fully repaid` + final_loans$`written off`)
final_loans$default_rate<- format(round(final_loans$default_rate, 1), nsmall = 1)
final_loans$default_rate <- as.numeric(final_loans$default_rate)
final_loans$year <- 1978:2020

#plotting the default rate
library(ggplot2)
library(grid)
ggplot(data=final_loans, aes(x=year, y=default_rate, group=1)) +
  geom_line(color="#56B4E9", size=1)+
  geom_point(color="#56B4E9", size=1.25) +
  geom_text(aes(label=default_rate), size=3.5, vjust=-1.1)+
  ggtitle("Revolving Loan Fund Default Rates, by Year")+
  xlab("Year") + 
  ylab("Percent Default Rate (Closed/Inactive Loans)")+
  scale_y_continuous(breaks=seq(0,40,5))+
  theme_minimal()


