#loading the data
library(readxl)
rlf_industry_percentages <- read_excel("G:/Shared/Disaster_RECOVERY_Assistance/COVID-19/Ryan's Working Files/Research Project - RLFs/rlf_industry_percentages.xlsx")

#library
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# extending the color palette to accommodate large number of categories
nb.cols <- 20
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)

#building the plot
ggplot(rlf_industry_percentages, aes(x=Industry, y=Percent, fill=Industry)) +
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label=Percent))+
  scale_fill_manual(values = mycolors) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())