library(tidyverse)
library(dplyr)
library(data.table)
library(ggplot2)
library(hrbrthemes)
library(kableExtra)
library(scales)

#AP Score Criterion Table
scores <- c("5", "4", "3", "2", "1")
representation <- c("extremely well-qualified", "well-qualified", "qualified", "possibly-qualified", "no recommendation")
criterion <- data.frame(scores, representation)

#Annual Growth of AP Program Measured by Total Exams Taken
grand_total <- data.table(read_csv("../inputs/data/GrandTotal.csv"))
grand_total %>%
  tail(10) %>%
  ggplot(aes(x=year, y=exams_taken)) +
  geom_line(color="grey") +
  geom_point(shape=20, color="black", fill="green", size=6) +
  ggtitle("Annual Growth of AP Exams Taken") 

#AP Student Demographic Represented in Ethnic Pie Charts (per major US Region)
ethnicData <- read_csv("inputs/data/EthnicitiesData.csv")
cali <- ethnicData %>% filter(State == "California")
masscheut <- ethnicData %>% filter(State == "Massachusetts")
texas <- ethnicData %>% filter(State == "Texas")
illinois <- ethnicData %>% filter(State == "Illinois")
florida <- ethnicData %>% filter(State == "Florida")
ny <- ethnicData %>% filter(State == "New York")

ggplot(cali, aes(x = "Percentages", y = Students, fill = Ethnicity)) +
  geom_col() +
  xlab("Percentage Points") +
  geom_text(aes(label = round(Students/sum(Students)*100, digits=2)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

ggplot(masscheut, aes(x = "Percentages", y = Students, fill = Ethnicity)) +
  geom_col() +
  xlab("Percentage Points") +
  geom_text(aes(label = round(Students/sum(Students)*100, digits=2)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

ggplot(texas, aes(x = "Percentages", y = Students, fill = Ethnicity)) +
  geom_col() +
  xlab("Percentage Points") +
  geom_text(aes(label = round(Students/sum(Students)*100, digits=2)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

ggplot(illinois, aes(x = "Percentages", y = Students, fill = Ethnicity)) +
  geom_col() +
  xlab("Percentage Points") +
  geom_text(aes(label = round(Students/sum(Students)*100, digits=2)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

ggplot(ny, aes(x = "Percentages", y = Students, fill = Ethnicity)) +
  geom_col() +
  xlab("Percentage Points") +
  geom_text(aes(label = round(Students/sum(Students)*100, digits=2)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

ggplot(florida, aes(x = "Percentages", y = Students, fill = Ethnicity)) +
  geom_col() +
  xlab("Percentage Points") +
  geom_text(aes(label = round(Students/sum(Students)*100, digits=2)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

#2019 Exam Numbers and Averages (per major US region) and Bar Plots
iexams <- read_csv("inputs/data/ArtsSTEMSubjectExams.csv")
iaverages <- read_csv("inputs/data/ArtsSTEMSubjectAverages.csv")

ggplot(iexams, aes(x=Subject, y=Exams, fill=State)) +
  geom_bar(stat="identity")

ggplot(iaverages, aes(x=Subject, y=Averages, fill=State)) +
  geom_bar(stat="identity")