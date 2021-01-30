# students_performane
#1. Introduction 
1. Introduction
2. Preparation
    a. Libraries
    b. Importing data 
    c. Data structure
3. Data cleaning/EDA
4. In-depth EDA:Parents Education
5. Linear Regression
    a. Correlation
    b. Linear Model
    c. Predicting writing score
    d. Actual score vs. Predicted score

#  For this project, I did data analysis on students' academics performance. I especially was curious if their parents' background education has to do with their test scores. Also, I created a linear model that predicts students' writing scores using math and reading score. Hope y'all like it!

#2. Preparation 
  a. libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caTools)
library(plotly)

   b. Upload data
per <- read_csv("StudentsPerformance.csv")

   c. Data Structure
str(per)

3. Data cleaning/EDA
#fixing the columns names 
colnames(per)[2] <- "race_ethnicity"
colnames(per)[3] <- "parental_education"
colnames(per)[5] <- "test_prep"
colnames(per)[6] <- "math_score"
colnames(per)[7] <- "reading_score"
colnames(per)[8] <- "writing_score"

per

#total number of male and female 
sum(per$gender == "female")    
sum(per$gender == "male")

#Sum of each ethnicity 
ggplot(per,aes(x=race_ethnicity, fill = race_ethnicity))+
  geom_bar(stat="count")+
  xlab("ethnicity")+
  ylab("total count")
ggtitle("Race/Ethnicity")

#Sum of parents education background
pie <- ggplot(per,aes(x=parental_education, fill = parental_education))+
  geom_bar()+
  theme(axis.line = element_blank(),
        plot.title = element_text(hjust=0.6))
pie + coord_polar(theta = "x",start=0)  

#Sum of standard lunch and reduced lunch 
ggplot(per,aes(x=lunch, fill=gender))+
  geom_bar()+
  xlab("standard/reduced lunch")+
  ylab("total count")+
  facet_wrap(~gender)
  
#Sum of completed and incomplete test prep
ggplot(per,aes(x=test_prep, fill=gender))+
  geom_bar()+
  xlab("test prep")+
  ylab("total count")+
  facet_wrap(~gender)
  
4. In-dept EDA about parents educational background
# I made a new datafame of parents each latest education to find a trend. The df includes sum of lunch (standard & reduced), test prep (completed & incomplete), and average of all test scores.


#somehighschool data 
some_highschool <- filter(per,parental_education == "some high school")
nrow(some_highschool[some_highschool$test_prep == "completed",])
nrow(some_highschool[some_highschool$test_prep != "completed",])
nrow(some_highschool[some_highschool$lunch == "standard",])
nrow(some_highschool[some_highschool$lunch != "standard",])
mean(some_highschool$math_score)
mean(some_highschool$reading_score)
mean(some_highschool$writing_score)

new_some_highschool <- data.frame(number = "179",parental_education = "some_highschool",standard_lunch = "118",reduced_lunch = "61", completed_test_prep = "77",incompleted_test_prep = "102", mean_math = "63.50",mean_reading = "66.94",mean_writing = "64.89")
new_some_highschool

#highschool data 
highschool <- filter(per,parental_education == "high school")
nrow(highschool[highschool$test_prep == "completed",])
nrow(highschool[highschool$test_prep != "completed",])
nrow(highschool[highschool$lunch == "standard",])
nrow(highschool[highschool$lunch != "standard",])
mean(highschool$math_score)
mean(highschool$reading_score)
mean(highschool$writing_score)

new_highschool <- data.frame(number = "196",parental_education = "highschool",standard_lunch = "126",reduced_lunch = "70", completed_test_prep = "56",incompleted_test_prep = "140", mean_math = "62.14",mean_reading = "64.70",mean_writing = "62.45")
new_highschool

#somecollege data 
some_college <- filter(per,parental_education == "some college")
nrow(some_college[some_college$test_prep == "completed",])
nrow(some_college[some_college$test_prep != "completed",])
nrow(some_college[some_college$lunch == "standard",])
nrow(some_college[some_college$lunch != "standard",])

mean(some_college$math_score)
mean(some_college$reading_score)
mean(some_college$writing_score)

new_some_college <- data.frame(number = "226",parental_education = "some college",standard_lunch = "147",reduced_lunch = "79", completed_test_prep = "77",incompleted_test_prep = "149", mean_math = "67.13",mean_reading = "69.46",mean_writing = "68.84")
new_some_college

#Associate's degree data
associate_degree <- filter(per,parental_education == "associate's degree")
nrow(associate_degree[associate_degree $test_prep == "completed",])
nrow(associate_degree[associate_degree $test_prep != "completed",])
nrow(associate_degree[associate_degree $lunch == "standard",])
nrow(associate_degree[associate_degree $lunch != "standard",])

mean(associate_degree$math_score)
mean(associate_degree$reading_score)
mean(associate_degree$writing_score)

new_associate_degree <- data.frame(number = "222",parental_education = "associate's degree",standard_lunch = "145",reduced_lunch = "77", completed_test_prep = "82",incompleted_test_prep = "140", mean_math = "67.88",mean_reading = "70.93",mean_writing = "69.90")
new_associate_degree

new_associate_degree <- data.frame(number = "222",parental_education = "associate's degree",standard_lunch = "145",reduced_lunch = "77", completed_test_prep = "82",incompleted_test_prep = "140", mean_math = "67.88",mean_reading = "70.93",mean_writing = "69.90")

#Bachelor's degree data
bachelor_degree <- filter(per,parental_education == "bachelor's degree")
nrow(bachelor_degree[bachelor_degree$test_prep == "completed",])
nrow(bachelor_degree[bachelor_degree$test_prep != "completed",])
nrow(bachelor_degree[bachelor_degree$lunch == "standard",])
nrow(bachelor_degree[bachelor_degree$lunch != "standard",])
mean(bachelor_degree$math_score)
mean(bachelor_degree$reading_score)
mean(bachelor_degree$writing_score)

new_bachelor_degree <- data.frame(number = "118",parental_education = "bachelor's degree",standard_lunch = "74",reduced_lunch = "44", completed_test_prep = "46",incompleted_test_prep = "72", mean_math = "69.39",mean_reading = "73.00",mean_writing = "73.38")

Master's degree data
master_degree <- filter(per,parental_education == "master's degree")
nrow(master_degree[master_degree$test_prep == "completed",])
nrow(master_degree[master_degree$test_prep != "completed",])
nrow(master_degree[master_degree$lunch == "standard",])
nrow(master_degree[master_degree$lunch != "standard",])

mean(master_degree$math_score)
mean(master_degree$reading_score)
mean(master_degree$writing_score)

new_master_degree <- data.frame(number = "59",parental_education = "master's degree",standard_lunch = "35",reduced_lunch = "24", completed_test_prep = "20",incompleted_test_prep = "39", mean_math = "69.75",mean_reading = "75.37",mean_writing = "75.68")
new_master_degree

#As we can see parents education does not have any pattern when it comes to lunch or test prep. My initial hypothesis was that parents with high educational background tend to understand the significance for preparations. Despite the sample size for bachelor and master degree is small, it is safe to conclude that education level has nothing to do with their children’s preparation. 

#Can parents education background affect children's test score ? 
Boxplot for each test scores 

#math score 
box_math <- ggplot(per,aes(x=fct_reorder(parental_education,math_score),y=math_score,fill=parental_education))+
  geom_boxplot()+
  facet_wrap(~gender)+
  ggtitle("Boxplot:math score")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))
box_math <- ggplotly(box_math)
box_math

#reading score 
box_reading <- ggplot(per,aes(x=fct_reorder(parental_education,reading_score),y=math_score,fill=parental_education))+
  geom_boxplot()+
  facet_wrap(~gender)+
  ggtitle("Boxplot:reading score")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))
box_math <- ggplotly(box_reading)
box_math

#writing score 
box_writing <- ggplot(per,aes(x=fct_reorder(parental_education,writing_score),y=writing_score,fill=parental_education))+
  geom_boxplot()+
  facet_wrap(~gender)+
  ggtitle("Boxplot:writing score")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))

box_writing <- ggplotly(box_writing)
box_writing

#parents tests score 
#math score 
plot_df <- data.frame(parental_education = c("some_highschool","highschool","some_college","associate_degree","bachelor_degree","master_degree"),
               math = c(63.50,62.14,67.13,67.88,69.39,69.75),
               reading = c(66.94,64.70,69.46,70.93,73.00,75.37),
               writing  = c(64.89,62.45,68.84,69.90,73.38,75.68))
ggplot(plot_df,aes(x=factor(parental_education,level=c("some_highschool","highschool","some_college","associate_degree","bachelor_degree","master_degree")),y=math,group=1))+
  geom_line(color="blue")+
  geom_label(aes(label = math, size = NULL), nudge_y = 0.7)+
  ggtitle("Math score")+
  xlab("Parental education")+
  ylab("Score")

#reading score 
ggplot(plot_df,aes(x=factor(parental_education,level=c("some_highschool","highschool","some_college","associate_degree","bachelor_degree","master_degree")),y=reading,group=1))+
  geom_line(color="red")+
  geom_label(aes(label = reading, size = NULL,color=NULL), nudge_y = 0.7)+
  ggtitle("Reading score")+
  xlab("Parental education")+
  ylab("Score")

#writing score 
ggplot(plot_df,aes(x=factor(parental_education,level=c("some_highschool","highschool","some_college","associate_degree","bachelor_degree","master_degree")),y=writing,group=1))+
  geom_line(color="green")+
  geom_label(aes(label = writing, size = NULL,color=NULL), nudge_y = 0.7)+
  ggtitle("Writing score")+
  xlab("Parental education")+
  ylab("Score")

5. Linear Regression 
#predicting the writing score using math score 
cor(per$writing_score,per$math_score)

score_df <- per[,-2:-5]
plot_ly(data=score_df,x=~writing_score,y=~math_score,
        type="scatter",mode="markers",
        text= ~paste("gender: ",gender,"<br>writing_score: ",writing_score,"<br>math_score: ",math_score))
        
#split dataset into two 

split <- sample.split(score_df,SplitRatio = 0.7)
train <- subset(score_df, split="TRUE")
test <- subset(score_df, split="FALSE")
#linear model 
lm <- lm(per$writing_score~per$math_score,data=train)
summary(lm)
#predicing the writing score 
predict_writing_1 <- predict(lm,train)
#plotting the actual score vs. predicted score 
plot(per$writing_score,type="l",lty=1.8,col="red")
lines(predict_writing_1,type="l", col="blue")

#predicting the writing score using reading score 
cor(per$writing_score,per$reading_score)

plot_ly(data=score_df,x=~writing_score,y=~reading_score,
        type="scatter",mode="markers",
        text= ~paste("gender: ",gender,"<br>writing_score: ",writing_score,"<br>math_score: ",math_score))%>%
  layout(title="Specific Detail",xaxis=list(title="scores"),yaxis=list(title="count"))
#linear model 
lm_2 <- lm(score_df$writing_score~score_df$reading_score,data=train)
summary(lm_2)
#predicting 
predict_writing_2 <- predict(lm_2,train)
#plot actutal score vs. the predicted score 
plot(score_df$writing_score,type="l",col="red")
lines(predict_writing_2,type="l",col="blue")

  





  

