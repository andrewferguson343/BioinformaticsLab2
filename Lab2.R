########################################################
# Name: Andrew Ferguson
# CSC-315
# Lab #2: Graphical and Numerical Summaries of Data
########################################################



##########################################################################
# Add R code to the script below and create a Notebook to complete
# the steps and explicitly answer the following questions

# Note: all graphs must be given an appropriate title, x-axis label, and
#   y-axis label. The ggplot2 library must be used to generate all
#   graphs unless stated otherwise.
##########################################################################

# 1.load our classes survey data 
#   (available at https://gdancik.github.io/CSC-315/data/datasets/csc-315_survey.xlsx)
#   and add the code for this to the script. Note that the survey is
#   saved as an Excel spreadsheet. Using Import Dataset in RStudio will
#   download the file to your current working directory and then
#   read it into R

library(readxl)
url <- "https://gdancik.github.io/CSC-315/data/datasets/csc-315_survey.xlsx"
destfile <- "csc_315_survey.xlsx"
csc_315_survey <- read_excel(destfile)
survey <- csc_315_survey

# 2. How many students completed the survey?
nrow(survey)
  
# 3. How many questions were asked (i.e., how many columns are there)?
ncol(survey)

# 4. Construct a frequency bar graph for the response to "Are you a cat or a dog person?",
#    where the bars are colored in using the default colors. Remove the legend by 
#    adding the following component to the end of your 
#    ggplot() code: theme(legend.position = "none")
library(ggplot2)
catOrDog <-  table(survey$CatOrDogPerson)
catOrDog
catOrDog <- data.frame(catOrDog)

g <- ggplot(catOrDog, aes(x = catOrDog$Var1, y= catOrDog$Freq)) +
geom_col(aes(fill = catOrDog$Var1)) +
ggtitle("Cat or dog person") +
labs(x = "Animal", y = "NumberOfStudents")+
theme(legend.position = "none")
print(g)

# 5. Construct a relative frequency table for favorite CSC course. Because
#  the data is not consistent, first run the code below so that
#  courses are in a consistent notation. This code assumes the data 
#  is stored in 'survey'. If that's not the case then 'survey' 
#  should be changed to the name of the object in your workspace where
#  the data is stored in the code below

#  Note: this response has missing values!

#  Note: the code below uses regular expressions which we will
#  not cover in this course, but if you have questions I am happy
#  to answer


library(stringr) 


# convert responses to lowercase
survey$Favorite.CSC.Course <- tolower(survey$Favorite.CSC.Course)

# remove all hyphens, i.e., replace "-" with ""
courses <- gsub("-", "", survey$Favorite.CSC.Course)

# find all courses mentioned (responses may include more than
# one course)
courses <- str_extract_all(courses, "csc ?[0-9]{3}")

# function to extract the course, but if no courses or more
# than one are specified, return NA
getCourse <- function(x) {
  if (length(x) != 1) {
    return(NA)
  }
  return(x)
}

# extract the courses
courses <- sapply(courses, getCourse)

# put all courses in a standard format
courses <- gsub("csc ?", "csc-", courses)

# reassign courses to the survey
survey$Favorite.CSC.Course <- courses
CoursesTable <-  table(courses)
CoursesTable
# 6. Construct a Pareto Chart for favorite CSC course
coursesData = data.frame(CoursesTable)
coursesData
ggplot(coursesData ,aes(x= CoursesTable, y=Freq)) + 
  geom_col(aes(fill = coursesData$courses)) +
  ggtitle("Favorite Class of Students") +
  labs(x = "Favorite Class", y = "Frequency")

# 7. Construct a relative frequency table for whether or not a student consumes alcohol
#    at least 1 day per week, on average (i.e., consumes alcohol > 0 days per week). 
#    Note: Your relative frequency should show only 2 possible values, corresponding
#    to whether a person does consume alcohol or does not. The names of the table 
#    should reflect this, rather than, e.g., saying TRUE or FALSE. Since the frequency
#    table is stored as a vector, change the names using the names() function.

survey$Alcohol
alcTable <- table(survey$Alcohol > 0) / sum(table(survey$Alcohol))
names(alcTable) <- c("Drinks", "Does Not Drink")
alcTable

# 8. Out of the "Cat" people in this class, what has been their favorite
#    CSC course so far? Answer this question by first creating a new
#    data.frame for "Cat" people only. Then generate a relative frequency table
#    for favorite programming language. Then answer the same question for "Dog" 
#    people. What do you conclude about the favorite course for cat and dog people (for 
#    students in this class) based on this data?
library(dplyr)
catPeople <-filter(survey, CatOrDogPerson == "Cat")
CatPeople.class <- table(catPeople$Favorite.CSC.Course) /sum(table(catPeople$Favorite.CSC.Course))
CatPeople.class

dogPeople <-filter(survey, CatOrDogPerson == "Dog")
dogPeople.class <- table(dogPeople$Favorite.CSC.Course) /sum(table(dogPeople$Favorite.CSC.Course))
dogPeople.class

# The highest percentage of cat people chose csc-314 as their favorite class. The most popular class among dog people is tied
# between csc 270 and csc 210

# 9. Construct a histogram for Alchol consumption, by using the hist() function with the argument
#    breaks = 14 to set the number of groupings. Describe the shape of its distribution. 
#    Is it unimodal, bimodal, or flat. Is it skewed right, skewed left, or symmetric?

hist(survey$Alcohol, breaks = 14)
#This graph is unimodal skewed right


# 10. Calculate the mean and median for Alcohol consumption. 
#    Which is a better measure of averages? (Note: although these numbers are similar,
#    one would still be considered better than the other -- why?)
alcoholConsump <- survey$Alcohol 
mean(alcoholConsump)
median(alcoholConsump)

#The mean would be a better indication of average. If we used the
#median it would seem as if nobody in the class drinks.


# 11. What is the 75th percentile for HS GPA??
hsGPA <-  survey$HS.GPA
quantile(hsGPA, .75)
# 12. Ten percent of indivduals have HS GPAs above what value?
quantile(hsGPA, .9)
# 13. Create side-by-side boxplots showing the average hours of sleep based on
#     whether a person is a cat or dog person.  Does there appear 
#     to be a significant difference in the GPAs between these 
#     groups? Are there any outliers? If so, how many?

s = split(survey$HoursOfSleep, survey$CatOrDogPerson)

boxplot(s, ylab = "Hours of Sleep", col = c("pink", "blue"), 
        main = "hours of sleep by preferred pet")

gpa <- split(survey$College.GPA, survey$CatOrDogPerson)
boxplot(gpa, ylab = "GPA", col = c("pink", "blue"), 
        main = "GPA by preferred pet")
#For Gpa the median gpa is higher than the median gpa of dog people, but cat people have a much larger spread. Dog people seem to be consistently above a 3.0 GPA.
#For Hours of sleep, the median is almost excatly even with dog people having a higher spread. There is one outlier on the lower end of the dog people graph.

# 14. For college GPA, what is the variance and standard deviation?
colgpa <- survey$College.GPA
var(colgpa)
sd(colgpa)
# 15. Create a vector with 20 values that has a standard deviation of 0.
vec <- rep(20,20)
sd(vec)
