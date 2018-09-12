########################################################
# Name:
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


# 2. How many students completed the survey?


# 3. How many questions were asked (i.e., how many columns are there)?


# 4. Construct a frequency bar graph for the response to "Are you a cat or a dog person?",
#    where the bars are colored in using the default colors. Remove the legend by 
#    adding the following component to the end of your 
#    ggplot() code: theme(legend.position = "none")


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


# 6. Construct a Pareto Chart for favorite CSC course

# 7. Construct a relative frequency table for whether or not a student consumes alcohol
#    at least 1 day per week, on average (i.e., consumes alcohol > 0 days per week). 
#    Note: Your relative frequency should show only 2 possible values, corresponding
#    to whether a person does consume alcohol or does not. The names of the table 
#    should reflect this, rather than, e.g., saying TRUE or FALSE. Since the frequency
#    table is stored as a vector, change the names using the names() function.



# 8. Out of the "Cat" people in this class, what has been their favorite
#    CSC course so far? Answer this question by first creating a new
#    data.frame for "Cat" people only. Then generate a relative frequency table
#    for favorite programming language. Then answer the same question for "Dog" 
#    people. What do you conclude about the favorite course for cat and dog people (for 
#    students in this class) based on this data?



# 9. Construct a histogram for Alchol consumption, by using the hist() function with the argument
#    breaks = 14 to set the number of groupings. Describe the shape of its distribution. 
#    Is it unimodal, bimodal, or flat. Is it skewed right, skewed left, or symmetric?


# 10. Calculate the mean and median for Alcohol consumption. 
#    Which is a better measure of averages? (Note: although these numbers are similar,
#    one would still be considered better than the other -- why?)


# 11. What is the 75th percentile for HS GPA??

# 12. Ten percent of indivduals have HS GPAs above what value?

# 13. Create side-by-side boxplots showing the average hours of sleep based on
#     whether a person is a cat or dog person.  Does there appear 
#     to be a significant difference in the GPAs between these 
#     groups? Are there any outliers? If so, how many?


# 14. For college GPA, what is the variance and standard deviation?


# 15. Create a vector with 20 values that has a standard deviation of 0.

