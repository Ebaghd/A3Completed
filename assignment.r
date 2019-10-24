# a3-using-data

# Before you get started, set your working directory using the Session menu

###################### Data Frame Manipulation (24 POINTS) #####################

# Create a vector `students` holding 1,000 values representing students
# They should have the values "Student 1", "Student 2",..., "Student 1000"
students <- paste("Student", 1:1000, " ")

# Create a vector `math_grades` that holds 1000 random values in it
# (these represent grades in a math course)
# These values should be normally distributed with a mean of 88 and a
# standard deviation of 10
math_grades <- rnorm(1000, mean = 88, sd = 10)

# Replace any values in the `math_grades vector` that are above 100 with
# the number 100
math_grades <- ifelse(math_grades > 100, 100, math_grades)
# Create a vector `spanish_grades` that holds 1000 random values in it
# (these represent grades in a spanish course)
# These values should be normally distributed with a mean of 85 and a
# standard deviation of 12
spanish_grades <- rnorm(1000, mean = 85, sd = 12 )

# Replace any values in the `spanish_grades` that are above 100 with
# the number 100
spanish_grades <- ifelse(spanish_grades > 100, 100, spanish_grades)

# Create a data.frame variable `grades` by combining
# the vectors `students`, `math_grades`, and `spanish_grades`
# Make sure to properly handle strings
grades <- data.frame(students, math_grades, spanish_grades)

# Create a variable `num_students` that contains the
# number of rows in your dataframe `grades`
num_students <- nrow(grades)
# Create a variable `num_courses` that contains the number of columns
# in your dataframe `grades` minus one (b/c of their names)
num_courses <- ncol(grades)
# Add a new column `grade_diff` to your dataframe, which is equal to
# `students$math_grades` minus `students$spanish_grades`
grades$grade_diff <- (math_grades - spanish_grades)

# Add another column `better_at_math` as a boolean (TRUE/FALSE) variable that
# indicates that a student got a better grade in math
grades$better_at_math <- math_grades > spanish_grades
# Create a variable `num_better_at_math` that is the number
# (i.e., one numeric value) of students better at math
num_better_at_math <- length(grades$better_at_math[grades$better_at_math == TRUE] )
print(num_better_at_math)
# Write your `grades` dataframe to a new .csv file inside your data/ directory
# with the filename `grades.csv`. Make sure *not* to write row names.
# (you'll need to create the `data/` directory, which you can do outside of R)
write.csv(file = "data", x = grades)


########################### Built in R Data (28 points) ########################

# In this section, you'll work with the `Titanic` data set, which
# is built into the R environment.
# This data set actually loads in a format called a *table*
# See https://cran.r-project.org/web/packages/data.table/data.table.pdf
# Use the `is.data.frame()` function to test if it is a table
is.data.frame(Titanic)

# Create a variable `titanic_df` by converting `Titanic` into a data frame;
# you can use the `data.frame()` function or `as.data.frame()`
# Be sure to **not** treat strings as factors!
titanic_df <- data.frame(Titanic, stringsAsFactors = FALSE)

# It's important to understand the _meaning_ of each column before analyzing it
# NOTE: Seek to fully understand the structure of the data before
# proceeding (Hint: How is the variable, Freq, related to the other variables?)
# Using comments below, describe what information is stored in each column
# For categorical variables, list all possible values
# Class: Describes which class of passenger is being described, (1st class is the most luxurious, 2nd is less luxurious, and 3rd is the least luxurious)
# Sex: Describes the gender of the passengers
# Age: Describes the age of the passengers
# Survived: Whether they survived the sinking of the Titanic
# Freq: How many passengers are being described for each condition



# Create a variable `children` that are the *only* the rows of the data frame
# with information about the number children on the Titanic.
children <- titanic_df[titanic_df$Age == "Child",]

# Create a variable `num_children` that is the total number of children.
# Hint: remember the `sum()` function!
num_children <- sum(children$Freq)

# Create a variable `most_lost` which has the *row* with the
# largest absolute number of losses (people who did not survive).
# Tip: if you want, you can use multiple statements (lines of code)
# if you find that helpful to create this variable.
most_lost <-  max(titanic_df$Freq)
most_lost <- titanic_df[titanic_df$Freq == most_lost,]

# Define a function called `survival_rate()` that takes in two arguments:
# - a ticket class (e.g., "1st", "2nd"), and
# - the dataframe itself (it's good practice to explicitly pass in data frames)
# This function should return the following
# sentence that states the *survival rate* (# survived / # in group)
# of adult men and "women and children" in that ticketing class.
# It should read (for example):
# Of Crew class, 87% of women and children survived and 22% of men survived.
# The approach you take to generating the sentence to return is up to you.
# A good solution will likely utilize filtering to produce the required data.
# You must round values and present them as percentages in the sentence.
##ticket_class <- c("1st", "2nd", "3rd", "Crew")
#print(ticket_class)
View(titanic_df)
survival_rate <- function(ticket_class, df){
  male_children <- subset(titanic_df, Class == ticket_class & Sex == "Male" & Age == "Child")
  women <- subset(titanic_df, Class == ticket_class & Sex == "Female")
  survived_women <- subset(women, Survived == "Yes")
  survived_male_children <- subset(male_children, Survived == "Yes")
  survived_women_and_children <- sum(survived_women$Freq, survived_male_children$Freq)
  died_male_children <- subset(male_children, Survived == "No")
  died_women <- subset(women, Survived == "No")
  died_women_and_children <- sum(died_male_children$Freq, died_women$Freq) 
  total_woman_and_children <- sum(died_women_and_children, survived_women_and_children)
  percent_died_women_and_children <- round((100 * (died_women_and_children / total_woman_and_children)), digits = 0) 
  percent_survived_women_and_children <- round((100 * (survived_women_and_children / total_woman_and_children)), digits = 0)
  male_adults <- subset(titanic_df, Class == ticket_class & Sex == "Male" & Age == "Adult")
  survived_male_adult <- subset(male_adults, Survived == "Yes")
  died_male_adult <- subset(male_adults, Survived == "No")
  total_male_adult <- sum(survived_male_adult$Freq, died_male_adult$Freq)
  percent_died_male_adult <- round((100 * (died_male_adult$Freq / total_male_adult)), digits = 0) 
  percent_survived_male_adult <- round((100 * (survived_male_adult$Freq / total_male_adult)), digits = 0)
  final <- paste0("Of ", ticket_class, " class, ", percent_survived_women_and_children, "% of women and children survived and ", percent_survived_male_adult, "% of men survived.")
  return(final)
}


# Create variables `first_survived`, `second_survived`, `third_survived` and
# `crew_survived` by passing each class and the `titanic_df` data frame
# to your `survival_rate` function
# (`Crew`, `1st`, `2nd`, and `3rd`), passing int
first_survived <- survival_rate("1st", titanic_df)
second_survived <- survival_rate("2nd", titanic_df)
third_survived <- survival_rate("3rd", titanic_df)
crew_survived <- survival_rate("Crew", titanic_df)

# What notable differences do you observe in the survival rates across classes?
# Note at least 2 observations.
#1. As the passenger class decreased, (for example, 1st class to 3rd class), the survival rate of men
# decreased besides the third class.
#2. However, more women, children, and men survived from the Crew class than Third Class

# What notable differences do you observe in the survival rates between the
# women and children versus the men in each group?
# Note at least 2 observations.
#1. In every ticket class, more women and children survived than men.
#2. The least women and children survived in 3rd class yet more men survived in 3rd class than 2nd class


########################### Reading in Data (43 points)#########################
# In this section, you'll work with .csv data of life expectancy by country
# First, you should download a .csv file of Life Expectancy data from GapMinder:
# https://www.gapminder.org/data/
# You should save the .csv file into your `data` directory

# Before getting started, you should explore the GapMinder website to understand
# the *original* source of the data (e.g., who calculated these estimates)
# Place a brief summary of the each data source here (e.g., 1 - 2 sentences
# per data source)
# 1st Data source: Period of 1800 - 1970, based on 100 sources compiled by Mattias Lindgren. He also
# made rough predictions of sudden dips in life expectany.
# 2nd Data Source: Period of 1970 - 2016, main source: IMHE, (Institute for Health Metrics and 
# Evaluation). Published September 2017, University of Washington, Seattle - IMHE uses data to prevent child deaths.
# 3rd Data Source: Period of 2017 - 2099. This data source uses UN forecasts from the World 
# Population Prospects 2017. Published in a file with Annually interpolated demograpic indicators.


# Using the `read.csv` function, read the life_expectancy_years.csv file into
# a variable called `life_exp`. Makes sure not to read strings as factors
life_exp <- read.csv("life_expectancy_years.csv" , header = TRUE, stringsAsFactors = FALSE)
# Write a function `get_col_mean()` that takes in a column name and a data frame
# and returns the mean of that column. Make sure to properly handle NA values
#print(titanic_df)
get_col_mean <- function(df, column){
  mean_of_col <- mean(df[[column]], na.rm = TRUE)
  mean_of_col
}
# Create a list `col_means` that has the mean value of each column in the
# data frame (except the `Country` column). You should use your function above.
  minus_country <- names(life_exp)
  minus_country <- minus_country[minus_country != country]
  col_means <- lapply(minus_country, get_col_mean, df = life_exp)

  
# Create a variable `avg_diff` that holds the difference in average country life
# expectancy between 1800 and 2018?

avg_diff <- (get_col_mean(life_exp, "X2018")) - (get_col_mean(life_exp, "X1800"))

# Create a column `life_exp$change` that is the change
# in life expectancy from 2000 to 2018. Increases in life expectancy should
# be *positive*
change <- (get_col_mean(life_exp, "X2018")) - (get_col_mean(life_exp, "X2000"))
life_exp$change <- change
# Create a variable `most_improved` that is the *name* of the country
# with the largest gain in life expectancy
# Make sure to filter NA values!
most_improved_vector <- unlist(col_means)
most_improved_num <- most_improved_vector[!is.na(most_improved_vector)]
most_improved_num <- max(most_improved_num)

# Create a variable `num_small_gain` that has the *number* of countries
# whose life expectance has improved less than 1 year between 2000 and 2018
# Make sure to filter NA values!
  num_small_gains <- avg_diff < 1

# Write a function `country_change()` that takes in a country's name,
# two (numeric) years, and the `life_exp` dataframe as parameters.
# It should return the phrase:
# "Between YEAR1 and YEAR2, the life expectancy in COUNTRY went DIRECTION by
# SOME_YEARS years".
# Make sure to properly indictate the DIRECTION as "up" or "down"

country_change <- function(country_name, year1, year2, life_exp){
  selected_country <- subset(life_exp, country == country_name)
  return("Between YEAR1 and YEAR2, the life expectancy in COUNTRY went DIRECTION by SOME_YEARS")
}
tee <- country_change("Andorra", "1880", "1912", life_exp)
# Using your `country_change()` function, create a variable `sweden_change`
# that is the change in life expectancy from 1960 to 1990 in Sweden
  sweden_change <- country_change("Sweden", "X1960", "X1990", life_exp)

# Write a function `compare_change()` that takes in two country names and your
# `life_exp` data frame as parameters, and returns a sentence that describes
# their change in life expectancy from 2000 to 2018 (the `change` column)
# For example, if you passed the values "China", and "Bolivia" to you function,
# It would return this:
# "The country with the bigger change in life expectancy was China (gain=6.9),
#  whose life expectancy grew by 0.6 years more than Bolivia's (gain=6.3)."
# Make sure to round your numbers to one digit (though only after calculations!)
compare_change <- function(country1, country2, df){
  country1_values <- subset(life_exp, country == country1,)
  country2_values <- subset(life_exp, country == country2,)
  country1_values <- get_col_mean(country1_values, "X2000")
  country2_values <- get_col_mean(country2_values, "X2000")
  
}

# Using your `bigger_change()` function, create a variable `usa_or_france`
# that describes who had a larger gain in life expectancy (the U.S. or France)
  usa_or_france <- max(bigger_change("United States", "China", life_exp))

# Write your `life_exp` data.frame to a new .csv file to your
# data/ directory with the filename `life_exp_with_change.csv`.
# Make sure not to write row names.
write.csv(life_exp, "life_exp_with_change.csv", row.names = FALSE)