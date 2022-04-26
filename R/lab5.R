library(dplyr)
library(stringr)

# load the data from the csv file located inside this repository (called `any_drinking.csv`) put it in a variable called `drinking_data`
any_drinking <- read.csv('data/any_drinking.csv')

# use View() to open this data
# View(any_drinking)

# find the number of rows and columns in this dataset
print(paste("Number of Rows:", nrow(any_drinking)))
print(paste("Number of Columns:", ncol(any_drinking)))

# extract the columns, 'state', 'location', 'both_sexes_2002', 'both_sexes_2003' and 'both_sexes_2004' and store it in a variable called `both_sex_drinking`
both_sex_drinking <- any_drinking[, c('state', 'location', 'both_sexes_2002', 'both_sexes_2003', 'both_sexes_2004')]

both_sex_drinking <- any_drinking %>% 
  select(state, location, both_sexes_2002, both_sexes_2003, both_sexes_2004)

# create a function `find_mean` that takes in a vector of values (column from data set). Return the average of these values
# use your function to pass in each of the numerical data type columns in `both_sex_drinking`
find_mean <- function(col) {
  return(mean(col))
}

find_mean(both_sex_drinking$both_sexes_2002)
find_mean(both_sex_drinking$both_sexes_2003)
find_mean(both_sex_drinking$both_sexes_2004)


# create a function `county_count` that takes in a state and return the number of counties that have data collected in the state
county_count <- function(state_name) {
  return(sum(str_detect(both_sex_drinking$state, state_name)))
}

county_count2 <- function(state_name) {
  count <- both_sex_drinking %>% 
    select(state) %>% 
    filter(state == state_name)
  return(nrow(count))
}