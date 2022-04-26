library(dplyr)
library(stringr)

# load the data from the csv file located inside this repository (called `any_drinking.csv`) put it in a variable called `drinking_data`

# use View() to open this data
# View(any_drinking)

# find the number of rows and columns in this dataset

# extract the columns, 'state', 'location', 'both_sexes_2002', 'both_sexes_2003' and 'both_sexes_2004' and store it in a variable called `both_sex_drinking`

# create a function `find_mean` that takes in a vector of values (column from data set). Return the average of these values
# use your function to pass in each of the numerical data type columns in `both_sex_drinking`

# create a function `county_count` that takes in a state and return the number of counties that have data collected in the state
