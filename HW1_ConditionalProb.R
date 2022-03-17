## Elise Rust
## Homework #1
## ANLY 511
## Professor Gamage
## Sept. 13, 2021

## Load necessary packages
library(tidyverse)

#### Question #1:
## Define the variables

# calculate a - 11 digit to the right of decimal in sin(1.23)
options(digits = 11) # to stop the sin() function from truncating decimal places
x = sin(1.23)
x_vector <- as.numeric(strsplit(as.character(x), "")[[1]]) # convert to a vector to be able to subset 11th digit
  
a = x_vector[13]
#a = 3

# calculate b
b = sqrt(a^2 + pi*a^3)

# calculate c - number of digits to the left of the decimal
y = exp((log(b))^3)
decimal = as.integer(gregexpr(pattern = ".", y, fixed = TRUE)) # number of digits until decimal
c = decimal - 1

# calculate summation (d)
# i = index
j = 1
i <- j:c
d = sum(j^(7/2))

# calcualte e
e = floor(d)


### Question #2
# select random sample of 1,000 adults from 100 million
population = 1000000000
sample_1 <- sample(population,1000)

# a) What is the probability that you will be in this sample?
p1 = (1/population)*1000
# a.k.a. 1/100,000,000 chance of being selected * 1,000 people selected
print(p1)

# b) 2,000 samples of 1,000 adults are independently chosen. What is the probability I am not in any of these samples?
# independently = without replacement

## p(I am NOT in any single sample)
single_p = 1 - p1
# p(not in any of the 2000 samples)
p2 = single_p^2000
print(p2)
# equivalent to the summation between 1 and 2000 of (1-p1)

# c) How many samples must be selected for you to have a 0.5 probability of being in at least one sample?
outcome_probability = 0.5
p1 # probability of being in a sample

# summation (1 to n) of (p1) = 0.5
# 0.5 = (p1)^n
# log(0.5) = log(p1^n)
# log(0.5) = n*log(p1)
# log(0.5)/log(p1) = n
n = log10(outcome_probability)/log10(p1)
print(n)

### Question #3: Consider the baby names data for 2014. 
yob2014 <- read.csv("~/Desktop/Papers/Georgetown/Fall 2021/ANLY 511/Module1/yob2014.txt", header = FALSE)
names(yob2014) <- c("name","gender","count") # rename the columns of yob2014
head(yob2014) # print the first 6 rows of yob2014

#Write a function that computes the conditional probability P(gender = F |name = XXX) for a given character string XXX 

prob_gender_name <- function(input_name) {
  
  # convert input to a string
  input_name = toString(input_name)
  # define probability of given name in dataset yob2014
  yob2014$prob <- yob2014$count/sum(yob2014$count)
  # calculate intersection of gender = F and name = input name
  p_female_name = yob2014$prob[yob2014$name == input_name & yob2014$gender == "F"]
  # calculate probability of input name
  prob_name = yob2014$prob[yob2014$name == input_name & yob2014$gender == "F"] + yob2014$prob[yob2014$name == input_name & yob2014$gender == "M"]
  # calculate conditional probability
  conditional_probability = p_female_name/prob_name
  
  # print output
  cat("For baby name ",input_name, " the conditional probability of the baby's gender being 'Female' given their name is: ", conditional_probability, ". \n")
  
}

# Compute these conditional probabilities for the 10 most common female baby names of that year. 
popular_names <- yob2014 %>%
  filter(yob2014$gender == "F") %>%
  top_n(10, count)

for (i in popular_names$name) {
  prob_gender_name(i)
}

## For these names, the name with the maximal conditional probability is "Emma". This means that of the top female
# baby names, babies with the name "Emma" have the highest chance of being "female" (99.94%).


### Question #4: 

age_18_29 = .15
age_30_49 = .26
age_50 = .59

sum(age_50, age_30_49, age_18_29)
# should, and does, equal 1

perc_18_29 = .68
perc_30_49 = .54
perc_50 = .26

percentage = (age_18_29*perc_18_29) + (age_30_49*perc_30_49) + (age_50*perc_50)

cat("The percentage of all adult internet users who post photos or videos that they have found online is: ", percentage)

### Question #5:

prob_A = 1/3

# use runif(n) to simulate 100,000 Uniform(0,1) distributed R.V.
X <- runif(100000)
Y <- runif(100000)

# make a data frame for X and Y uniform random variables
df <- data.frame(X,Y)

# compute two additional columns for the events A and B as given in the problem
df$A <- df$X <= 1/3
df$B <- df$Y < sin(pi*X)

# estimate the relevant probabilities by subsetting
prob_B = sum(df$B == "TRUE")/length(df$B)
cat("The probability of event B being true is: ", prob_B)

# find intersection P(A and B) by subsetting
B.true <- df[df$B == "TRUE",]
A.true <- df[df$A == "TRUE",]
# Find the set intersection of the names
both <- intersect(B.true, A.true)
head(both)

A_and_B = length(both$A)/length(df$A)

# conditional probability P(A|B)
A_given_B = A_and_B/prob_B
print(A_given_B)

# conditional probability P(B|A)
B_given_A = A_and_B/prob_A
print(B_given_A)


### Question #6:






