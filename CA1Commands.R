#Reading in the cars data set
cars <- read.csv("cars.csv")
print(cars)

# Dataset description link: https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html

# Objective: To find the central point in the data set. To see how data is distributed 
# around the center point of the data. Use visualisations to show the center point and 
# the data distribution.


######### 1. First statistical test measuring Centrality,dispersion and plotting #########

# Use MPG as it varies with the size of the engine, make and model along with other variables and
# does not have any missing, incomplete or invalid data

# Calculate the Mode, Mean and Median to get the centrality

# Read in mpg data
MilesPerGal <- cars$mpg

# Getting the average Miles per gallon of all the cars combined in the dataset.
# Print out the mpg mean using cat to concatenate the values for output
cat('Mean MPG is: ', mean(MilesPerGal))

# Print out the mpg median
cat('Median MPG is: ', median( MilesPerGal))

# Creating a mode function and assign to variable
get_mode <- function(vector) {
  # Get the list of unique values
  uniqv <- unique(vector)
  #print(uniqv)
  # Get the value with the most occurrences
  uniqv[which.max(tabulate(match(vector, uniqv)))]
}

#Printing the mode of Miles per gallon in the cars data set. 
cat('Mode of MPG is: ', get_mode(MilesPerGal))
# This determines the most frequently occurring number in the mpg column. 
# Which is 21 miles per gallon. This is very plausible as its close to the mean MPG value.


# The standard deviation provides data on the data distribution

# Getting the st deviation of Miles Per Gallon to see the dispersion of the data
# This tells me the data is dispersed in the Miles per gallon column as the 
# mean and std have a big range between each other.
cat('Standard Dev MPG is: ', sd(MilesPerGal))



# A histogram and boxplox will show more information on the dispersion of the data

#Creating a boxplot and histogram 1. to confirm if the mean looks correct and 2. to confirm if the mode is correct
hist(MilesPerGal, main="Histogram showing the mpg in cars")

#No outliers either according to the boxplot.
boxplot(MilesPerGal, main="Miles per gallon of cars Boxplot",  horizontal = TRUE)



######################################### Q2 Statistical test #############################
# The purpose of this test is to check if there are any non normailities 
# in this dataset, are there any outliers? if more data is to be added to 
# this dataset over time is there any evidence of serial correlation?. 
#
# I will attempt to answer this by printing out 4 visualizations, a boxplot,
# histogram and 2 scatter plots.

# Get all the MPG values
mpg_values <- cars$mpg

par(mfrow=c(2,2)) # 2 rows 2 columns
plot(mpg_values, main="MPG Scatter Plot") # y relation graph

boxplot(mpg_values, main="MPG Boxplot",  horizontal = TRUE ) # boxplot
# Plot the mean as a blue point on the diagram
points(mean(mpg_values), col="blue", pch=19)
# Draw a historgram of the mpg values and call it Histogram
hist(mpg_values, main="Histogram")


# no outliers in the mpg variable
# As the scatter plots show there are no outliers or irregularities in the data the 
# scatterplots are dense.
# The Histogram shows the data has a weak positive skew
# Plot mpg values and name the plot scatter plot
plot(mpg_values, main="Scatter Plot")


# Interquartile range is between 15.43 and 22.80. (22.80 x 1.5=34.20) Anything above 24.20 is 
# deemed an outlier. (15.43/1.5=10.29) Any below 10.29 is deemed an outlier.

# A general rule is an outlier is 1.5 times the interquartile range (Anything above the 3rd quartile or below the first quartile).
# Looking at the min and max when getting the IQR there are no deemed outliers in Miles Per Gallon.
# Checking the mean median mode min max and interquartile range of the dataset
summary(mpg_values)

# Testing for normality
# I am going to use a QQPlot to check for data normality in the cars dataset
par(mfrow=c(1,1))
# Check if the data is normal
qqnorm(mpg_values)
# Draw the exptected normal distribution line
qqline(mpg_values,lty=2)


#################### Q3.	Modelling Relationships #########################
# The purpose of the scatter plot im going to display, is to see if there is a relationship
# between Horsepower(hp) and the number of cylinders in a car (cyl)
# I am going to output multiple visualisations to confirm if there is any sort of relationship.
horsepower <- cars$hp
carCylinders <- cars$cyl

# As expected it seems there is a strong correlation between the number of cylinders in the car,
# the more horsepower it can provide.
plot(carCylinders, horsepower, main="Relationship of hp and cyl", xlab="Car cylinders", ylab="Horsepower", pch=15)
 
# Use plots to determine the correlation between the hp and cyl variables.
cor(carCylinders, horsepower)
cor.test(carCylinders, horsepower)
# The pearson correlation coefficient test confirms these two variables show a very strong relationship (0.8324475) as the scatterplot above showed. Concluding the more cylinders a car has means the more horsepower the car provides.

#Q3B) I would like to test displacement and car weight. I dont expect their to be any relationship between the two as displacement is the volume of cylinders in the car and weight of the car is self explanatory. However the heavier a car is the more power it needs to move than cars with less weight.
carWeight <- cars$wt
Displacement <- cars$disp
carCylinders <- cars$cyl

#Plotting car weight against engine displacement
plot(carWeight, Displacement, main="relationship between car weight and engine displacement",xlab="Car weight", ylab="engine Displacement", pch=25)

cor(carWeight, Displacement)
cor.test(carWeight, Displacement)
# To my suprise car weight and engine displacement have a strong postive correlation. This was a suprise to me because I wasnt aware perhaps of how much more horsepower it takes to move heavier cars. Because this dataset consists of fast cars this could explain the much more powerful engines to get these cars driving very fast.

# In further Detail, I will compare car cylinders to car displacement and car weight as displacement and cylinders are needed to to drive the car. 
# As expected these two variables have a higher correlation again (0.9) compared to car weight and displacement. This tells me they are heavily dependent on each other to run the car engine.
cor(Displacement, carCylinders)

#Pearson Correlation test
cor.test(Displacement, carCylinders)

## I would like to compare car Cylinders to car weight and to to see if there are any interesting observations.
cor(carCylinders, carWeight)

#Pearson Correlation test
cor.test(carCylinders, carWeight)
## The relationship is 0.78, which is not too suprising as there needs to be more cylinders in the engine to move the car faster. 

