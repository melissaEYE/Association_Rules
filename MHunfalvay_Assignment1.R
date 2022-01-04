# Melissa Hunfalvay. Last Revised: 6-7-2021
# Data 630
# Thoracic Surgery Dataset. Assignment 1
# Professor Firdu

#Section 1 
#Loading the data in R

# Set working directory and read the data
setwd("/Users/melissahunfalvay/Documents/HUN/My Professional Development/Machine Learning Data 630/Assignments/Assignment 1") 
# display the file names in the current working directory
dir()
#Use the read.csv command to load the ThoraricSurgerery.csv data into RStudio.  
ThoraricSurgerery<-read.csv(file="ThoraricSurgerery.csv", header=TRUE, sep=",", as.is = FALSE)
# Preview the data in spreadsheet View to ensure the read in looks accurate
View(ThoraricSurgerery)

# Change  variable names so they are more descriptive 

names(ThoraricSurgerery)[names(ThoraricSurgerery) == "DGN"] <- "Diagnosis"
names(ThoraricSurgerery)[names(ThoraricSurgerery) == "PRE4"] <- "FVC"
names(ThoraricSurgerery)[names(ThoraricSurgerery) == "PRE5"] <- "FEV1"
names(ThoraricSurgerery)[names(ThoraricSurgerery) == "PRE6"] <- "Zubrod"
names(ThoraricSurgerery)[names(ThoraricSurgerery) == "PRE7"] <- "Pain"
names(ThoraricSurgerery)[names(ThoraricSurgerery) == "PRE8"] <- "Haemoptysis"
names(ThoraricSurgerery)[names(ThoraricSurgerery) == "PRE9"] <- "Dyspnoea"
names(ThoraricSurgerery)[names(ThoraricSurgerery) == "PRE10"] <- "Cough"
names(ThoraricSurgerery)[names(ThoraricSurgerery) == "PRE11"] <- "Weakness"
names(ThoraricSurgerery)[names(ThoraricSurgerery) == "PRE14"] <- "Tumor_Size"
names(ThoraricSurgerery)[names(ThoraricSurgerery) == "PRE17"] <- "Diabetes"
names(ThoraricSurgerery)[names(ThoraricSurgerery) == "PRE19"] <- "Heart_A"
names(ThoraricSurgerery)[names(ThoraricSurgerery) == "PRE25"] <- "PAD"
names(ThoraricSurgerery)[names(ThoraricSurgerery) == "PRE30"] <- "Smoking"
names(ThoraricSurgerery)[names(ThoraricSurgerery) == "PRE32"] <- "Asthma"

# Run the command to preview the first 10 data rows.
head(ThoraricSurgerery, 10)

#Section 2
#Analysis-Model Development


# Perform exploratory analysis

# a) Provide basic description of the data
str(ThoraricSurgerery)

# b) Descriptive Statistics
# Run the summary command to display the descriptive statistics for all variables 
summary (ThoraricSurgerery)

# check for missing values
apply(ThoraricSurgerery, MARGIN =2, anyNA)

# c) Visualize the distribution of the data 

#Load packages 
#Load arules package 
#install.packages ("arules") #install the package (one time thing)
library("arules") #Run the library command (do this every time the package is needed)

#Load the arulesViz package for rule visualization
#install.packages("arulesViz")
library("arulesViz")

# Load ggplot2 for higher level visualization graphics
#install.packages("ggplot2")
library("ggplot2")

?hist # how to change the histogram

# By variable: Diagnosis, type = factor
# Simple Pie Chart
pie(table(ThoraricSurgerery$Diagnosis))
# Bar chart
barplot(table(ThoraricSurgerery$Diagnosis))

# By variable: Forced Vital Capacity (FVC), type = num
#histogram
hist(ThoraricSurgerery$FVC, xlab="Forced Vital Capacity", ylab = "Frequency")
#box plot
boxplot(ThoraricSurgerery$FVC, xlab="Forced Vital Capacity", ylab = "test")
# display the standard deviation
sd(ThoraricSurgerery$FVC)

# By variable: Volume exhaled (FEV1), type = num
#histogram
hist(ThoraricSurgerery$FEV1, xlab="Forced Exhaled Volume", ylab = "Frequency")
#box plot
boxplot(ThoraricSurgerery$FEV1, xlab="Volume Exhaled", ylab = "test")
# display the standard deviation
sd(ThoraricSurgerery$FEV1)

# By variable: Zubrod, type = factor (3)
# Simple Pie Chart
pie(table(ThoraricSurgerery$Zubrod))
# Bar plot
barplot(table(ThoraricSurgerery$Zubrod))

# By variable: PRE7,	Pain before surgery, type = logi
# Simple Pie Chart
pie(table(ThoraricSurgerery$Pain))
# Bar plot
barplot(table(ThoraricSurgerery$Pain))
# Percentage
table(ThoraricSurgerery$Pain)/length(ThoraricSurgerery$Pain)

# By variable: Haemoptysis, type = logi
# Simple Pie Chart
pie(table(ThoraricSurgerery$Haemoptysis))
# Bar plot
barplot(table(ThoraricSurgerery$Haemoptysis))
# Percentage
table(ThoraricSurgerery$Haemoptysis)/length(ThoraricSurgerery$Haemoptysis)

# By variable: Dyspnoea, type = logi
# Simple Pie Chart
pie(table(ThoraricSurgerery$Dyspnoea))
# Bar plot
barplot(table(ThoraricSurgerery$Dyspnoea))
# Percentage
table(ThoraricSurgerery$Dyspnoea)/length(ThoraricSurgerery$Dyspnoea)

# By variable: Cough, type = logi
# Simple Pie Chart
pie(table(ThoraricSurgerery$Cough))
# Bar plot
barplot(table(ThoraricSurgerery$Cough))
# Percentage
table(ThoraricSurgerery$Cough)/length(ThoraricSurgerery$Cough)

# By variable: Weakness, type = logi
# Simple Pie Chart
pie(table(ThoraricSurgerery$Weakness))
# Bar plot
barplot(table(ThoraricSurgerery$Weakness))
# Percentage
table(ThoraricSurgerery$Weakness)/length(ThoraricSurgerery$Weakness)

# By variable: Tumor_Size, type = factor (4)
# Simple Pie Chart
pie(table(ThoraricSurgerery$Tumor_Size))
# Bar plot
barplot(table(ThoraricSurgerery$Tumor_Size))

# By variable: Diabetes, type = logi
# Simple Pie Chart
pie(table(ThoraricSurgerery$Diabetes))
# Bar plot
barplot(table(ThoraricSurgerery$Diabetes))
# Percentage
table(ThoraricSurgerery$Diabetes)/length(ThoraricSurgerery$Diabetes)

# By variable: Heart_A, type = logi
# Simple Pie Chart
pie(table(ThoraricSurgerery$Heart_A))
# Bar plot
barplot(table(ThoraricSurgerery$Heart_A))
# Percentage
table(ThoraricSurgerery$Heart_A)/length(ThoraricSurgerery$Heart_A)

# By variable: PAD, type = logi
# Simple Pie Chart
pie(table(ThoraricSurgerery$PAD))
# Bar plot
barplot(table(ThoraricSurgerery$PAD))
# Percentage
table(ThoraricSurgerery$PAD)/length(ThoraricSurgerery$PAD)

# By variable: Smoking, type = logi
# Simple Pie Chart
pie(table(ThoraricSurgerery$Smoking))
# Bar plot
barplot(table(ThoraricSurgerery$Smoking))
# Percentage
table(ThoraricSurgerery$Smoking)/length(ThoraricSurgerery$Smoking)

# By variable: Asthma, type = logi
# Simple Pie Chart
pie(table(ThoraricSurgerery$Asthma))
# Bar plot
barplot(table(ThoraricSurgerery$Asthma))
# Percentage
table(ThoraricSurgerery$Asthma)/length(ThoraricSurgerery$Asthma)

# By variable: AGE, At time of surgery, type = int
#histogram
hist(ThoraricSurgerery$AGE)
#box plot
boxplot(ThoraricSurgerery$AGE, xlab="Age", ylab = "Test")
# display the standard deviation
sd(ThoraricSurgerery$AGE)

# By variable: Risk1Yr, survival T is died, type = logi
# Simple Pie Chart
pie(table(ThoraricSurgerery$Risk1Yr))
# Bar plot
barplot(table(ThoraricSurgerery$Risk1Yr))
# Percentage
table(ThoraricSurgerery$Risk1Yr)/length(ThoraricSurgerery$Risk1Yr)

#Preprocessing

# a) outliers

#FVC variable

# before
boxplot(ThoraricSurgerery$FVC, xlab="Forced Vital Capacity", ylab = "Frequency") # boxplot
summary(ThoraricSurgerery$FVC) # descriptives

# Find the outliers using the quantile() function to find the 25th and the 75th percentile of the dataset, and the IQR() function 
Q <- quantile(ThoraricSurgerery$FVC, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(ThoraricSurgerery$FVC)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

#Eliminate the outliers
eliminated<- subset(ThoraricSurgerery, FVC > (Q[1] - 1.5*iqr) & FVC < (Q[2]+1.5*iqr))

#after
boxplot(eliminated$FVC, xlab="Forced Vital Capacity", ylab = "Frequency") # boxplot
summary(eliminated$FVC) #descriptives

#FEV1 variable

# before
boxplot(ThoraricSurgerery$FEV1, xlab="Volume Exhaled", ylab = "test") # boxplot
summary(ThoraricSurgerery$FEV1) # descriptives


# Find the outliers using the quantile() function to find the 25th and the 75th percentile of the dataset, and the IQR() function 
Q <- quantile(ThoraricSurgerery$FEV1, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(ThoraricSurgerery$FEV1)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

#Eliminate the outliers
eliminated<- subset(ThoraricSurgerery, FEV1 > (Q[1] - 1.5*iqr) & FEV1 < (Q[2]+1.5*iqr))

#after
boxplot(eliminated$FEV1, xlab="Volume Exhaled", ylab = "test") # boxplot
summary(eliminated$FEV1) #descriptives

#AGE variable

# before
boxplot(ThoraricSurgerery$AGE, xlab="Age", ylab = "Years") #boxplot
summary(ThoraricSurgerery$AGE) # descriptives


# Find the outliers using the quantile() function to find the 25th and the 75th percentile of the dataset, and the IQR() function 
Q <- quantile(ThoraricSurgerery$AGE, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(ThoraricSurgerery$AGE)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

#Eliminate the outliers
eliminated<- subset(ThoraricSurgerery, AGE > (Q[1] - 1.5*iqr) & AGE < (Q[2]+1.5*iqr))

# after
boxplot(eliminated$AGE, xlab="Age", ylab = "Years") # boxplot
summary(eliminated$AGE) # Descriptives


# b) Remove unwanted variables


# Remove the Heart_A as all the values but 2 are false therefore provides no additional insight 
eliminated$Heart_A<-NULL

# Remove the PAD as all the values but 8 are false therefore provides no additional insight
eliminated$PAD<-NULL

# Remove Asthma as all the values but 2 are false therefore provides no additional insight
eliminated$Asthma<-NULL

# Remove Zubrod as all the values are between 02 on the scale indicating well functioning
eliminated$Zubrod<-NULL

head(eliminated)

# c) Discretize variables

# Notes: temp=ThoraricSurgerery # allows for iterating without rerunning whole script
  
# Discretize age 

summary(eliminated$AGE)    # Before
# permanent variable for discretization of age
eliminated$AGE<-cut(eliminated$AGE, breaks =c(0, 50, 60, 70, 80, 100), labels=c('-50', '50_59', '60_69', '70_79', '80+'))
summary(eliminated$AGE)    # After

# Discretize FVC - create categorical variable

summary(eliminated$FVC)    # Before
eliminated$FVC<-cut(eliminated$FVC, breaks =c(-Inf, 1.539, 2.411, 3.282, 4.153, 5.025, Inf), labels=c('-3SD', '-2SD', '-1SD','+1SD', '+2SD', '+3SD'))
summary(eliminated$FVC)     # After

# Discretize FEV1 - create categorical variable

summary(eliminated$FEV1)    # Before
eliminated$FEV1<-cut(eliminated$FEV1, breaks =c(-Inf, 1.011, 1.775, 2.499, 3.243, 3.987, Inf), labels=c('-3SD', '-2SD', '-1SD','+1SD', '+2SD', '+3SD'))
summary(eliminated$FEV1)    # After

summary(eliminated)
head(eliminated)

# Rules and Model Generation
# Run the method with default parameters
rules<-apriori(eliminated)
# preview the first 15 rules
inspect(rules[1:15])
#Run the summary command on rules
summary(rules)

# minlen option
#show the rules that have at least 2 items.
rules <- apriori(eliminated, parameter= list(minlen=2))
# preview the first 15 rules0,
inspect(rules[1:15])

# Sorting for easier inspection
#sort rules by lift descending and preview the first 10 rules
inspect(head(sort(rules, by="lift"),15))

# Set support and confidence thresholds
rules <- apriori(eliminated, parameter= list(minlen=2, supp=0.15, conf=0.90))
# preview the rules
inspect(rules[1:5])

# Pull the rules with the support above 0.10
rules.subset<-subset(rules, support>0.10)
summary(rules.subset)
inspect(rules[1:5])

#11. Rules Pruning

# remove redundant rules
rules.subset <- rules.subset[!is.redundant(rules.subset, measure = "confidence")]
inspect(head(rules.subset, 5))
#how many rules remain after pruning
length(rules.subset)
summary(rules.subset)
inspect(rules.subset)

#Pick a target variable and inspect
rules.subset <- apriori(eliminated, parameter = list(minlen = 2)) 
rules.subset2 <- subset(rules, subset = rhs %pin% "Risk1Yr")
summary(rules.subset2)

#Generate rules for a specific itemset
unique(eliminated["Smoking"])
rules.subset<-apriori(eliminated,  parameter= list(minlen=2, supp=0.15, conf=0.90),appearance=list(rhs=c("Smoking=FALSE", "Smoking=TRUE"), default="lhs"))
inspect(rules.subset[1:5])

#Final inspection
rules <- apriori(eliminated, parameter= list(minlen=2, supp=0.15, conf=0.90))
# preview the rules
inspect(rules[1:5])
summary(rules)
length(rules)

# Additional statistics

interestMeasure(rules, c("chiSquare", "conviction", "cosine", "coverage", "leverage", "oddsRatio"), eliminated)

data("eliminated")
rules <- apriori(eliminated, parameter = list(support = 0.15))
is.significant(rules, eliminated)


# Visualize results

#Scatterplot
plot(rules.pruned)
#graph
plot(rules.pruned, method="graph")

#Interactive plot

plot(rules.pruned,shading="lift",  measure="support")

plot(rules.pruned,method="grouped", shading="confidence",  measure="support")

plot(rules.pruned, shading="order", control=list(main = "Two-key plot"))

plot(rules.pruned, method="paracoord")

plot(rules.pruned, method="matrix", measure="lift", control=list(reorder="support"))

plot(rules.pruned, method="matrix", engine="3d", measure="lift", control=list(reorder="support"))


#Exit
q()
