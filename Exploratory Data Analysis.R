# The objective is to find out the weak areas where work can be done to improve profit 
# and to find out business problems from the data.
#Installing packages and loading the libraries using [install.packages() and library()]
# ggplot2, fast dummies, dlookr (already installed)

##Loading of data
data<- read.csv("C:/Users/HP/Desktop/R Practice/SampleSuperstore.csv", header = TRUE)

##Structure of data
str(data)

#Summary of the data (Finding the central tendencies)
summary(data)

#Visualisation of the data

ggplot(data, aes(x = State, fill = State))+geom_bar(stat = "count")
ggplot(data, aes(x = Country, fill = Country))+geom_bar(stat = "count")
ggplot(data, aes(x = Sub.Category, fill = Sub.Category))+geom_bar(stat = "count")
ggplot(data, aes(x = Category, fill = Category))+geom_bar(stat = "count")
ggplot(data, aes(x = Ship.Mode, fill = Ship.Mode))+geom_bar(stat = "count")
ggplot(data, aes(x = Segment, fill = Segment))+geom_bar(stat = "count")
ggplot(data, aes(x=Sub.Category, y=Profit)) + geom_col()
ggplot(data, aes(x=Sub.Category, y=Sales)) + geom_col()
pairs(~Quantity+Sales+Profit, data = data)



#Observations
  #No null/missing values
  #Sales and Profit may have outliers or skewed data
  #Profit have some functional relation with Sales
  #Country variable can be ignored since the entire data is from USA 

#Outlier Treatment
quantile(data$Sales, 0.99)
uv = 3*quantile(data$Sales, 0.99)
data$Sales[data$Sales>uv] <-uv

summary(data)

quantile(data$Profit, 0.99)
uv_1 = 3*quantile(data$Profit, 0.99)
data$Profit[data$Profit>uv_1] <- uv_1


summary(data)

#Removing useless data
data_new <- data[, -1]
data_new <- data_new[, -2]
data_new <- data_new[, -2]
data_new <- data_new[, -2]
data_new <- data_new[, -2]

#Transformation of Categorical Variables
data_new = dummy_cols(data_new, remove_first_dummy = TRUE)
data_new <- data_new[,-1]
data_new <- data_new[,-1]
data_new <- data_new[,-1]
data_new <- data_new[,-1]


#Data Analysis
describe(data_new)
corr<- correlate(data_new)
corr <- correlate(data_new[c("Sales", "Quantity", "Discount", "Profit")])
plot(correlate(data_new))


#Normalisation of the data
normality(data_new, sample = 9994)
?normality          
[c("Sales", "Quantity", "Discount", "Profit")])
plot_normality(Sales, .data = data_dummyy)
plot_normality(Profit, .data = data_dummy)
plot_normality(Discount, .data = data_dummy)


##Conclusion
#Data had no null values
#Data was cleared and adjusted for outlier/skewed values
#Categorical Variables were assigned dummies
#Correlation and bar-plots were created and examined to determine relations between variables 
#The data can be used to identify: Sale of item from what category/sub-category was profitable 
#The data can be used to identify: Sale from store in which state and city was profitable
#The data can be used to identify: Where was the sale highest
#The data can be used to identify: What category was the sale highest