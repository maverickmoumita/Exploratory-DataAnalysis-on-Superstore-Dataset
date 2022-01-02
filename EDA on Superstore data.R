

###TASK 4 HAA###
library(readxl)
library(tidyverse) # metapackage with lots of helpful functions
library(dplyr)
library(readr)
library(corrplot)
library(qgraph)
library(jtools)
library(caret)
library(DataExplorer)
library(funModeling)
library(ggm)
library(survival)





#Exploratory Data Analysis (EDA) On Superstore Data_Task_2

#Reading dataset
data<- read_excel("C:/Users/DELL/OneDrive/Desktop/STATSLY/projects/Dataset/Superstore (1).xlsx")

str(data)

df_status(data)

introduce(data)

length(data)

plot_intro(data)

summary(data)


#Data preparation and Cleaning
#any null values?
is.null(Superstore_task_2)

#any duplicacy?
df1 <- data %>% distinct() ##yes, duplicates were removed


##We see that there is an outlier in the Sales feature, an unusual hike. Let's replace it with the mean of sales.
maxSales <- max(data$Sales)
df1$Sales <- replace(df1$Sales, df1$Sales==maxSales,mean(df1$Sales))


#Data Visualization

##Frequency table for categorial variables
freq(data)

# Plot bar charts with `price` feature
plot_bar(data,binary_as_factor = TRUE,ggtheme = theme_classic(),title="Barplots for each categorical variable")

#histogram
plot_histogram(data, binary_as_factor = TRUE,geom_histogram_args = list(bins = 30L), title = "Histogram Charts",ggtheme = theme_get(), nrow = 3L,  ncol = 2L)



#In the below graph, we see the following pattern that most of the sales have been triggered by the standard class of shipment mode.

#Sales vs Quantity      
ggplot(data=df1,aes(x=Quantity,y=Sales,fill=`Ship Mode`))+ geom_bar(stat = "identity")


#Sales vs Profit
ggplot(data = df1, aes(x = Sales, y = Profit, color = `Ship Mode`)) + geom_point()
#we see more profits/loss have been availed from the standard shipment class. But there are not higher range profits seen this feature.


#Sales vs Discount
ggplot() + geom_point(data = df1, aes(x = Discount, y = Sales, color = `Ship Mode`)) 
#It is evident from the above graph that discounts attract more sales. But discounts attract mostly the Standard Class shipment. Same day shipment mode receives the least discount offers.



#Profits vs Discount
ggplot() + geom_bar(data = df1, aes(x = Discount, y = Profit, fill = `Ship Mode`), stat = "identity") 
#Yes, we see clearly, the more discounts have been offered and redeemed, the lesser profits the segments have achieved. Products with no discounts show high range of profits but as the discount range increases, we only see more and more loss with hardly any profit.


##Profits vs Subcategory
ggplot() + geom_bar(data = df1, aes(x =`Sub-Category`, y = Profit, fill = Region), stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#We see that more losses have been incurred by the Binders industry mainly in the Central region and Machines and * Tables * industry.

#sales vs category
ggplot() + geom_bar(data = df1, aes(x = Category, y = Sales, fill = Region), stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#More Sales have been incurred by the technology category, then Furniture and office supplies. Mostly sales have been made from the West and East regions


ggplot() + geom_bar(data = df1, aes(x = Category, y = Profit, fill = Region), stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#The furniture category occurs more losses than losses in the technology and Office Supplies category.

# Displaying the coralation matrix
df <- df1[ c(18:21) ]
##Visualize the correlation matrix.
#make a correlation plot that will show the correlation of each variable with the others.##
plot_correlation(df, type = "c")

