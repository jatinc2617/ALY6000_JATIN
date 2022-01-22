print("JATIN CHOPRA")


#2. Install the vcd package
install.packages('vcd', repos = "http://cran.us.r-project.org")


#3. Import the vcd library
library(vcd)

#4. Plot a sales ~ temp scatter plot using the data below:
#  Sales data: (7,11,15,20,19,11,18,10,6,22)
#Temperature data: (69,81,77,84,80,97,87,70,65,90)
Sales_data <- c(7,11,15,20,19,11,18,10,6,22)
Temperature_data <- c(69,81,77,84,80,97,87,70,65,90)
plot(Sales_data,Temperature_data, type = 'p', xlab = 'Sales Data', ylab = 'Temprature Data',main = 'Scatter plot for sales and temprature data')

#5. Find the mean temperature
mean(Temperature_data)

#6. Delete the 3rd element from the sales vector
updated_sales_data <- c(Sales_data[ - 3])
updated_sales_data


#7. Insert 16 as the 3rd element into the sales vector
Inserted_sales_data <- append(Sales_data,16,2)
Inserted_sales_data


#8. Create a vector <names> with elements Tom, Dick, Harry
names<- c("Tom","Dick","Harry")
names

#9. Create a 5 row and 2 column matrix of 10 integers
dimensions<- list(c("row1","row2","row3","row4","row5"),c("col1","col2"))
mat<- matrix(1:10,nrow = 5,ncol = 2,byrow = TRUE,dimnames = dimensions)
mat

#10. Create a data frame <icSales> with sales and temp attributes
icSales<- data.frame(Sales_data,Temperature_data)
icSales


#11. Display the data frame structure of icScales
str(icSales)

#12. Display a summary of the icScales data frame
summary(icSales)

#13. Import the dataset Student.csv
df<- read.csv("C:\\Users\\jatin\\Documents\\ALY 6000\\R script\\Student.csv", header = TRUE)
df

#14. Display only the variable names of the Student.csv dataset
colnames(df)




