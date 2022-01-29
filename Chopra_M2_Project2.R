
print("Plotting Basics: Chopra")


# Installing and loading packages for basic plotting
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)

#loading BullTroutRML data in package FSA and FSAdata

BullTroutRML2
class(BullTroutRML2)
head(BullTroutRML2,3)
tail(BullTroutRML2,3)
summary(BullTroutRML2)
str(BullTroutRML2)
var(BullTroutRML2)
sd(BullTroutRML2$age)
sd(BullTroutRML2$fl)

#checking distribution of data of age and fl
print(skewness(BullTroutRML2$age))
hist(BullTroutRML2$age, main = "Distribution shape of age",xlab = "Age (yrs)",col = "red")

print(skewness(BullTroutRML2$fl))
hist(BullTroutRML2$fl, main= "Distribution shape of Fork Length (mm)", xlab = "Fork Length (mm)",col = "blue")


#Filtering Data
FilteredBullTroutRML2 <- filterD(BullTroutRML2,BullTroutRML2$lake == "Harrison")

headtail(FilteredBullTroutRML2,5)



str(FilteredBullTroutRML2)
summary(FilteredBullTroutRML2)

#scatter plot for fl and age

plot(FilteredBullTroutRML2$fl,FilteredBullTroutRML2$age,xlim = c(0,500),ylim = c(0,15),
     main = "Plot 1: Harrison Lake Trout",ylab = "Age (yrs)",xlab = "Fork Length (mm)",pch=16,col = "orange") 



# Histogram for age attribute
hist(FilteredBullTroutRML2$age, ylab = "Frequency", xlab = "Age (Yrs)", main = "Plot 2: Harrison Fish Age Distribution",
     col.main= "cadetblue", xlim = c(0,15), ylim = c(0,15),col = "cadetblue")


# Overdense plot for age and fl attributes 

x<- c(FilteredBullTroutRML2$fl)
y<- c(FilteredBullTroutRML2$age)

p <- ggplot(FilteredBullTroutRML2, aes(x,y)) + geom_point(size=8,alpha = 0.5, color = "Darkgreen")
p + labs(x= "Fork Length (mm) " , y="Age (yrs)", title="Plot 3: Harrison Density Shaded by Era") + 
  xlim(0,500) + ylim(0,15)


#  Combining head and tales
tmp <- rbind(head(FilteredBullTroutRML2,3),tail(FilteredBullTroutRML2,3))
tmp


# Values in era column in new tmp
tmp$era

# creating new vectors
pchs <- c(3,4)
colS <- c("red", "gray60")


#changing era attribute from factor to numeric
tmp$era<- as.numeric(tmp$era)
str(tmp$era)


# initializing cols vector with temp

colS[tmp$era]


#Plot 4 

plot(FilteredBullTroutRML2$fl,FilteredBullTroutRML2$age,main = "Plot 4: Symbol & Color by Era",xlim = c(0,500),ylim = c(0,15),
     xlab = "Fork Length (mm)", ylab = "Age (yrs)", pch = pchs, col = colS)


#plot 5 

plot(FilteredBullTroutRML2$fl,FilteredBullTroutRML2$age,main = "Plot 5: Regression Overlay",xlim = c(0,500),ylim = c(0,15),
     xlab = "Fork Length (mm)", ylab = "Age (yrs)", pch = pchs, col = colS[tmp$era])
abline(lm(FilteredBullTroutRML2$age~FilteredBullTroutRML2$fl,FilteredBullTroutRML2),col= "brown")

#checking regression values
reg<- lm(FilteredBullTroutRML2$fl ~ FilteredBullTroutRML2$age,FilteredBullTroutRML2)
reg



# plot 6 

plot(FilteredBullTroutRML2$fl,FilteredBullTroutRML2$age,main = "Plot 6: :Legend Overlay",xlim = c(0,500),ylim = c(0,15),
     xlab = "Fork Length (mm)", ylab = "Age (yrs)", pch = pchs, col = colS[tmp$era])
abline(lm(FilteredBullTroutRML2$age~FilteredBullTroutRML2$fl,FilteredBullTroutRML2),col= "brown")

legend("topleft",inset=c(0.05), legend = c("1977-80","1997-01"),pch = pchs, col = colS ,border = "black", title = "Levels of era",
       title.col = "blue",box.lwd = 1,box.lty = 2,box.col = "Black" , bg = rgb(1, 0, 0, alpha = 0.15),cex = 1.5)














