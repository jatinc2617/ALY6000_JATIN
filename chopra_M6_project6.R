print("Jatin Chopra")

# Downloading packages
install.packages("tidyverse")
install.packages("ggalt")  #for dumbbell plots
install.packages("countrycode")
install.packages("rworldmap")
install.packages("gridExtra")
install.packages("broom")
install.packages("ggplot2")
install.packages("reader")
install.packages("magrittr")

#Installing packages
library(tidyverse)
library(ggalt)
library(countrycode)
library(rworldmap)
library(gridExtra)
library(broom)
library(ggplot2)
library(reader)
library(magrittr)
theme_set(theme_light())

# Importing data set
df<- read.csv("C:\\Users\\jatin\\Documents\\R\\ALY 6000\\Module 6\\R Script\\master.csv",header = TRUE)
class(df)
str(df)  #getting an idea of data set

head(df,5)
tail(df,5)


#changing column names
colnames(df)[1] <- "Country"
colnames(df)[7] <- "country_year"
colnames(df)[9] <- "GDP_for_year"
colnames(df)[10] <- "GDP_per_capita"

#plotting graph for missing values for easy visualization of missing values in the Data 
plot_Nullvalues <- function(data, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(df), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$a <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), a = unlist(data_temp$a))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(a))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

plot_Nullvalues(df[,colSums(is.na(df)) > 0, with = FALSE])


#Removing column HDI.for.year as there are 80% null values
df<- select(df, -c("HDI.for.year"))
head(df,5)

#checking the data
unique(df$generation)
unique(df$age)
unique(df$sex)

#there are 5 generation values and 6 age groups. So it doesn't makes any sense. So I try to avoid using generation column

#this should give 12 rows for each country_year
count_country_year <- df %>% group_by(country_year) %>%
  count() # it seems that 2016 data is incomplete so deleting 2016 data is its very less

df<- df%>% filter (year != 2016)

#checking for number of years each country data is recorded
country_count<- df%>% group_by(Country)%>% 
  summarise(rows = n(),years = rows/12)%>%
  arrange(years)



# Arranging the Data
df$continent <- countrycode(sourcevar = df[, "Country"],
                            origin = "country.name",
                            destination = "continent")

#Changing Data type for elements age and generation and making them ordinal by utilizing factor work

df$age<- factor(df$age, ordered = T,
                levels = c("5-14 years",
                           "15-24 years",
                           "25-34 years",
                           "35-54 years",
                           "55-74 years",
                           "75+ years"))

df$generation <- factor(df$generation, 
                        ordered = T, 
                        levels = c("G.I. Generation", 
                                   "Silent",
                                   "Boomers", 
                                   "Generation X", 
                                   "Millenials", 
                                   "Generation Z"))



glimpse(df)


Global_suicide_mean <- (sum(as.numeric(df$suicides_no)) / sum(as.numeric(df$population))) * 100000



df$suicide_per_100k<- (df$suicides_no/df$population)*100000

#############PLOT 1 - Scatter plot for Suicides per 100K over time period of 1985 - 2015##################

df %>%group_by(year) %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000) %>%
  ggplot(aes(x = year, y = suicides_per_100k)) + 
  geom_line(col = "deepskyblue3", size = 1) + 
  geom_point(col = "deepskyblue3", size = 2) + 
  geom_hline(yintercept = Global_suicide_mean, linetype = 2, color = "grey35", size = 1) +
  labs(title = "Global Suicides (per 100k)",
       subtitle = "Trend over time, 1985 - 2015.",
       x = "Year", 
       y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) + 
  scale_y_continuous(breaks = seq(10, 20))


##################PLOT 2#####################################
######  - Total suicide cases from year 1985 - 2015 
#https://community.rstudio.com/t/doubt-about-the-function-joincountrydata2map/31777/7


country_heat <- df %>%
  group_by(Country) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

countrydata <- joinCountryData2Map(country_heat, joinCode = "NAME", nameJoinColumn = "Country")

par(mar=c(0, 0, 0, 0)) # margins

mapCountryData(countrydata, 
               nameColumnToPlot="suicide_per_100k", 
               mapTitle="", 
               colourPalette = "heat", 
               oceanCol="lightblue", 
               missingCountryCol="grey65", 
               catMethod = "pretty")

mapCountryData(countrydata, 
               nameColumnToPlot="suicide_per_100k", 
               mapTitle="", 
               mapRegion = "eurasia", 
               colourPalette = "heat", 
               oceanCol="lightblue", 
               missingCountryCol="grey65", 
               addLegend = FALSE, 
               catMethod = "pretty")
















#####################################################################################################################
############PLOT 3##############
#Total suicides by continent and over time

df$continent <- countrycode(sourcevar = df[, "Country"],
                              origin = "country.name",
                              destination = "continent")
#Now that we have 5 continents added in our data frame we can see how suicide rates varies over time from 1985- 2015 in each continent

continent <- df %>%
  group_by(continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  arrange(suicide_per_100k)#count of suicides per 100K in each continent

continent$continent <- factor(continent$continent, ordered = T, levels = continent$continent)

continent_plot <- ggplot(continent, aes(x = continent, y = suicide_per_100k, fill = continent)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Worldwide Suicides per 100K by Continent",
       x = "Continent", 
       y = "Suicides per 100k", 
       fill = "Rainbow") +
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq(0, 20, 1), minor_breaks = F)


continent_time <- df %>%
  group_by(year, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

continent_time$continent <- factor(continent_time$continent, ordered = T, levels = continent$continent)

continent_time_plot <- ggplot(continent_time, aes(x = year, y = suicide_per_100k, col = factor(continent))) + 
  facet_grid(continent ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Continent", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Continent") + 
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)

grid.arrange(continent_plot, continent_time_plot, ncol = 2)







####################PLOT 4##########################################
#######Global suicides by gender and over time#################


sex_plot <- df %>%
  group_by(sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = sex, y = suicide_per_100k, fill = sex)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Global suicides (per 100k), by Sex",
       x = "Sex", 
       y = "Suicides per 100k") +
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq(0, 25), minor_breaks = F)

### with time
sex_time_plot <- df %>%
  group_by(year, sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = factor(sex))) + 
  facet_grid(sex ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Sex", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Sex") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)

grid.arrange(sex_plot, sex_time_plot, ncol = 2)



####################PLOT 5##########################################
#######Global suicides by age group and over time#################

age_plot <- df %>%
  group_by(age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = age, y = suicide_per_100k, fill = age)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Global suicides per 100k, by Age",
       x = "Age", 
       y = "Suicides per 100k") +
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F)

### with time
age_time_plot <- df %>%
  group_by(year, age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = age)) + 
  facet_grid(age ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Age", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Age") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)


grid.arrange(age_plot, age_time_plot, ncol = 2)



##########################PLOT 6######################################################
###### male female death rate by country###########


country_long <- df %>%
  group_by(Country, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  mutate(sex = "OVERALL")

sex_country_long <- df %>%
  group_by(Country, continent, sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)


sex_country_wide <- sex_country_long %>%
  spread(sex, suicide_per_100k) %>%
  arrange(male - female)


sex_country_wide$country <- factor(sex_country_wide$Country, 
                                   ordered = T, 
                                   levels = sex_country_wide$Country)

sex_country_long$country <- factor(sex_country_long$Country, 
                                   ordered = T, 
                                   levels = sex_country_wide$Country) # using the same order


country_gender_prop <- sex_country_wide %>%
  mutate(Male_Proportion = male / (female + male)) %>%
  arrange(Male_Proportion)

sex_country_long$country <- factor(sex_country_long$country, 
                                   ordered = T,
                                   levels = country_gender_prop$country)

ggplot(sex_country_long, aes(y = suicide_per_100k, x = Country, fill = sex)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportions of suicides that are Male & Female, by Country", 
       x = "Country", 
       y = "Suicides per 100k",
       fill = "Sex") + 
  coord_flip()
#4 countries data went missing due to some error


#############################################################################################
############################################################################################
#steepest increasing and decresing trend

df

df_specific <- df %>%
  filter(Country %in% c("Iceland", 
                        "Luxembourg",
                        "Norway", 
                        "Switzerland", 
                        "United States"))

df_specific %>% 
  group_by(Country, year) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = Country)) + 
  geom_smooth(se = F, span = 0.2) +
  scale_x_continuous(breaks = seq(1985, 2015, 2), minor_breaks = F) + 
  labs(title = " Suicide cases for United States, Iceland, Luxembourg, Switzerland, Norway", 
       subtitle = "Suicides per 100k population globally, 1985 - 2015", 
       x = "Year ", 
       y = "Suicides per 100k Globally", 
       col = "Country")+
  theme(axis.text.x = element_text(angle = 90)) 





#Thanks 
#JATIN CHOPRA



