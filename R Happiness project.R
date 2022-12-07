# Packages to load
library(data.table)
library (dplyr)
library(tibble)
library(tidyverse)
library(ggplot2)
library(scales)
library(plotly)
library(gridExtra)
library(plotly)
library(car)
# Visualizing Happiness
# Read the files
df_2015 <- read_csv("C:/Users/chimi/Desktop/Refactored_Py_DS_ML_Bootcamp-master/10-Data-Capstone-Projects/2016.csv")
df_2016 <- read_csv("C:/Users/chimi/Desktop/Refactored_Py_DS_ML_Bootcamp-master/10-Data-Capstone-Projects/2016.csv")
df_2017 <- read_csv("C:/Users/chimi/Desktop/Refactored_Py_DS_ML_Bootcamp-master/10-Data-Capstone-Projects/2017.csv")
df_2018 <- read_csv("C:/Users/chimi/Desktop/Refactored_Py_DS_ML_Bootcamp-master/10-Data-Capstone-Projects/2018.csv")
df_2019 <- read_csv("C:/Users/chimi/Desktop/Refactored_Py_DS_ML_Bootcamp-master/10-Data-Capstone-Projects/2019.csv")
df_2020 <- read_csv("C:/Users/chimi/Desktop/Refactored_Py_DS_ML_Bootcamp-master/10-Data-Capstone-Projects/2020.csv")
df_2021 <- read_csv("C:/Users/chimi/Desktop/Refactored_Py_DS_ML_Bootcamp-master/10-Data-Capstone-Projects/2020.csv")
df_2022 <- read_csv("C:/Users/chimi/Desktop/Refactored_Py_DS_ML_Bootcamp-master/10-Data-Capstone-Projects/2020.csv")
# Shape of the data
dim_list <- list(dim(df_2015),dim(df_2016),dim(df_2017),dim(df_2018),
     dim(df_2019),dim(df_2020),dim(df_2021),dim(df_2022))
print(dim_list)
#Data cleansing

# Add a new year column to all dataframes
years <- list(2015,2016,2017,2018,2019,2020,2021,2022) 
df_list <- list(df_2015,df_2016,df_2017,df_2018,df_2019,df_2020,df_2021,df_2022)

# Add a new year column to all years
for (i in seq_along(years)){
  df_list[[i]][['Year']] <- years[i]
}

# Re - save the column changes in the original.
df_2015 <- rbind(df_list[[1]])
df_2016 <- rbind(df_list[[2]])
df_2017 <- rbind(df_list[[3]])
df_2018 <- rbind(df_list[[4]])
df_2019 <- rbind(df_list[[5]])
df_2020 <- rbind(df_list[[6]])
df_2021 <- rbind(df_list[[7]])
df_2022 <- rbind(df_list[[8]])
# Report 2015
colnames(df_2015)
df_2015 <- df_2015 %>% rename('Rank' = 'Happiness Rank',
                              'Score'=  'Happiness Score',
                              'GDP_per_capita' = 'Economy (GDP per Capita)',
                              'Life_Expectancy' = 'Health (Life Expectancy)',
                              'Social_support' = 'Family',
                              'Corruption' =       'Trust (Government Corruption)',
                             'Country_Generosity' = 'Generosity')

# Display rows
head(df_2015, 5)
tail(df_2015, 5)
# 2016
colnames(df_2016)
df_2016 <- df_2016 %>% rename('Rank' = 'Happiness Rank',
                              'Score'=  'Happiness Score',
                              'GDP_per_capita' = 'Economy (GDP per Capita)',
                              'Life_Expectancy' = 'Health (Life Expectancy)',
                              'Social_support' = 'Family',
                              'Corruption' =       'Trust (Government Corruption)',
                              'Country_Generosity' = 'Generosity')

# Display rows
head(df_2016, 5)
tail(df_2016, 5)
#2017
colnames(df_2017)
df_2017 <- df_2017 %>% rename('Rank' = 'Happiness.Rank',
                              'Score'=  'Happiness.Score',
                              'GDP_per_capita' = 'Economy..GDP.per.Capita.',
                              'Life_Expectancy' = 'Health..Life.Expectancy.',
                              'Social_support' = 'Family',
                              'Corruption' =       'Trust..Government.Corruption.',
                              'Country_Generosity' = 'Generosity')
#Display rows
head(df_2017, 5)
tail(df_2017, 5)

#2018
colnames(df_2018)
df_2018 <- df_2018 %>% rename('Rank' = 'Overall rank',
                              'Country' = 'Country or region',
                              'GDP_per_capita' = 'GDP per capita',
                              'Social_support' = 'Social support',
                              'Life_Expectancy' = 'Healthy life expectancy',
                              'Freedom'         = 'Freedom to make life choices',
                              'Corruption'      =  'Perceptions of corruption',
                              'Country_Generosity' = 'Generosity')
# Display rows
head(df_2018, 5)
head(df_2018, 5)

# 2019
colnames(df_2019)
df_2019 <-df_2019 %>% rename('Rank' = 'Overall rank',
                             'Country' = 'Country or region',
                             'GDP_per_capita' = 'GDP per capita',
                             'Social_support' = 'Social support',
                             'Life_Expectancy'= 'Healthy life expectancy',
                             'Freedom'        = 'Freedom to make life choices',
                             'Corruption'     = 'Perceptions of corruption',
                             'Country_Generosity' = 'Generosity')
# Display rows
head(df_2019)
tail(df_2019)
# 2020
colnames(df_2020)
df_2020 <- df_2020 %>% rename('Country'= 'Country name',
                              'Region' = 'Regional indicator',
                              'Score'  = 'Ladder score',
                              'Social_support'= 'Explained by: Social support',
                              'GDP_per_capita'= 'Explained by: Log GDP per capita',
                              'Life_Expectancy'= 'Explained by: Healthy life expectancy',
                              'Freedom'       =  'Explained by: Freedom to make life choices',
                              'Corruption' =    'Explained by: Perceptions of corruption', 
                              'Country_Generosity' = 'Explained by: Generosity')

# Display rows
head(df_2020, 5 )
tail(df_2020, 5)
#2021
colnames(df_2021)
df_2021 <- df_2021 %>% rename('Country'='Country name',
                   'Region'= 'Regional indicator',
                   'Score' = 'Ladder score',
                   'GDP_per_capita' = 'Explained by: Log GDP per capita',
                   'Social_support' = 'Explained by: Social support',
                   'Life_Expectancy' = 'Explained by: Healthy life expectancy',
                   'Freedom'         = 'Explained by: Freedom to make life choices',
                   'Country_Generosity'     =  'Explained by: Generosity',
                   'Corruption' =    'Explained by: Perceptions of corruption')

# Displays rows 
head(df_2021, 5)
tail(df_2021, 5)

# 2022
colnames(df_2022)
df_2022 <- df_2022 %>% rename('Country'='Country name',
                   'Region'= 'Regional indicator',
                   'Score' = 'Ladder score',
                   'GDP_per_capita' = 'Explained by: Log GDP per capita',
                   'Social_support' = 'Explained by: Social support',
                   'Life_Expectancy' = 'Explained by: Healthy life expectancy',
                   'Freedom'         = 'Explained by: Freedom to make life choices',
                   'Country_Generosity'     =  'Explained by: Generosity',
                   'Corruption' =    'Explained by: Perceptions of corruption')
# Display rows and columns 
head(df_2021, 5)
tail(df_2022, 5)


# Reorder the columns
df_list <- list(df_2015, df_2016, df_2017, df_2018, 
                df_2019, df_2020, df_2021, df_2022)
df_list <- lapply(df_list, \(x) {
  if(is.character(x$Corruption)) x$Corruption <- as.numeric(x$Corruption)
  x
})

col_names <- lapply(df_list, names)
common_cols <- Reduce(intersect, col_names)

#Combined final nested list
files <- lapply(df_list, `[`, common_cols)
files <- Reduce(dplyr::full_join, files)

# Convert to a dataframe
files <- data.frame(lapply(files,FUN=unlist))

# Shape of the final 
print(ncol(files))
print(nrow(files))


# Final columns
colnames(files)

#Structure of data
cat(str(files))

# Displays the first rows and last rows of the files
head(files, 5)
tail(files, 5)

#Descriptive Stats
summary(files)

# Check for duplicates
sum(duplicated(files))

# Treat outliers
attach(files)
par(mfrow=c(4,2))
fig.dim=c(8,6)
boxplot(Score, main='Boxplot of Score')
boxplot(GDP_per_capita, main='Boxplot of GDP_per_capita')
boxplot(Social_support,main='Boxplot of Social_support')
boxplot(Life_Expectancy, main='Boxplot of Life_Expectancy')
boxplot(Freedom, main='Boxplot of Freedom')
boxplot(Corruption,main='Boxplot of Corruption')
boxplot(Country_Generosity,main='Boxplot of Country_Generosity')

# Outlier Treatment By Capping for Corruption and Country_Geneoristy
files$Corruption <- squish(files$Corruption, quantile(files$Corruption, c(.05, .95),na.rm=TRUE))
files$Country_Generosity <- squish(files$Country_Generosity, quantile(files$Country_Generosity, c(.05, .95)))

# Missing values entire columns
apply(X = is.na(files), MARGIN = 2, FUN = sum)

# Drop rows with missing values
files <- na.omit(files)

# Uni-variate data analysis of numeric data
numeric_data <- files[2:8]
attach(numeric_data)
lapply(X=c(names(numeric_data)),FUN=function(x)hist(numeric_data[,x],main=paste('Distribution of',x),xlab=x))

#Bivariate Analysis between numeric data
for (i in 2: length(numeric_data)){
  print(ggplot(numeric_data, aes(x= Score, y = numeric_data[,i]))+
        geom_point() +ylab(colnames(numeric_data)[i])+
          ggtitle('Bivariate Analysis between Score and',colnames(numeric_data)[i])
        )
}

# Correlation plot
library(ggcorrplot)
ggcorrplot::ggcorrplot(cor(numeric_data))

# Which countries were consistently ranked the top 10 happiest from year 2015-2022?
top_10 <- files %>%
  group_by(Year)%>%
  head(10) %>%
  group_by(Country)%>%
  summarise(across(everything(), mean))

# Which countries have consistently ranked as the top 10 happiest countries from Year 2015 to 2022?

plot1 <- ggplot(top_10,aes(x=reorder(Country,+Score),y=Score)) +
  geom_bar(stat='identity',fill='purple') +
  coord_flip() 

# which Countries have consistently ranked as the bottom 10 unhappiest countries?
bottom_10 <- files %>%
  group_by(Year)%>%
  tail(10) %>%
  group_by(Country)%>%
  summarise(across(everything(), mean))

plot2 <- ggplot(bottom_10,aes(x=reorder(Country,-Score),y=Score)) +
  geom_bar(stat='identity',fill='purple') +
  coord_flip() 

grid.arrange(arrangeGrob(plot1, top = 'Top 10 Happiest Countries '), arrangeGrob(plot2, top = '10 Unhappiest Countries'), top = "Global Title", ncol=2)

# Overall average happiness and social factors score of top 10 countries
plot_ly(top_10, x=~Score, y=~`Country`, type='bar', name='Score',text = top_10$Score, textposition = 'auto') %>%
add_trace(x=~GDP_per_capita,y=~`Country`, name='GDP_per_capita',text = top_10$GDP_per_capita, textposition = 'auto') %>%
add_trace(x=~Social_support,y=~`Country`, name='Social_Support',text = top_10$Social_support, textposition = 'auto') %>%
add_trace(x=~Social_support,y=~`Country`, name='Life_Expectancy',text = top_10$Life_Expectancy, textposition = 'auto') %>%
add_trace(x=~Freedom,y=~`Country`, name='Freedom',text = top_10$Freedom, textposition = 'auto') %>%
add_trace(x=~Corruption,y=~`Country`, name='Freedom',text = top_10$Corruption, textposition = 'auto') %>%
add_trace(x=~Country_Generosity,y=~`Country`, name='Freedom',text = top_10$Country_Generosity, textposition = 'auto')%>%
layout(yaxis = list(title = 'Average'), 
         title = 'Figure 1.8: Interactive stacked bar chart on Age and Favorite',
         barmode = 'stack')

# Overall average happiness and social factors score of top 10 countries
plot_ly(bottom_10, x=~Score, y=~`Country`, type='bar', name='Score',text = top_10$Score, textposition = 'auto') %>%
  add_trace(x=~GDP_per_capita,y=~`Country`, name='GDP_per_capita',text = top_10$GDP_per_capita, textposition = 'auto') %>%
  add_trace(x=~Social_support,y=~`Country`, name='Social_Support',text = top_10$Social_support, textposition = 'auto') %>%
  add_trace(x=~Social_support,y=~`Country`, name='Life_Expectancy',text = top_10$Life_Expectancy, textposition = 'auto') %>%
  add_trace(x=~Freedom,y=~`Country`, name='Freedom',text = top_10$Freedom, textposition = 'auto') %>%
  add_trace(x=~Corruption,y=~`Country`, name='Freedom',text = top_10$Corruption, textposition = 'auto') %>%
  add_trace(x=~Country_Generosity,y=~`Country`, name='Freedom',text = top_10$Country_Generosity, textposition = 'auto')%>%
  layout(yaxis = list(title = 'Average'), 
         title = 'Figure 1.8: Interactive stacked bar chart on Age and Favorite',
         barmode = 'stack')

#
#Which country had the greatest increase in happiness from 2015 to 2022?
# Finland has had the great increase
top10_trendanalysis<- files %>%
  group_by(Country) %>%
  summarise(mean_score = mean(Score)) %>%
  arrange(-mean_score) %>%
  slice_head(n=10)

top10_clist <- c(top10_trendanalysis$Country)

df_clist <- subset(files,Country %in% top10_clist)

ggplot(df_clist,  aes(x = Year,y = Score,color = Country))+  geom_line()

# Social factors and trend-they remain flat after covid.
ggplot(files, aes(x=Year))+
  stat_summary(aes(y = GDP_per_capita, 
                   colour= 'Economy'), 
               geom = 'line',
               fun = "mean")+
  stat_summary(aes(y = Social_support, 
                   colour= 'Social_support'), 
               geom = 'line',
               fun = "mean")+
  stat_summary(aes(y = Life_Expectancy, 
                 colour= 'Life_Expectancy'), 
             geom = 'line',
             fun = "mean")+
  stat_summary(aes(y = Freedom, 
                   colour= 'Freedom'), 
               geom = 'line',
               fun = "mean")+
  stat_summary(aes(y = Corruption, 
                   colour= 'Corruption'), 
               geom = 'line',
               fun = "mean")+
  stat_summary(aes(y = Country_Generosity, 
                   colour= 'Generosity'), 
               geom = 'line',
               fun = "mean")+
labs(y='Mean Social Score')


# Store only social factors
files <- files[,c('Score','GDP_per_capita','Social_support','Life_Expectancy' ,'Freedom','Corruption','Country_Generosity')]
glimpse(files)

#Make sure there are no missing values on the final model
colSums(is.na(files))

# Partition the data into 80 and 20 -80% for train, and 20 for test
set.seed(163)
intial_filesplit <- sample(c(TRUE,FALSE),nrow(files),replace=TRUE,prob=c(0.8,0.2))
train <- files[intial_filesplit,]
test <- files[!intial_filesplit,]

# Print shape of train and test
dim(train)
dim(test)

# Building a model
lm_model1 <- lm( formula= Score ~.,data=train)
lm_model2 <- step(lm_model1, direction='both')
summary(lm_model2) # Keeping all the variable since P value < 0.05 and all AIC values are low.


#Step wise Regression using AIC criteria
library(MASS)

#Moderate correlation between variables as VIF is less than 5.
vif(lm_model2)

#Validate with the test data
pred_test <- predict(lm_model2, newdata=test)
head(pred_test)
#Plot the actual and predicted amount spent
# All variables are independent of each other due to VIF being less than 1 to 5
vif(lm_model2)

#
qqPlot(lm_model2$residuals)

#Normal distribution
plot(density(lm_model2$residuals))

# Heterodascity
plot(lm_model2$fitted.values,
     lm_model2$residuals)

# Error check
library(Metrics)
RMSE = rmse(test_pred, test$Score)


plot(lm_model)

