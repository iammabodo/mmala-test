##lOADING LIBRARIES


library(tidyverse)
library(skimr)
library(tidymodels)
library(gt)



#Loading data

myData <- read.csv("data/MMALA_question3.csv") |> 
  #Converting the variables below into factors
  mutate_at(.vars = c("restwma", "posse", "event"),
            .funs = as.factor) |> 
  #Converting the variables below to double
  mutate_at(.vars = c("age", "baseef", "dobef"),
            .funs = as.double) |> 
  #Converting the ID variable to character since there are no statistics to be performed on this variable
  mutate_at(.vars = "id",
            .funs = as.character)

myData1 <- read.csv("data/MMALA_question3 (1).csv")


vars <- myData1 |> select(-id)

# Correlation coefficient

mutate_all(vars, as.numeric) |> cor() |> corrplot::corrplot(addCoef.col = "white",
                                                            addCoefasPercent = T,
                                                            type = "upper",
                                                            order = "AOE")



#Basic Summary Statistics of the Data
summary_table <- myData|> 
  group_by(event, restwma, posse) |>
  summarise(mean_age = mean(age),
            se_age = sd(age)/sqrt(n()),
            median_age = median(age), 
            mean_baseef = mean(baseef),
            se_baseef = sd(baseef)/sqrt(n()),
            median_baseef = median(baseef),
            mean_dobef = mean(dobef),
            se_dobef = sd(dobef)/sqrt(n()),
            median_dobef = median(dobef),
            n_sample = n()) |>  
  ungroup()


#Summary Statistics using the psych package
sumtable <- as.data.frame(round(myData |> 
                                  select(age, baseef, dobef) |> 
                                  psych::describe(), 2)) |> 
  select(-vars,-trimmed, -mad, -range) |> 
  mutate(vars = c("Age", "Base EF", "Dob EF")) |> 
  select(vars, dplyr::everything())

## Formatting the table using the gt package
sumtable |>  gt() |> 
  tab_header(title = "Descritpive Statistics for the Continous Variables") |> 
  cols_label(vars = "Variables",
             n = "Sample",
             mean = "Mean",
             sd = "SD",
             median = "Median",
             min = "Minimum",
             max = "Maximum",
             skew = "Skew",
             kurtosis = "Kurtosis",
             se = "SE") |> 
  tab_source_note("Source: Author's Calculations")

## Reading in data as numeric in order to calculate the correlation coefficients 
myData1 <- read.csv("../data/MMALA_question3 (1).csv")

## Removing the id variable from the data frame
vars <- myData1 |> select(-id)

## Calculating and plotting correlations using the corrplot package
mutate_all(vars, as.numeric) |> cor() |> corrplot::corrplot(addCoef.col = "white",
                                                            addCoefasPercent = T,
                                                            type = "upper",
                                                            order = "AOE")


# Model Building 

model <- glm(event ~ restwma +dobef*posse, myData, family = "binomial")

model1 <- glm(event ~ restwma +dobef*posse + age, myData, family = "binomial")

model2 <- glm(event ~ restwma +dobef*posse + baseef, myData, family = "binomial")

model3 <- glm(event ~ restwma +dobef*posse + age + baseef, myData, family = "binomial")
summary(model1)
