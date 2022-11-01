########## Assignment 5

## Import libraries

# install.packages("pacman")
pacman::p_load(dplyr, tidyr, caret, ggplot2, caTools, corrplot, 
               PerformanceAnalytics, AER, MASS, stargazer, pscl, 
               jtools, Hmisc, ggcorrplot, rpart, rpart.plot, readxl,
               lme4, merTools, lattice, foreign, plm)

# Read file
df <- read_excel('C:/Users/Scott/Downloads/BigMartSales.xlsx', sheet = 'Data')

#################
# Data cleaning

str(df)

df$Item_Fat_Content <- as.factor(df$Item_Fat_Content)
df$Item_Type <- as.factor(df$Item_Type)
df$Outlet_Size <- as.factor(df$Outlet_Size)
df$Outlet_Type <- as.factor(df$Outlet_Type)
df$City_Type <- as.factor(df$City_Type)

levels(df$Outlet_Type)

###############################
# Data visualization
hist(df$Item_Sales)

histogram(~Item_Sales, data=df)

histogram(~log(Item_Sales), data=df)

#############################
# Creating models

m1 <- lmer(Item_Sales ~ Item_Weight + (1 | City_Type), data=df, REML=FALSE) 
summary(m1)
confint(m1)
AIC(m1)
fixef(m1)                                       # Magnitude of fixed effects
ranef(m1)                                       # Magnitude of random effects
coef(m1)                                        # Magnitude of total effects


m2 <- lmer(Item_Sales ~ Item_Fat_Content + Item_Type + Item_MRP + Item_Weight + Outlet_Year + Outlet_Size +
             ( 1 | City_Type), data = df, REML = FALSE)
summary(m2)
confint(m2)
AIC(m2)
fixef(m2)                                       # Magnitude of fixed effects
ranef(m2)                                       # Magnitude of random effects
coef(m2)                                        # Magnitude of total effects

m3 <- lmer(Item_Sales ~ Outlet_Type + Item_Visibility + Item_Fat_Content + Item_Type + Item_MRP + Item_Weight  + Outlet_Size +
             ( 1 | City_Type), data = df, REML = FALSE)
summary(m3)
confint(m3)
AIC(m3)
fixef(m3)                                       # Magnitude of fixed effects
ranef(m3)                                       # Magnitude of random effects
coef(m3)                                        # Magnitude of total effects

stargazer(m1, m2, m3, type="text", single.row=TRUE)
AIC(m1, m2, m3)

m4 <- lmer(Item_Sales ~ Outlet_Type + City_Type + (1 | Item_Weight), data=df, REML=FALSE) 
summary(m4)
