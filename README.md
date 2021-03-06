# myrepo
Repository for testing my Git/GitHub setup

---
title: "QMM: Bulk Carriers Data Case"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Library setup
```{r, warning=FALSE, results=FALSE, message=FALSE}
library(dplyr)
library(lubridate)
```

# Step 1: Exploratory analysis 

Remember first to make sure that you are working in \texttt{R} in the same directory where you data files are located. 

## 1a) 

First, load the data and check the column types of each. 

```{r}
ship1 <- read.csv("ship1.csv", head=TRUE, row.names = NULL)
str(ship1)
```

You see that the date is a Factor, not a Date. Coerce it to be using the \texttt{as.Date()} function. Then, check that everything is in order. 

```{r}
ship1$Date <- as.Date(ship1$Date)
str(ship1)
```

You can then proceed. Use the \texttt{apply()} function to compute the mean over the columns (with MARGIN=2 - or over the rows with MARGIN=1) of the variables, excluding the Date variable: 

```{r}
apply(ship1[,-1], MARGIN=2, mean)
```

Do the same for the standard deviation, simply changing the function in apply:

```{r}
apply(ship1[,-1], MARGIN=2, sd)
```

## 1b) 

In the following, we plot the revenues over time. 

```{r}
plot(ship1$Date,ship1$Freight, type="l", xlab="Date",  ylab="Freight (BSI)")
```

We see that, in 2007 and 2008, the revenues were very high on average. This corresponds to a price bubble induced by the chinese economic boom (imports and exports were high at that time, alongside high speculation on the transportation prices). 

## 1c) 

In order to get the means of all variables for each year, one can use the \texttt{dplyr} verbs to do it efficiently. These verbs will be of crucial importance for those of you in the Business Analytics track that consider to take the Data Science lecture. 

First, one has to "mutate" a new column to the existing data frame \texttt{ship1}. Then, one can group the observations by this new column and summarise all variables means in a new object and display it. 

```{r}
ship1 <- mutate(ship1, Year=year(Date))
means <- summarise_all(group_by(ship1, Year), "mean")
print.data.frame(means)
```

Using the so-called pipe operator, %>%, one rewrite the above code as: 

```{r, results=FALSE, eval=FALSE}
ship1 <- ship1 %>% mutate(Year=year(Date))
(means <- ship1 %>% group_by(Year) %>% summarise(across(.fns=mean)))
```

This is another way to write the above code that helps to separate the operations. For example, when creating the means table, one uses the \texttt{ship1} data frame, group the observations by year and then summarise all columns by using their means. 

## 1d) 

The correlation matrix for the variables of interest can be found by coercing the variables of interest into a matrix and by then using the \texttt{cor()} function. 

```{r}
cor(as.matrix(ship1[,-c(1,6)]))
```

# Step 2: Simple regression: selling price as a function of age

## 2a) 

To easily access the columns, remember to use the \texttt{attach()} function: 

```{r}
attach(ship1)
plot(VesselAge, SellingPrice, xlab="Vessel age", ylab="Selling price", pch=16, cex=0.7)
```

## 2b) 

To fit a linear model in the third order, one uses the \texttt{I()} function with the correct polynomial expression, as, above order 2, an expression of the form VesselAge^3 will not be recognised. One uses the usual \texttt{lm()} and one can then obtain the usual summary for a regression of this type: 

```{r}
mod1 <- lm(SellingPrice~VesselAge+I(VesselAge^2)+I(VesselAge^3))
summary(mod1)
```

The regression equation is therefore: $$sp_i = 39.4027 - 1.4013 va_i - 0.0125 va_i^2 + 0.0007 va_i^3.$$

The coefficient of determination is 0.4921. This means that the cubic model explains approximately half of the variation in the selling price. 

If you want now to plot the regression line on the scatterplot from point 2a), you have to use a trick: you first create a sequence of points covering the interval of your x-axis, below it is the \texttt{seq} sequence, with a fine enough resolution (I chose steps of 0.5 for the line to be smooth enough); then, for each of the position in the sequence, you use the regression coefficients from the model fitted above and you use as the value of VesselAge this position in the sequence. The result is the red curve on the graph below. 

```{r}
plot(VesselAge, SellingPrice, xlab="Vessel age", ylab="Selling price", pch=16, cex=0.7)
seq <- seq(from=-5, to=35, by=0.5)
lines(seq, as.numeric(mod1$coefficients[1])+as.numeric(mod1$coefficients[2])*seq
  +as.numeric(mod1$coefficients[3])*seq^2+as.numeric(mod1$coefficients[4])*seq^3, col="red")
abline(lm(SellingPrice~VesselAge), col="blue")
```

For comparison, I added a blue curve with the automatic procedure when one has a simple linear regression, simply in VesselAge. One can see that the model in the third order better fits the behaviour of the points, at a cost of maybe overfitting. 

## 2c) 

In order to address this question, we first create a vector with 36 rows corresponding to the vessel ages from 0 to 35 years old. We fit our model at each of these ages and we create a new data frame with the two. We then use a for loop to obtain the relative changes over time. We augment our data frame with this new information.  
```{r, results=FALSE}
ageseq <- seq(0, 35, by=1)
fitval <- as.numeric(mod1$coefficients[1])+as.numeric(mod1$coefficients[2])*ageseq
  +as.numeric(mod1$coefficients[3])*ageseq^2+as.numeric(mod1$coefficients[4])*ageseq^3
dfmod1 <- as.data.frame(cbind(ageseq, fitval))

relvar <- c()
for(i in 1:nrow(dfmod1)){
  if(i==1){
    relvar[i] <- 0
  }else{
    relvar[i] <- (dfmod1$fitval[i]-dfmod1$fitval[i-1])/dfmod1$fitval[i-1]
  }
}
```

The resulting dataframe is: 

```{r}
dfmod1 <- as.data.frame(cbind(dfmod1, relvar))
print(dfmod1)
```

What we observe is that the selling prices are decreasing up to 31 years old and that they increase after this, by substantial amount. However, note that they increase in regions over which we have no data points (we have no vessels aged more than 32 years old). This is just the behaviour that you see in the graph above, with the red curve increasing again for high ages, a particularity of this third order model. You would observe a constant decrease with a simple linear model. 

## 2d) 

Thanks to the description of the exponential model in terms of a linear regression, one simply has to convert the selling prices in log and to then undertake a simple linear regression. 

```{r}
lnSellingPrice = log(SellingPrice) #note that the log is by default the natural log
mod2 <- lm(lnSellingPrice~VesselAge)
summary(mod2)
```

Now, to find $\beta_0$, one has to take the exponential (the inverse of the log base $e$), which is $e^{ 3.706520}=40.7119$ to report the model equation in the following form: $$sp_i = 40.7119e^{-0.0651 va_i}. $$

The coefficient of determination is 0.6869, a bit higher than the previous one. This model seems to better explain the variation in the response, i.e. the selling price. One can again plot the regression line of the exponential model on the scatterplot of 2a), by simply changing the equation in the above code, again using the sequence along the x-axis to create the curve, in green in the graph below: 

```{r}
plot(VesselAge, SellingPrice, xlab="Vessel age", ylab="Selling price", pch=16, cex=0.7)
seq <- seq(from=-5, to=35, by=0.5)
lines(seq, exp(as.numeric(mod2$coefficients[1]))*exp(mod2$coefficients[2]*seq), col="green")
lines(seq, as.numeric(mod1$coefficients[1])+as.numeric(mod1$coefficients[2])*seq
  +as.numeric(mod1$coefficients[3])*seq^2+as.numeric(mod1$coefficients[4])*seq^3, col="red")
```

For comparison, I added in red the curve corresponding to the cubic model. Note that, with the exponential model, we do not have this increasing effect in the right tail of the selling price, as we do in the cubic model. However, note that this exponential model does not take into account the residual value of the ships. The green curve decreases smoothly, yet when a ship is sold to be demolished, the scrapped materials (steel and others) are still worth 4 to 5 million USD, behaviour inconsistent with such exponential model. 
