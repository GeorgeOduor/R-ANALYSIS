---
title: "EFFECT OF DIET AND EXCERCISE ON WEIGHT LOSS"
author: "George"
date: "August 13, 2018"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#PROBLEM

How should someone loose weight effectively?What can we say about diets and exercises?.For an answer 180 participants were assigned to one of 3 diets and one of 3 exercise levels.The participants weight loss afer two months was taken.

In this project i am going to use Two way ANOVA technique to solve the problem.

*Why Am I Using Two Way ANOVA?*

Unlike one way anova where we have one factorial independent variable,Two way ANOVA compares mean of a single dependent continuous variable based two factorial independent variables.In this case i have two independent factorial variables (diet and exercise) and one continuous dependent variablet( weight loss).


#DATA IMPORT

```{r}
library(foreign)
wghtlss=read.spss("weightloss.sav",to.data.frame = TRUE,use.value.labels = TRUE)
names(wghtlss)
head(wghtlss)
```
#DATA CHEKING

```{r}
library(dplyr)
glimpse(wghtlss)
anyNA(wghtlss)#Checking for any missing value
summary(wghtlss)
```

#Descriptive Visualizations

```{r}
library(ggplot2)
wghtlss %>% ggplot(mapping = aes(x=wloss))+geom_histogram()+facet_wrap(~exercise)
```

##CONTINGENCY TABLES
```{r}
attach(wghtlss)
table(diet,exercise)
```

From this output its evident that we are dealing with a balanced design ie equal samples per group.

```{r}
wghtlss %>% group_by(diet,exercise) %>% summarise(mean=mean(wloss),Sdev=sd(wloss),Min=min(wloss),Max=max(wloss))
  
```
```{r}
fit =aov(wloss~exercise*diet)
summary(fit)
```

Above output gives us the two way anova table.The Pr(>F) colums tels us whether the effects and interactions are significant or not.If the effect of exercise was same for all the diets,then we can see we have 0.444 probability of finding our sample results.

###Report

An interaction betweeen diet and exercise could not be demonstrated ,F(4,171)=0.937,p=0.444.

##Result visualization

```{r}
interaction.plot(exercise,diet,wloss,col = rainbow(3),pch = c(16,18,20),main = "interaction between Exercise and Diet")
```














