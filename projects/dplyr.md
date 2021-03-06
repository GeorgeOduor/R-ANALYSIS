---
title: "DPLYR"
author: "George"
date: "July 17, 2018"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## ILLUSTRATING DPLYR PACKAGE CODES

Dplyr package is used to manipulate ,clean and and summarise datasets.Aldhough there are aditional base functions to use in R,dplyr packege makes life easy.Lets dig in!
```{r}
#loading  the package
library(dplyr)
```

## Data 
The data set is the iris data distributed with the base instalation.

## SELECTING RANDOM N ROWS
we use sample_function to select a random sample of n rows.
```{r}
rand=sample_n(iris,6)
rand
```
The above code selects a random sample of 6 rows.
##REMOVING OF DUPLICATE ROWS
In order to remove duplicate rows we use the distinct function.
Example
```{r}
dup=distinct(iris)
head(dup)
```
In case of our data the R will output all the rows because there is no duplicate record in the dat set.

## Select Function
We use select function to select only desired variables.

The syntax for looks like this:

select(data,vaiable_1,variable_2,...,variable_n) where "data" is the dataset we are dealing with and the variables are the different columns in the data set.

Example

Lets say we want to select only Sepal width,and petal Length in our iris data set,this is how the code will look like.

```{r}
swpl=select(iris,Sepal.Width,Petal.Length)
head(swpl)
```

# DROPPING VARIABLES.
It is a common case in analysis to drop some unwanted variables and exclude them from analysis.This can be done quite aesily by putting the  minus sign before the variable name while usingour "select()" function above.

Example

Lets say in the swpl data set created above we are intrested in dropping one variable say "Sepal.Width",

```{r}
new_swpl=select(swpl,-Petal.Length)
head(new_swpl)
```
# Starting with and ending with.
Lets say you want to select variables starting and ending with a particular letter.

The syntax is simple 

```{r}
#select(data,starts_with("prefix"))
#select(data,ends_with("prefix"))
```


Adding a negative sign before the statement starts_with() will automaticaly lead to dropping of the variables. 

# SELECTING VARIABLES THAT CONTAIN A PARTICULAR LETTER
The syntax is also simple in dyplr package
```{r}
#select(data,contains("match"))
```

# REORDERING VARIABLES IN R
Just in case you need to rearrange variables in R,we can use select function and type the variables in the direction we need.

Example

Say you want to put Species as the first variable in our data,

```{r echo=TRUE}
newsort=select(iris,Species,everything())
names(newsort)
```

# RENAMING VARIABLES WITH DPLYR

With dplyr we use "rename()" function to rename variables in a data frame.

The syntax is simple:
```{r}
#rename(data,new_name=old_name)
```

## FILTERING DATA IN DPLYR
Filtering removes data that is not required.

The syntax is:
```{r}
# filter(data,logical condition)
```
Example

Say we want to filter out oune specie in the iris data,

```{r}
setosa=filter(iris,Species=="setosa")
head(setosa)
```

# SUMMARISING DATA
In order to carry a quick descriptive statistics in our data,we can ise the summarise() function in R

Example

```{r}
summarise(iris,Petal.Length_mean=mean(Petal.Length))
```

# SUMMARIZING MULTIPLE
```{r}
summarise_at(iris,vars(Petal.Length,Petal.Width),funs(n(),mean,median))
```

```{r}
grouped=iris %>% group_by(Species)
```

# SUMMARISING BY CATEGORICAL VARIABLE

```{r}
sumgrp=iris %>% group_by(Species) %>% summarise_at(vars(Sepal.Width,Sepal.Length),funs(n(),mean(.,na.rm = TRUE)))
sumgrp
```

Simple!
