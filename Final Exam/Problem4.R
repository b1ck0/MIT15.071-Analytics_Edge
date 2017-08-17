# PERSONAL NUTRITIONIST VIA OPTIMIZATION

## Choosing a healthy and balanced eating pattern can help maintain a healthy body weight and reduce the risk for chronic diseases such as Type 2 diabetes and cardiovascular diseases. 
## The U.S. Department of Agriculture and Department of Health and Human Services drafted an updated dietary guidelines for 2015-2020 that, for the first time, specifically limits the intake of sugar to no more than 
## 10 percent of daily calories.
## With the changing guidelines, further understanding in nutrition science, and personal preferences and dietary restrictions, consumers can be faced with a multitude of choices at the grocery store aisle. In this 
## problem, we look at some real food data and use optimization to derive personalized nutritional recommendations.
## We get the nutritional values of each food from U.S. Department of Agriculture's API service (https://ndb.nal.usda.gov/ndb/doc/index). The results are aggregated and subsetted to some common food types. 
## In addition, the consumer can enter his or her preference for each food on a scale from 1 (least favorite) to 5 (most favorite).

## The data has the following fields:
## name: name of the food
## measure: the unit measure 
## energy: Calories (in kcal)
## protein: protein in gram
## sugar: total sugars in grams
## fat: total lipid (fat) in grams
## VC: Vitamin C in miligrams
## happiness: the preference rating from 1-5, with 5 being the most preferred

## The personal goal is to eat less sugar (in fact, as little as possible), while maintaining other nutritional requirements. 

## THIS PROBLEM IS SOLVED IN EXCEL

setwd("C:/Users/vay/Documents/GitHub/MIT15.071-Analytics_Edge/Final Exam")