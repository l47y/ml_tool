# ml_tool

Try it here: https://l47y.shinyapps.io/ml_tool/
Note that it may be very slow when running on shinyapps.io
To run it locally you can use ```shiny::runGitHub("ml_tool", "l47y")```

## Summary
This shiny app serves as a little user interface in purpose of demonstrating some standard tasks of machine learning. You upload a csv file of some example data and you play around with it. Originally I just wanted to make it capable of producing some plots of data (and to try shiny) but I decided to extend it a little bit. Mainly the mlr package is used in the background. I tested it with the following well known datasets: 
* Titanic data from Kaggle (downloadable here: https://www.kaggle.com/c/titanic/data)
* Boston House Prices data from Kaggle (https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data)
* Iris data (available within R)         

I never had the intention to perform any real data science task seriously with this tool, since this is not possible within GUIs. But it maybe could be useful to get a first idea of small and simple datasets and to perform some simple baseline modelling. 

## Extendibility
I justed used some basic learning algorithms but it is easily possible to add further learning algorithms. Theoretically you can add all existing algorithms with all corresponding parameters which are available in the mlr package at once. If you would like to use this Tool and want to have some additional functionality just let me know it. 

## Example screenshots from the tool 
After loading the data you get an overview
![Alt text](https://github.com/l47y/ml_tool/blob/master/images/overview.png "Optional title")

Look in detail into the table
![Alt text](https://github.com/l47y/ml_tool/blob/master/images/viewtable.png "Optional title")

See feature importance and delete less important features
![Alt text](https://github.com/l47y/ml_tool/blob/master/images/featureselection.png "Optional title")

Get an idea of the text columns
![Alt text](https://github.com/l47y/ml_tool/blob/master/images/text.png "Optional title")

Train a model in get a performance overview
![Alt text](https://github.com/l47y/ml_tool/blob/master/images/learn.png "Optional title")
