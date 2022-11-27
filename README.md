# Creation of a package : PLS Regression for the classement

### DESCRIPTION

This project is part of our training in Data Science at the University of Lyon 2.  The main objective is to reproduce differents outputs of the PLSDA Regression for the classement and to display them in a R Shiny Application. 

The PLS (Partial Least Square) Regression is a machine learning method which was born in the early 1980s. This regression maximizes the variance of the predictors Xi and the correlation between the X (explanatory variables) and Y (the target varaible). This method borrows its process from both principal components analysis and linear regression. Here are the different functionalities of our package that we will present in the following lines :

* Installation of our package 
* The fit function 
* The predict function
* The Classification Report function
* Overcharge functions : 
  - Fit function 
  - Predict function
* Variables choice
* Plots
  - Components choice  
  - Individual plot 
  - Principal Components Analysis
 
### Installation of our package 

### The fit function 

Our fit fonction is made as you can see below

```sh
plsda.fit=function(formula,data,ncomp)
```
This function have differents parameters :
  - formula : Formula an object of class formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
  - data : Dataframe containing the variables in the model
  - ncomp : Number of components extracted in NIPALS algorithm. 

You can launch the fit fonction and affect it to a variable 

```sh
model=plsda.fit(fomula = Species~.,data=iris,ncomp=2)
```

Then, if you print you result, you can see the differents results of our fonction as you can see below

![image](https://user-images.githubusercontent.com/83652394/204126978-fdab5a96-8504-4e48-a3e4-498c5bc1eba7.png)

### The predict function

With our package you can predict the class of new indivudals. 

Here is the signature of our predict function.

```sh
plsda.predict=function(PLSDA,newdata,type="class")
```
PLSDA is a model we fitted with the function plsda.fit, that's why you must laucnh the fit function before test the predict. 
newdata is a new dataset for which we want to predict class.
type is what we want to return. If you want to return the class, you must write type="class" (this is the parameter by default). 
If you want to have the probability of belonging of each class, write type="posterior".
### Overcharge functions

#### The print function

This function aims to show to the user the classement coefficient and the intercept of the PLS model

Here is the signature of our print function 

```sh
plsda.print=function(PLS)
```
There is one argument for this function : the model we fit in the 'plsda.fit' function

You can lauch this fonction as you can see below : 

```sh
plsda.print(model)
```
where model is the result of the 'plsda.fit' function

Here are the result : 

![image](https://user-images.githubusercontent.com/83652394/204127883-e2df6fac-4e86-46dc-aaf6-f57493c0695b.png)

We create an other function to see the result : the summary function 

#### The summary function







