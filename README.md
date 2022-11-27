# Creation of a package : PLS Regression for the classement

### DESCRIPTION

This project is part of our training in Data Science at the University of Lyon 2.  The main objective is to reproduce differents outputs of the PLSDA Regression for the classement and to display them in a R Shiny Application. 

The PLS (Partial Least Square) Regression is a machine learning method which was born in the early 1980s. This regression maximizes the variance of the predictors Xi and the correlation between the X (explanatory variables) and Y (the target varaible). This method borrows its process from both principal components analysis and linear regression. Here are the different functionalities of our package that we will present in the following lines :

* Installation of our package 
* The fit function 
* The predict function
* The classification report function
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

You can launch the fit fonction and affect it to a variable as you can see below

```sh
model=plsda.fit(fomula = Species~.,data=iris,ncomp=2)
```
Then, if you print you result, you can see the differents results of our fonction as you can see below

![image](https://user-images.githubusercontent.com/83652394/204126978-fdab5a96-8504-4e48-a3e4-498c5bc1eba7.png)

### The predict function

With our package you can predict the class of new indivudals or test the model on data of our dataset. 

Here is the signature of our predict function.

```sh
plsda.predict=function(PLSDA,newdata,type="class")
```
PLSDA is a model we fitted with the function plsda.fit, that's why you must laucnh the fit function before test the predict. 

Newdata is a new dataset for which we want to predict class.

Type is what we want to return. If you want to return the class, you must write type="class" (this is the parameter by default). 
If you want to have the probability of belonging of each class, write type="posterior".

Here's how you can use the predict function.

```sh
Xtest=iris[60:120,1:4]
pred=plsda.predict(model,Xtest)
```
Then, if you print the result, you can see something like this. 

![image](https://user-images.githubusercontent.com/83652394/204149935-93985037-47ba-49b9-968d-5c40aee2e0ba.png)

if you write  ``` type="posterior" ```, the result will be like this.

![image](https://user-images.githubusercontent.com/83652394/204150060-01ab67dc-cd8d-4a21-a2e2-3955afe3792e.png)

After the predict function and if you want to watch the performance of your model, you can do the classification report with a function that we create especially for this functionnality.

### The classification report function 

You can see below how we created this function 
```sh
plsda_Classification_report <- function(observed,predict)
```
Observed is the vector which contains the class of the individuals we select in our dataset. 

Predict is what we predicted with the predict function. 

Here how you can use this function :

```sh
Yobs = iris[25:50,5]
report=plsda_Classification_report(Yobs,pred)
```
If you print the function result (```print(report)```), you can see the following result : 

![image](https://user-images.githubusercontent.com/83652394/204151881-5e6973f6-e05d-4c66-b0d7-c87a16a2706e.png)

You can access directly to the fscore metric by writing ```report$f1_score```
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







