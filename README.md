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
* Variables selection
* Plots
  - Components choice  
  - Individual plot 
  - Principal Components Analysis
* R Shiny Application
  - Import data  
  - Fit the model
  - Predict the target variable of test data 
  - Plots

### Installation of our package and utilisation

First of all, you need to install the package to be able to use it.
To do it, you need to be sure that the library "devtools" is installed and loaded.
Then, run this command : install_github("AntoGrd/PLS-Regression").
The package should be installed and ready to use.
To see the documentation of our package, you can go in the tab "Packages" in RStudio, and search for "PLSDA". 
Click on it and you will see all the Help Pages available for each functions in the package.
You can also run the command : help("function") to see the help of a specific function.
```sh
Example : help(train_test_split)
```
To run the differents function, you need to use the command PLSDA::"function".
```sh
Example : PLSDA::train_test_split(iris)
```
### The fit function 

Our fit fonction is made as you can see below

```sh
fit=function(formula,data,ncomp)
```
This function have differents parameters :
  - formula : Formula an object of class formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
  - data : Dataframe containing the variables in the model
  - ncomp : Number of components extracted in NIPALS algorithm. 

You can launch the fit fonction and affect it to a variable as you can see below

```sh
model=PLSDA::fit(fomula = Species~.,data=iris,ncomp=2)
```
Then, if you print you result, you can see the differents results of our fonction as you can see below

![image](https://user-images.githubusercontent.com/83652394/204126978-fdab5a96-8504-4e48-a3e4-498c5bc1eba7.png)

### The predict function

With our package you can predict the class of new indivudals or test the model on data of our dataset. 

Here is the signature of our predict function.

```sh
predict=function(PLSDA,newdata,type="class")
```
PLSDA is a model we fitted with the function plsda.fit, that's why you must laucnh the fit function before test the predict. 

Newdata is a new dataset for which we want to predict class.

Type is what we want to return. If you want to return the class, you must write type="class" (this is the parameter by default). 
If you want to have the probability of belonging of each class, write type="posterior".

Here's how you can use the predict function.

```sh
Xtest=iris[60:120,1:4]
pred=PLSDA::predict(model,Xtest)
```
Then, if you print the result, you can see something like this. 

![image](https://user-images.githubusercontent.com/83652394/204149935-93985037-47ba-49b9-968d-5c40aee2e0ba.png)

if you write  ``` type="posterior" ```, the result will be like this.

![image](https://user-images.githubusercontent.com/83652394/204150060-01ab67dc-cd8d-4a21-a2e2-3955afe3792e.png)

After the predict function and if you want to watch the performance of your model, you can do the classification report with a function that we create especially for this functionnality.

### The classification report function 

You can see below how we created this function 
```sh
classification_report <- function(observed,predict)
```
Observed is the vector which contains the class of the individuals we select in our dataset. 

Predict is what we predicted with the predict function. 

Here how you can use this function :

```sh
Yobs = iris[25:50,5]
report=PLSDA::classification_report(Yobs,pred)
```
If you print the function result (```print(report)```), you can see the following result : 

![image](https://user-images.githubusercontent.com/83652394/204151881-5e6973f6-e05d-4c66-b0d7-c87a16a2706e.png)

You can access directly to the fscore metric by writing ```report$f1_score```

### Overcharge functions

We created two functions to have a better readibily of results in our fit and predict function. 

First of all, there is the print function.

#### The print function

This function aims to show to the user the classement coefficient and the intercept of the PLS model

Here is the signature of our print function 

```sh
print.plsda=function(PLS)
```
There is one argument for this function : the model we fit in the 'plsda.fit' function

You can lauch this fonction as you can see below : 

```sh
PLSDA::print.plsda(model)
```
where model is the result of the 'plsda.fit' function

Here are the result : 

![image](https://user-images.githubusercontent.com/83652394/204127883-e2df6fac-4e86-46dc-aaf6-f57493c0695b.png)

We create an other function to see the result : the summary function 

#### The summary function

We created this function in the following way : 

```sh
summary.plsda=function(PLS,Xtest,ytest)
```
PLS is a PLS type model we fitted with the fit function. 

Xtest is a dataset test which contains explanatory variables. 

ytest is a dataset test which contains the target variable. 

Here's how you can test the function4

```sh
Xtest=iris[25:50,1:4]
ytest=iris[25:50,5]
summary=summary.plsda(model,Xtest,ytest)
```

And here's the result : 

![image](https://user-images.githubusercontent.com/83652394/204152877-f3524c37-441f-4034-8a60-31f6be14047a.png)

As you can see, we decided to put the coefficients of the model and the elements of the classification report. 

You can access to classification raport by typing ```summary$classification_report```.

Now, let's give a look at the variables selection. 

### Variables selection 

We created a fuction which has a objective to implement a variables selection. 

You can see below the name of the function (VIP for Variable Importance in Projection) ans its parameters
```sh
plsda.vip<-function(PLS,threshold=0.8)
```
The parameter PLS is our model that we fitted with the fit function. 

Threshehold is the criterion which is going to serve to select variables with the most importance in our model. By defalut, this parameter is fixed at 0.8.

You can see below how to use this function. 

```sh
vip=vip(model)
```
If you execute this function you will have the following result : 

![image](https://user-images.githubusercontent.com/83652394/204156102-7d230637-a828-4218-899d-d001259e5107.png)

If you want to retreive the new dataset with kept varibles, type ```newdf=vip$newX```

You will have the dataset with the most importants variables. 

### Plots 

Then, we decided to make some plots to have a visual application. All of those graphs were made with the library plotly. 

First of all, here is the plot for the component choice. 

#### Component choice

Our application proposes a choice of component functionnality. 

In the fit function, we can choose the number of components for which we can fit the model. 

This is our function ```plsda_scree_plot```

This is how it is built 

```sh
scree_plot=function(PLSDA)
```

The parameter acp is, as usual, a result of the function fit (model in the case of our tutorial). 

Let's execute this function. 

```sh
PLSDA::scree_plot(model)
```

The following graph appears : 

![image](https://user-images.githubusercontent.com/83652394/204156960-9effecba-a42e-4a22-bfe7-a60af6cbe447.png)

The component in red is the component we must keep. For exemple, in our case, only the first component is useful. This parameter can be used a posteriori in the function fit.

#### Individual plot 

We created a plot which has as main objective to show the repartition of the target variable depending of two explanaotory variables (which are quantitative). 

```sh
explanatory_variables_plot=function(var1,var2, color)
```
Var1 is an explanatory variable.

Var2 is an other explanatory variable.

Color is the color of the points of our scatter plot. 

For example, you can see below how to use this function. 

```sh
PLSDA::explanatory_variables_plot(var1 = iris$Sepal.Length,var2=iris$Sepal.Width,color=iris$Species)
```
The code ```color=iris$Species``` means that the color depends of the modality of Species, in our case the target variable. 

We can see this graph appearing. 

![image](https://user-images.githubusercontent.com/83652394/204157743-50b8ad9c-7d06-4021-b928-f5dd4b4e9eb0.png)

#### The Principal Composent Analysis 

As the dataset of explanatory variables is made of quantitatives variables, we can make a Principal Composent Analysis (PCA) to see the correlation between them. 

```sh
circle.plot <- function(acp)
```

Our parameter ACP is a model fitted. 

To launch our function, you have to type the following code

```sh
PLSDA::circle.plot(model)
```

This graph appears. 

![image](https://user-images.githubusercontent.com/83652394/204158373-405f3d86-c1d9-488a-81b0-f4809529ca86.png)

PS : You must follow the order of execution presented in this tutorial to have a functionnal application. First of all, execute the function fit, then the predict one. You can then execute the classification report, later the cross validation and finally you can appreciate all the graphs.

Then and finally, we will present you our R Shiny Application

### R Shiny Application 

First of all, the first element of our page is a window on which we can import data we want. 

#### Import the data 

On the window below, you can choose the file you want to study by clicking on "browse..."

![image](https://user-images.githubusercontent.com/83652394/205399442-01b1dcb0-0118-4220-994a-fe79978e05a7.png)

Let's take the well-known iris dataset

Then, you can choose different options like the presence or the absence of header, the separator between each column and the quotes. 

![image](https://user-images.githubusercontent.com/83652394/205400514-d7ce1f9c-7f8e-4d97-829e-9934ba7832bf.png)

Choose the elements which allow a good readibilty of your dataset. You can see the overview next to the window of options. 

Your dataset is now loaded. 

Now, you can fit a model with our tab "fit". 

#### Fit a model 

On this tab, you can choose the percentage of the whole dataset which is going to be in the train split. 

You can also choose which variables are going to be explanatory variables and which one is going to be the target variable in your PLS model (see below). 

![image](https://user-images.githubusercontent.com/83652394/205402049-2d08ac6c-9585-4dd8-bc00-0434d063151d.png)

If you want to have all the dataset, select nothing in the "Select your X variables" list. 

Now, click on "Fit the Data" with the parameters you choose (be careful that you do the train test split before fit the model). 

The following window is appearing. 

![image](https://user-images.githubusercontent.com/83652394/205402419-4f53a526-9e05-4c09-93b9-6cb73609d1c8.png)

You can see the coefficient of the model. 

Then, let's test our model with the tab "Predict". 

#### Predict the target variable of test data 

Our R Shiny application allows you to perdict the class or the probabily of belonging of the data in the test dataset (which you built in the tab "fit"). 

By clicking on "Do the predicion", you will see a confusion matrix appearing on the right part of the window. 

![image](https://user-images.githubusercontent.com/83652394/205403121-02ac3149-ac68-42bb-a11d-9fc898d7f21d.png)

Finally, our last tab concerns the differents plots we made during our project. 

#### Plots

The last tab of our RShiny Application concerns plots we did for our project. 

For example, you want to see the scatter plot of our target variable depending on two explanatory variables of our dataset. 

Let's see below how to process. 

![image](https://user-images.githubusercontent.com/83652394/205455649-5eec9e81-1e2b-4330-87e3-335f7327cb05.png)

Choose in the dropdown list of the scatter plot the two variables you want to see on the plot. Click on 'View / Update graphic". 

![image](https://user-images.githubusercontent.com/83652394/205455730-97fa758a-0d69-44ee-af6d-47ab7a01faae.png)

You can see this graph appearing. 

If you want to know the ideal number of compenents, in the dropdown list of the graphs to choose, there is the ScreePlot. 

![image](https://user-images.githubusercontent.com/83652394/205455878-4c7a8fcf-a31f-4fc3-89f5-8619322952cc.png)

The two other graphs that are possible to appreciate are the correlation between variable plot and the indivdual plot. 

