# Basic-R-commands
**Here the codes developed during the learning of the R language are being maintained**<br /><br />
**What will be included:** 

***Basic R programming, initial concepts***<br />
***Basic data visualization***<br />
***Application of probability and inference concepts using R***<br />
***Initial data modeling concepts using R***</sub>

# Information about the credit model used for more detailed learning/application of R learning
**Functions**<br />
*pre_processing model -> Includes a function to initially remove variables, change variable format, and check some metrics.*
*normalization -> Standardizes data, checks correlation, and removes highly correlated variables.*<br />

Both functions are used in the general file credit_model_analisis.R, which also modifies some information, plots graphs to better analyze the data, separates the data into training and testing, and trains and checks metrics for various models (Random Forest, KNN, and Regression).

Information relevant to the model is saved for use in final validation/production.
The prod_models.R file performs a production simulation, where a new database undergoes the training stage processing and is then applied to the models to verify the model in a "real" situation.
