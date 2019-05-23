# FinVol
Repository for the HSG Course "Financial Volatility"


# Running the Code

Please read this document before running the code.
1. Running or sourcing the R code in this folder will automatically install the following packages from the CRAN server, if they are not installed already:

-snow
-portes
-rugarch
-rmgarch
-tseries
-zoo
-forecast
-vars
-MTS
-dyn
-magrittr
-psych  
-rstudioapi
-stats
-tsDyn
-ggplot2


2. The Code is split into two parts. "main.R" conducts all necessary computations and is the primary object of interest. "resources.R" holds a number of functions that will be called during the evaluation of "main.R". Thus, please do not separate the two files. The necessary data is stored in the "Data" folder. NOTE: please do not separate these files. Otherwise, the code won't run properly.





# OPTIONAL:

3. benchmarking.R: The function fit_dcc_quick includes a handy possibilities to improve the model choice. By increasing the number of GARCH families (vector assigned to ‚models‘), the pool of GARCH models that can be fitted improves. 
Due to the limited amount of GARCH models covered in class, we only covered the standard GARCH(p,q) models and GJR models to cover possible leverage effects.

