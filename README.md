
# To conduct survival analysis on carcinogenicity based on different tumor onset rates, competing risks survival rates, and tumor lethality rates.
#	To validated poly-k adjusted test through Monte Carlo simulation.
#	Generated data sets with Weibull tumor onset distribution.
#	Access the size and the power of the test.
# 10-fold cross validation, leave one out cross validation, bagging, confusion matrix,and bootstrap were applied.
# Runing order
## 1	T1,T2,T3.R          ## To define time function. t1, time to tumor onset, t2, time to death due to tumor, t3,time to death due to competing risks.
## 2	testfunction.R      ## Test statistics. H0, there is no linear trend in the overall proportions of tumored animal accross all dose group.
## 3	Schemes.R           ## Applied age ajustment schem.
## 4	Monte carlo.R      ## Monte carlo simulation. 
## 5	Result_size_and_power.R  ## Baging, bootstrap, leave one out cross validation, 10-fold cross validation were applied.
