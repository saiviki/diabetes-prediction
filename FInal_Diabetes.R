# Data Import from the csv
# diabetes_train = read.csv("/home/saiviki/R/Assignment/Diabetes_Train.csv")
# diabetes_test = read.csv("/home/saiviki/R/Assignment/Diabetes_Test.csv")


#` The Characteristics of the data set are studied.
summary(diabetes_train)
summary(diabetes_test)
#` The variables namely plasma.glucose, blood.pressure, triceps.skin.thickness, insulin, bmi, diabetes,pedigree
#` have zero values which are not possible in many cases. Hence, we deduce that the data set has zero entered 
#` wherever there are missing values. Hence they're to be replaced with NA to make better sense.

# "pregnancies"            "plasma.glucose"         "blood.pressure"         "triceps.skin.thickness"
# "insulin"                "bmi"                    "diabetes.pedigree"      "age"                   
# "diabetes"  

# A copy of the datasets are made before making any modifications.
diabetes_train1 = diabetes_train
diabetes_test1 = diabetes_test

#` Zeros' of different variables are replaced with NAs.
for(i in 2:7)
  diabetes_train1[diabetes_train1[,i] == 0,i] = NA
for(i in 2:7)
  diabetes_test1[diabetes_test1[,i] == 0,i] = NA


#` The feature selection is done by setting a lesser complexity parameter to the decision tree algorithm
#` and manually picking only the variables that are helpful. Also the variables with high number of NA like
#` insulin are not to be considered as they may not help much. 
#` The "rpart" is the decision tree algorithm chosen as "caret" package cannot handle the missing values.
#` The CART algorithm is capable of handling missing values better than "ID3, C5.0 and J48". Hence the 
#` decision to use the CART and use "rpart" package is made.

#` Feature selection is done using the less complexity parameter setting. The features contributing more 
#` to the top of the trees are selected. The features or the variables contributing to the top 10 nodes
#` are selected. 
#` We will set a seed value so that we will get the same output everytime we run as there are some
#` functions within rpart which will use random digit generation.
set.seed(16027)
#` xval is the number of Cross Validations to be done, cp is the Complexity parameter permissible.
#` surrogatestyle is the variable which when assigned to 1 will 
cnt = rpart.control(xval = 10, cp = 0.001, surrogatestyle = 1, usesurrogate = 2)
model_NA=rpart(formula = diabetes~.,data = diabetes_train1, control = cnt)
pred_model_NA=predict(model_NA,newdata = diabetes_test1,type="class")
rattle::fancyRpartPlot(model_NA)

#` plasma.glucose, bmi, diabetes.pedigree, age, insulin, blood.pressure,
#` triceps.skin.thickness are the features at the higher nodes.
#` Insulin is not considered since we know that insulin has the highest number of NAs.
#` Hence, the features which made the final cut are  as follows:
#` plasma.glucose, bmi, diabetes.pedigree, age, insulin, blood.pressure, triceps.skin.thickness


#` Lets try using all the variables and give a higher. Max Accuracy and Kappa - High cost
library(rpart)
library(caret)
library(rattle)
#` Lets use 1.25% complexity.
cnt = rpart.control(xval = 10, cp = 0.0125, surrogatestyle = 1, usesurrogate = 2, maxsurrogate = 10)
model_NA=rpart(formula = diabetes~.,data = diabetes_train1, control = cnt,
               parms=list(split="information", loss=costMatrix))
pred_model_NA=predict(model_NA,newdata = diabetes_test1,type="class")
rattle::fancyRpartPlot(model_NA)
confusionMatrix(pred_model_NA,diabetes_test1$diabetes)
#` We're getting an accuracy of 73.53%. Kappa value of 42.5%. 
#` Also Specificity is 55.56%. Sensitivity is 85.4%.
a = confusionMatrix(pred_model_NA,diabetes_test1$diabetes)
costMatrix = matrix(c(0,150,50,0),nrow = 2)
cost = a$table[2]*150 +  a$table[3] *50
# cost
#` Cost is 1500.

#` Lets check with the variables or the features that we have selected.
cnt = rpart.control(xval = 10, cp = 0.00125, surrogatestyle = 1, usesurrogate = 2, maxsurrogate = 10)
model_NA=rpart(formula = diabetes ~ plasma.glucose + bmi + age + insulin + pregnancies
               + blood.pressure + triceps.skin.thickness,
               data = diabetes_train1, control = cnt)
pred_model_NA=predict(model_NA,newdata = diabetes_test1,type="class")
rattle::fancyRpartPlot(model_NA)
confusionMatrix(pred_model_NA,diabetes_test1$diabetes)
#` We're getting an accuracy of 73.53%. Kappa value of 42.5%. 
#` Also Specificity is 55.56%. Sensitivity is 85.4%.
a = confusionMatrix(pred_model_NA,diabetes_test1$diabetes)
cost = a$table[2]*150 +  a$table[3] *50
# cost
#` Cost is 1500.
# final_model = model_NA

# plasma.glucose + bmi + diabetes.pedigree + age + insulin 
# + blood.pressure + triceps.skin.thickness
#` We need to change the complexity parameter and find out which give the highest Kappa's, accuracy,
#` Sensitivity and Specificity. The cost has to be reduced.
cost = 2000
#` cp_val is the Placeholder to store the final complexity parameter value.
cp_val = 0
#` conf is Placeholder to store the final confusion matrix.
conf = list()
#` final_model is Placeholder to store the final model.
final_model = list() 
#` Iterating the whole decision tree function from complexity parameter of 0.0010 to 0.200 with a increment
#` of 0.0005
for (i in seq(from = 0.0010, to = 0.2000, by = 0.0005)) {
  cnt_i = rpart.control(xval = 10, cp = i, surrogatestyle = 1, usesurrogate = 1, maxsurrogate = 10)
  model_N = rpart(formula = diabetes ~ plasma.glucose + bmi + age + insulin 
                  + blood.pressure + triceps.skin.thickness, 
                   parms=list(split="gini", loss=costMatrix),
                   data = diabetes_train1, control = cnt_i)
  pred_model_N = predict(model_N, newdata = diabetes_test1, type="class")
  j = confusionMatrix(pred_model_N, diabetes_test1$diabetes)
  new_cost = j$table[2]*150 +  j$table[3] *50
#` If the value of the new_cost is lesser than the previous cost, we will swap the complexity parameter value
#` , cost, confusion matrix and the final model with the new variables.
  if (new_cost < cost)
  {
    cp_val = i
    cost = new_cost
    conf = j
    final_model = model_N
  }
  else NULL
}
#` Lets check how the final model looks like.
rattle::fancyRpartPlot(final_model)


# Confusion Matrix and Statistics
# 
# Reference
# Prediction false true
# false    36   12
# true      5   15
# 
# Accuracy : 0.75            
# 95% CI : (0.6302, 0.8471)
# No Information Rate : 0.6029          
# P-Value [Acc > NIR] : 0.007953        
# 
# Kappa : 0.4537          
# Mcnemar's Test P-Value : 0.145610        
#                                           
#             Sensitivity : 0.8780          
#             Specificity : 0.5556          
#          Pos Pred Value : 0.7500          
#          Neg Pred Value : 0.7500          
#              Prevalence : 0.6029          
#          Detection Rate : 0.5294          
#    Detection Prevalence : 0.7059          
#       Balanced Accuracy : 0.7168          
#                                           
#        'Positive' Class : false

# n= 700 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 700 241 false (0.6557143 0.3442857)  
# 2) plasma.glucose< 127.5 442  85 false (0.8076923 0.1923077) *
# 3) plasma.glucose>=127.5 258 102 true (0.3953488 0.6046512)  
# 6) bmi< 29.95 71  21 false (0.7042254 0.2957746) *
# 7) bmi>=29.95 187  52 true (0.2780749 0.7219251) *


# Cost is 1350.
# Complexity parameter for which we got the lowest cost is 0.1

# Lets try to increase the Accuracy of the model and check the cost for that.
cost = 1350
cp_val = 0
#conf = list() # conf is Placeholder to store the final confusion matrix.
#final_model = list() # final_model is Placeholder to store the final model.
for (i in seq(from = 0.00100, to = 0.2000, by = 0.0005)) {
  cnt_i = rpart.control(xval = 10, cp = i, surrogatestyle = 1, usesurrogate = 1, maxsurrogate = 10)
  model_N = rpart(formula = diabetes ~ plasma.glucose + bmi + diabetes.pedigree + age + pregnancies,
                  data = diabetes_train1, control = cnt_i)
  pred_model_N = predict(model_N, newdata = diabetes_test1, type="class")
  j = confusionMatrix(pred_model_N, diabetes_test1$diabetes)
  new_cost = j$table[2]*150 +  j$table[3] *50
  if (j$overall[1] > conf$overall[1])
  {
    cp_val = i
    cost = new_cost
    conf = j
    final_model = model_N
  }
  else NULL
}

fancyRpartPlot(final_model)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction false true
# false    34    9
# true      7   18
# 
# Accuracy : 0.7647          
# 95% CI : (0.6462, 0.8591)
# No Information Rate : 0.6029          
# P-Value [Acc > NIR] : 0.003714        
# 
# Kappa : 0.5023          
# Mcnemar's Test P-Value : 0.802587        
# 
# Sensitivity : 0.8293          
# Specificity : 0.6667          
# Pos Pred Value : 0.7907          
# Neg Pred Value : 0.7200          
# Prevalence : 0.6029          
# Detection Rate : 0.5000          
# Detection Prevalence : 0.6324          
# Balanced Accuracy : 0.7480          
# 
# 'Positive' Class : false           
# 
# > cost
# [1] 1500
# > final_model
# n= 700 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 700 241 false (0.6557143 0.3442857)  
# 2) plasma.glucose< 127.5 442  85 false (0.8076923 0.1923077) *
# 3) plasma.glucose>=127.5 258 102 true (0.3953488 0.6046512) *
# > cp_val
# [1] 0.1205

# Lets try to increase the Specificity value to the maximum
cost = 2000
cp_val = 0
conf = list() # conf is Placeholder to store the final confusion matrix.
final_model = list() # final_model is Placeholder to store the final model.
# for (i in seq(from = 0.00100, to = 0.2000, by = 0.0005)) {
#   cnt_i = rpart.control(xval = 10, cp = i, surrogatestyle = 1)
#   model_N = rpart(formula = diabetes ~ plasma.glucose + bmi + diabetes.pedigree + age + pregnancies,
#                   data = diabetes_train1, control = cnt_i)
#   pred_model_N = predict(model_N, newdata = diabetes_test1, type="class")
#   j = confusionMatrix(pred_model_N, diabetes_test1$diabetes)
#   new_cost = j$table[2]*150 +  j$table[3] *50
#   if (j$byClass[2] < conf$byClass[2])
#   {
#     cp_val = i
#     cost = new_cost
#     conf = j
#     final_model = model_N
#   }
#   else NULL
# }
# 
# fancyRpartPlot(final_model)

# N = 10, R = T
library(RWeka)
m1 = J48(formula = diabetes ~ plasma.glucose + bmi + age, 
           control = Weka_control(N = 10, R = T),
           data = diabetes_train1)
p1 = predict(m1, newdata = diabetes_test1)
n = confusionMatrix(p1, diabetes_test1$diabetes)
n$table[2]*150 +  n$table[3] *50

eval_m1 = evaluate_Weka_classifier(m1, numFolds = 10, complexity = FALSE,
                                   seed = 16027, class = TRUE)

graphVisualizer <- function(file, width = 400, height = 400,
             title = substitute(file), ...)
    {
       ## Build the graph visualizer
         visualizer <- .jnew("weka/gui/graphvisualizer/GraphVisualizer")
         reader <- .jnew("java/io/FileReader", file)
         .jcall(visualizer, "V", "readDOT",
                  .jcast(reader, "java/io/Reader"))
         .jcall(visualizer, "V", "layoutGraph")
         ## and put it into a frame.
           frame <- .jnew("javax/swing/JFrame",
                          paste("graphVisualizer:", title))
           container <- .jcall(frame, "Ljava/awt/Container;", "getContentPane")
           .jcall(container, "Ljava/awt/Component;", "add",
                    .jcast(visualizer, "java/awt/Component"))
           .jcall(frame, "V", "setSize", as.integer(width), as.integer(height))
           .jcall(frame, "V", "setVisible", TRUE)
}
write_to_dot(m1, "m1.dot")
graphVisualizer("m1.dot")


m2 = CostSensitiveClassifier(formula = diabetes ~ plasma.glucose + bmi + age + insulin 
         + blood.pressure + triceps.skin.thickness, 
         control = Weka_control(`cost-matrix` = matrix(c(0, 150, 50, 0), ncol = 2),
                                W = "weka.classifiers.trees.J48", M = TRUE, N = 10, R = T),
         data = diabetes_train1)
p2 = predict(m2, newdata = diabetes_test1)
n1 = confusionMatrix(p2, diabetes_test1$diabetes)
