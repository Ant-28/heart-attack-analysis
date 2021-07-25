# Heart attack analysis

# Loading the required libraries, can add required libraries later in vector (same code as used in previous project, source: Ananthajit Srikanth)
required_libraries <- c("caret", "matrixStats", "tidyverse", "knitr", "broom", "ranger", "knitr", "rmarkdown", "mice", "e1071", "kernlab", "nnet", "rpart", "randomForest", "MASS", "naivebayes","adabag", "plyr","nnet", "stringr")
# The for loop installs and loads libraries one by one from required_libraries
for(i in 1:length(required_libraries)){
  if(!require(required_libraries[i], character.only = TRUE)){
    install.packages(required_libraries[i], repos = "http://cran.us.r-project.org")
    library(required_libraries[i], character.only = TRUE)
  }
  else{
    require(required_libraries[i], character.only = TRUE)}}

# Run this line only if necessary:
# update.packages()

# Show that the files are comma-separated
download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", destfile = 'processed.cleveland.data')
readLines(con = 'processed.cleveland.data', n = 1)


download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.hungarian.data", destfile = "processed.hungarian.data")

download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.va.data", destfile = "processed.va.data")

download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.switzerland.data", destfile = "processed.switzerland.data")

noquote("Hungarian")
readLines(con = 'processed.hungarian.data', n = 1)
noquote("Switzerland")
readLines(con = 'processed.switzerland.data', n = 1)
noquote("va")
readLines(con = 'processed.va.data', n = 1)


# download extra files for help and reading
download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/heart-disease.names", destfile = "heart-disease.txt")

download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/cleve.mod", destfile = "cleve.txt")

# Why you shouldn't use cleveland.data 
download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/WARNING", destfile = "warn.txt")

# Downloading the processed.data files as csv

download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", destfile = "cleveland.csv")

download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.hungarian.data", destfile = "hungarian.csv")

download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.va.data", destfile = "va.csv")

download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.switzerland.data", destfile = "switzerland.csv")



# Column names for data frames
colnames_heartattack <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")

cleveland <- read_csv("cleveland.csv", col_names = colnames_heartattack)



any(str_detect(cleveland$thal, pattern = "\\?"))

# Convert all ? to NA
cleveland[cleveland == "?"] <- NA
# Convert all columns to numeric
cleveland <- apply(cleveland, 2, function(x) as.numeric(x)) %>% as.data.frame()


# specify the dataset region
cleveland <- cleveland %>% dplyr::mutate(dataset = 'cleveland')



switzerland <- read_csv("switzerland.csv", col_names = colnames_heartattack)
switzerland[switzerland == "?"] <- NA
switzerland <- apply(switzerland, 2, function(x) as.numeric(x)) %>% as.data.frame()
switzerland <- switzerland %>% dplyr::mutate(dataset = 'switzerland')

hungarian <- read_csv("hungarian.csv", col_names = colnames_heartattack)
hungarian[hungarian == "?"] <- NA
hungarian <- apply(hungarian, 2, function(x) as.numeric(x)) %>% as.data.frame()
hungarian <- hungarian %>% dplyr::mutate(dataset = "hungarian")

va <- read_csv("va.csv", col_names = colnames_heartattack)
va[va == "?"] <- NA
va <- apply(va, 2, function(x) as.numeric(x)) %>% as.data.frame()
va <- va %>% dplyr::mutate(dataset = "va")



# join the four datasets together
total_set <- bind_rows(cleveland, switzerland, hungarian, va)


for(i in 1:(ncol(total_set)-1)) {
 x <- total_set[,i]
 x <- data.frame(x = x)
 name <- names(total_set)[i]
  print(x %>% ggplot(aes(x)) + geom_histogram() + xlab(label = name) + ylab(label = paste0("Frequency/Count of ",name)))
 }


# check error cholesterol

total_set %>% group_by(dataset) %>% dplyr::summarize(zeros = sum(chol == 0, na.rm = TRUE)) %>% knitr::kable()



mutated_chol <- total_set$chol
mutated_chol[mutated_chol == 0] <- NA
total_set <- total_set %>% dplyr::mutate(chol = mutated_chol)




# covert all zeros in trestbps to NA, because 0 bp is only possible at death
# look at trestbps

sum(total_set$trestbps == 0, na.rm = TRUE)
# only one


# covert the one erroneous value into an NA
mutated_trestbps <- total_set$trestbps
mutated_trestbps[mutated_trestbps == 0] <- NA

total_set <- total_set %>% dplyr::mutate(trestbps = mutated_trestbps)

# convert factors
total_set <- total_set %>% dplyr::mutate(num = as.factor(num), sex = as.factor(sex), cp = as.factor(cp), fbs = as.factor(fbs), restecg = as.factor(restecg),exang = as.factor(exang), slope = as.factor(slope), ca = as.factor(ca), thal = as.factor(thal), dataset=as.factor(dataset))

str(total_set)


# Converts 2,3 and 4 into 1.
# as.character has to be used since as.numeric converts factor into level index, for example 0 becomes 1, 1 becomes 2, etc.
temp_num <- as.numeric(as.character(total_set$num))

temp_num_2 <- case_when(temp_num == 0 ~ 0,
                        temp_num %in% c(1,2,3,4) ~ 1,
                        TRUE ~ NA_real_
                      )
remove(temp_num)

total_set <- total_set %>% dplyr::mutate(num = factor(temp_num_2))
str(total_set$num)
total_set$num %>% head()



table(total_set$num)

apply(total_set,2,function(x)sum(is.na(x))) %>% knitr::kable()

nas <- apply(total_set, 1, function(x)sum(is.na(x)))
hist(nas, col = '#8898f2')

# set.seed for consistency
set.seed(2000, sample.kind = 'Rounding') # remove sample.kind argument if you use R 3.5

split_index <- createDataPartition(y = total_set$num, times = 1, p = 250/960, list = FALSE)

train_set <- total_set[-split_index,]
temp_set <- total_set[split_index,]

set.seed(9000, sample.kind = 'Rounding')

# split debug and test into approx 125 examples each
split_index_2 <- createDataPartition(y = temp_set$num, times = 1, p = 0.5, list = FALSE)

debug_set <- temp_set[-split_index_2,]
test_set <- temp_set[split_index_2,]

remove(temp_set, split_index, split_index_2)


# debug set structure

str(debug_set)

# test set structure
str(test_set)

# set.seed for consistency
set.seed(1900, sample.kind = "Rounding") #remove sample.kind if you use R 3.5
# impute (insert by prediction) values using mice
# rf works with all data types and is fast and efficient


# remove outcome column so that imputation does not rely on outcome:

train_set_no_outcomes <- train_set %>% dplyr::select(-num)
# run mice

mice_imputation <- mice(train_set_no_outcomes, blocks = c("trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak","slope", "ca", "thal"), method = 'rf', maxit = 50, m = 10)
train_set_2 <- complete(mice_imputation)

# First entries don't have NAs
# identical(head(test), head(total_set_no_outcomes)) is TRUE so order is maintained
# sum(is.na(train_set_2)) is 0

train_set_2 <- train_set_2 %>% dplyr::mutate(num = train_set$num)

set.seed(3000, sample.kind = "Rounding") #remove sample.kind if you use R 3.5
# repeat for debug and test set
debug_set_no_outcomes <- debug_set %>% dplyr::select(-num)
# run mice

mice_imputation_debug <- mice(debug_set_no_outcomes, blocks = c("trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak","slope", "ca", "thal"), method = 'rf', maxit = 50, m = 10)
debug_set_2 <- complete(mice_imputation_debug)
debug_set_2 <- debug_set_2 %>% dplyr::mutate(num = debug_set$num)

set.seed(5000, sample.kind = "Rounding") #remove sample.kind if you use R 3.5
test_set_no_outcomes <- test_set %>% dplyr::select(-num)
# run mice

mice_imputation_test <- mice(test_set_no_outcomes, blocks = c("trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak","slope", "ca", "thal"), method = 'rf', maxit = 50, m = 10)
test_set_2 <- complete(mice_imputation_test)
test_set_2 <- test_set_2 %>% dplyr::mutate(num = test_set$num)



#set crossvalidation parameters
# bootstrap sampling is used as dataset is small
# 20 fold cross validation with 15% as samples. 15% is just large enough for 20 folds.
# percentage is amount used in training set, which is 1-0.15
control <- trainControl(method = 'boot', number = 20, p = 0.85)


# logistic model, family=binomial sets logistic regression

fit_glm <- train(num~., data=train_set_2, method = 'glm', trControl = control, family = 'binomial')

# see result on debug set
# glm has no tuning parameters
predict_glm <- predict(fit_glm, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas1 <- F_meas(data = predict_glm, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas1)

# lda model
fit_lda <- train(num~., data=train_set_2, method = 'lda', trControl = control)

# see result on debug set

predict_lda <- predict(fit_lda, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas2 <- F_meas(data = predict_lda, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas2)
print(fmeas2 > fmeas1)

# qda model
fit_qda <- train(num~., data=train_set_2, method = 'qda', trControl = control)

# see result on debug set

predict_qda <- predict(fit_qda, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas3 <- F_meas(data = predict_qda, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas3)




# naive bayes model
# modelLookup('naive_bayes') there are three tuning parameters
# DO NOT SET adjust to zero you cannot have 0 bin width
fit_nb <- train(num~., data=train_set_2, method = 'naive_bayes', trControl = control, tuneGrid = data.frame(expand.grid(laplace = seq(0,1,0.05),adjust = seq(0.05,0.75,0.05),usekernel = c(TRUE, FALSE))))

# see result on debug set
# modelLookup('naive_bayes') there are three tuning parameters
predict_nb <- predict(fit_nb, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas4 <- F_meas(data = predict_nb, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas4)



# modelLookup('rpart') there is one tuning parameter
# classification trees model
fit_cart <- train(num~., data=train_set_2, method = 'rpart', trControl = control, tuneGrid = data.frame(cp = seq(0.1, 1.0, 0.05)))

# see result on debug set

predict_cart <- predict(fit_cart, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas5 <- F_meas(data = predict_cart, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas5)





# modelLookup('rf') there is one tuning parameter
# random forest model
# based on help documentatation, rf randomly samples variables in each split using mtry
fit_rf <- train(num~., data=train_set_2, method = 'rf', trControl = control, tuneGrid = data.frame(mtry = seq(1, (ncol(train_set_2) - 1),1)))

# see result on debug set

predict_rf <- predict(fit_rf, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas6 <- F_meas(data = predict_rf, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas6)



# modelLookup('ranger') there are three tuning parameters
# ranger random forest model
# based on help documentatation, rf randomly samples variables in each split using mtry
# split rule is gini, extratrees or hellinger for classification (ranger also supports regression)
# min.node.size is the minimum size of a decision tree node (possibly to determine whether more splits should be done?)
tgrid <- data.frame(expand.grid(mtry = seq(1, (ncol(train_set_2) - 1),1), splitrule = c('gini', 'extratrees', 'hellinger'),min.node.size = seq(10,20,2)))

fit_ranger <- train(num~., data=train_set_2, method = 'ranger', trControl = control, tuneGrid = tgrid)

# see result on debug set

predict_ranger <- predict(fit_ranger, newdata = debug_set_2)
# levels(debug_set_2$num)[2] is '1'
fmeas7 <- F_meas(data = predict_ranger, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas7)
print(fmeas7 == fmeas6)

# This takes time
# modelLookup('adabag') there are two tuning parameters
# adabag random forest model
# maxdepth limited as AdaBag takes too long
fit_adaboost <- train(num~., data=train_set_2, method = 'AdaBag', trControl = control, tuneGrid = data.frame(expand.grid(mfinal = 200,maxdepth = 2)))
# see result on debug set

predict_adaboost <- predict(fit_adaboost, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas8 <- F_meas(data = predict_adaboost, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas8)



# modelLookup('nnet') there are two tuning parameters
# Neural Network
# preprocess to center and scale numerics for neural networks
suppressWarnings(fit_nn <- train(num~., data=train_set_2, method = 'nnet', trControl = control, preProcess = c('center', 'scale'),tuneGrid = data.frame(expand.grid(size = seq(10,50,10), decay = seq(0.1,1,0.1)))))
# see result on debug set

predict_nn <- predict(fit_nn, newdata = debug_set_2)
# levels(debug_set_2$num)[2] is '1'
fmeas9 <- F_meas(data = predict_nn, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas9)




# SVMs
# first, linear boundary
# modelLookup('svmLinear') there is one tuning parameter, the cost
# cost of constraints violation (default: 1) this is the ‘C’-constant of the regularization term in the Lagrange formulation.

# explanation of cost given in help file ^^
# also requires preprocess
fit_svml <- train(num~., data=train_set_2, method = 'svmLinear', trControl = control, preProcess = c('center', 'scale'), tuneGrid = data.frame(C = seq(1,10,1)))
# see result on debug set

predict_svml <- predict(fit_svml, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas10 <- F_meas(data = predict_svml, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas10)





# SVMs
# polynomial kernel
# modelLookup('svmPoly') there are three tuning parameters, the cost, the degree and the scale
# cost of constraints violation (default: 1) this is the ‘C’-constant of the regularization term in the Lagrange formulation.
# degree is degree of polynomial kernel: 1,2,3 etc

# explanation of cost given in help file ^^

fit_svmp <- train(num~., data=train_set_2, method = 'svmPoly', trControl = control, preProcess = c('center', 'scale'), tuneGrid = data.frame(expand.grid(C = seq(1,10,1), degree = c(2:5), scale = seq(0.1,1,0.1))))
# see result on debug set

predict_svmp <- predict(fit_svmp, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas11 <- F_meas(data = predict_svmp, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas11)


# fmeas11 < fmeas10 is true so it is a step down.


# SVM with radial basis function kernel (think bin smoothing with gaussian kernel but it's an SVM)

# modelLookup('svmRadial') there are two tuning parameters, the cost, and sigma
# cost of constraints violation (default: 1) this is the ‘C’-constant of the regularization term in the Lagrange formulation.


# explanation of cost given in help file ^^
# optimal values of the sigma width parameter are shown to lie in between the 0.1 and 0.9 quantile of the \|x- x'\| statistics, also from help file
fit_svmr <- train(num~., data=train_set_2, method = 'svmRadial', trControl = control, preProcess = c('center', 'scale'), tuneGrid = data.frame(expand.grid(C = seq(1,10,1), sigma = seq(0.1,0.9,0.05))))
# see result on debug set

predict_svmr <- predict(fit_svmr, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas12 <- F_meas(data = predict_svmr, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas12)



# k nearest neighbours

# modelLookup('knn') there is one tuning parameter, k, which is the k closest values that will be selected when smoothing
# There are 679 training examples, so ks between 5 to 400 can be used

fit_knn <- train(num~., data=train_set_2, method = 'knn', trControl = control, tuneGrid = data.frame(k = seq(5,400,5)))
# see result on debug set

predict_knn <- predict(fit_knn, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas13 <- F_meas(data = predict_knn, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas13)





# Results table for later use

results_table <- tibble(model = c("Logistic Regression", "LDA", "QDA", "Naive Bayes", "CART", "Random Forests - randomForest", "Random Forests - ranger", "Random Forests - adaboost", "Neural Network", "SVM - Linear Kernel", "SVM - Polynomial Kernel", "SVM - Radial Basis Kernel", "K-Nearest Neighbours"), f1 = c(fmeas1,fmeas2,fmeas3,fmeas4,fmeas5,fmeas6,fmeas7,fmeas8,fmeas9,fmeas10,fmeas11,fmeas12, fmeas13), result = list(predict_glm, predict_lda, predict_qda, predict_nb, predict_cart, predict_rf, predict_ranger, predict_adaboost, predict_nn, predict_svml, predict_svmp, predict_svmr, predict_knn))

# results_ensembling contains each model's predictions as one column
foo <- results_table$result
names(foo) <- results_table$model

results_ensembling <- data.frame(foo)

# to show that each column matches the respective model, you can run this code

# test <- apply(results_ensembling,2,function(x)F_meas(as.factor(x), debug_set_2$num, relevant = levels(debug_set_2$num)[2]))
# test <- unname(test)
# identical(test, results_table$f1)
# this returns true so the column names match the results vectors
# # Best model
# results_table$model[which.max(results_table$f1)]


# convert results_ensembling into a matrix
# first convert all factors to numerics
results_ensembling_temp <- apply(results_ensembling,2,function(x)as.numeric(as.character(x))) %>% as.data.frame()


results_ensembling_matrix <- as.matrix(results_ensembling_temp)
results_ensembling_means <- rowMeans(results_ensembling_matrix)

predict_ensemble_all <- ifelse(results_ensembling_means >= 0.5, 1, 0) %>% as.factor()

fmeas14 <- F_meas(data = predict_ensemble_all, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas14)

# top 3 models
dplyr::arrange(results_table %>% dplyr::select(-result), dplyr::desc(f1)) %>% head(3) %>% knitr::kable()

# select the best three models

results_ensembling_top3 <- results_ensembling_temp %>% dplyr::select(Random.Forests...randomForest, Random.Forests...ranger, SVM...Radial.Basis.Kernel)

results_ensembling_mat_top3 <- as.matrix(results_ensembling_top3)

results_ensembling_means_top3 <- rowMeans(results_ensembling_mat_top3)

predict_ensemble_top3 <- ifelse(results_ensembling_means_top3 >= 0.5, 1,0) %>% as.factor()

fmeas15 <- F_meas(data = predict_ensemble_top3, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
print(fmeas15)


# Results table for debug set:

results_table_debug <- tibble(model = c("Logistic Regression", "LDA", "QDA", "Naive Bayes", "CART", "Random Forests - randomForest", "Random Forests - ranger", "Random Forests - adaboost", "Neural Network", "SVM - Linear Kernel", "SVM - Polynomial Kernel", "SVM - Radial Basis Kernel", "K-Nearest Neighbours", "Ensembling - All Models", "Ensembling - Top 3 Models"), f1 = c(fmeas1,fmeas2,fmeas3,fmeas4,fmeas5,fmeas6,fmeas7,fmeas8,fmeas9,fmeas10,fmeas11,fmeas12, fmeas13, fmeas14, fmeas15))

results_table_debug %>% knitr::kable()


print(results_table_debug$model[which.max(results_table_debug$f1)])



# create prediction vectors
predict_svmr_test <- predict(fit_svmr, newdata = test_set_2)

# F1 score

fmeas16 <- F_meas(data = predict_svmr_test, reference = test_set_2$num, relevant = levels(test_set_2$num)[2])
print(fmeas16)

results_table_final <- tibble(model = c("Logistic Regression", "LDA", "QDA", "Naive Bayes", "CART", "Random Forests - randomForest", "Random Forests - ranger", "Random Forests - adaboost", "Neural Network", "SVM - Linear Kernel", "SVM - Polynomial Kernel", "SVM - Radial Basis Kernel", "K-Nearest Neighbours", "Ensembling - All Models", "Ensembling - Top 3 Models", "Best Model (SVM, Radial Basis Kernel) on Test Set"), f1 = c(fmeas1,fmeas2,fmeas3,fmeas4,fmeas5,fmeas6,fmeas7,fmeas8,fmeas9,fmeas10,fmeas11,fmeas12, fmeas13, fmeas14, fmeas15, fmeas16))

results_table_final %>% knitr::kable()


