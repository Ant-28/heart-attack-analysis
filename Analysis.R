knitr::opts_chunk$set(echo = TRUE, cache = TRUE)

# Heart attack analysis

# Loading the required libraries, can add required libraries later in vector (same code as used in previous project, source: Ananthajit Srikanth)
required_libraries <- c("caret", "matrixStats", "tidyverse", "knitr", "broom", "ranger", "knitr", "rmarkdown", "mice")
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
cleveland <- cleveland %>% mutate(dataset = 'cleveland')


invisible({
switzerland <- read_csv("switzerland.csv", col_names = colnames_heartattack)
switzerland[switzerland == "?"] <- NA
switzerland <- apply(switzerland, 2, function(x) as.numeric(x)) %>% as.data.frame()
switzerland <- switzerland %>% mutate(dataset = 'switzerland')

hungarian <- read_csv("hungarian.csv", col_names = colnames_heartattack)
hungarian[hungarian == "?"] <- NA
hungarian <- apply(hungarian, 2, function(x) as.numeric(x)) %>% as.data.frame()
hungarian <- hungarian %>% mutate(dataset = "hungarian")

va <- read_csv("va.csv", col_names = colnames_heartattack)
va[va == "?"] <- NA
va <- apply(va, 2, function(x) as.numeric(x)) %>% as.data.frame()
va <- va %>% mutate(dataset = "va")
})


# join the four datasets together
total_set <- bind_rows(cleveland, switzerland, hungarian, va)


for(i in 1:(ncol(total_set)-1)) {
 x <- total_set[,i]
 x <- data.frame(x = x)
 name <- names(total_set)[i]
  print(x %>% ggplot(aes(x)) + geom_histogram() + xlab(label = name))
 }


# check error cholesterol

total_set %>% group_by(dataset) %>% summarize(zeros = sum(chol == 0, na.rm = TRUE)) %>% knitr::kable()



mutated_chol <- total_set$chol
mutated_chol[mutated_chol == 0] <- NA
total_set <- total_set %>% mutate(chol = mutated_chol)




# convert factors
total_set <- total_set %>% mutate(num = as.factor(num), sex = as.factor(sex), cp = as.factor(cp), fbs = as.factor(fbs), restecg = as.factor(restecg),exang = as.factor(exang), slope = as.factor(slope), ca = as.factor(ca), thal = as.factor(thal), dataset=as.factor(dataset))

str(total_set)


# Converts 2,3 and 4 into 1.
# as.character has to be used since as.numeric converts factor into level index, for example 0 becomes 1, 1 becomes 2, etc.
temp_num <- as.numeric(as.character(total_set$num))

temp_num_2 <- case_when(temp_num == 0 ~ 0,
                        temp_num %in% c(1,2,3,4) ~ 1,
                        TRUE ~ NA_real_
                      )
remove(temp_num)

total_set <- total_set %>% mutate(num = factor(temp_num_2))
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


# impute (insert by prediction) values using mice
# rf works with all data types and is fast and efficient


# remove outcome column so that imputation does not rely on outcome:

train_set_no_outcomes <- train_set %>% select(-num)
# run mice
invisible({
mice_imputation <- mice(train_set_no_outcomes, blocks = c("trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak","slope", "ca", "thal"), method = 'rf', maxit = 50, m = 10)})
train_set_2 <- complete(mice_imputation)

# First entries don't have NAs
# identical(head(test), head(total_set_no_outcomes)) is TRUE so order is maintained
# sum(is.na(train_set_2)) is 0

train_set_2 <- train_set_2 %>% mutate(num = train_set$num)

# repeat for debug and test set
debug_set_no_outcomes <- debug_set %>% select(-num)
# run mice
invisible({
mice_imputation_debug <- mice(debug_set_no_outcomes, blocks = c("trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak","slope", "ca", "thal"), method = 'rf', maxit = 50, m = 10)})
debug_set_2 <- complete(mice_imputation_debug)
debug_set_2 <- debug_set_2 %>% mutate(num = debug_set$num)

test_set_no_outcomes <- test_set %>% select(-num)
# run mice
invisible({
mice_imputation_test <- mice(test_set_no_outcomes, blocks = c("trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak","slope", "ca", "thal"), method = 'rf', maxit = 50, m = 10)})
test_set_2 <- complete(mice_imputation_test)
test_set_2 <- test_set_2 %>% mutate(num = test_set$num)



# logistic model because why not

fit_glm <- train(num~., data=train_set_2, method = 'glm')

# see result on debug set

predict_glm <- predict(fit_glm, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas1 <- F_meas(data = predict_glm, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
fmeas1

# lda model
fit_lda <- train(num~., data=train_set_2, method = 'lda')

# see result on debug set

predict_lda <- predict(fit_lda, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas2 <- F_meas(data = predict_lda, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
fmeas2

# qda model
fit_qda <- train(num~., data=train_set_2, method = 'qda')

# see result on debug set

predict_qda <- predict(fit_qda, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas3 <- F_meas(data = predict_qda, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
fmeas3




# naive bayes model
fit_nb <- train(num~., data=train_set_2, method = 'naive_bayes')

# see result on debug set

predict_nb <- predict(fit_nb, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas4 <- F_meas(data = predict_nb, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
fmeas4



# classification trees model
fit_cart <- train(num~., data=train_set_2, method = 'rpart')

# see result on debug set

predict_cart <- predict(fit_cart, newdata = debug_set_2)

# levels(debug_set_2$num)[2] is '1'
fmeas5 <- F_meas(data = predict_cart, reference = debug_set_2$num, relevant = levels(debug_set_2$num)[2])
fmeas5



# do nnet later
