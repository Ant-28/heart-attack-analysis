# Heart attack analysis

# Loading the required libraries, can add required libraries later in vector 
required_libraries <- c("caret", "matrixStats", "tidyverse", "knitr", "lubridate", "broom", "randomForest", "rattle", "rpart")
# The for loop installs and loads libraries one by one from required_libraries
for(i in 1:length(required_libraries)){
  if(!require(required_libraries[i], character.only = TRUE)){
    install.packages(required_libraries[i])
    library(required_libraries[i], character.only = TRUE)
  }
  else{
    require(required_libraries[i], character.only = TRUE)}}

# Run this line only if necessary:
# update.packages()
download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", destfile = "cleveland.csv")

download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.hungarian.data", destfile = "hungarian.csv")

download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.va.data", destfile = "va.csv")

download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.switzerland.data", destfile = "switzerland.csv")


colnames_heartattack <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")



cleveland <- read_csv("cleveland.csv", col_names = colnames_heartattack)
cleveland[cleveland == "?"] <- NA
cleveland <- apply(cleveland, 2, function(x) as.numeric(x)) %>% as.data.frame()
cleveland <- cleveland %>% mutate(dataset = 'cleveland')


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

total_set <- bind_rows(cleveland, switzerland, hungarian, va)
total_set <- total_set %>% mutate(num = as.factor(num))

split_index <- createDataPartition(y = total_set$num, times = 1, p = 150/960, list = FALSE)

train_set <- total_set[-split_index,]
test_set <- total_set[split_index,]



fit_knn <- train(num ~ ., data = train_set, method = 'knn', na.action = na.omit)

test <- predict(fit_knn, newdata = test_set)

fit_rpart <- train(num ~., data = train_set, method = 'rpart', na.action = na.omit)

fit_rf <- train(num ~., data = train_set, method = 'rf', na.action = na.omit)

fit_nn <- train(num ~ ., data = train_set, method = 'nnet', na.action = na.omit, tuneGrid = as.data.frame(expand.grid(size = 1:50, decay = c(0,0.5,1))))

fit_lda <- train(num ~ ., data = train_set, method = 'lda', na.action = na.omit)

fit_amdai <- train(num ~ ., data = train_set, method = 'amdai', na.action = na.omit)
