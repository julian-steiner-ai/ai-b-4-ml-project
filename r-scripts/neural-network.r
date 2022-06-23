selected_features <- c('smoking',
                       'gender',
                       'age',
                       'height.cm.',
                       'weight.kg.',
                       'systolic',
                       'relaxation',
                       'fasting.blood.sugar',
                       'triglyceride',
                       'HDL',
                       'hemoglobin',
                       'serum.creatinine',
                       'ALT',
                       'Gtp',
                       'dental.caries',
                       'tartar')

# Read and preprocess data
setwd('/home/steinerj/Documents/ai-b/semester-4/machine-learning/ai-b-4-ml-project/data')
data <- read.csv("smoking.csv",header=TRUE,sep=",",fill=TRUE,stringsAsFactors=TRUE)
model.data <- data[,selected_features]

# Summary model
summary(model.data)

# Convert tartar to dummy-variable
model.data[,"tartar"] <- ifelse(data[,"tartar"] == 'Y', 1, 0)
model.data[,"tartar"] <- as.factor(model.data[,"tartar"])

# Convert categorical variables
model.data[,"smoking"] <- as.factor(model.data[,"smoking"])
model.data[,"dental.caries"] <- as.factor(model.data[,"dental.caries"])

# Summary model
summary(model.data)

# Split datset into training, validation and test set
n <- length(model.data[,1])
index <- sample(1:n,n,replace=FALSE)
model.data <- model.data[index,]

fivtyPercentLimit <- round(length(model.data[,1]) * 0.5,0)
seventyPercentLimit <- round(length(model.data[,1]) * 0.7,0)

model.data.train <- model.data[1:fivtyPercentLimit,]
model.data.validate <- model.data[(fivtyPercentLimit+1):seventyPercentLimit,]
model.data.test <- model.data[(seventyPercentLimit+1):n,]

length(model.data[,1])
length(model.data.train[,1])
length(model.data.validate[,1])
length(model.data.test[,1])

# Load library for neural network
library(ANN2)

# Tuning the parameter for hidden layers and neurons on validation set

# Define X and y
X <- model.matrix(smoking ~ ., data=model.data.train)
X <- X[,-1]
summary(X)

y <- model.data.train[,"smoking"]

# Compute model
nn_model <- neuralnetwork(X, y, 
                          hidden.layers=c(10,12,10),
                          regression = FALSE, 
                          loss.type = "log", 
                          learn.rates = 1e-04,
                          n.epochs = 100,
                          verbose=FALSE)

# Calculate prediction error and confusion matrix on validation set
X.validate <- model.matrix(smoking ~ ., data = model.data.validate)
X.validate <- X.validate[,-1]

predictions <- predict(nn_model,X.validate)$predictions

y.validate <- model.data.validate[,"smoking"]

A <- matrix(0,ncol=2,nrow=2)

colnames(A) <- c("Real: No smoker", " Real: Smoker")
rownames(A) <- c("Prognose: No smoker", "Prognose: Smoker") 

A[1,1] <- sum(ifelse(y.validate == 0 & predictions == 0, 1,0))
A[1,2] <- sum(ifelse(y.validate == 1 & predictions == 0, 1,0))
A[2,1] <- sum(ifelse(y.validate == 0 & predictions == 1, 1,0))
A[2,2] <- sum(ifelse(y.validate == 1 & predictions == 1, 1,0))

A

# Missclassification error rate
(A[1,2]+A[2,1])/(A[1,1]+A[1,2]+A[2,1]+A[2,2])

# True-Positive & Ture-Negative Rate
A[2,2]/(A[1,2]+A[2,2])   # True-Positive-Rate
A[1,1]/(A[1,1]+A[2,1])   # True-Negative-Rate

# Compute model
nn_model <- neuralnetwork(X, y, 
                          hidden.layers=c(15,12,10),
                          regression = FALSE, 
                          loss.type = "log", 
                          learn.rates = 1e-04,
                          n.epochs = 150,
                          verbose=FALSE)

# Calculate prediction error and confusion matrix on validation set
X.validate <- model.matrix(smoking ~ ., data = model.data.validate)
X.validate <- X.validate[,-1]

predictions <- predict(nn_model,X.validate)$predictions

y.validate <- model.data.validate[,"smoking"]

A <- matrix(0,ncol=2,nrow=2)

colnames(A) <- c("Real: No smoker", " Real: Smoker")
rownames(A) <- c("Prognose: No smoker", "Prognose: Smoker") 

A[1,1] <- sum(ifelse(y.validate == 0 & predictions == 0, 1,0))
A[1,2] <- sum(ifelse(y.validate == 1 & predictions == 0, 1,0))
A[2,1] <- sum(ifelse(y.validate == 0 & predictions == 1, 1,0))
A[2,2] <- sum(ifelse(y.validate == 1 & predictions == 1, 1,0))

A

# Missclassification error rate
(A[1,2]+A[2,1])/(A[1,1]+A[1,2]+A[2,1]+A[2,2])

# True-Positive & Ture-Negative Rate
A[2,2]/(A[1,2]+A[2,2])   # True-Positive-Rate
A[1,1]/(A[1,1]+A[2,1])   # True-Negative-Rate

# Compute model
nn_model <- neuralnetwork(X, y, 
                          hidden.layers=c(15,12,10,15),
                          regression = FALSE, 
                          loss.type = "log", 
                          learn.rates = 1e-04,
                          n.epochs = 200,
                          verbose=FALSE)

# Calculate prediction error and confusion matrix on validation set
X.validate <- model.matrix(smoking ~ ., data = model.data.validate)
X.validate <- X.validate[,-1]

predictions <- predict(nn_model,X.validate)$predictions

y.validate <- model.data.validate[,"smoking"]

A <- matrix(0,ncol=2,nrow=2)

colnames(A) <- c("Real: No smoker", " Real: Smoker")
rownames(A) <- c("Prognose: No smoker", "Prognose: Smoker") 

A[1,1] <- sum(ifelse(y.validate == 0 & predictions == 0, 1,0))
A[1,2] <- sum(ifelse(y.validate == 1 & predictions == 0, 1,0))
A[2,1] <- sum(ifelse(y.validate == 0 & predictions == 1, 1,0))
A[2,2] <- sum(ifelse(y.validate == 1 & predictions == 1, 1,0))

A

# Missclassification error rate
(A[1,2]+A[2,1])/(A[1,1]+A[1,2]+A[2,1]+A[2,2])

# True-Positive & Ture-Negative Rate
A[2,2]/(A[1,2]+A[2,2])   # True-Positive-Rate
A[1,1]/(A[1,1]+A[2,1])   # True-Negative-Rate

# Compute model
nn_model <- neuralnetwork(X, y, 
                          hidden.layers=c(15,8,15),
                          regression = FALSE, 
                          loss.type = "log", 
                          learn.rates = 1e-04,
                          n.epochs = 120,
                          verbose=FALSE)

# Calculate prediction error and confusion matrix on validation set
X.validate <- model.matrix(smoking ~ ., data = model.data.validate)
X.validate <- X.validate[,-1]

predictions <- predict(nn_model,X.validate)$predictions

y.validate <- model.data.validate[,"smoking"]

A <- matrix(0,ncol=2,nrow=2)

colnames(A) <- c("Real: No smoker", " Real: Smoker")
rownames(A) <- c("Prognose: No smoker", "Prognose: Smoker") 

A[1,1] <- sum(ifelse(y.validate == 0 & predictions == 0, 1,0))
A[1,2] <- sum(ifelse(y.validate == 1 & predictions == 0, 1,0))
A[2,1] <- sum(ifelse(y.validate == 0 & predictions == 1, 1,0))
A[2,2] <- sum(ifelse(y.validate == 1 & predictions == 1, 1,0))

A

# Missclassification error rate
(A[1,2]+A[2,1])/(A[1,1]+A[1,2]+A[2,1]+A[2,2])

# True-Positive & Ture-Negative Rate
A[2,2]/(A[1,2]+A[2,2])   # True-Positive-Rate
A[1,1]/(A[1,1]+A[2,1])   # True-Negative-Rate


# Compute model
nn_model <- neuralnetwork(X, y, 
                          hidden.layers=c(15,8,15,10,15,3,15),
                          regression = FALSE, 
                          loss.type = "log", 
                          learn.rates = 1e-04,
                          n.epochs = 120,
                          verbose=FALSE)

# Calculate prediction error and confusion matrix on validation set
X.validate <- model.matrix(smoking ~ ., data = model.data.validate)
X.validate <- X.validate[,-1]

predictions <- predict(nn_model,X.validate)$predictions

y.validate <- model.data.validate[,"smoking"]

A <- matrix(0,ncol=2,nrow=2)

colnames(A) <- c("Real: No smoker", " Real: Smoker")
rownames(A) <- c("Prognose: No smoker", "Prognose: Smoker") 

A[1,1] <- sum(ifelse(y.validate == 0 & predictions == 0, 1,0))
A[1,2] <- sum(ifelse(y.validate == 1 & predictions == 0, 1,0))
A[2,1] <- sum(ifelse(y.validate == 0 & predictions == 1, 1,0))
A[2,2] <- sum(ifelse(y.validate == 1 & predictions == 1, 1,0))

A

# Missclassification error rate
(A[1,2]+A[2,1])/(A[1,1]+A[1,2]+A[2,1]+A[2,2])

# True-Positive & Ture-Negative Rate
A[2,2]/(A[1,2]+A[2,2])   # True-Positive-Rate
A[1,1]/(A[1,1]+A[2,1])   # True-Negative-Rate

# Compute model
nn_model <- neuralnetwork(X, y, 
                          hidden.layers=c(8,15,8),
                          regression = FALSE, 
                          loss.type = "log", 
                          learn.rates = 1e-04,
                          n.epochs = 120,
                          verbose=FALSE)

# Calculate prediction error and confusion matrix on validation set
X.validate <- model.matrix(smoking ~ ., data = model.data.validate)
X.validate <- X.validate[,-1]

predictions <- predict(nn_model,X.validate)$predictions

y.validate <- model.data.validate[,"smoking"]

A <- matrix(0,ncol=2,nrow=2)

colnames(A) <- c("Real: No smoker", " Real: Smoker")
rownames(A) <- c("Prognose: No smoker", "Prognose: Smoker") 

A[1,1] <- sum(ifelse(y.validate == 0 & predictions == 0, 1,0))
A[1,2] <- sum(ifelse(y.validate == 1 & predictions == 0, 1,0))
A[2,1] <- sum(ifelse(y.validate == 0 & predictions == 1, 1,0))
A[2,2] <- sum(ifelse(y.validate == 1 & predictions == 1, 1,0))

A

# Missclassification error rate
(A[1,2]+A[2,1])/(A[1,1]+A[1,2]+A[2,1]+A[2,2])

# True-Positive & Ture-Negative Rate
A[2,2]/(A[1,2]+A[2,2])   # True-Positive-Rate
A[1,1]/(A[1,1]+A[2,1])   # True-Negative-Rate
# Compute model
nn_model <- neuralnetwork(X, y, 
                          hidden.layers=c(15,5,12),
                          regression = FALSE, 
                          loss.type = "log", 
                          learn.rates = 1e-04,
                          n.epochs = 150,
                          verbose=FALSE)

# Calculate prediction error and confusion matrix on validation set
X.validate <- model.matrix(smoking ~ ., data = model.data.validate)
X.validate <- X.validate[,-1]

predictions <- predict(nn_model,X.validate)$predictions

y.validate <- model.data.validate[,"smoking"]

A <- matrix(0,ncol=2,nrow=2)

colnames(A) <- c("Real: No smoker", " Real: Smoker")
rownames(A) <- c("Prognose: No smoker", "Prognose: Smoker") 

A[1,1] <- sum(ifelse(y.validate == 0 & predictions == 0, 1,0))
A[1,2] <- sum(ifelse(y.validate == 1 & predictions == 0, 1,0))
A[2,1] <- sum(ifelse(y.validate == 0 & predictions == 1, 1,0))
A[2,2] <- sum(ifelse(y.validate == 1 & predictions == 1, 1,0))

A

# Missclassification error rate
(A[1,2]+A[2,1])/(A[1,1]+A[1,2]+A[2,1]+A[2,2])

# True-Positive & Ture-Negative Rate
A[2,2]/(A[1,2]+A[2,2])   # True-Positive-Rate
A[1,1]/(A[1,1]+A[2,1])   # True-Negative-Rate

# Take nn hidden layers and neurons with best results on validation set.
# Compute prediction error and true-positive/true-negative rate on test set.
nn_model <- neuralnetwork(X, y, 
                          hidden.layers=c(15,5,12),
                          regression = FALSE, 
                          loss.type = "log", 
                          learn.rates = 1e-04,
                          n.epochs = 150,
                          verbose=FALSE)

# Calculate prediction error and confusion matrix on validation set
X.test <- model.matrix(smoking ~ ., data = model.data.test)
X.test <- X.test[,-1]

predictions <- predict(nn_model,X.test)$predictions

y.test <- model.data.test[,"smoking"]

A <- matrix(0,ncol=2,nrow=2)

colnames(A) <- c("Real: No smoker", " Real: Smoker")
rownames(A) <- c("Prognose: No smoker", "Prognose: Smoker") 

A[1,1] <- sum(ifelse(y.test == 0 & predictions == 0, 1,0))
A[1,2] <- sum(ifelse(y.test == 1 & predictions == 0, 1,0))
A[2,1] <- sum(ifelse(y.test == 0 & predictions == 1, 1,0))
A[2,2] <- sum(ifelse(y.test == 1 & predictions == 1, 1,0))

A

# Missclassification error rate
(A[1,2]+A[2,1])/(A[1,1]+A[1,2]+A[2,1]+A[2,2])

# True-Positive & Ture-Negative Rate
A[2,2]/(A[1,2]+A[2,2])   # True-Positive-Rate
A[1,1]/(A[1,1]+A[2,1])   # True-Negative-Rate