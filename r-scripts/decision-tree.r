# Define features for machine learning model
selected_features = c('smoking',
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

# Loading pakacge with 'tree'
library(tree)

# Calculate the best K on the validation set
for (k in 2:10) {
    print(k)
    tree <- tree(smoking ~ ., data=model.data.train)
    tuning <- cv.tree(tree, K=k)
    t <- which.min(tuning$dev)
    EndNotes <- tuning$size[t]
    tree_model <- prune.tree(tree,best=EndNotes)

    X.validate <- model.data.validate[,selected_features]
    X.validate <- subset(X.validate,select=-smoking)
    y.validate <- model.data.validate[,"smoking"]

    predictions <- predict(tree_model,X.validate)
    predictions <- round(predictions[,2])

    A <- matrix(0,ncol=2,nrow=2)

    colnames(A) <- c("Real: No smoker", " Real: Smoker")
    rownames(A) <- c("Prognose: No smoker", "Prognose: Smoker") 

    A[1,1] <- sum(ifelse(y.validate == 0 & predictions == 0, 1,0))
    A[1,2] <- sum(ifelse(y.validate == 1 & predictions == 0, 1,0))
    A[2,1] <- sum(ifelse(y.validate == 0 & predictions == 1, 1,0))
    A[2,2] <- sum(ifelse(y.validate == 1 & predictions == 1, 1,0))

    print((A[1,2]+A[2,1])/(A[1,1]+A[1,2]+A[2,1]+A[2,2]))

    print(A[2,2]/(A[1,2]+A[2,2]))
    print(A[1,1]/(A[1,1]+A[2,1]))
}

# Different k, always the same result. 
# Look at some plots.

tree <- tree(smoking ~ ., data=model.data.train)
tuning <- cv.tree(tree, K=3)
t <- which.min(tuning$dev)
EndNotes <- tuning$size[t]
tree_model <- prune.tree(tree,best=EndNotes)
plot(tree_model)
text(tree_model)

tree <- tree(smoking ~ ., data=model.data.train)
tuning <- cv.tree(tree, K=5)
t <- which.min(tuning$dev)
EndNotes <- tuning$size[t]
tree_model <- prune.tree(tree,best=EndNotes)
plot(tree_model)
text(tree_model)

tree <- tree(smoking ~ ., data=model.data.train)
tuning <- cv.tree(tree, K=10)
t <- which.min(tuning$dev)
EndNotes <- tuning$size[t]
tree_model <- prune.tree(tree,best=EndNotes)
plot(tree_model)
text(tree_model)

# Always the same tree calculated
# Two nodes: gender and gtp (< 34.5)

# Compute prediction error and true-positive/true-negative rate on test set.
tree <- tree(smoking ~ ., data=model.data.train)
tuning <- cv.tree(tree, K=2)
t <- which.min(tuning$dev)
EndNotes <- tuning$size[t]

tree_model <- prune.tree(tree,best=EndNotes)

plot(tree_model)
text(tree_model)

X.test <- model.data.test[,selected_features]
X.test <- subset(X.test,select=-smoking)
y.test <- model.data.test[,"smoking"]

predictions <- predict(tree_model,X.test)
predictions <- round(predictions[,2])

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