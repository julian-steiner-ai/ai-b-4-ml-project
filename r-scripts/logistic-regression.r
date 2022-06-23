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

# Split datset into training and test set
n <- length(model.data[,1])
index <- sample(1:n,n,replace=FALSE)
model.data <- model.data[index,]
seventyPercentLimit <- round(length(model.data[,1]) * 0.7,0)
model.data.train <- model.data[1:seventyPercentLimit,]
model.data.test <- model.data[(seventyPercentLimit+1):n,]

length(model.data[,1])
length(model.data.train[,1])
length(model.data.test[,1])

# Compute model
glm_model <- glm(smoking ~ ., data=model.data.train, binomial(link = "logit"))
glm_model

# Calculate prediction error
X.test <- model.data.test[,selected_features]
X.test <- subset(X.test,select=-smoking)

z <- predict(glm_model,X.test)
predictions <- round(exp(z)/(1+exp(z)))

y <- model.data.test[,"smoking"]

# Create confusion matrix

A <- matrix(0,ncol=2,nrow=2)

colnames(A) <- c("Real: No smoker", " Real: Smoker")
rownames(A) <- c("Prognose: No smoker", "Prognose: Smoker") 

A[1,1] <- sum(ifelse(y == 0 & predictions == 0, 1,0))
A[1,2] <- sum(ifelse(y == 1 & predictions == 0, 1,0))
A[2,1] <- sum(ifelse(y == 0 & predictions == 1, 1,0))
A[2,2] <- sum(ifelse(y == 1 & predictions == 1, 1,0))

A

# Missclassification error rate
(A[1,2]+A[2,1])/(A[1,1]+A[1,2]+A[2,1]+A[2,2])

# True-Positive & Ture-Negative Rate
A[2,2]/(A[1,2]+A[2,2])   # True-Positive-Rate
A[1,1]/(A[1,1]+A[2,1])   # True-Negative-Rate