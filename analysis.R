#you'll need to install these two packages
library(glmnet)
library(pROC)

# --- Read training data ---
train <- read.csv("training_data.csv")

# --- Logistic regression ---
fit_logistic <- glm(heart_attack ~ ., data = train, family = binomial)

cat("=== Logistic Regression Coefficients ===\n")
print(round(coef(fit_logistic), 4))

# --- Lasso ---
x_train <- as.matrix(train[, -1])
y_train <- train$heart_attack

fit_lasso <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)

cat("\n=== Lasso Coefficients ===\n")
print(round(coef(fit_lasso), 4))

# --- Read test data and evaluate ---
test <- read.csv("test_data.csv")
x_test <- as.matrix(test[, -1])
y_test <- test$heart_attack

# Predicted probabilities
pred_logistic <- predict(fit_logistic, newdata = test, type = "response")
pred_lasso    <- predict(fit_lasso, newx = x_test, s = "lambda.min", type = "response")[, 1]

# AUC
auc_logistic <- auc(roc(y_test, pred_logistic, quiet = TRUE))
auc_lasso    <- auc(roc(y_test, pred_lasso, quiet = TRUE))

cat("\n=== Test Set AUC ===\n")
cat("Logistic regression:", round(auc_logistic, 4), "\n")
cat("Lasso:             ", round(auc_lasso, 4), "\n")

# --- ROC curve for Lasso ---
roc_lasso <- roc(y_test, pred_lasso, quiet = TRUE)

par(bty = "n")
plot(roc_lasso, col = "blue", lwd = 2, main = "Lasso ROC Curve",
     legacy.axes = TRUE, xlab = "1 - Specificity", ylab = "Sensitivity")
legend("bottomright",
       legend = paste("AUC =", round(auc(roc_lasso), 3)),
       col = "blue", lwd = 2)
