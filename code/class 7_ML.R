rm(list = ls())

library(dplyr)
library(keras)
library(tensorflow)

library(mlbench)

tensorflow::install_tensorflow(version = "2.7", force=TRUE, update=FALSE)

# Data ----

data(BostonHousing)
dim(BostonHousing)
head(BostonHousing)
str(BostonHousing)

# Separate into training and testing
index <- sample(nrow(BostonHousing), nrow(BostonHousing) * 0.7, replace = FALSE)
Dtrain <- BostonHousing[index,]
Dtest <- BostonHousing[-index,]

X_train <- Dtrain %>%
    select(-chas) %>%
    select(-medv) %>%
    scale()

y_train <- Dtrain %>%
    select(medv) %>%
    scale()

X_test <- Dtest %>%
    select(-chas) %>%
    select(-medv) %>%
    scale()

y_test <- Dtest %>%
    select(medv) %>%
    scale()

# Model ----
model <- keras_model_sequential()
model %>%
    layer_dense(units = 30, activation = "relu") %>%
    layer_dropout(rate = 0.25) %>%
    layer_dense(units = 30, activation = "relu") %>%
    layer_dense(units = 1, activation = "linear")

model %>% compile(
    loss = "mean_squard_error",
    optimizer = optimizer_adam(learning_rate = 0.01, decay = 0)
)

history = model%>%
    fit(x = X_train, y = y_train,
        validation_split = 0.1,
        epochs = 50, batch_size = 20
    )

pred <- predict(model, X_test)
e <- y_test - pred

sqrt(mean(e^2))
mean(abs(e))

library(ranger)    
mrf <- ranger(y = y_train, x = X_train)
pred <- predict(mrf, X_test)$predictions
