library(ggplot2)
library(torch)
library(luz)
library(dplyr)
library(caret)
library(iml)
library(ggcorrplot)
library(car)

data <- read.csv("~/Downloads/intrvw23/fmli241.csv", row.names=1)
data <- data %>%
  select(
    CUID, #Household ID
    TOTEXPCQ, #Total Expenditures
    FOODCQ, #Food Expenditure
    ALCBEVCQ, #Alcohol Expenditure
    HOUSCQ, #Housing Expenditure
    APPARCQ, #Apparel and Services Expenditure
    TRANSCQ, #Transportation Expenditure
    HEALTHCQ, #Healthcare Expenditure
    ENTERTCQ, #Entertainment Expenditure
    PERSCACQ, #Personal Care Expenditure
    READCQ, #Reading Expenditure
    EDUCACQ, #Education Expenditure
    TOBACCCQ, #Tobacco and Smoking Expenditure
    LIFINSCQ, #Life and Other Personal Insurance Expenditure
    MISCCQ, #Miscellaneous Expenditure
    CASHCOCQ, #Cash Contributions Expenditure
    RETPENCQ, #Retirement, Pensions, and Social Security Expenditure
    FINCBTXM, #Total Family Income Before Taxes
    NO_EARNR, #Number of Income-Earning CU Members
    AGE_REF, #Age of Reference Person
    AGE2, #Age of Spouse
    FAM_SIZE, #Number of Members of CU
    CHILDAGE, #Age of Children (Categorical)
    HIGH_EDU, #Highest Level of Education in CU
    BLS_URBN, #Urban or Rural Status
  )
data <- data %>%
  mutate(AVG_AGE = rowMeans(select(., "AGE_REF", "AGE2"), na.rm = TRUE))
data %>%
  summarize(across(c(
    FINCBTXM, NO_EARNR, AVG_AGE, FAM_SIZE), list(
      mean=mean, median=median, min=min, max=max, sd=sd), na.rm=TRUE)) %>%
  kable(digits = 3
        , caption = "Summary Statistics for Numeric Explanatory Variables")

childage_summary <- vars %>%
  count(CHILDAGE) %>%
  mutate(percentage = n / sum(n) * 100)
childage_summary

highedu_summary <- vars %>%
  count(HIGH_EDU) %>%
  mutate(percentage = n / sum(n) * 100)
highedu_summary

bls_urbn_summary <- vars %>%
  count(BLS_URBN) %>%
  mutate(percentage = n / sum(n) * 100)
bls_urbn_summary

data %>%
  summarize(across(c(FOODCQ, ALCBEVCQ, HOUSCQ, APPARCQ, TRANSCQ,
                     HEALTHCQ, ENTERTCQ, PERSCACQ, READCQ, EDUCACQ,
                     TOBACCCQ, LIFINSCQ, MISCCQ, CASHCOCQ, RETPENCQ),
                   list(mean=mean, median=median, min=min, max=max, sd=sd),
                   na.rm = TRUE)) %>%
  kable(digits = 3, caption = "Summary Statistics for Expenditure Categories")

factors <- data %>%
  select(
    FINCBTXM, #Total Family Income Before Taxes
    NO_EARNR, #Number of Income-Earning CU Members
    AVG_AGE, #Average Age of Parents
    FAM_SIZE, #Number of Members of CU
    CHILDAGE, #Age of Children (Categorical)
    HIGH_EDU, #Highest Level of Education in CU
    BLS_URBN, #Urban or Rural Status
  )

cor_matrix <- cor(factors, use = "pairwise.complete.obs") 
print(cor_matrix)

png("correlation_plot.png", width = 800, height = 600, res = 150)
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("blue", "white", "red"),
           outline.col = "black")
dev.off()


vif_model <- lm(FINCBTXM ~ NO_EARNR + AVG_AGE + FAM_SIZE +
                  CHILDAGE + HIGH_EDU + REGION + BLS_URBN, data = vars)
vif_values <- vif(vif_model)
print(vif_values)

data$pFOODCQ = data$FOODCQ / data$TOTEXPCQ
data$pALCBEVCQ = data$ALCBEVCQ / data$TOTEXPCQ
data$pHOUSCQ = data$HOUSCQ / data$TOTEXPCQ
data$pAPPARCQ = data$APPARCQ / data$TOTEXPCQ
data$pTRANSCQ = data$TRANSCQ / data$TOTEXPCQ
data$pHEALTHCQ = data$HEALTHCQ / data$TOTEXPCQ
data$pENTERTCQ = data$ENTERTCQ / data$TOTEXPCQ
data$pPERSCACQ = data$PERSCACQ / data$TOTEXPCQ
data$pREADCQ = data$READCQ / data$TOTEXPCQ
data$pEDUCACQ = data$EDUCACQ / data$TOTEXPCQ
data$pTOBACCCQ = data$TOBACCCQ / data$TOTEXPCQ
data$pLIFINSCQ = data$LIFINSCQ / data$TOTEXPCQ
data$pMISCCQ = data$MISCCQ / data$TOTEXPCQ
data$pCASHCOCQ = data$CASHCOCQ / data$TOTEXPCQ
data$pRETPENCQ = data$RETPENCQ / data$TOTEXPCQ

data$FINCBTXM <- scale(data$FINCBTXM)
data$NO_EARNR <- scale(data$NO_EARNR)
data$AVG_AGE <- scale(data$AVG_AGE)
data$FAM_SIZE <- scale(data$FAM_SIZE)
data$CHILDAGE <- scale(data$CHILDAGE)
data$HIGH_EDU <- scale(data$HIGH_EDU)
data$BLS_URBN <- scale(data$BLS_URBN)

data$Total <- rowSums(data[, c("pFOODCQ", "pALCBEVCQ", "pHOUSCQ", "pAPPARCQ",
                               "pTRANSCQ", "pHEALTHCQ", "pENTERTCQ",
                               "pPERSCACQ", "pREADCQ", "pEDUCACQ", "pTOBACCCQ",
                               "pLIFINSCQ", "pMISCCQ", "pCASHCOCQ",
                               "pRETPENCQ")])
data <- data[!is.na(data$Total), ]

single_nn <- function(x_col){
  torch_manual_seed(123)
  
  y <- c(
    "pFOODCQ", "pALCBEVCQ", "pHOUSCQ", "pAPPARCQ", "pTRANSCQ",
    "pHEALTHCQ", "pENTERTCQ", "pPERSCACQ", "pREADCQ", "pEDUCACQ",
    "pTOBACCCQ", "pLIFINSCQ", "pMISCCQ", "pCASHCOCQ", "pRETPENCQ"
  )
  
  X <- as.matrix(data[, x_col, drop = FALSE])
  Y <- as.matrix(data[, y])
  
  set.seed(123)
  N <- nrow(X)
  idx <- sample.int(N, size = floor(0.8 * N))
  
  X_train <- X[idx, , drop = FALSE]
  Y_train <- Y[idx, ]
  X_test  <- X[-idx, , drop = FALSE]
  Y_test  <- Y[-idx, ]
  
  my_dataset <- dataset(
    name = "my_dataset",
    
    initialize = function(x_mat, y_mat) {
      self$x <- torch_tensor(x_mat, dtype = torch_float())
      self$y <- torch_tensor(y_mat, dtype = torch_float())
    },
    
    .getitem = function(i) {
      list(self$x[i, ], self$y[i, ])
    },
    
    .length = function() {
      self$x$size()[1]
    }
  )
  
  train_ds <- my_dataset(X_train, Y_train)
  test_ds  <- my_dataset(X_test, Y_test)
  
  train_dl <- dataloader(train_ds, batch_size = 32, shuffle = TRUE)
  test_dl  <- dataloader(test_ds, batch_size = 32, shuffle = FALSE)
  
  model_module <- nn_module(
    initialize = function(input_size = 1, output_size = 15) {
      self$dense <- nn_linear(input_size, output_size)
    },
    forward = function(x) {
      x %>%
        self$dense() %>%
        nnf_log_softmax(dim = 2)
    }
  )
  
  my_cross_entropy_loss <- function(input, target) {
    loss <- - (target * input) %>%
      torch_sum(dim = 2) %>%
      torch_mean()
    loss
  }
  
  model <- model_module %>%
    setup(
      loss = my_cross_entropy_loss,
      optimizer = optim_adam
    ) %>%
    set_opt_hparams(lr = 0.001)
  
  fitted <- model %>%
    fit(
      train_dl,
      epochs = 10,
      valid_data = test_dl
    )
  
  test_metrics <- evaluate(fitted, test_dl)
  return(test_metrics)
}

loss_vector <- numeric(100)
for (i in 1:100) {
  set.seed(i)
  col_name <- paste0("RANDOM_NORM_", i)
  
  data[[col_name]] <- rnorm(n = nrow(data), mean = 0, sd = 1)
  
  test_metrics <- suppressWarnings(single_nn(col_name))
  
  loss_vector[i] <- test_metrics[[2]]$metrics$valid[[1]]$loss
}
loss_vector

feature_names <- c("FINCBTXM", "NO_EARNR", "AVG_AGE", "FAM_SIZE",
                   "CHILDAGE", "HIGH_EDU", "BLS_URBN")

loss_values <- c(
  suppressWarnings(single_nn("FINCBTXM")[[2]]$metrics$valid[[1]]$loss),
  suppressWarnings(single_nn("NO_EARNR")[[2]]$metrics$valid[[1]]$loss),
  suppressWarnings(single_nn("AVG_AGE")[[2]]$metrics$valid[[1]]$loss),
  suppressWarnings(single_nn("FAM_SIZE")[[2]]$metrics$valid[[1]]$loss),
  suppressWarnings(single_nn("CHILDAGE")[[2]]$metrics$valid[[1]]$loss),
  suppressWarnings(single_nn("HIGH_EDU")[[2]]$metrics$valid[[1]]$loss),
  suppressWarnings(single_nn("BLS_URBN")[[2]]$metrics$valid[[1]]$loss)
)

loss_density <- density(loss_vector)

all_losses <- c(loss_vector, loss_values)
min_x <- min(all_losses)
max_x <- max(all_losses)

buffer <- 0.05 * (max_x - min_x)
x_min <- min_x - buffer
x_max <- max_x + buffer

plot(loss_density,
     main = "Density Plot of Test Loss Values",
     xlab = "Test Loss",
     ylab = "Density",
     xlim = c(x_min, x_max)
)

base_y <- max(loss_density$y) * 0.8

for(i in seq_along(loss_values)) {
  abline(v = loss_values[i], col = "red", lwd = 2, lty = 2)
  text(
    x = loss_values[i],
    y = base_y - (i - 1) * (0.05 * max(loss_density$y)),
    labels = feature_names[i],
    srt = 90,
    col = "red",
    pos = 3
  )
}

multiple_nn <- function(X){
  torch_manual_seed(123)
  
  y <- c(
    "pFOODCQ", "pALCBEVCQ", "pHOUSCQ", "pAPPARCQ", "pTRANSCQ",
    "pHEALTHCQ", "pENTERTCQ", "pPERSCACQ", "pREADCQ", "pEDUCACQ",
    "pTOBACCCQ", "pLIFINSCQ", "pMISCCQ", "pCASHCOCQ", "pRETPENCQ"
  )
  Y <- as.matrix(data[, y])
  
  set.seed(123)
  N <- nrow(X)
  idx <- sample.int(N, size = floor(0.8 * N))
  
  X_train <- X[idx, ]
  Y_train <- Y[idx, ]
  X_test  <- X[-idx, ]
  Y_test  <- Y[-idx, ]
  
  my_dataset <- dataset(
    name = "my_dataset",
    
    initialize = function(x_mat, y_mat) {
      self$x <- torch_tensor(x_mat, dtype = torch_float())
      self$y <- torch_tensor(y_mat, dtype = torch_float())
    },
    
    .getitem = function(i) {
      x_i <- self$x[i, ]
      y_i <- self$y[i, ]
      list(x_i, y_i)
    },
    
    .length = function() {
      self$x$size()[1]
    }
  )
  
  train_ds <- my_dataset(X_train, Y_train)
  test_ds  <- my_dataset(X_test,  Y_test)
  
  train_dl <- dataloader(train_ds, batch_size = 32, shuffle = TRUE)
  test_dl  <- dataloader(test_ds,  batch_size = 32, shuffle = FALSE)
  
  model_module <- nn_module(
    initialize = function(input_size = 7, hidden_size = 16, output_size = 15) {
      self$dense1 <- nn_linear(input_size, hidden_size)
      self$dense2 <- nn_linear(hidden_size, hidden_size)
      self$dense3 <- nn_linear(hidden_size, output_size)
    },
    forward = function(x) {
      x %>%
        self$dense1() %>%
        nnf_relu() %>%
        self$dense2() %>%
        nnf_relu() %>%
        self$dense3() %>%
        nnf_log_softmax(dim = 2)
    }
  )
  
  my_cross_entropy_loss <- function(input, target) {
    loss <- - (target * input) %>%
      torch_sum(dim = 2) %>%
      torch_mean()
    loss
  }
  
  model <- model_module %>%
    setup(
      loss = my_cross_entropy_loss,
      optimizer = optim_adam
    ) %>%
    set_opt_hparams(lr = 0.001)
  
  fitted <- model %>%
    fit(
      train_dl,
      epochs = 10,
      valid_data = test_dl
    )
  
  test_metrics <- evaluate(fitted, test_dl)
  return(test_metrics)
}

multiple_loss_vector <- numeric(100)
for (i in 1:100) {
  set.seed(i)
  X <- matrix(rnorm(nrow(data) * 7, mean = 0, sd = 1), 
              nrow = nrow(data), 
              ncol = 7)
  
  test_metrics <- suppressWarnings(multiple_nn(X))
  
  multiple_loss_vector[i] <- test_metrics[[2]]$metrics$valid[[1]]$loss
}
multiple_loss_vector

x_col <- c("FINCBTXM", "NO_EARNR", "AVG_AGE", "FAM_SIZE",
           "CHILDAGE", "HIGH_EDU", "BLS_URBN")
X <- as.matrix(data[, x_col])
multiple_loss_value <- c(suppressWarnings(
  multiple_nn(X)[[2]]$metrics$valid[[1]]$loss))

loss_density <- density(multiple_loss_vector)

all_losses <- c(multiple_loss_vector, multiple_loss_value)
min_x <- min(all_losses)
max_x <- max(all_losses)

buffer <- 0.05 * (max_x - min_x)
x_min <- min_x - buffer
x_max <- max_x + buffer

plot(loss_density,
     main = "Density Plot of Test Loss Values",
     xlab = "Test Loss",
     ylab = "Density",
     xlim = c(x_min, x_max)
)

base_y <- max(loss_density$y) * 0.8

for(i in seq_along(multiple_loss_value)) {
  abline(v = multiple_loss_value[i], col = "red", lwd = 2, lty = 2)
  text(
    x = multiple_loss_value[i],
    y = base_y - (i - 1) * (0.05 * max(loss_density$y)),
    labels = "Model Loss",
    srt = 90,
    col = "red",
    pos = 3
  )
}
