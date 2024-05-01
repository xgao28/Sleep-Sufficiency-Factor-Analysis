source("data_process.R")
glm_model <- glm(if_enough_sleep ~ ., all_p_mr, family = binomial(link = logit))
stepAIC(glm_model, direction = "both", trace = 0)

glm_aic <- glm(formula = if_enough_sleep ~ gender + education_level + age + 
            snore_freq + feel_sleepy_freq + depression_score, family = binomial(link = logit), 
            data = all_p_mr)

rf <- randomForest(
  if_enough_sleep ~.,
  data = all_p_mr,
  na.action = na.omit,
)

set.seed(370)
param_grid <- expand.grid(
  eta = seq(0.01, 0.2, by = 0.04),
  nrounds = 500,
  max_depth = c(3, 6, 9),
  gamma = 0,
  subsample = 1,
  min_child_weight = 1,
  colsample_bytree = 0.6
)

ctrl <- trainControl(method = "cv", 
                     number = 5,
                     search = "grid")

xgb_model <- caret::train(
  if_enough_sleep ~.,
  data = all_p_mr,
  na.action = na.omit,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = param_grid,
  verbosity = 0
)