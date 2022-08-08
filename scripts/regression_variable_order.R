# regression variable order example
# to show that the variable order on the right-hand side does not matter in R
library(tidyverse)
library(sjPlot)  # pretty output tables for regression models

data(mtcars) # classic built-in R dataset

(
  cars <- mtcars |> 
    rownames_to_column(var = 'model') |>  # make model a bonefide column
    as_tibble() |>                        # just to improve printing
    mutate(cyl_factor = as.factor(cyl))   # make cycles a factor, like our HOLC grades
  )

table(cars$cyl)
table(cars$cyl, cars$cyl_factor) # same, but recoded as factor

# baseline model, factor only
(model_1 <- lm(mpg ~ cyl_factor, data = cars)) # again, analogous to HOLC grades, were A is the intercept

# one factor to match baseline (cyl_factor) and one continuous predictor (gear)
(model_2 <- lm(mpg ~ cyl_factor + gear, data = cars)) 

# same as above, but the order is switched (continuous, then factor)
(model_3 <- lm(mpg ~ gear + cyl_factor, data = cars)) 

# compare model coefficients side by side
tab_model(
    model_1
  , model_2
  , model_3
)  

# the coefficients, which are the same (as per above) are in a different order
# in the model object
coef(model_2)
coef(model_3)

# extract coefficients for each model
(coef_2 <- model_2 |> coef() |> as.data.frame() |> rownames_to_column(var = 'coef'))
(coef_3 <- model_3 |> coef() |> as.data.frame() |> rownames_to_column(var = 'coef'))

# join to compare
(combined_coefs <- coef_2 |> left_join(coef_3, by = 'coef')) # visually the same

all.equal(combined_coefs$`coef(model_2)`, combined_coefs$`coef(model_3)`) # true

