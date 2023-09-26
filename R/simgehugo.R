linreg <- function(formula, data){
  #CREATE MATRIX FROM DATAFRAME
  x_matrix <- model.matrix(formula, data)
  #Extract the dependent variable
  y_data <- data.frame(iris[, (all.vars(formula)[1])])
  y_matrix <- as.matrix(y_data)
  colnames(y_matrix) <- all.vars(formula)[1]
  
  #NECESSARY STATISTICS
  #Regressions coefficients:
  reg_coef <- solve(t(x_matrix) %*% x_matrix) %*% t(x_matrix) %*% y_matrix
  colnames(reg_coef) <- "Coefficients:"
  #The fitted values
  fitted_val <- x_matrix %*% reg_coef
  #The residuals:
  res <- y_matrix - fitted_val
  #The degrees of freedom:
  dof <- nrow(iris) - length(reg_coef)
  #The residual variance: 
  res_var <- (t(res) %*% res) / dof
  #The variance of the regression coefficients:
  sigma_hat <- sqrt(sum(res^2) / dof)
  var_reg_coef <- (sigma_hat^2) * diag(solve(t(x_matrix) %*% x_matrix))
  #The t-values for each coefficient:
  t_values <- reg_coef / sqrt(var_reg_coef)
  #p-value
  p_values <- c()
  for (i in 1:length(t_values)) {
    p_values[i] <- 2*pt(-abs(t_values[i]), df = dof, lower.tail = TRUE)
  }
  
  #CREATE CLASS
  linreg <- setRefClass("linreg",
                        fields = list(reg_coef = "matrix",
                                      fitted_val = "matrix",
                                      res = "matrix", 
                                      dof = "integer",
                                      res_var = "matrix",
                                      var_reg_coef = "numeric", 
                                      t_values = "matrix",
                                      p_values = "numeric"),
                        methods = list(
                          print = function(){
                            return(reg_coef)
                          },
                          resid = function(){
                            return(res)
                          },
                          pred = function(){
                            return(fitted_val)
                          },
                          coef = function(){
                            names <- rownames(reg_coef)
                            values <- c(reg_coef)
                            named_vector <- setNames(values, names)
                            return(named_vector)
                          },
                          plot = function(){
                            data2 <- data.frame(Fitted = fitted_val, Residuals = res)
                            
                            # Create a residuals vs. fitted values plot using ggplot2
                            ggplot(data2, aes(x = fitted_val, y = res)) +
                              geom_point() +
                              stat_summary(aes(y = res, group = 1), fun=median, color ="red", geom="line", group=1) +
                              labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs. Fitted Values Plot")
                          }
                        )
  )
  
  linreg <- linreg$new(reg_coef = reg_coef,
                       fitted_val = fitted_val,
                       res = res, 
                       dof = dof,
                       res_var = res_var, 
                       var_reg_coef = var_reg_coef,
                       t_values = t_values,
                       p_values = p_values)  
  return(linreg)
}

data(iris)
k <- linreg(Petal.Length~Species, iris)
k$plot()