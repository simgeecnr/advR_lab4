
linreg <- function(data, formula){
  
  #CREATING MATRIX FROM DATAFRAME ACCORDING TO MODEL
  x_matrix <- model.matrix(formula, data = data)
  
  #Extract the dependent variable
  y_data <- data.frame(iris[, (all.vars(formula)[1])])
  y_matrix <- as.matrix(y_data)
  colnames(y_matrix) <- all.vars(formula)[1]
  
  #----------NECESSARY STATISTICS-----------
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
  
  #p-values:
  p_values <- c()
  for (i in 1:length(t_values)) {
    p_values[i] <- 2*pt(-abs(t_values[i]), df = dof, lower.tail = TRUE)
  }
  
  #CREATED CLASS 
  linreg <- setRefClass("linreg",
                        fields = list(reg_coef = "matrix",
                                      fitted_val = "matrix",
                                      res = "matrix", 
                                      dof = "integer",
                                      res_var = "matrix",
                                      var_reg_coef = "numeric", 
                                      t_values = "matrix",
                                      p_values = "numeric"
                        ),
                        methods = list(
                          print = function(){
                            return(reg_coef)
                          },
                          #-------------------
                          plot = function(){
                            #Work in progress
                            
                            #One plot with Residuals (res) vs Fitted values (fitted_val)
                            df1 <- data.frame(
                              Residuals = res,
                              Fitted = fitted_val
                            )
                            ggplot(data = df1,
                                   mapping = aes(x = Fitted, y = Residuals)) +
                              geom_point() +
                              geom_line(color = "red")
                            labs(
                              title = "Residuals vs Fitted",
                              x = "Fitted Values",
                              y = "Residuals"
                            )
                            
                            #One plot sqrt(abs(standardized residuals)) vs fitted values
                            df2 <- data.frame(
                              StdRes = sqrt(res),
                              Fitted = fitted_val
                            )
                            ggplot(data = df1,
                                   mapping = aes(x = Fitted, y = StdRes)) +
                              geom_point() +
                              geom_line(color = "red")
                            labs(
                              title = "Scale-Location",
                              x = "Fitted Values",
                              y = "Standardized residuals"
                            )
                            
                            #optional step : add theme
                            return(NULL)
                          },
                          #----------------
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
                          #--------------
                          summary = function(){
                            #Work in progress:
                            #Should return the coefs with std error, t-val, p-val sqrt(res_val), dof
                            sigma <- sqrt(res_var)
                            
                            
                            #Data frame for the summary table
                            summary_df <- data.frame(
                              Coefficients = reg_coef,
                              Std.Error = sqrt(res_var),
                              t.value = t_values,
                              p.value = p_values
                            )
                            summary_df <- rbind(summary_df, c("Sigma", sigma, NA, NA, NA))
                            summary_df <- rbind(summary_df, c("Degrees of Freedom", dof, NA, NA, NA))
                            
                            return(summary_df)
                          }
                          #--------------
                        ) #methods closing )
  ) #linreg class closing )
  
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