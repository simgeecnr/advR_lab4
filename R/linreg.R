#' Linear Regression Model with Linear Algebra
#'
#' @description This function do the linear regression
#' @param formula a linear regression model
#' @param data a dataframe
#' @return returns a RC class
#' @export
#' @examples
#' linreg(Petal.Length~Species, data = iris)


library(ggplot2)
linreg <- setRefClass("linreg",
                      fields = list(formula = "formula",
                                    data = "data.frame",
                                    reg_coef = "matrix",
                                    fitted_val = "matrix",
                                    res = "matrix", 
                                    dof = "integer",
                                    res_var = "matrix",
                                    var_reg_coef = "numeric",
                                    t_values = "matrix",
                                    p_values = "numeric",
                                    x_matrix = "matrix",
                                    y_matrix = "matrix",
                                    data_name = "character"),
                      
                      methods = list(
                        initialize = function(formula, data){
                          
                          #formula <- as.formula(formula)
                          #data <- as.data.frame(data)
                          
                          data_name <- deparse(substitute(data))
                          
                          #CREATE MATRIX FROM DATAFRAME
                          x_matrix <- model.matrix(formula, data)
                          #Extract the dependent variable
                          y_data <- data.frame(iris[, (all.vars(formula)[1])])
                          y_matrix <- as.matrix(y_data)
                          colnames(y_matrix) <- all.vars(formula)[1]
                          
                          #NECESSARY STATISTICS
                          #Regressions coefficients:
                          reg_coef <- solve(t(x_matrix) %*% x_matrix) %*% t(x_matrix) %*% y_matrix
                          #colnames(reg_coef) <- "Coefficients:"
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
                          
                          #Set the instance variables
                          .self$data_name <<- data_name
                          .self$formula <<- formula
                          .self$data <<- data
                          .self$reg_coef <<- reg_coef
                          .self$fitted_val <<- fitted_val
                          .self$res <<- res
                          .self$dof <<- dof
                          .self$res_var <<- res_var
                          .self$var_reg_coef <<- var_reg_coef
                          .self$t_values <<- t_values
                          .self$p_values <<- p_values
                        },
                        
                        print = function(){
                          
                          cat("\nCall:\n")
                          #Problem : cant get dataset name !
                          cat("linreg(formula = ", deparse(formula), ", data = ", data_name,")\n", sep = "")
                          output_obj = t(reg_coef)
                          cat("\nCoefficients:\n")
                          print.default(output_obj[1,])
                          #return()
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
                        
                        summary = function(){
                          #********This part is not working
                          formula_to_char <- as.character(formula)
                          
                          n <- length(y_matrix)
                          x1 <- sqrt(var_reg_coef)
                          x2 <- t_values
                          x3 <- p_values
                          x4 <- sum(res^2) / (n - length(reg_coef) - 1)
                          x5 <- dof
                          summary <- list(formula_to_char, x1, x2, x3, x4, dof)
                          
                          output_sum <- data.frame(
                            Estimate = reg_coef,
                            `Std. Error` = x1,
                            `t value` = t_values,
                            `p value` = p_values
                          )
                          
                          cat("\nCoefficients:\n")
                          
                          #as the estimate of ˆσ and the degrees of freedom in the model.
                          standardized_residuals <- res %*% (1 / sqrt(res_var))
                          
                          
                          #*********This part is working
                          cat("Residual standard error: ", sqrt(res_var), " on ", dof, " degrees of freedom", sep="")
                        },
                        
                        plot = function(theme = "none"){
                          source("R/liu_theme.R")
                          data2 <- data.frame(Fitted = fitted_val, Residuals = res)
                          #---------------PLOT 1---------------------------
                          # Create a residuals vs. fitted values plot using ggplot2
                          p1 <-ggplot(data2, aes(x = fitted_val, y = res)) +
                            geom_point() +
                            stat_summary(aes(y = res, group = 1), fun=median, color ="red", geom="line", group=1) +
                            labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs. Fitted Values Plot")
                          #--------------PLOT 2---------------------------
                          standardized_residuals <- res %*% (1 / sqrt(res_var))
                          y_val <- sqrt(abs(standardized_residuals))
                          data3 <- data.frame(Fitted = fitted_val, StdRes = y_val)
                          colnames(data3) <- c("Fitted", "StdRes")
                          
                          # Create a residuals vs. fitted values plot using ggplot2
                          p2 <- ggplot(data3, aes(x = fitted_val, y = y_val)) +
                            geom_point() +
                            stat_summary(aes(y = y_val, group = 1), fun=mean, color ="red", geom="line", group=1) +
                            labs(x = "Fitted Values", y = "Standardized Residuals", title = "Scale-Location")
                          #to show abs and sqrt in axis title, use expression()
                          #Theme selection
                          if(theme == "liu_light"){
                            p1 <- p1 + liu_theme_light()
                            p2 <- p2 + liu_theme_light()
                            return(list(p1,p2))
                          }else if(theme == "liu_dark"){
                            p1 <- p1 + liu_theme_dark()
                            p2 <- p2 + liu_theme_dark()
                            return(list(p1,p2))
                          }else {
                            p1 <- p1 + theme_bw()
                            p2 <- p2 + theme_bw()
                            return(list(p1,p2))
                          }
                        }
                      )
)

#need to fix "shell" function

#test code
#data(iris)

#linreg_mod <- linreg$new(Petal.Length~Species, data = iris)
#linreg_mod$print()
#linreg_mod$plot()
#linreg_mod$plot(theme="dark")
#linreg_mod$plot(theme="light")