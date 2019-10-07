rrat.default <-
function(x, y, taus = 0.5, ...){
# x: data.frame of training data, NOT design matrix
# y: response vector
# tau: quantiles
# intercept: TRUE/FALSE

rq_model <- list()
rq_y <- matrix(0, nrow=length(y), ncol=length(taus))
rq_coefficients <- matrix(0, nrow=ncol(x)+1, ncol=length(taus))

temp_data <- cbind(Y=y, x)
for (j in 1:length(taus)){
rq_model[[j]] <- quantreg::rq(Y~., tau=taus[j], data=temp_data,...)
rq_y[,j] <- rq_model[[j]] $ fitted.values
rq_coefficients[,j] <- rq_model[[j]] $ coefficients
}
rq_y_mean <- rowMeans(rq_y)
rownames(rq_coefficients) <- names(rq_model[[j]] $ coefficients)
rrat_coefficients <- rowMeans(rq_coefficients)

bias_correction <- mean( y - rq_y_mean)
fitted_values   <- rq_y_mean + bias_correction
rrat_coefficients[1] <- rrat_coefficients[1] + bias_correction

result <- list( taus = taus
              , rrat_coefficients = rrat_coefficients
              , rq_coefficients = rq_coefficients
              , bias_correction = bias_correction
              , fitted_values = fitted_values
              , rq_model = rq_model
              , rq_y = rq_y
              )
class(result) <- 'rrat'
result
}
