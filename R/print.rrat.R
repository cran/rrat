print.rrat <-
function(x, ...){
cat('Robust Regression with Asymmetric Heavy-Tail Noise Distributions', '\n')
cat('\n')
cat('taus: ', x$taus, '\n')
cat('Bias Correction: ', format(x $ bias_correction, digits=4), '\n')
cat('Coefficients: \n')
print.default(format( x $ rrat_coefficients, digits = 4), print.gap = 2L, 
quote = FALSE)
}
