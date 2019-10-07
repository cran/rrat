predict.rrat <-
function(object, newdata, ...){
new_rq_y <- matrix(0, nrow=nrow(newdata), ncol=length(object $ taus))
for (j in 1:ncol(new_rq_y)){
new_rq_y[,j] <- quantreg::predict.rq(object $ rq_model[[j]], newdata=newdata)
}
new_rq_y_mean <- rowMeans(new_rq_y)
as.numeric( new_rq_y_mean + object $ bias_correction)
}
