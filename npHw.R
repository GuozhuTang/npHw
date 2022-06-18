#boot package
library(boot)
data("motor")
head(motor)
dim(motor)
X<-motor$times
Y<-motor$accel
#####
n = length(X)
# n: sample size
h_seq = seq(from=1.5,to=5, by=0.1)
#kernel density estimation
# smoothing bandwidths we are using
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    X_val = X[i]
    Y_val = Y[i]
    # validation set
    X_tr = X[-i]
    Y_tr = Y[-i]
    # training set
    Y_val_predict = ksmooth(x=X_tr,y=Y_tr,kernel = "normal",bandwidth=h_using,
                            x.points = X_val)
    CV_err[i] = (Y_val - Y_val_predict$y)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err)
}
plot(x=h_seq, y=CV_err_h, type="b", lwd=1, col="blue",
     xlab="Bandwidth", ylab="CV")
hbest=h_seq[which.min(CV_err_h)]
hbest
#kernel regression (cv bandwidth selection)
Kreg = ksmooth(x=X,y=Y,kernel = "normal",bandwidth = hbest)
#local linear estimation
# smoothing bandwidths we are using
h_seq = seq(from=0.1,to=0.4, by=0.02)
CV_err_h = rep(NA,length(h_seq))
for(j in 1:length(h_seq)){
  h_using = h_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    X_val = X[i]
    Y_val = Y[i]
    # validation set
    X_tr = X[-i]
    Y_tr = Y[-i]
    # training set
    lo = loess(Y_tr~X_tr,degree = 1,span = h_using, control = loess.control(surface = 'direct'))
    Y_val_predict = predict(lo,X_val)
    CV_err[i] = (Y_val - Y_val_predict)^2
    # we measure the error in terms of difference square
  }
  CV_err_h[j] = mean(CV_err)
}
plot(x=h_seq, y=CV_err_h, type="b", lwd=1, col="blue",
     xlab="Bandwidth", ylab="CV")
hbest=h_seq[which.min(CV_err_h)]
hbest
# local linear regression
locreg <- loess(Y~X,degree=1, span = hbest,control=loess.control(surface="direct"))
#cubic B-spline  estimation
library(splines)
# CV for knots selection
k_seq = seq(from=3,to=15, by=1)
CV_err_k = rep(NA,length(k_seq))
for(j in 1:length(k_seq)){
  k_using = k_seq[j]
  CV_err = rep(NA, n)
  for(i in 1:n){
    X_val = X[i]
    Y_val = Y[i]
    # validation set
    X_tr = X[-i]
    Y_tr = Y[-i]
    # training set
    fit<-lm(Y_tr ~ bs(X_tr,df=3+k_using))
    data_val = data.frame(X_tr=X_val)
    Y_val_predict = predict(fit,newdata=data_val)
    CV_err[i] = (Y_val - Y_val_predict)^2
    # we measure the error in terms of difference square
  }
  CV_err_k[j] = mean(CV_err)
}
plot(x=k_seq, y=CV_err_k, type="b", lwd=1, col="blue",
     xlab="knots", ylab="CV")
kbest=k_seq[which.min(CV_err_k)]
kbest
fit<-lm(accel ~ bs(times,df=3+kbest),data = motor)
###
# plot data
plot(motor$times, motor$accel, pch=20,cex=0.8,
     xlab = "Time (ms)", ylab = "Acceleration (g)")
# add fit lines
lines(Kreg, lwd = 2)
lines(locreg, lwd = 2, lty = 2, col = "red")
times.grid<-seq(from=min(motor$times), to = max(motor$times))
lines(times.grid,predict(fit,newdata = list(times=times.grid)),col="blue",lwd=2,lty=3)
# add legend
legend("bottomright", c("KDE.cv", "Local linear.cv", "B-spline.cv"),
       lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")
