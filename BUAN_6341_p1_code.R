#set working directory
setwd('E:/AML - BUAN 6341')

# Part 1
# reading data 
hour_all<-read.csv("hour.csv")


#removing nstant,dteday,casual,registered columns from the dataset
hour_factr<-subset(hour_all,select = -c(instant,dteday,casual,registered))


# splitting data into train and test data sets
set.seed(123)
sample <- sample(nrow(hour_factr),0.70*nrow(hour_factr), replace = FALSE)
train <- hour_factr[sample, ]
test  <- hour_factr[-sample, ]

# traget and independent variables split in training data
y_train<-data.matrix(train$cnt)
x_train<-data.matrix(subset(train,select = -c(cnt)))

# Addinga unit column to independent variable data for beta 0 and converting it to matrix
X_train<-cbind(rep(1,12165),x_train)
X_train<-data.matrix(X_train)
theta<-rep(0,13)
m<-nrow(X_train)

#compute cost function for training data:
cost<-function(X_train,y_train,theta){
  m<-nrow(X_train)
  J_cost<-sum(((X_train%*%theta)-y_train)^2)/(2*m)
  return(J_cost)
}


#gradient descent functions for loop for training data:
grad_descent<-function(X_train,y_train,theta,alpha,delta,thresh) {
  m<-nrow(X_train)
  while (delta>thresh) {
    J_hist<-sum(((X_train%*%theta)-y_train)^2)/(2*m)
    theta<-theta - (alpha * (1/m)* (t(X_train)%*%((X_train%*%theta) - y_train)))
    J_cur<-cost(X_train,y_train,theta)
    delta=(J_hist - J_cur)/J_hist
  }
  result<-list(J_cur,theta,delta)
  
  return(result)
}


# traget and independent variable split in test data
y_test<-data.matrix(test$cnt)
x_test<-data.matrix(subset(test,select = -c(cnt)))
X_test<-cbind(rep(1,5214),x_test)
X_test<-data.matrix(X_test)
theta_t<-rep(0,13)
m_t<-nrow(X_test)

#compute cost function for test data:
cost_t<-function(X_test,y_test,theta_t){
  m_t<-nrow(X_test)
  J_cost_t<-sum(((X_test%*%theta_t)-y_test)^2)/(2*m_t)
  return(J_cost_t)
}


#*******************************************************************************
# to compute initial parameter estimates and squared error values

#gradient descent functions for loop for test data:
grad_descent_t<-function(X_test,y_test,theta_t,alpha,delta_t,thresh) {
  m_t<-nrow(X_test)
  while (delta_t>thresh) {
    J_hist_t<-sum(((X_test%*%theta_t)-y_test)^2)/(2*m_t)
    theta_t<-theta_t - (alpha * (1/m_t)* (t(X_test)%*%((X_test%*%theta_t) - y_test)))
    J_cur_t<-cost_t(X_test,y_test,theta_t)
    delta_t=(J_hist_t - J_cur_t)/J_hist_t
  }
  result_t<-list(J_cur_t,theta_t,delta_t)
  return(result_t)
}

#initial parameters 
alpha<-0.01
thresh<-0.001
delta<-100
delta_t<-100

#calling functions into resluts
results<-grad_descent(X_train,y_train,theta,alpha,delta,thresh)
results_t<-grad_descent(X_test,y_test,theta_t,alpha,delta_t,thresh)


#viewing and collecting results to variables
results
results_t

theta<-results[[2]]
J_cur<-results[[1]]
delta=results[[3]]
theta_t<-results_t[[2]]
J_cur_t<-results_t[[1]]
delta_t=results_t[[3]]

#*********************************************************************************************
# plotting for the best alpha value:

#gradient descent functions for loop for training data to find optimum alpha:
grad_descent_alpha<-function(X_train,y_train,theta,alpha_a,delta,thresh) {
  m<-nrow(X_train)
  while (delta>thresh) {
    J_hist<-sum(((X_train%*%theta)-y_train)^2)/(2*m)
    theta<-theta - (alpha_a * (1/m)* (t(X_train)%*%((X_train%*%theta) - y_train)))
    J_cur<-cost(X_train,y_train,theta)
    delta=(J_hist - J_cur)/J_hist
  }
  result<-list(J_cur)
  
  return(result)
}

#gradient descent functions for loop for test data to find optimum alpha:
grad_descent_alpha_t<-function(X_test,y_test,theta_t,alpha_a,delta_t,thresh) {
  m_t<-nrow(X_test)
  while (delta_t>thresh) {
    J_hist_t<-sum(((X_test%*%theta_t)-y_test)^2)/(2*m_t)
    theta_t<-theta_t - (alpha_a * (1/m_t)* (t(X_test)%*%((X_test%*%theta_t) - y_test)))
    J_cur_t<-cost_t(X_test,y_test,theta_t)
    delta_t=(J_hist_t - J_cur_t)/J_hist_t
  }
  result_t<-list(J_cur_t)
  return(result_t)
}

# creating an alpha sequence
alpha_a<-seq(0.001,0.01,0.0005)
alpha_a
delta<-100
delta_t<-100
thresh=0.001
#functions to run the alpha value in loop to generate cost function values
J_train<-rep(0,19)
J_test<-rep(0,19)
for(i in 1:19){
  J_train[i]<-grad_descent_alpha(X_train,y_train,theta,alpha_a[i],delta,thresh)
  J_test[i]<-grad_descent_alpha_t(X_test,y_test,theta_t,alpha_a[i],delta_t,thresh)
}
#J_train
#J_test

#plot alpha vs cost of training and test
plot(alpha_a,J_train,type = "o",col = "red", xlab = "alpha", ylab = "cost", 
     main = "alpha vs cost function")

lines(alpha_a,J_test, type = "o", col = "blue")


#******************************************************************************************************
# plotting for best threshold value

#gradient descent functions for loop for training data to find optimum alpha:
grad_descent_thresh<-function(X_train,y_train,theta,alpha_n,delta,thresh_a) {
  m<-nrow(X_train)
  while (delta>thresh_a) {
    J_hist<-sum(((X_train%*%theta)-y_train)^2)/(2*m)
    theta<-theta - (alpha_n * (1/m)* (t(X_train)%*%((X_train%*%theta) - y_train)))
    J_cur<-cost(X_train,y_train,theta)
    delta=(J_hist - J_cur)/J_hist
  }
  result<-list(J_cur)
  
  return(result)
}

#gradient descent functions for loop for test data to find optimum alpha:
grad_descent_thresh_t<-function(X_test,y_test,theta_t,alpha_n,delta_t,thresh_a) {
  m_t<-nrow(X_test)
  while (delta_t>thresh_a) {
    J_hist_t<-sum(((X_test%*%theta_t)-y_test)^2)/(2*m_t)
    theta_t<-theta_t - (alpha_n * (1/m_t)* (t(X_test)%*%((X_test%*%theta_t) - y_test)))
    J_cur_t<-cost_t(X_test,y_test,theta_t)
    delta_t=(J_hist_t - J_cur_t)/J_hist_t
  }
  result_t<-list(J_cur_t)
  return(result_t)
}

#best alpha value
alpha_n<-0.0085
delta<-100
delta_t<-100

#creating sequence for threshold
thresh_a<-seq(0.00001,0.0005,0.00002)
thresh_a

J_train_th<-rep(0,25)
J_test_th<-rep(0,25)
for(i in 1:25){
  J_train_th[i]<-grad_descent_thresh(X_train,y_train,theta,alpha_n,delta,thresh_a[i])
  J_test_th[i]<-grad_descent_thresh_t(X_test,y_test,theta_t,alpha_n,delta_t,thresh_a[i])
}

#J_train_th
#J_test_th

#plot alpha vs cost of training and test
plot(thresh_a,J_train_th,type = "o",col = "red", xlab = "thresh", ylab = "cost", 
     main = "thresh vs cost function")

lines(thresh_a,J_test_th, type = "o", col = "blue")

#**************************************************************************************************

#3 random factors

hour_factr_r<-subset(hour_all,select = c(mnth,workingday,windspeed,cnt))

set.seed(123)
sample_r <- sample(nrow(hour_factr_r),0.70*nrow(hour_factr_r), replace = FALSE)
train_r <- hour_factr_r[sample_r, ]
test_r <- hour_factr_r[-sample_r, ]

# traget and independent variable split in training data random
y_train_r<-data.matrix(train_r$cnt)
x_train_r<-data.matrix(subset(train_r,select = -c(cnt)))
X_train_r<-cbind(rep(1,12165),x_train_r)
X_train_r<-data.matrix(X_train_r)
theta_r<-rep(0,4)
m_r<-nrow(X_train_r)


#compute cost function random features:
cost_r<-function(X_train_r,y_train_r,theta_r){
  m_r<-nrow(X_train_r)
  J_cost_r<-sum(((X_train_r%*%theta_r)-y_train_r)^2)/(2*m_r)
  return(J_cost_r)
}


#gradient descent functions for loop , random features:
grad_descent_r<-function(X_train_r,y_train_r,theta_r,alpha_r,delta,thresh_r) {
  m_r<-nrow(X_train_r)
  while (delta>thresh_r) {
    J_hist_r<-sum(((X_train_r%*%theta_r)-y_train_r)^2)/(2*m_r)
    theta_r<-theta_r - (alpha_r * (1/m_r)* (t(X_train_r)%*%((X_train_r%*%theta_r) - y_train_r)))
    J_cur_r<-cost_r(X_train_r,y_train_r,theta_r)
    delta=(J_hist_r - J_cur_r)/J_hist_r
  }
  result_r<-list(J_cur_r,alpha_r,theta_r)
  return(result_r)
}

#test data random features

# traget and independent variable split in training data, random features
y_test_r<-data.matrix(test_r$cnt)
x_test_r<-data.matrix(subset(test_r,select = -c(cnt)))
X_test_r<-cbind(rep(1,5214),x_test_r)
X_test_r<-data.matrix(X_test_r)
theta_t_r<-rep(0,4)
m_t_r<-nrow(X_test_r)


#compute cost function random features:
cost_t_r<-function(X_test_r,y_test_r,theta_t_r){
  m_t_r<-nrow(X_test_r)
  J_cost_t_r<-sum(((X_test_r%*%theta_t_r)-y_test_r)^2)/(2*m_t_r)
  return(J_cost_t_r)
}


#gradient descent functions for loop random features:
grad_descent_t_r<-function(X_test_r,y_test_r,theta_t_r,alpha_r,delta_t,thresh_r) {
  m_t_r<-nrow(X_test_r)
  while (delta_t>thresh_r) {
    J_hist_t_r<-sum(((X_test_r%*%theta_t_r)-y_test_r)^2)/(2*m_t_r)
    theta_t_r<-theta_t_r - (alpha_r * (1/m_t_r)* (t(X_test_r)%*%((X_test_r%*%theta_t_r) - y_test_r)))
    J_cur_t_r<-cost_t_r(X_test_r,y_test_r,theta_t_r)
    delta_t=(J_hist_t_r - J_cur_t_r)/J_hist_t_r
  }
  result_t_r<-list(J_cur_t_r,alpha_r,theta_t_r)
  return(result_t_r)
}

#passing parameters initial for random features
alpha_r<-0.0085
alpha_r
delta<-100
delta_t<-100
thresh_r<-0.0001
thresh_r

#function loaded into results matrix
results_r<-grad_descent_r(X_train_r,y_train_r,theta_r,alpha_r,delta,thresh_r)
results_t_r<-grad_descent_r(X_test_r,y_test_r,theta_t_r,alpha_r,delta_t,thresh_r)

#displaying results for random features
results_r
results_t_r

#***********************************************************************************************************
# 3 factors chosen

hour_factr_c<-subset(hour_all,select = c(temp,weathersit,hr,cnt))

set.seed(123)
sample_c <- sample(nrow(hour_factr_c),0.70*nrow(hour_factr_c), replace = FALSE)
train_c <- hour_factr_c[sample_c, ]
test_c <- hour_factr_c[-sample_c, ]

# traget and independent variable split in training data random
y_train_c<-data.matrix(train_c$cnt)
x_train_c<-data.matrix(subset(train_c,select = -c(cnt)))
X_train_c<-cbind(rep(1,12165),x_train_c)
X_train_c<-data.matrix(X_train_c)
theta_c<-rep(0,4)
m_c<-nrow(X_train_c)


#compute cost function random features:
cost_c<-function(X_train_c,y_train_c,theta_c){
  m_c<-nrow(X_train_c)
  J_cost_c<-sum(((X_train_c%*%theta_c)-y_train_c)^2)/(2*m_c)
  return(J_cost_c)
}


#gradient descent functions for loop , random features:
grad_descent_c<-function(X_train_c,y_train_c,theta_c,alpha_c,delta,thresh_c) {
  m_c<-nrow(X_train_c)
  while (delta>thresh_c) {
    J_hist_c<-sum(((X_train_c%*%theta_c)-y_train_c)^2)/(2*m_c)
    theta_c<-theta_c - (alpha_c * (1/m_c)* (t(X_train_c)%*%((X_train_c%*%theta_c) - y_train_c)))
    J_cur_c<-cost_c(X_train_c,y_train_c,theta_c)
    delta=(J_hist_c - J_cur_c)/J_hist_c
  }
  result_c<-list(J_cur_c,alpha_c,theta_c)
  return(result_c)
}

#test data random features

# traget and independent variable split in training data, random features
y_test_c<-data.matrix(test_c$cnt)
x_test_c<-data.matrix(subset(test_c,select = -c(cnt)))
X_test_c<-cbind(rep(1,5214),x_test_c)
X_test_c<-data.matrix(X_test_c)
theta_t_c<-rep(0,4)
m_t_c<-nrow(X_test_c)


#compute cost function random features:
cost_t_c<-function(X_test_c,y_test_c,theta_t_c){
  m_t_c<-nrow(X_test_c)
  J_cost_t_c<-sum(((X_test_c%*%theta_t_c)-y_test_c)^2)/(2*m_t_c)
  return(J_cost_t_c)
}


#gradient descent functions for loop random features:
grad_descent_t_c<-function(X_test_c,y_test_c,theta_t_c,alpha_c,delta_t,thresh_c) {
  m_t_c<-nrow(X_test_c)
  while (delta_t>thresh_c) {
    J_hist_t_c<-sum(((X_test_c%*%theta_t_c)-y_test_c)^2)/(2*m_t_c)
    theta_t_c<-theta_t_c - (alpha_c * (1/m_t_c)* (t(X_test_c)%*%((X_test_c%*%theta_t_c) - y_test_c)))
    J_cur_t_c<-cost_t_c(X_test_c,y_test_c,theta_t_c)
    delta_t=(J_hist_t_c - J_cur_t_c)/J_hist_t_c
  }
  result_t_c<-list(J_cur_t_c,alpha_c,theta_t_c)
  return(result_t_c)
}

#passing parameters initial for random features
alpha_c<-0.0085
delta<-100
delta_t<-100
thresh_c<-0.0001
thresh_c

#function loaded into results matrix
results_c<-grad_descent_c(X_train_c,y_train_c,theta_c,alpha_c,delta,thresh_c)
results_t_c<-grad_descent_c(X_test_c,y_test_c,theta_t_c,alpha_c,delta_t,thresh_c)

#displaying results for random features
results_c
results_t_c

