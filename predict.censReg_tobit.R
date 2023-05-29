######################################################################
# This function requires the packages censReg and plm to be loaded.
#####################################################################

######################################################################
# Write the function
#####################################################################

predict.censReg_tobit <- function(model, data) {
  # 1) Write a function for the inverse Mills Ratio
  lambda <- function(x) {
    lambda <- dnorm(x)/pnorm(x)
    lambda
  }
  # 2) Calculate the predicted latent variable and store the standard deviation (sd) estimate  
  # The output differs depending on whether a random effects model was estimated or not
  # Therefore we need to check the rownames of the output first
  if(!is.element("logSigmaMu", rownames(summary(model)$estimate))){ # Was a random effects model (not) estimated?
    # If logSigmaMu is not contained, model is a cross-sectional/ pooled regression
    # The last estimate contains the log-sd of the error term logSigma
    # In order to store the coefficients only, we therefore need to discard the last row 
    coefficients <- summary(model)$estimate[1:(length(summary(model)$estimate[,1])-1),1]
    namesCoef <- names(coefficients)
    # Which columns in the original data contain our covariates?
    indices <- unlist(lapply(namesCoef[-1], function(x) which(colnames(data)==x)))
    # Check if all variables are contained in the data set and predict the latent variable
    if (length(indices)==(length(coefficients)-1)) { # Check if all variables are contained in the data set
      # If so: store the data that shall be used for prediction in data_pred 
      data_pred <- cbind(rep(1, nrow(data)), do.call(cbind, lapply(indices, function(x) data[,x])))
      # and multiply it with the coefficients to get predictions of the latent variable 
      latent_var_pred <- (as.matrix(data_pred) %*% coefficients)[,1]
      # store the sd estimate 
      sigma <- exp(summary(model)$estimate[nrow(summary(model)$estimate),1]) 
        } else {
      # otherwise show a warning message
      warning("The dataset does not contain some of the variables.") 
    }
    # 3) Return the predicted values
      # Predict the dependent variable in the testing_set with the trained model
      p0 <- pnorm(latent_var_pred/sigma) # probability of a non-zero observation
      ey0 <- latent_var_pred + sigma*lambda(latent_var_pred/sigma) # conditional expectation of the censored y given that it is non-zero
      predicted <- p0*ey0 # unconditional expectation
      predicted
        } else {
          # LogSigmaNu is the log-sd of the remaining disturbance (that is not attributed to the individual specific effects)
          # If a random effects model was estimated, the the last two estimates contain the sd of the individual specific effects and the remaining disturbance
          # In order to store the coefficients only, we therefore need to discard the last two rows 
          coefficients <- summary(model)$estimate[1:(length(summary(model)$estimate[,1])-2),1]
          namesCoef <- names(coefficients)
          # Which columns in the original data contain our covariates?
          indices <- unlist(lapply(namesCoef[-1], function(x) which(colnames(data)==x)))
          # In caase of ":" , we have to create a new variable by multiplying the two variables to the left and to the right of ":"
          nbefore <- ncol(data)
          for(i in 1:length(namesCoef)){
            if(grepl(":", namesCoef[i])){
              data[paste0(sub(":.*","", namesCoef[i]), sub(".*:","", namesCoef[i]))] <- data[sub(":.*","", namesCoef[i])]*data[sub(".*:","", namesCoef[i])]
            }
          }
          # And in this case the indices vector has to be extended to these variables
          if(ncol(data) > nbefore){
            indices <- c(indices, (nbefore+1):ncol(data))   
          } 
          # Check if all variables are contained in the data set and predict the latent variable
          if (length(indices)==(length(coefficients)-1)) { # Check if all variables are contained in the data set
            # If yes: store the data that shall be used for prediction in data_pred 
            data_pred <- cbind(rep(1, nrow(data)), do.call(cbind, lapply(indices, function(x) data[,x])))
            # and multiply it with the coefficients to get predictions of the latent variable 
            latent_var_pred <- (as.matrix(data_pred) %*% coefficients)[,1]
            # store the sd estimate 
            # (in this case: of the remaining sd, i.e. exp(logSigmaNu)) 
            sigma <- exp(summary(model)$estimate[nrow(summary(model)$estimate),1]) 
        } else {
          # otherwise show a warning message
          warning("The dataset does not contain some of the variables.") 
        }
        # 3) Return the predicted values 
          # Predict the dependent variable in the testing_set with the trained model
          p0 <- pnorm(latent_var_pred/sigma) # probability of a non-zero observation
          # lambda <- function(x) dnorm(x)/pnorm(x) # inverse Mills ratio
          ey0 <- latent_var_pred + sigma*lambda(latent_var_pred/sigma) # conditional expectation of the censored y given that it is non-zero
          predicted <- p0*ey0 # unconditional expectation
          predicted
          }}


