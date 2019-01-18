#create safelog function
safelog <- function(x) {
  return (log(x + 1e-100))
}

#number of classes and number of samples
K <- 5
N <- 195

#Assign first 25 to training set for A, last 14 to test set 
#and create a label matrix

A_training_matrix <- read.csv(file = "hw01_data_set_images.csv",
                              header=FALSE,
                              nrows = 25)

A_test_matrix <- read.csv(file="hw01_data_set_images.csv",
                          header = FALSE,
                          skip=25,
                          nrows=14)

A_label_matrix <- read.csv(file = "hw01_data_set_labels.csv",
                           header = FALSE,
                           nrows = 39)

#Assign first 25 to training set for B, last 14 to test set 
#and create a label matrix

B_training_matrix <- read.csv(file="hw01_data_set_images.csv",
                              header=FALSE,
                              skip=39,
                              nrows=25)

B_test_matrix <- read.csv(file="hw01_data_set_images.csv",
                          header=FALSE,
                          skip=64,
                          nrows=14)

B_label_matrix <- read.csv(file = "hw01_data_set_labels.csv",
                           header = FALSE,
                           skip = 39,
                           nrows= 39)

#Assign first 25 to training set for B, last 14 to test set 
#and create a label matrix

C_training_matrix <- read.csv(file="hw01_data_set_images.csv",
                              header=FALSE,
                              skip=78,
                              nrows=25)

C_test_matrix <- read.csv(file="hw01_data_set_images.csv",
                          header=FALSE,
                          skip=103,
                          nrows=14)

C_label_matrix <- read.csv(file = "hw01_data_set_labels.csv",
                           header = FALSE,
                           skip = 78,
                           nrows= 39)

#Assign first 25 to training set for D, last 14 to test set 
#and create a label matrix

D_training_matrix <- read.csv(file="hw01_data_set_images.csv",
                              header=FALSE,
                              skip=117,
                              nrows=25)

D_test_matrix <- read.csv(file="hw01_data_set_images.csv",
                          header=FALSE,
                          skip=139,
                          nrows=14)

D_label_matrix <- read.csv(file = "hw01_data_set_labels.csv",
                           header = FALSE,
                           skip = 117,
                           nrows= 39)

#Assign first 25 to training set for E, last 14 to test set 
#and create a label matrix

E_training_matrix <- read.csv(file="hw01_data_set_images.csv",
                              header=FALSE,
                              skip=156,
                              nrows=25)

E_test_matrix <- read.csv(file="hw01_data_set_images.csv",
                          header=FALSE,
                          skip=181,
                          nrows=14)

E_label_matrix <- read.csv(file = "hw01_data_set_labels.csv",
                           header = FALSE,
                           skip = 156,
                           nrows= 39)

#create the training set and the test set

training_set <- rbind(A_training_matrix,B_training_matrix,
                      C_training_matrix,D_training_matrix,
                      E_training_matrix)

test_set <- rbind(A_test_matrix,B_test_matrix,
                  C_test_matrix,D_test_matrix,E_test_matrix)

#calculate parameter estimations

label_matrix <- read.csv("hw01_data_set_labels.csv",header = FALSE)
label_y <- label_matrix$V1

p_yisA <- nrow(A_label_matrix)/nrow(label_matrix)
p_yisB <- nrow(A_label_matrix)/nrow(label_matrix)
p_yisC <- nrow(A_label_matrix)/nrow(label_matrix)
p_yisD <- nrow(A_label_matrix)/nrow(label_matrix)
p_yisE <- nrow(A_label_matrix)/nrow(label_matrix)

#calculate sample means for A

A_sample_total <- matrix(0,nrow=1,ncol=320)
for(i in 1:320){
  for(a in 1:25){
    A_sample_total[1,i] <- A_sample_total[1,i] + A_training_matrix[a,i]
  }
}


A_sample_mean <- matrix(0,nrow=1,ncol=320)
for(i in 1:320){
  for(a in 1:25){
    A_sample_mean[1,i] = A_sample_total[1,i] / 25
  }
}

#calculate sample mean for B

B_sample_total <- matrix(0,nrow=1,ncol=320)
for(i in 1:320){
  for(a in 1:25){
    B_sample_total[1,i] <- B_sample_total[1,i] + B_training_matrix[a,i]
  }
}


B_sample_mean <- matrix(0,nrow=1,ncol=320)
for(i in 1:320){
  for(a in 1:25){
    B_sample_mean[1,i] = B_sample_total[1,i] / 25
  }
}

#calculate sample mean for C

C_sample_total <- matrix(0,nrow=1,ncol=320)
for(i in 1:320){
  for(a in 1:25){
    C_sample_total[1,i] <- C_sample_total[1,i] + C_training_matrix[a,i]
  }
}


C_sample_mean <- matrix(0,nrow=1,ncol=320)
for(i in 1:320){
  for(a in 1:25){
    C_sample_mean[1,i] = C_sample_total[1,i] / 25
  }
}

#calculate sample mean for D

D_sample_total <- matrix(0,nrow=1,ncol=320)
for(i in 1:320){
  for(a in 1:25){
    D_sample_total[1,i] <- D_sample_total[1,i] + D_training_matrix[a,i]
  }
}


D_sample_mean <- matrix(0,nrow=1,ncol=320)
for(i in 1:320){
  for(a in 1:25){
    D_sample_mean[1,i] = D_sample_total[1,i] / 25
  }
}

#calculate sample mean for E

E_sample_total <- matrix(0,nrow=1,ncol=320)
for(i in 1:320){
  for(a in 1:25){
    E_sample_total[1,i] <- E_sample_total[1,i] + E_training_matrix[a,i]
  }
}


E_sample_mean <- matrix(0,nrow=1,ncol=320)
for(i in 1:320){
  for(a in 1:25){
    E_sample_mean[1,i] = E_sample_total[1,i] / 25
  }
}

#bind all sample means together, find pcd 

pcd<- rbind(A_sample_mean,B_sample_mean,C_sample_mean,
            D_sample_mean,E_sample_mean)


# calculate score functions for training data

score_matrix <- matrix(0,nrow=125,ncol=5)
for(i in 1:125){
  for(c in 1:5){
    for(b in 1:320){
      score_matrix[i,c] <- score_matrix[i,c] + (training_set[i,b] * safelog(pcd[c,b])) + (1-training_set[i,b]) * safelog(1-pcd[c,b])
      score_matrix[i,c] <- score_matrix[i,c] + 0.2
    }
  }
}

#get the y estimation matrix for training data

y_estimation_matrix <- matrix(0,nrow=125,ncol=1)
for(i in 1:125){
  for(r in 1:5){
    if(score_matrix[i,r] == apply(score_matrix,1,max)[i]){
      y_estimation_matrix[i] <- r
    }
  }
}

#get the y truth matrix for training data

y_truth_matrix <- matrix(0,nrow=125,ncol=1)
counter<- 1
for(i in 1:125){
  y_truth_matrix[i] <- counter
  if((i %% 25) == 0){
    counter <- counter + 1
  }
}

#get the table of estimated and truth for the training data 

confusion_matrix_training <- table(y_estimation_matrix,y_truth_matrix)
print(confusion_matrix_training)

#calculate the score matrix for test data

score_test_matrix <- matrix(0, nrow=70,ncol=5)
for(i in 1:70){
  for(c in 1:5){
    for(b in 1:320){
      score_test_matrix[i,c] <- score_test_matrix[i,c] + (test_set[i,b] * safelog(pcd[c,b])) + (1-test_set[i,b]) * safelog(1-pcd[c,b])
      score_test_matrix[i,c] <- score_test_matrix[i,c] + 0.2
    }
  }
}

#get the y estimation for test data

y_test_estimation_matrix <- matrix(0,nrow=70,ncol=1)
for(i in 1:70){
  for(r in 1:5){
    if(score_test_matrix[i,r] == apply(score_test_matrix,1,max)[i]){
      y_test_estimation_matrix[i] <- r
    }
  }
}

#get the y truth matrix for test

y_test_truth_matrix <- matrix(0,nrow=70,ncol=1)
counter<- 1
for(i in 1:70){
  y_test_truth_matrix[i] <- counter
  if((i %% 14) == 0){
    counter <- counter + 1
  }
}

#get the table of estimated and truth for test data

confusion_matrix_test <- table(y_test_estimation_matrix,y_test_truth_matrix)
print(confusion_matrix_test)
