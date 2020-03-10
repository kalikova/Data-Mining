install.packages("foreign")
library("foreign")

data <- read.arff("~/Desktop/CS699_Final_Project_Kakimzhanova_Kalikova/datasets/initial_kidney_transplantation_dataset.arff")
head(data)

############################# Data Preprecessing/Cleaning #####################################

# return columns that contain NA values
na_columns <- colnames(data)[colSums(is.na(data)) > 0]
na_columns

# 12
nrow(data) # 3439 
data <- subset(data, !is.na(data$EBV_D) )
nrow(data) # 3042


#1
b <- data$BMI_R
for (i in 1:length(b)) {
  if (is.na(b[i])) {
    if (data$SEX_R[i] == "F") {
      if (data$TRANSP_AGE_R[i] <= 30) {
        b[i] <- mean(b[(data$SEX_R == "F") & (data$TRANSP_AGE_R <= 30)], na.rm=TRUE)
      } 
      else if (data$TRANSP_AGE_R[i] > 30 & data$TRANSP_AGE_R [i] <= 60) {
        b[i] <- mean(b[(data$SEX_R == "F") & (data$TRANSP_AGE_R > 30 & data$TRANSP_AGE_R <= 60)], na.rm=TRUE)
      }
      else {
        b[i] <- mean(b[(data$SEX_R == "F") & (data$TRANSP_AGE_R > 60)], na.rm=TRUE)
      }
    }
    else if (data$SEX_R[i] == "M") {
      if (data$TRANSP_AGE_R[i] <= 30) {
        b[i] <- mean(b[(data$SEX_R == "M") & (data$TRANSP_AGE_R <= 30)], na.rm=TRUE)
      } 
      else if (data$TRANSP_AGE_R[i] > 30 & data$TRANSP_AGE_R [i] <= 60) {
        b[i] <- mean(b[(data$SEX_R == "M") & (data$TRANSP_AGE_R > 30 & data$TRANSP_AGE_R <= 60)], na.rm=TRUE)
      }
      else {
        b[i] <- mean(b[(data$SEX_R == "M") & (data$TRANSP_AGE_R > 60)], na.rm=TRUE)
      }
    }
  }
}
data$BMI_R <- b
table(is.na(data$BMI_R))

#2
data$TIME_REG_TRANSP[is.na(data$TIME_REG_TRANSP) == TRUE] <- mean(data$TIME_REG_TRANSP, na.rm = TRUE)
table(is.na(data$TIME_REG_TRANSP))

#3
data$CIT_HOUR[is.na(data$CIT_HOUR) == TRUE] <- mean(data$CIT_HOUR, na.rm = TRUE)
table(is.na(data$CIT_HOUR))

#4
data$INIT_DISEASE[is.na(data$INIT_DISEASE) == TRUE] <- 
  unique(data$INIT_DISEASE)[which.max(tabulate(match(data$INIT_DISEASE, unique(data$INIT_DISEASE))))]
table(is.na(data$INIT_DISEASE))

#5
data$TECH_DIAL[is.na(data$TECH_DIAL) == TRUE] <- 
  unique(data$TECH_DIAL)[which.max(tabulate(match(data$TECH_DIAL, unique(data$TECH_DIAL))))]
table(is.na(data$TECH_DIAL))


#6 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

vec1 <- data$TRANSP_RANK[(data$TRANSP_RANK == 1 | data$TRANSP_RANK == 2)]
mode1 <- getmode(vec1)
data$INCOMP_ABDR[is.na(data$INCOMP_ABDR) == TRUE & (data$TRANSP_RANK == 1 | data$TRANSP_RANK == 2)] <- mode1

vec2 <- data$TRANSP_RANK[(data$TRANSP_RANK == 3 | data$TRANSP_RANK == 4)]
mode2 <- getmode(vec2)
data$INCOMP_ABDR[is.na(data$INCOMP_ABDR) == TRUE & (data$TRANSP_RANK == 3 | data$TRANSP_RANK == 4)] <- mode2

vec3 <- data$TRANSP_RANK[(data$TRANSP_RANK == 5 | data$TRANSP_RANK == 6)]
mode3 <- getmode(vec3)
data$INCOMP_ABDR[is.na(data$INCOMP_ABDR) == TRUE & (data$TRANSP_RANK == 5 | data$TRANSP_RANK == 6)] <- mode3
# check
data$INCOMP_ABDR[is.na(data$INCOMP_ABDR) == TRUE]

#7
data$AGE_D[is.na(data$AGE_D) == TRUE  & (data$SEX_D == "F")] <- 
  as.integer(mean(data$AGE_D[(data$SEX_D == "F")], na.rm=TRUE))

data$AGE_D[is.na(data$AGE_D) == TRUE  & (data$SEX_D == "M")] <- 
  as.integer(mean(data$AGE_D[(data$SEX_D == "M")], na.rm=TRUE))
table(is.na(data$AGE_D))

#8
c <- data$CREAT_D
for (i in 1:length(c)) {
  if (is.na(c[i])) {
    if (data$SEX_D[i] == "F") {
      if (data$AGE_D[i] <= 30) {
        c[i] <- mean(c[(data$SEX_D == "F") & (data$AGE_D <= 30)], na.rm=TRUE)
      } 
      else if (data$AGE_D[i] > 30 & data$AGE_D [i] <= 60) {
        c[i] <- mean(c[(data$SEX_D == "F") & (data$AGE_D > 30 & data$AGE_D <= 60)], na.rm=TRUE)
      }
      else {
        c[i] <- mean(c[(data$SEX_D == "F") & (data$AGE_D > 60)], na.rm=TRUE)
      }
    }
    else if (data$SEX_D[i] == "M") {
      if (data$AGE_D[i] <= 30) {
        c[i] <- mean(c[(data$SEX_D == "M") & (data$AGE_D <= 30)], na.rm=TRUE)
      } 
      else if (data$AGE_D[i] > 30 & data$AGE_D [i] <= 60) {
        c[i] <- mean(c[(data$SEX_D == "M") & (data$AGE_D > 30 & data$AGE_D <= 60)], na.rm=TRUE)
      }
      else {
        c[i] <- mean(c[(data$SEX_D == "M") & (data$AGE_D > 60)], na.rm=TRUE)
      }
    }
  }
}
data$CREAT_D <- as.integer(c)
table(is.na(data$CREAT_D))

#9
mean(data$TIME_TRANSP_DEATH)

r <- data$RELAT_DR
for (i in 1:length(r)) {
  if (is.na(r[i])) {
    if (data$TIME_TRANSP_DEATH[i] < mean(data$TIME_TRANSP_DEATH)) {
      r[i] <- unique(r[data$TIME_TRANSP_DEATH <= mean(data$TIME_TRANSP_DEATH)], na.rm=TRUE)[which.max(tabulate(match(r, unique(r))))]
    }
    else {
      r[i] <- unique(r[data$TIME_TRANSP_DEATH > mean(data$TIME_TRANSP_DEATH, na.rm=TRUE)])[which.max(tabulate(match(r, unique(r))))]
    }
  }
}
data$RELAT_DR <- r
table(is.na(data$RELAT_DR))

#10
d <- data$CAUSE_DEATH_D
for (i in 1:length(d)) {
  if (is.na(d[i])) {
    if (data$SEX_D[i] == "F") {
      if (data$AGE_D[i] <= 30) {
        d[i]<- unique(d[(data$SEX_D == "F") & (data$AGE_D <= 30)], na.rm=TRUE)[which.max(tabulate(match(d, unique(d))))]
      } 
      else if (data$AGE_D[i] > 30 & data$AGE_D [i] <= 60) {
        d[i]<- unique(d[(data$SEX_D == "F") & (data$AGE_D > 30 & data$AGE_D <= 60)], na.rm=TRUE)[which.max(tabulate(match(d, unique(d))))]
      }
      else {
        d[i]<- unique(d[(data$SEX_D == "F") & (data$AGE_D > 60)], na.rm=TRUE)[which.max(tabulate(match(d, unique(d))))]
      }
    }
    else if (data$SEX_D[i] == "M") {
      if (data$AGE_D[i] <= 30) {
        d[i]<- unique(d[(data$SEX_D == "M") & (data$AGE_D <= 30)], na.rm=TRUE)[which.max(tabulate(match(d, unique(d))))]
      } 
      else if (data$AGE_D[i] > 30 & data$AGE_D [i] <= 60) {
        d[i]<- unique(d[(data$SEX_D == "M") & (data$AGE_D > 30 & data$AGE_D <= 60)], na.rm=TRUE)[which.max(tabulate(match(d, unique(d))))]
      }
      else {
        d[i]<- unique(d[(data$SEX_D == "M") & (data$AGE_D > 60)], na.rm=TRUE)[which.max(tabulate(match(d, unique(d))))]
      }
    }
  }
}
data$CAUSE_DEATH_D <- d
table(is.na(data$CAUSE_DEATH_D))

#11
#converting blanks to NA
data$CMV_D <- sapply(data$CMV_D, function(f){is.na(f)<-which(f == '');f})

#replacing missing values
d <- data$CMV_D
for (i in 1:length(d)) {
  if (is.na(d[i])) {
    if (data$SEX_D[i] == "F") {
      if (data$AGE_D[i] <= 30) {
        d[i]<- unique(d[(data$SEX_D == "F") & (data$AGE_D <= 30)], na.rm=TRUE)[which.max(tabulate(match(d, unique(d))))]
      } 
      else if (data$AGE_D[i] > 30 & data$AGE_D [i] <= 60) {
        d[i]<- unique(d[(data$SEX_D == "F") & (data$AGE_D > 30 & data$AGE_D <= 60)], na.rm=TRUE)[which.max(tabulate(match(d, unique(d))))]
      }
      else {
        d[i]<- unique(d[(data$SEX_D == "F") & (data$AGE_D > 60)], na.rm=TRUE)[which.max(tabulate(match(d, unique(d))))]
      }
    }
    else if (data$SEX_D[i] == "M") {
      if (data$AGE_D[i] <= 30) {
        d[i]<- unique(d[(data$SEX_D == "M") & (data$AGE_D <= 30)], na.rm=TRUE)[which.max(tabulate(match(d, unique(d))))]
      } 
      else if (data$AGE_D[i] > 30 & data$AGE_D [i] <= 60) {
        d[i]<- unique(d[(data$SEX_D == "M") & (data$AGE_D > 30 & data$AGE_D <= 60)], na.rm=TRUE)[which.max(tabulate(match(d, unique(d))))]
      }
      else {
        d[i]<- unique(d[(data$SEX_D == "M") & (data$AGE_D > 60)], na.rm=TRUE)[which.max(tabulate(match(d, unique(d))))]
      }
    }
  }
}
data$CMV_D <- d
table(is.na(data$CMV_D))


############################# Saving cleaned data into a file #####################################
# Checking for columns that contain NA values
na_columns <- colnames(data)[colSums(is.na(data)) > 0]
na_columns

nrow(data)

dataset <- data[-c(1)]
dataset

# checking for duplicates after removing ID
dataset[duplicated(dataset)]

#write.arff(dataset, file = "~/Desktop/CS699_Final_Project_Kakimzhanova_Kalikova/datasets/cleaned_kidney_transplantation_dataset.arff")

cleaned_data <- read.arff("~/Desktop/CS699_Final_Project_Kakimzhanova_Kalikova/datasets/cleaned_kidney_transplantation_dataset.arff")
cleaned_data 

cleaned_data[duplicated(cleaned_data) ==TRUE] # no duplicate intances

nrow(cleaned_data)


######################### Balancing unbalanced dataset ##################################

install.packages("ROSE")
library ('ROSE')

training_unbalanced <- read.arff("~/Desktop/CS699_Final_Project_Kakimzhanova_Kalikova/datasets/cleaned_training_kidney_transplantation_dataset.arff")
table(training_unbalanced$DEATH_FUNCTGRAFT)

#undersampling
training_balanced_under <- ovun.sample(DEATH_FUNCTGRAFT~., data = training_unbalanced,
                                       N = nrow(training_unbalanced),
                                       method = "under", seed = 3)$data
table(training_balanced_under$DEATH_FUNCTGRAFT)

#oversampling
training_balanced_over <- ovun.sample(DEATH_FUNCTGRAFT~., data = training_unbalanced,
                                      N = nrow(training_unbalanced),
                                      method = "over", seed = 3)$data

table(training_balanced_over$DEATH_FUNCTGRAFT)

#hybrid sampling

training_balanced_hybrid <- ovun.sample(DEATH_FUNCTGRAFT~., data = training_unbalanced,
                                        N = nrow(training_unbalanced),
                                        method = "both", seed = 10)$data

table(training_balanced_hybrid$DEATH_FUNCTGRAFT)

#write.arff(training_balanced_hybrid, file="~/Desktop/CS699_Final_Project_Kakimzhanova_Kalikova/datasets/cleaned_training_balanced_kidney_transplantation_dataset.arff")

training_balanced <- read.arff("~/Desktop/CS699_Final_Project_Kakimzhanova_Kalikova/datasets/cleaned_training_balanced_kidney_transplantation_dataset.arff")
table(training_balanced$DEATH_FUNCTGRAFT)
