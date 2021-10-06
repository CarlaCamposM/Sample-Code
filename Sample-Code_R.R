library(ISLR)
RNGkind(sample.kind="Rounding")
options(digits=3)
library(tidyverse)
library(leaps)
library(GGally)
require(car)
library(glmnet)
library(class)
library(MASS)
library(ggplot2)
library(ggfortify)
library(GGally)
library(factoextra)
library(cowplot)
library(dplyr)

wiki = read.csv("wiki4HE.csv", header=T, sep=";", na.strings="?")
str(wiki)

library(writexl)
write_xlsx(x = wiki, path = "wiki.xlsx", col_names = TRUE)

#remove position variables
wiki.revised = wiki[,-c(7:9)]
head(wiki.revised)

#impute DOMAIN with mode=6
summary(wiki.revised$DOMAIN)
wiki.revised$DOMAIN[is.na(wiki.revised$DOMAIN)] = 6
summary(wiki.revised$DOMAIN)

#impute USERWIKI with mode mode=0
summary(wiki.revised$USERWIKI)
wiki.revised$USERWIKI[is.na(wiki.revised$USERWIKI)] = 0
summary(wiki.revised$USERWIKI)

#impute continuous variables with median
library(caret)
wiki.imp = predict(preProcess(wiki.revised, method = 'medianImpute'), newdata = wiki.revised)
str(wiki.imp)
write_xlsx(x = wiki.imp, path = "wiki_imp.xlsx", col_names = TRUE)
summary(wiki.imp)

#created copy just to be safe
wiki.clean = wiki.imp
#code UNIVERSITY as factors 1=UOC 2=UPF 
wiki.clean$UNIVERSITY = factor(wiki.clean$UNIVERSITY, levels = c(1,2), labels = c("UOC","UPF"))
str(wiki.clean)
#code DOMAIN as factors
wiki.clean$DOMAIN = factor(wiki.clean$DOMAIN, levels = c(1,2,3,4,5,6), 
                           labels = c("Arts & Humanities","Sciences","Health Sciences","Engineering & Architecture",
                                      "Law & Politics", "Social Sciences"))
str(wiki.clean)
#code GENDER as factors: 0=Male; 1=Female 
wiki.clean$GENDER = factor(wiki.clean$GENDER, levels = c(0,1), labels = c("Male","Female"))

#code PhD as factor: 0=No; 1=Yes 
wiki.clean$PhD = factor(wiki.clean$PhD, levels = c(0,1), labels = c("No PhD","Has PhD"))

#code USERWIKI as factor: 0=No; 1=Yes 
wiki.clean$USERWIKI = factor(wiki.clean$USERWIKI, levels = c(0,1), labels = c("No","Yes"))
str(wiki.clean)

### Concatenate UNI and Domain for stratified split
wiki.clean$UNI_DOMAIN = paste0(wiki.clean$UNIVERSITY, wiki.clean$DOMAIN)
str(wiki.clean)

#code USE3 into binary with 1,2=no and 3,4,5=yes
wiki.clean$response = factor(wiki.clean$Use3, levels = c(1,2,3,4,5), labels = 
                               c("No","No","Yes","Yes","Yes"))
str(wiki.clean)

#code USE3 into binary with 1,2,3=no and 4,5=yes
wiki.clean$response2 = factor(wiki.clean$Use3, levels = c(1,2,3,4,5), labels = 
                                c("No","No","No","Yes","Yes"))
str(wiki.clean)

#create copy just to be safe
wiki2 = wiki.clean
str(wiki2)

#create stratified split so Uni and Domain remain representative 
set.seed(1)

inTrainingSet = createDataPartition(y = wiki2$UNI_DOMAIN, p = .70, list = FALSE)
train = wiki2[inTrainingSet,]
test = wiki2[-inTrainingSet,]

#made sure the split worked
summary(train)
table(train$DOMAIN, train$UNIVERSITY)

#select only Likert-scale predictor variables in TRAINING set for PCA analysis
head(train[,c(8:29, 35:50)])
train.survey = train[,c(8:29, 35:50)]
str(train.survey)

#summary histogram just to get feel of survey responses
train.survey %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins=9)

#PCA
pr.out2=prcomp(train.survey, scale=TRUE)
summary(pr.out2)
pve=100*pr.out2$sdev^2/sum(pr.out2$sdev^2)

#scree plot
par(mfrow=c(1,2))
plot(pve, type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")
pr.out <- prcomp(train.survey, scale = TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation[,1:5]
dim(pr.out$x)
biplot(pr.out, scale=0, cex=0.5, expand=2)

#better biplot
autoplot(pr.out, x = 1, y = 2, data = train.survey, color = "gray30", fill = "cyan", shape = 23,
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3,
         loadings.colour = "black", loadings.label.colour = "black",
         loadings.label.repel = TRUE) +
  labs(title = "Biplot of PC2 vs PC1")

#group survey questions into categories
Categories <- c(rep.int("Perceived Usefulness", 3), rep.int("Perceived Ease of Use", 3), rep.int("Perceived 
Enjoyment", 2),
                rep.int("Quality", 5), rep.int("Visibility", 3), rep.int("Social Image", 3), rep.int("Sharing attitude", 
                                                                                                    3),
                rep.int("Profile 2.0", 3), rep.int("Job relevance", 2), rep.int("Behavioral intention", 2),
                rep.int("Incentives", 4), rep.int("Experience", 5))
new.contrib.cat <- data.frame(cbind(pr.out$rotation, Categories))

#biplot with survey question groups
fviz_pca_var(pr.out, labels = FALSE, habillage = new.contrib.cat$Categories)

#top 20 contributors just for perspective
fviz_pca_var(pr.out, labels = FALSE, habillage = new.contrib.cat$Categories, select.var = list(contrib = 
                                                                                                 20))
#plot for PC1 and PC3
fviz_pca_var(pr.out, axes = c(1, 3), labels = FALSE, habillage = new.contrib.cat$Categories)
fviz_pca_var(pr.out, axes = c(1, 3), labels = FALSE, habillage = new.contrib.cat$Categories, select.var = 
               list(contrib = 20))
#plot for PC2 and PC3
fviz_pca_var(pr.out, axes = c(2, 3), labels = FALSE, habillage = new.contrib.cat$Categories)
fviz_pca_var(pr.out, axes = c(2, 3), labels = FALSE, habillage = new.contrib.cat$Categories, select.var = 
               list(contrib = 20))
#plot for PC1 and PC4
fviz_pca_var(pr.out, axes = c(1, 4), labels = FALSE, habillage = new.contrib.cat$Categories)
fviz_pca_var(pr.out, axes = c(1, 4), labels = FALSE, habillage = new.contrib.cat$Categories, select.var = 
               list(contrib = 20))

#plot for PC2 and PC3
#create train survey dataset with PC1-PC5 and dems
train.PCA.X <- data.frame(pr.out$x[,1:5])
str(train.PCA.X)
train.survey.comb <- cbind(train[,c(1:7, 52:53)], train.PCA.X)
str(train.survey.comb)

#create test survey data set
str(test[,c(8:29, 35:50)])
test.survey.raw = test[,c(8:29, 35:50)]
str(test.survey.raw)

#extend PCA to testing set
test.survey.trans <- data.frame(predict(pr.out, test.survey.raw))
str(test.survey.trans)

#add demographic variables to testing set
test.survey.comb <- cbind(test[,c(1:7, 52:53)], test.survey.trans[,1:5])
str(test.survey.comb)

#logistic regression FULL model with all predictors
glm.fit = glm(response~AGE+GENDER+DOMAIN+PhD+YEARSEXP+UNIVERSITY+USERWIKI+PC1+PC2+PC3, 
              data=train.survey.comb, family=binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, test.survey.comb, type="response")
glm.pred = rep("No", length(glm.probs))

glm.pred[glm.probs>.5] = "Yes"
table(glm.pred, test.survey.comb$response)
mean(glm.pred == test.survey.comb$response)

#logistic regression less predictors
glm.fit = glm(response~GENDER+DOMAIN+YEARSEXP+USERWIKI+PC1+PC2+PC3, 
              data=train.survey.comb, family=binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, test.survey.comb, type="response")
glm.pred = rep("No", length(glm.probs))
glm.pred[glm.probs>.5] = "Yes"
table(glm.pred, test.survey.comb$response) 
mean(glm.pred == test.survey.comb$response)

#logistic regression PC1 PC2 PC3
glm.fit = glm(response~ PC1+PC2+PC3, data=train.survey.comb, family=binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, test.survey.comb, type="response")
glm.pred = rep("No", length(glm.probs))
glm.pred[glm.probs>.5] = "Yes"
table(glm.pred, test.survey.comb$response)
mean(glm.pred == test.survey.comb$response)

#logistic regression PC1 PC2
glm.fit = glm(response~PC1+PC2, data=train.survey.comb, family=binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, test.survey.comb, type="response")
glm.pred = rep("No", length(glm.probs))
glm.pred[glm.probs>.5] = "Yes"
table(glm.pred, test.survey.comb$response)
mean(glm.pred == test.survey.comb$response)

#logistic regression DOMAIN PC1 PC2
glm.fit = glm(response~DOMAIN+PC1+PC2, data=train.survey.comb, family=binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, test.survey.comb, type="response")
glm.pred = rep("No", length(glm.probs))
glm.pred[glm.probs>.5] = "Yes"
table(glm.pred, test.survey.comb$response) 
mean(glm.pred == test.survey.comb$response)

#logistic regression GENDER PC1 PC2
glm.fit = glm(response~GENDER+PC1+PC2, data=train.survey.comb, family=binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, test.survey.comb, type="response")
glm.pred = rep("No", length(glm.probs))
glm.pred[glm.probs>.5] = "Yes"
table(glm.pred, test.survey.comb$response)

mean(glm.pred == test.survey.comb$response)

#LDA
library(MASS)
lda.fit = lda(response~GENDER+PC1+PC2, data=train.survey.comb)
lda.fit
lda.pred = predict(lda.fit, test.survey.comb)
table(lda.pred$class, test.survey.comb$response)
mean(lda.pred$class == test.survey.comb$response)
plot(lda.fit)

#LDA PC1 PC2
lda.fit = lda(response~PC1+PC2, data=train.survey.comb)
lda.fit
lda.pred = predict(lda.fit, test.survey.comb)
table(lda.pred$class, test.survey.comb$response)
mean(lda.pred$class == test.survey.comb$response)
plot(lda.fit)

#LDA PC1 PC2 PC3
lda.fit = lda(response~PC1+PC2+PC3, data=train.survey.comb)
lda.fit
lda.pred = predict(lda.fit, test.survey.comb)
table(lda.pred$class, test.survey.comb$response)
mean(lda.pred$class == test.survey.comb$response)
plot(lda.fit)

#QDA GENDER PC1 PC2
qda.fit = qda(response~GENDER+PC1+PC2, data=train.survey.comb)
qda.fit
qda.class = predict(qda.fit, test.survey.comb)$class
table(qda.class, test.survey.comb$response) 
mean(qda.class == test.survey.comb$response) 

#QDA PC1 PC2
qda.fit = qda(response~PC1+PC2, data=train.survey.comb)
qda.fit
qda.class = predict(qda.fit, test.survey.comb)$class
table(qda.class, test.survey.comb$response)
mean(qda.class == test.survey.comb$response)

#QDA PC1 PC2 PC3
qda.fit = qda(response~PC1+PC2+PC3, data=train.survey.comb)
qda.fit
qda.class = predict(qda.fit, test.survey.comb)$class
table(qda.class, test.survey.comb$response)
mean(qda.class == test.survey.comb$response)

#KNN
library(class)
train.X = as.matrix(train.survey.comb[,10:12])
test.X = as.matrix(test.survey.comb[,10:12])
train.Use3 = train.survey.comb$response
set.seed(1)
knn.pred = knn(train.X, test.X, train.Use3, k=1)
table(knn.pred, test.survey.comb$response) 
mean(knn.pred == test.survey.comb$response) 

#KNN
library(class)
train.X = as.matrix(scale(train.survey.comb[,10:11]))
test.X = as.matrix(scale(test.survey.comb[,10:11]))
train.Use3 = train.survey.comb$response
set.seed(1)
knn.pred = knn(train.X, test.X, train.Use3, k=3)
table(knn.pred, test.survey.comb$response)
mean(knn.pred == test.survey.comb$response)

## KNN with loop
errors <- c()
for(i in seq(1,100,1)){
  set.seed(1)
  knn.pred <- knn(train.X,test.X,train.Use3,k=i)
  table(knn.pred, test.survey.comb$response)
  errors <- c(1-mean(knn.pred == test.survey.comb$response),errors)
} 
results <- cbind(seq(1,100,1),errors)
plot(results,type="l",xlab="k")
which.min(errors) #k=23
knn.pred <- knn(train.X,test.X,train.Use3,k=which.min(errors))
table(knn.pred, test.survey.comb$response)
mean(knn.pred == test.survey.comb$response)