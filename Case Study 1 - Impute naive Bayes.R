###                                              ###
#                                                  #
#  Question 3                                      #
#  used mean & sd range for imputing ABV           #
#  using naive Bayes for imputing IBU              #
#                                                  #
#  group: Will Sherman, Kati Schuerger, Randy Kim  #
#                                                  #
###                                              ###


#Combined Data Set --- change name if needed based on this ---
combined <- merge(beers_df, breweries_df, by.x="Brewery_id", by.y="Brew_ID")

#created editable dataset
combined_df <- combined
#Generalized Imputation on Mean
#calculated mean for all non-missing values & replaced NA's with mean.
combined_df$ABV <- ifelse(is.na(combined_df$ABV),
                          round(sample((mean(combined_df$ABV, na.rm=T) - sd(combined_df$ABV, na.rm=T)):
                                         (mean(combined_df$ABV, na.rm=T) + sd(combined_df$ABV, na.rm=T)),
                                       size=sum(is.na(combined_df$ABV)), replace=T), 0), combined_df$ABV)

ibu_known <- combined_df[which(!is.na(combined_df$IBU)),]
ibu_unknown <- combined_df[which(is.na(combined_df$IBU)),]

#Training nB for classifying IBU
model <- naiveBayes(IBU~., data=ibu_known)

###multiple iterations
iterations = 100
masterAcc = matrix(nrow = iterations)

for(j in 1:iterations){
  train <- ibu_known[sample(seq(1:length(ibu_known$IBU)),
                            round(.7*length(ibu_known$IBU))),]
  test <- ibu_known[-sample(seq(1:length(ibu_known$IBU)),
                            round(.7*length(ibu_known$IBU))),]
  
  pred <- predict(model, train)
  t1 <- table(factor(pred, union(pred, train$IBU)),
              factor(train$IBU, union(pred, train$IBU)))
  CM <- confusionMatrix(t1)
  masterAcc[j] = CM$overall[1]
}
colMeans(masterAcc)
var(masterAcc)

#Impute nB
imp <- predict(model, ibu_unknown)
ibu_unknown_nB <- ibu_unknown

for(i in 1:nrow(ibu_unknown_nB)){
  ibu_unknown_nB$IBU[i] <- imp[i]
}
combined_df_nB <- rbind(ibu_known,ibu_unknown_nB)
combined_df_nB <- combined_df_nB[order(combined_df_nB$Brewery_id),]