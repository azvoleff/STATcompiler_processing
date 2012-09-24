# Use to predict rate of natural population increase (RNI, as a percentage) 
# using only total fertility rate. Uses the method devised by David Carr.

pop_data <- read.csv("Observed_RNI.csv")

RNI_model <- lm(RNI ~ log(TFR), data=pop_data)

# Make predictions for RNI using the model
RNI_pred <- predict(RNI_model)

# Calculate the doubling time as log(2) / log(1+r/100)
Td_pred <- log(2) / log(1 + (RNI_pred / 100))

pop_data <- cbind(pop_data, RNI_pred, Td_Pred)

write.csv(pop_data, file="Predicted_RNI.csv")
