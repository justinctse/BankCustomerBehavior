# Predicting Bank Customer Behavior and Satisfaction
This was an independent project based on data provided by Santander bank. The dataset consisted of demographic and transactional behavior of customers over a 17-month period. I used this data to predict which products customers would buy in a certain timeframe and also to create a measure for customer satisfaction.

The models used were association rule learning, logistic regression, random forests, gradient tree boosting, and neural networks. The main challenge of this project was coming up with ways to deal with heavily imbalanced classes (<1%). Class rebalancing via over-sampling or under-sampling proved to be an efficent way to improve results.

___

Folders: 

- CustomerBehaviorAnalysis - *Contains models and visualizations for analyzing which products customers will purchase in the future.*
- CustomerSatisfactionAnalysis - *Contains models and visualizations for analyzing customer satisfaction.*
- DataPreparation - *Contains scripts used to clean the data and to create new features.*
