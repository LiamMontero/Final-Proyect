# Data Scientist (Academic Project) – MovieLens Recommendation System

## Predictive Model Development in R

I took on the challenge of developing an end-to-end movie recommendation system, using the well-known and extensive MovieLens 10M dataset, which contains 10 million user ratings. The main objective was to build a machine learning model capable of predicting with high accuracy the rating a user would give to a movie they have not yet seen. This type of system is essential for improving personalization, user retention, and content discovery on digital platforms.

My role spanned the full lifecycle of the project. I began by performing an in-depth Exploratory Data Analysis (EDA) with R and Tidyverse to understand the structure of the dataset, identify user behavior patterns, analyze the distribution of ratings, and uncover temporal biases, such as the effect of a movie's release year on its average rating.

A key technical challenge was selecting the right tool to handle the volume of data. Initially, I experimented with the recommenderlab library, but identified that its performance degraded significantly with datasets of this magnitude, making it impossible to complete training. Therefore, I made the strategic decision to migrate to the recosystem library, which is specifically optimized for parallelization and efficient handling of large data matrices. This decision was crucial to the project's viability and demonstrated my ability to diagnose bottlenecks and find efficient technical solutions. I implemented a Matrix Factorization model, a collaborative filtering technique, and used cross-validation to perform rigorous hyperparameter tuning. 

I trained the final model with the optimal combination of parameters to minimize error and evaluated it on a holdout dataset to validate its performance on unseen data. Measurable Achievements:
+ I achieved a final RMSE (Root Mean Square Error) of 0.7829 on the validation set, demonstrating the model's high predictive accuracy.
+ I successfully managed a large dataset (10 million records) by implementing a robust and efficient data pipeline in R.
+ I made an informed technical decision by replacing an inefficient library (recommenderlab) with an optimized one (recosystem), which allowed me to overcome hardware limitations and successfully complete the project.
+ I performed exploratory analysis that revealed key patterns about user behavior and data characteristics, which are critical for the development of any recommendation system.
+ I optimized model performance through hyperparameter tuning and cross-validation, ensuring that the final model was robust and generalizable.
+ I developed the entire project independently, covering everything from data cleansing and analysis to the implementation and evaluation of the machine learning model.
