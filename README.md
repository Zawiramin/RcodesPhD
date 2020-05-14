# RcodesPhD
[Date initiating readMe: 12 May 2020, 1:10PM]
This Repo is a dedication of my PhD works. Hopefully I can make this repo available for others to benefit a little from it if not all.(May12,2020;117PM)

(May14,2020;7:50AM)
My approach in each models:
Build cm with no xval
1. using rpart to build DT model with no xval using train data
2. build a confusion matrix (cm) using test data:
-step1: predict
-step2: mutate the predicted data
-step3: combine both predicted and mutate vectors
Build cm with xval
1. get the smallest cp from rpat 1
2. pass the min(cp) inside the new rpart with 10xval using train data
3. build cm using test data:
-step1: predict
-step2: mutate
-step3: combine