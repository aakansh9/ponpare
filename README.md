---
Title: "Kaggle Coupon Purchase Prediction Challenge"
Author: "Aakansh Gupta"
Date: "October 5, 2015"
---

This repo contains code for the [Recruit Ponpare Coupon Prediction Challenge](https://www.kaggle.com/c/coupon-purchase-prediction/). It gives map@10 score 0.0072 on Private Leaderboard.


This was executed and tested on r3.8xlarge ami on AWS. Execution requires R 3.2.2 along with package xgboost.

To run do the following:

1. [Download the data](https://www.kaggle.com/c/coupon-purchase-prediction/data)
2. Download the repo in the same folder.
3. Install packages as required in RunMe.R
4. Modify paths and run RunMe.R
5. A submission folder is then generated containing the submission.

Details about the implementation can be read in this blog post: http://datascience.blog.uhuru.co.jp/machine-learning/predicting-coupon-purchases-on-ponpare/

