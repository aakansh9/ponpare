ponpare
===============================
*ML compeitition at [Kaggle](https://www.kaggle.com/c/coupon-purchase-prediction/overview)*

---
Title: "Kaggle Coupon Purchase Prediction Challenge"
Author: "Aakansh Gupta"
First Created: "October 5, 2015"
---

This repo contains code for the [Recruit Ponpare Coupon Prediction Challenge](https://www.kaggle.com/c/coupon-purchase-prediction/). It gives map@10 score 0.0072 on Private Leaderboard.


This was executed and tested on r3.8xlarge ami on AWS. Execution requires R 3.2.2 along with the package xgboost and others as mentioned in RunME.R.

**To run do the following:**

1. [Download the data](https://www.kaggle.com/c/coupon-purchase-prediction/data)
2. Download the repo at the same location.
3. Create a new folder 'raw_data' and unzip the data files in the folder.
4. Install packages as required in RunMe.R
5. Modify paths and run RunsMe.R
6. A submission folder is then generated containing the submissions.


**NOTE on using MyMediaLite for Matrix Factorization:**

The binaries of the required version of MyMediaLite are already cloned inside the code folder in this repo. Just fulfill the requirements for running MyMediaLite:
 - Mono 2.8.x or another recent .NET runtime (Mono 2.10.x recommended)
 

Details about the implementation can be found in this blog post: http://datascience.blog.uhuru.co.jp/machine-learning/predicting-coupon-purchases-on-ponpare/

