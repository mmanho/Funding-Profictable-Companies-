# Funding Companies With High Growth Potential

The primary objective of this project is about finding correlations between companies that successfully make the jump from Series A to a profitable exit (IPO, M&A). In doing so, this could ultimately help Latimer ventures in deliberating Investment decisions by finding out the start-up companies with high growth potential that are led by BIPOC founders and investing in them. 

The dataset we worked on consists of investment and financial data of 10000 institutions including Venture Capital, Private Equity, IPO and M&A firms, which exitted from January 20th, 2010 to July 1st, 2022, and are predominantly operating in the United states.

We experimented with several models - logistic regression, bagging, boosting, random forest and xgboost. Out of these models, the random forest model gave the highest accuracy (80.4%) in predicting whether a company is successful or not. 'Total Raised’, ‘Year Founded’ and ‘AvgWordLenDescription’ (average word length in a company’s description) are useful variables for predicting success as measured by the random forest model.
