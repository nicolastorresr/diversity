# Content Novelty Measures

Load base package and dependences
```R
library('recommenderlab')

source('AAA.R')
source('calcPredictionAccuracy.R')
source('Content_Novelty.R')
source('evaluate.R')
```

Load dataset ML100K
```R
data("MovieLense")
```
Train a user-based collaborative filtering recommender
```R
e <- evaluationScheme(MovieLense, method='cross-validation', train=0.8, k=5, given=15, goodRating=4)
```

Evaluate the recommender with the Content Novelty Measures
```R
r <- evaluate(e, method='UBCF', type='ratingMatrix', n=10, param=list(method='cosine', nn=50))

avg(r)
      a_NDCG  ab_NDCG  ag_NDCG  abg_NDCG   TOT_DIV abg_TOT_DIV
10 0.8905016 0.616119 0.724511 0.4497598 0.1370402  0.06163856
```
### Note:

* In order to use another dataset (further ML100K) a binary nugget matrix is necessary. 

* The required format must be the following:

. | Nugget 1 | Nugget 2
------------ | ------------- | --
'Label item 1' | (0/1) | (0/1)
'Label item 2' | (0/1) | (0/1)

* Then, modify `<calcPredictionAccuracy.R>`and change filename up. **Nuggets Matrix is hard-coded !**
```R
# Lines 29-30
if (!file.exists("Nuggets_ML100K.dat")) stop("File of nuggets does not exist.")
nuggets <- read.table("Nuggets_ML100K.dat")
```
### References
* recommenderlab [reference manual](https://cran.r-project.org/web/packages/recommenderlab/recommenderlab.pdf)
