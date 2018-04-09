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
r <- evaluate(e, method = "UBCF", nMatrix="Nuggets_ML100K.dat", type = "topNList", subtype="Novelty", n = 10, param = list(method = "cosine", nn = 50))

avg(r)
      a_NDCG  ab_NDCG  ag_NDCG  abg_NDCG   TOT_DIV abg_TOT_DIV
10 0.8905016 0.616119 0.724511 0.4497598 0.1370402  0.06163856
```

### References
* recommenderlab [reference manual](https://cran.r-project.org/web/packages/recommenderlab/recommenderlab.pdf)
* MovieLens 100K [Dataset](https://grouplens.org/datasets/movielens/100k/)
