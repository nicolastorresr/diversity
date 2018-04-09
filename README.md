# Content Novelty Measures

#### The process for evaluate a recommender (e.g., UBCF) in a dataset (e.g., MovieLens 100K) is shown below:

Load base package and dependences
```R
library('recommenderlab')

source('AAA.R')
source('calcPredictionAccuracy.R')
source('Content_Novelty.R')
source('evaluate.R')
```

Load dataset [ML100K](https://grouplens.org/datasets/movielens/100k/)
```R
data("MovieLense")
```
Train a user-based collaborative filtering recommender
```R
e <- evaluationScheme(MovieLense, method='cross-validation', train=0.8, k=5, given=15, goodRating=4)
```

Evaluate the recommender with the Content Novelty Measures
```R
> r <- evaluate(e, method = "UBCF", nMatrix="../nuggets/Nuggets_ML100K.dat", type = "topNList", subtype="Novelty", n = 10, param = list(method = "cosine", nn = 50))
```    
    UBCF run fold/sample [model time/prediction time]
	 1  Content Novelty Top - 10 : 61.37076 0.8601649 0.6124878 0.7453946 0.4627718 0.112587 0.0521021 [0.006sec/1.432sec] 
	 2  Content Novelty Top - 10 : 62.0903 0.8657405 0.6160112 0.746436 0.4636326 0.1359802 0.06304484 [0.006sec/1.402sec] 
	 3  Content Novelty Top - 10 : 62.07039 0.8782153 0.614272 0.7430954 0.4599745 0.1379396 0.06344869 [0.007sec/1.239sec] 
	 4  Content Novelty Top - 10 : 61.54092 0.8674907 0.615574 0.7381999 0.4582225 0.1407969 0.06451631 [0.006sec/1.236sec] 
	 5  Content Novelty Top - 10 : 60.8042 0.862061 0.615142 0.7488091 0.4646916 0.1173486 0.05453092 [0.006sec/1.316sec] 

```R
> avg(r)
      a_DCG    a_NDCG   ab_NDCG  ag_NDCG  abg_NDCG   TOT_DIV abg_TOT_DIV
10 61.57531 0.8667345 0.6146974 0.744387 0.4618586 0.1289305  0.05952857
```

### References
* recommenderlab [reference manual](https://cran.r-project.org/web/packages/recommenderlab/recommenderlab.pdf)
* grouplens [Datasets](https://grouplens.org/datasets/)
