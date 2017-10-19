# Content Novelty Measures

```R

> library('recommenderlab')
# Load dependences
> source('AAA.R')
> source('calcPredictionAccuracy.R')
> source('Content_Novelty.R')
> source('evaluate.R')

# Load dataset
> data("MovieLense")

# Train
> e <- evaluationScheme(MovieLense, method='cross-validation', train=0.8, k=5, given=15, goodRating=4)
# Test
> r <- evaluate(e, method='UBCF', type='ratingMatrix', n=10, param=list(method='cosine', nn=50))
UBCF run fold/sample [model time/prediction time]
	 1  [0.004sec/0.9sec] 
	 2  [0.008sec/0.976sec] 
	 3  [0.008sec/0.972sec] 
	 4  [0.004sec/0.884sec] 
	 5  [0.004sec/1.084sec]
# Average
> avg(r)
      a_NDCG  ab_NDCG  ag_NDCG  abg_NDCG   TOT_DIV abg_TOT_DIV
10 0.8905016 0.616119 0.724511 0.4497598 0.1370402  0.06163856
```
