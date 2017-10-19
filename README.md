# Content Novelty Measures

```R

library('recommenderlab')
# Load dependences
source('AAA.R')
source('calcPredictionAccuracy.R')
source('Content_Novelty.R')
source('evaluate.R')

# Load dataset
data("MovieLense")

# Train
e <- evaluationScheme(MovieLense, method='cross-validation', train=0.8, k=5, given=15, goodRating=4)
# Test
r <- evaluate(e, method='UBCF', type='ratingMatrix', n=10, param=list(method='cosine', nn=50))
# Average
avg(r)
```
