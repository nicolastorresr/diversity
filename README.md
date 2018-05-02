# Evaluating Diversity Measures in Recommender Systems

All the diversity measures were developed in [RecommenderLab](https://cran.r-project.org/web/packages/recommenderlab/index.html), a R package library devoted to recommender systems.

#### The process to evaluate a recommender (e.g., *UBCF*) in a dataset (e.g., *MovieLens-100K*) is shown below:

Load base package and dependences.
```R
library('recommenderlab')

source('AAA.R')
source('calcPredictionAccuracy.R')
source('alpha_Measures.R')
source('BinomDiv.R')
source('evaluate.R')
```

Load *ML100K* dataset.
```R
data("MovieLense")
```
Create an `evaluationScheme` object from *MovieLense* data set using a 5-fold cross-validation.
```R
e <- evaluationScheme(MovieLense, method='cross-validation', train=0.8, k=5, given=15, goodRating=4)
```
Evaluate the recommender model given an evaluation scheme. Two approaches for diversity analysis are available: 
* `subtype = "a-nDCG"` for diversity measures based on alpha-nDCG (Clarke *et al*. 2008). 
* `subtype = "BinomDiv"` for Binomial Diversity (Vargas *et al*. 2014).
```R
> r <- evaluate(e, method = "UBCF", nMatrix = "../nuggets/Nuggets_ML100K.dat", type = "topNList", subtype = "a-nDCG", n = 10, param = list(method = "cosine", nn = 50))
```
Results for each fold

    UBCF run fold/sample [model time/prediction time]
	 1  a-nDCG Measures (top-10): 61.3707 0.860164 0.612487 0.745394 0.4627718 0.112587 0.0521021 [0.006sec/1.432sec] 
	 2  a-nDCG Measures (top-10): 62.0903 0.865740 0.616011 0.746436 0.4636326 0.135980 0.0630448 [0.006sec/1.402sec] 
	 3  a-nDCG Measures (top-10): 62.0703 0.878215 0.614272 0.743095 0.4599745 0.137939 0.0634486 [0.007sec/1.239sec] 
	 4  a-nDCG Measures (top-10): 61.5409 0.867490 0.615574 0.738199 0.4582225 0.140796 0.0645163 [0.006sec/1.236sec] 
	 5  a-nDCG Measures (top-10): 60.8042 0.862061 0.615142 0.748809 0.4646916 0.117348 0.0545309 [0.006sec/1.316sec] 

Overall performance
```R
> avg(r)
```
| | a_DCG  |  a_nDCG |  ab_nDCG | ag_nDCG | abg_nDCG |  TotDiv | abg_TotDiv |
| --      |:--------:|:--------:|:--------:|:--------:|:--------:|:--------:|:--------:|
| 10      | 61.5753 | 0.8667 | 0.6146 | 0.7443 | 0.4618 | 0.1289 |  0.0595 |


### References
* RecommenderLab ([Reference Manual](https://cran.r-project.org/web/packages/recommenderlab/recommenderlab.pdf))
* GroupLens [datasets](https://grouplens.org/datasets/)
* alpha-nDCG by Clarke *et al.* ([2008](https://plg.uwaterloo.ca/~gvcormac/novelty.pdf))
* BinomDiv by Vargas *et al.* ([2014](http://ir.ii.uam.es/saul/pubs/recsys2014-vargas-tid.pdf))
