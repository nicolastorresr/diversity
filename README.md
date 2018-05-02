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
Create an `evaluationScheme` object from `MovieLense` data set using a 5-fold cross-validation.
```R
e <- evaluationScheme(  MovieLense, method = 'cross-validation', train = 0.8, k = 5, 
                        given = 15, goodRating = 4)
```
Evaluate the recommender model given an evaluation scheme. Two approaches for diversity analysis are available: 
* `subtype = "a-nDCG"` for diversity measures based on alpha-nDCG ([Clarke *et al*. 2008](https://plg.uwaterloo.ca/~gvcormac/novelty.pdf)). 
* `subtype = "BinomDiv"` for Binomial Diversity (Vargas *et al*. 2014).
```R
> r <- evaluate(e, method = "UBCF", nMatrix = "../nuggets/Nuggets_ML100K.dat", type = "topNList", 
                subtype = "a-nDCG", n = 10, param = list(method = "cosine", nn = 50))
```
Results for each fold:

    UBCF run fold/sample [model time/prediction time]


| | a-DCG  |  a-nDCG |  ab-nDCG | ag-nDCG | abg-nDCG |  TotDiv | abg-TotDiv |
| --      |:--------:|:--------:|:--------:|:--------:|:--------:|:--------:|:--------:|
| 1      | 61.3707 | 0.8601 | 0.6124 | 0.7453 | 0.4627 | 0.1125 | 0.0521 |
| 2      | 62.0903 | 0.8657 | 0.6160 | 0.7464 | 0.4636 | 0.1359 | 0.0630 |
| 3      | 62.0703 | 0.8782 | 0.6142 | 0.7430 | 0.4599 | 0.1379 | 0.0634 |
| 4      | 61.5409 | 0.8674 | 0.6155 | 0.7381 | 0.4582 | 0.1407 | 0.0645 |
| 5      | 60.8042 | 0.8620 | 0.6151 | 0.7488 | 0.4646 | 0.1173 | 0.0545 |


Overall performance:
```R
> avg(r)
```
| | a-DCG  |  a-nDCG |  ab-nDCG | ag-nDCG | abg-nDCG |  TotDiv | abg-TotDiv |
| --      |:--------:|:--------:|:--------:|:--------:|:--------:|:--------:|:--------:|
| 10      | 61.5753 | 0.8667 | 0.6146 | 0.7443 | 0.4618 | 0.1289 |  0.0595 |


### References
* RecommenderLab ([Reference Manual](https://cran.r-project.org/web/packages/recommenderlab/recommenderlab.pdf))
* GroupLens [datasets](https://grouplens.org/datasets/)
* alpha-nDCG by Clarke *et al.* ([2008](https://plg.uwaterloo.ca/~gvcormac/novelty.pdf))
* BinomDiv by Vargas *et al.* ([2014](http://ir.ii.uam.es/saul/pubs/recsys2014-vargas-tid.pdf))
