# thesis-project
This is the private version of my thesis project, which I will then make available and public once I complete it. 
The thesis aims at testing the robustness and reliability of LLM's data reconstruction/retreival abilities. 
Two methods of imputation methods, classical multiple imputation with the MICE package and large language models (LLMs), can recover regional gender wage gap statistics in Italy given different missingness mechanisms.

Here are the steps of the experimental design: 
- **STEP 1**: establishing the real-data baseline and ground truth. First of all, the gender wage inequalities are measured from the real data: the composition-adjusted gap is computed from SES per macro-area and breakdown variable (more granular).
A preliminary MCAR test of MICE-PMM on SES will be executed to establish the pipeline noise, before introducing any missingness. In parallel, an uninformed LLM baseline is established on both datasets by direct prompting on a small sample of named cells. 
Together these serve as a basis for the comparison: MICE under MCAR sets the lower bound of error attributable to the procedure itself, the uninformed LLM  will indicate what can be recovered without data.

- **STEP 2**: engineering the missingness. MNAR is mirrored through a regional disadvantage index _'reg_dis'_, constructed from the regional total-wage cells, combining a reverse-signed standardised mean wage with a standardised p90/p10 dispersion ratio and adapted to the NUTS-1 macro areas present in the Structure of Earnings Survey. Areas are placed into low, moderate, and high disadvantage levels, then cells in the Female/Male hourly wages are dropped with probabilities of roughly 10%, 25%, and 45% respectively.

- **STEP 3**: the imputation processes. MICE with predictive mean matching is applied.
The same thing will be performed through LLMs. _Qwen-3 32B_, a thinking model, will be used, but it might be supported by a more advanced model.Row-by-row contextual and table completion will be tested, as well as a one-shot estimation according to the broad context given to the LLM. 

- **STEP 4**: evaluation. Bias, mean absolute error, root mean squared error, and Spearman rank correlation between true and imputed regional rankings are computed through a universal evaluation function, both for the data imputed through MICE and the imputation attempts with LLMs.
