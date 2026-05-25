# thesis-project
This is the private version of my thesis project, which I will then make available and public once I complete it. 
The thesis aims at testing the robustness and reliability of LLM's data reconstruction/retreival abilities. 
Two methods of imputation methods, classical multiple imputation with the MICE package and large language models (LLMs), can recover regional gender wage gap statistics in Italy given different missingness mechanisms.

Here are the steps of the experimental design: 
- **STEP 1**: establishing the real-data baseline and ground truth. First of all, the gender wage inequalities are measured from the real data: unadjusted gap is computed from RACLI at country, macro-area, and regional level, and the composition-adjusted gap is computed from SES per macro-area and breakdown variable (more granular).
A preliminary MCAR test of MICE-PMM on RACLI and on SES will be executed to establish the pipeline noise, before introducing any missingness. In parallel, an uninformed LLM baseline is established on both datasets by direct prompting on a small sample of named cells and by zero-shot estimation from an empty scheme. 
Together these serve as a basis for the comparison: MICE under MCAR sets the lower bound of error attributable to the procedure itself, the uninformed LLM  will indicate what can be recovered without data.

- **STEP 2**: engineering the missingness. For RACLI, MNAR is mirrored through a regional disadvantage index _'reg_dis'_, constructed from the regional total-wage cells, combining a reverse-signed standardised mean wage with a standardised p90/p10 dispersion ratio. Regions are placed into low, moderate, and high disadvantage levels, then cells in the mean column are dropped with probabilities of roughly 10%, 25%, and 45% respectively.
The SES data will mirror another realistic scenario when working with data, MCAR. Because _'reg_dis'_ does not reach NUTS-1 macro areas, the relevant slice is masked uniformly at 20% (higher than the one selected for the baseline established in step 1).

- **STEP 3**: the imputation processes. MICE with predictive mean matching is applied to both datasets (RACLI with MNAR missingness and SES with MCAR missingness), to recover missing information. 
The same thing will be performed through LLMs. In this regard, which LLM to use and whether more than one should be tested (maybe free and payed version) is still to be established. Both datasets will be tested under a minimal prompt. 

- **STEP 4**: evaluation. Bias, mean absolute error, root mean squared error, and Spearman rank correlation between true and imputed regional rankings are computed separately for the unadjusted gap on RACLI and the adjusted gap on SES, both for the data imputed through MICE and the data imputed with LLMs.
