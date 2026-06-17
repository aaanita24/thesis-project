# thesis-project
This project is an experiment developed by Anita Di Gennaro as a Master Thesis for the Computational Social Science course at the University Carlos III, Madrid.
It aims at testing the robustness and reliability of LLM's data reconstruction/retrieval abilities

Research question: _Under controlled missingness in official Italian wage data, how do LLM-based imputations preserve gender wage inequality estimates as compared to conventional imputation methods, especially in disadvantaged areas?_ 

The experimental design is based on a comparison of MICE - Predictive Mean Matching - under the missingness not at random assumption (fabricated through the code on the real ISTAT data) and on Qwen3-32B prompting (one-shot completion), asking the model to impute strictly basing itself on the given data and not drawing from the training/external information.
The main LLM's runs are two: one complete imputation, where the information received by the model is the same as what MICE receives, and one imputation where the geographical areas (NUTS-1 macro-areas) are anonymized. A global evaluation function provides measures of evaluation for each imputation method.

The code ends with phase 7, where methods to control for leakage and to ensure that the model is actually inferring rather than simply reproducing from memory.
Three attempts made:

- iteration of both methods to understand their variation and robustness - MICE repeated 50 times on a random seed and LLM's imputation 8 times on both versions, so the one-shot baseline with all the information and the anonymized version
- LLM's imputation on unseen data (wage for Male and Female multiplied by 1.18 and 1.12 respectively) to test performance on altered data
- covariate deletion on both methods, to remove the broader context and verify whether the model is still able to perform well, or if the MAE (mean absolute error) grows

For **reproducibility** it is necessary to have:
- R studio installed
- a API key from [OpenRouter.ai](https://openrouter.ai/) to run the prompts. Tokens are necessary, but the model in question is cheap and $2 can be enough to execute the calls.

Because of the black box mechanisms that are specific to both methods, especially for the Large Language Model Qwen3-32B, perfect and exact reproduction cannot be guaranteed.
