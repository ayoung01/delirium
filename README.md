# delirium

This repository contains code to generate the results reported in *Development and Validation of an Electronic Health Record-Based Machine Learning Model to Estimate Delirium Risk in Newly Hospitalized Patients*.

The data have been redacted due to HIPAA requirements, but model training and validation are reported in `caret_tuning.R`.

The models reported in the manuscript are in `/rda/models`. They are named with the suffix `_nudesc1` if the outcome was calculated using Nu-DESC≥1, else they correspond to the outcome calculated using Nu-DESC≥2.

To generate ROC curves and confusion matrices reported in the paper, run:
```
Rscript -e "rmarkdown::render('caret_analysis.Rmd')"
```

The results of this command are in `caret_analysis.html`

<a href="https://doi.org/10.5281/zenodo.1239421"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.1239421.svg" alt="DOI"></a>

