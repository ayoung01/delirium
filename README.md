# delirium

This repository contains code to generate the results reported in *Development and Validation of an Electronic Health Record-Based Machine Learning Model to Estimate Delirium Risk in Newly Hospitalized Patients*.

The data have been redacted due to HIPAA requirements, but the model tuning process is reported in `caret_tuning.R`.

The models reported in the manuscript are deposited in `/rda/models`. They are named with a suffix `_nudesc1` if the outcome was calculated using Nu-DESC $\ge$ 1, else they correspond to the outcome calculated using Nu-DESC $\ge$ 2.

To generate ROC curves and confusion matrices reported in the paper, run:
```
Rscript -e "rmarkdown::render('caret_analysis.Rmd')"
```

The results of this command are in `caret_analysis.html`