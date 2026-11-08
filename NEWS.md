# inlaops 0.1.2

* First CRAN release.

* `vetiver::vetiver_model()` accepts `bru` fits from inlabru, so they can be
  versioned with pins, served as REST APIs with posterior uncertainty, and
  packaged with Docker.

* `bru_reg()` is a parsnip model constructor with an inlabru engine, scoped
  to models with fixed effects and IID random intercepts.
  `bru_predict_conf_int()` gives posterior credible intervals at any level,
  and `bru_pred_formula()` gives the fixed-effect linear predictor formula.

* `bru_score()` computes continuous ranked probability, log, squared-error
  and Dawid-Sebastiani scores from `inlabru::generate()` posterior samples
  via scoringRules.

* `bru_cv_predict()` re-fits a model on `rsample::vfold_cv()` folds and
  returns the holdout predictions.

Changes made before the first CRAN release are listed in
[CHANGELOG.md](https://github.com/novica/inlaops/blob/main/CHANGELOG.md).
