#' Get prep object
#'
#' @inheritParams brms::posterior_predict
#'
#' @return prep object
#' @export
get_prep <- function(
    object, newdata = NULL, re_formula = NULL, re.form = NULL,
    transform = NULL, resp = NULL, negative_rt = FALSE,
    ndraws = NULL, draw_ids = NULL, sort = FALSE, ntrys = 5,
    cores = NULL, ...
) {
  cl <- match.call()
  if ("re.form" %in% names(cl)) {
    re_formula <- re.form
  }
  object <- brms::restructure(object)
  prep <- brms::prepare_predictions(
    object, newdata = newdata, re_formula = re_formula, resp = resp,
    ndraws = ndraws, draw_ids = draw_ids, check_response = FALSE, ...
  )
  return(prep)
}
