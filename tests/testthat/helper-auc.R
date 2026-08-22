# Brute-force reference for the area under the ROC curve.
#
# The trapezoidal area under a ROC curve equals the weighted Mann-Whitney
# concordance: the share of event/non-event pairs whose scores the model orders
# correctly, counting tied scores as half a correct pair. Computing it directly
# from every pair gives the tests a ground truth that does not depend on the
# curve construction being tested.
weighted_concordance_auc <- function(truth01, score, weights = NULL) {
  if (is.null(weights)) {
    weights <- rep(1, length(score))
  }
  weights <- as.numeric(weights)

  is_event <- truth01 == 1
  event_score <- score[is_event]
  event_weight <- weights[is_event]
  other_score <- score[!is_event]
  other_weight <- weights[!is_event]

  concordance <- outer(event_score, other_score, ">") +
    0.5 * outer(event_score, other_score, "==")
  pair_weight <- outer(event_weight, other_weight)

  sum(concordance * pair_weight) / sum(pair_weight)
}
