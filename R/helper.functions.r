#' Count distinct values in a vector
#'
#' @param vector.name name of your vector
#' @param model.type the kind of model to fit ("poly", "gam", "gam.adaptive","avg")
#' @description Works the same as sql function count(distinct x) when using a groupby statement
#' @export
#' @example 
count.distinct= function(vector.name){
  n.dist= length(unique(vector.name))
  n.dist
}
