#' Function to create a gstlearn object containing the parameters of 
#' the neighborhood used to perform kriging
#'
#' @keywords internal
#'
.createneigh<-function(neigh){
  if (neigh == "unique"){
    Neigh = NeighUnique()
  }
  else if ((sum(is.numeric(neigh))== 3) & (length(neigh == 3))){
    if ((neigh[1] <= neigh[2]) & (neigh[3]>0)){
      Neigh =NeighMoving_create(nmini=neigh[1], nmaxi=neigh[2], radius=neigh[3])
    }
  }
  else if ((sum(is.numeric(neigh))== 2) & (length(neigh == 2))){
    if (neigh[1] <= neigh[2]){
      Neigh =NeighMoving_create(nmini=neigh[1], nmaxi=neigh[2])
    }
  }
  else{
    stop("The neighborhood definition is not valid")
  }
  return(Neigh)
}

#' Perform Kriging
#'
#' Function to perform the kriging from a data set to a target  \pkg{gstlearn} DbGrid object.
#'
#' @param dbin Db containing the data.
#' @param dbout Target Db.
#' @param vname Name of the variable, stored as a string.
#' @param model A \pkg{gstlearn} Model object containing a fitted model.
#' @param type Type of kriging, either "simple" or "ordinary".
#' @param mean Mean value if simple kriging is requested 
#' @param neighborhood Type of neighborhood to used, either "unique" or a vector of the form (nmin, nmax) or (nmin, nmax, radius) see details. 
#' @param std Boolean value indicating whether the prediction standard deviation should be computed.
#'
#' @details the vector (nmin, nmax, radius) indicates the minimum and the maximum number of points in the neighborhood and its optional radius.
#' 
#' @return the dbout where the prediction results have been added with a prefix SK respectively OK for simple and ordinary kriging.
#'
#' @export
#'
#' @examples
#' library(minigst)
#'
#' #todo
#' 
#' 
#'  
#'
kriging_uni<-function (dbin, dbout, vname, model, type = "ordinary", mean = NULL, neighborhood = "unique", std = TRUE){
  setVar(dbin,vname)
  Neigh = .createneigh(neighborhood)
  if (type == "simple"){
    if (!as.numeric(mean)){stop('The mean is not properly specified for the simple kriging.')}
    err = model$setMean(mean)
    err = kriging(dbin=dbin, dbout=dbout, model=model, 
                  neigh=Neigh,
                  flag_est=TRUE, flag_std=std, flag_varz=FALSE,
                  namconv=NamingConvention("SK")
    )
  }
  else if (type == "ordinary"){
    err = model$addDrift(DriftM())
    err = kriging(dbin=dbin, dbout=dbout, model=model, 
                  neigh=Neigh,
                  flag_est=TRUE, flag_std=std, flag_varz=FALSE,
                  namconv=NamingConvention("OK")
    )
  }
  else {stop("The kriging type should be either simple or ordinary")}
  return(dbout)
}


#' Compute the cross validation error
#'
#' Function to perform the (1-fold) cross validation for a given kriging exercise.
#'
#' @param dbin Db containing the data.
#' @param vname Name of the variable, stored as a string.
#' @param model A \pkg{gstlearn} Model object containing a fitted model.
#' @param type Type of kriging, either "simple" or "ordinary".
#' @param mean Mean value if simple kriging is requested 
#' @param neighborhood Type of neighborhood to used, either "unique" or a vector of the form (nmin, nmax) or (nmin, nmax, radius) see details. 
#' @param std Boolean value indicating whether the prediction standard deviation should be computed.
#'
#' @details For each observation location x_i, compute the kriging prediction at x_i when using all the observations except the one located at x_i. 
#' The cross-validation error at x_i is then defined as the difference between this prediction and the actual observed value at x_i.
#' 
#' @return a db containing the same variables as the input db + two new variables: 
#' One variable (with name ending with .esterr) containing the cross-validation errors, 
#' and one variable (with name ending with .stderr) containing the cross-validation errors normalized by the kriging standard deviation.
#'
#' @export
#'
#' @examples
#' library(minigst)
#'
#' #todo
#' 
#' 
#'  
#'

xvalidation <- function (dbin, vname, model, type = "ordinary", mean = NULL, neighborhood = "unique", std = TRUE){
  setVar(dbin,vname)
  Neigh = .createneigh(neighborhood)
  if (type == "simple"){
    if (!as.numeric(mean)){stop('The mean is not properly specified for the simple kriging.')}
    err = model$setMean(mean)
    err = xvalid(db=dbin, model=model, 
                 neigh=Neigh,
                 flag_xvalid_est=1, flag_xvalid_std=1,  
                 namconv=NamingConvention_create("Xvalid", flag_locator = FALSE)
    )
  }
  else if (type == "ordinary"){
    err = model$addDrift(DriftM())
    err = xvalid(db=dbin, model=model, 
                 neigh=Neigh,
                 flag_xvalid_est=1, flag_xvalid_std=1,  
                 namconv=NamingConvention_create("Xvalid", flag_locator = FALSE)
    )
  }
  else {stop("The kriging type should be either simple or ordinary")}
  return(dbin)
}

