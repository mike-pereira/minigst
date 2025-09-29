#'Create a Model from a list of basic structures
#'
#'Function which returns a Model containing a list of basic structures
#'
#'
#'@param structs The name of a basic structure or a vector of names
#'
#' @return A \pkg{gstlearn} Model object containing the created model.
#' 
#' @details 
#'
#' The parameters of the model are arbitrary fixed. This model intends to be used in a fitting procedure.
#'  
#' @export
#'
#' @examples
#' library(minigst)
#' model = createModelFromList("EXPONENTIAL")
#' model$display()
#' 
#' model = createModelFromList(c("EXPONENTIAL","NUGGET"))
#' model$display()
#
createModelFromList <- function(structs)
{
  types = .checkStructNames(structs)
  if (length(structs) >= 1)
  {
    model = Model_createFromParam(ECov_fromKey(structs[1]))
    for (i in 2:length(structs))
      model$addCovFromParam(ECov_fromKey(structs[i]))
    return(model)
  } else
  {
    print("The list has to contain at least one valid name.")
  }
}

#'Prune a model 
#'
#'Function which reduces a Model by suppressing the lowest variance components if it is below a threshold.
#'
#'
#'@param model The name of the \pkg{gstlearn} Model
#'@param propVarMin The proportion of variance under which a component is suppressed.
#'
#' @return A boolean indicating if the model has been reduced and if there is more than one remaining structure
#' @export
#'
#' @examples
#' library(minigst)
#'
#' model = Model_createFromParam(ECov_fromKey("SPHERICAL"),sill = 0.9)
#' model$addCovFromParam(ECov_fromKey("NUGGET"),sill = 0.01)
#' model$addCovFromParam(ECov_fromKey("EXPONENTIAL"),sill = 0.1)
#' pruneModelF(model)
#' model$display()
#' 

pruneModelF <- function(model, propVarMin = 0.05)
{
  ncov = model$getNCov()
  if (model$getNCov() < 2)
    return(FALSE)
  vartot = model$getTotalSill()
  varMin = propVarMin * vartot
  index = NA
  for (icov in 0:(ncov-1))
  {
    sill = model$getSill(icov,0,0)
    if (sill < varMin)
    {
      index = icov
      varMin = sill
    }
  }
  if (is.na(index))
    return (FALSE)
  
  model$delCov(index)
  return (TRUE)
}
#' Fit a model by maximum likelihood
#'
#' Function to fit a model by maximum likelihood under Gaussian assumption. Vecchia approximation can be used for large data sets.
#'
#' @param db Db object.
#' @param vname Name(s) of the variable(s), stored as a (vector of) string(s).
#' @param polDrift Integer specifying the order of the polynomial drift. 
#' @param extDrift Name of the variable(s) specifying the external drift, stored as a (vector of) string(s).
#' @param struct Vector containing the names of the desired basic structures. The list of available structures is obtained by calling the function \code{printAllStruct()}.
#' @param pruneModel NOT YET AVAILABLE. Whether or not to prune the model. See \emph{Details}.
#' @param anisoModel Whether or not to fit an anistropic model.
#' @param reml Whether or not to use Restricted Maximum Likelihood.
#' @param nVecchia The number of neighbors to consider in Vecchia approximation (NA for full maximum likelihood)
#'
#' @details 
#'
#' The fitting function first tries to fit a model containing all the structures specified in \code{struct}. if \code{pruneModel = TRUE},
#' this model is pruned, i.e. the structures associated with negligible variances are discarded from the model, and a new fit is performed.
#' These last two steps are repeated until no basic structures can be removed.
#' 
#'
#' @return A list of with content
#' model: \pkg{gstlearn} Model object containing the fitted model;
#' driftCoeffs: the vector of the estimated mean parameters;
#' likelihood: the value of the likelihood.
#'
#' @export
#'
#' @examples
#' library(minigst)
#'
#' # Load Data
#' data("Scotland")
#'
#' # Create Db
#' db=dfToDb(df=Scotland, coordnames=c("Longitude", "Latitude"))
#' db$display()
#'
#' # Create directional experimental variograms along the directions 30 deg and -30 deg for the variable "Elevation"
#' varioExp = vario_exp(db=db, vname="January_temp", dir=c(30,-30), nlag=20, dlag=10.)
#'
#' # Fit a model
#' struct_names = c("NUGGET","SPHERICAL", "SPHERICAL")
#' result = model_MaximumLikelihood(db, "January_temp", struct=struct_names)
#' model = result$model
#' model$display() # Display the content of the model
#'
#' # Plot the experimental variogram and the fitted model
#' plot_vario(varioExp,model = model, pairDisplay = "size",
#'            title="Model adjustment for Elevation")
#'
#'
model_MaximumLikelihood<-function(db,vname,polDrift=NULL,extDrift=NULL,struct="SPHERICAL",pruneModel=TRUE,anisoModel=TRUE, reml = F, nVecchia = NA){
  types = .checkStructNames(struct)
  
  if(class(db)=="_p_gstlrn__Db"){
    
    dbaux = Db_createReduce(db,ranks = which(!is.na(db[vname])) - 1)
    model = createModelFromList(struct)
    setVar(dbaux,vname)
    if (!is.null(extDrift) || !is.null(polDrift)){
      ## Add drifts to model
      .addDriftsToModel(model,polDrift,length(extDrift))
      setVar(dbaux,extDrift,"Drift")
    } 
    else
    {
      model$setDriftIRF(0)
    }
    if (is.na(nVecchia))
      nVecchia = -1234567
    
    continue = T
    while (continue)
    {
      mop = ModelOptimParam_create(anisoModel) 
      ll = AModelOptimFactory_create(model,dbaux,NULL,NULL,NULL,mop = mop, nVecchia, reml)
      cost = ll$run()
      if (!pruneModel)
        break;
      continue = pruneModelF(model)
      
    }
      return(list(model = model, driftCoeffs = ALikelihood_getBeta(ll), likelihood = -cost))
    
  }else{
    stop("The argument 'db' expects a Db.")
  }
  
  return(model)
}
