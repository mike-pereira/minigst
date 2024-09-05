
#' Function to adapt the size of the model parameters passed to gstlearn functions
#' If n>0, it makes sure the argument arg has size 1 or n, and returns a copy of arg with size n.
#'
#' @keywords internal
#'
.rep_arg<-function(n,arg,argname){
  
  if(length(arg)==1){
    return(rep(arg,max(1,n)))
  }else if(n==0){
    stop(paste0("The length of the argument ",argname," should be ",1,"."))
  }else if(length(arg)!=n){
    stop(paste0("The length of the argument ",argname," should be ", ifelse(n>1,paste0("either ",1," or ",n,"."),"1.")))
  }else{
    return(arg)
  }
  
}


#' Function to create a gstlearn object containing the parameters needed to compute
#' an experimental variogram.
#' See the documentation of the function \code{vario_exp}
#' for details about the parameters.
#'
#' @keywords internal
#'
.createVarioParam<-function(db,dir=NULL,nlag=20, dlag=100,
                            toldis = 0.5, tolang= NULL){
  angles=F
  if(is.null(dim(dir))){
    if(db$getNDim()>2){
      stop("The directions have to be specified by vectors when the space dimension of the Db (",db$getNDim(),")is greater than 2.")
    }
    ndir=length(dir)
    angles=T
  }else{
    ndir=nrow(dir)
    ndim=ncol(dir)
    if(ndim!=db$getNDim()){
      stop("The number of columns of dir (",ndim,") should be the same as the space dimension in the Db (",db$getNDim(),").")
    }
  }
  
  nlag=.rep_arg(ndir,nlag,"nlag")
  dlag=.rep_arg(ndir,dlag,"dlag")
  toldis=.rep_arg(ndir,toldis,"toldis")
  if((is.null(tolang)) || (ndir==0)){
    tolang=rep(180/(2*ndir),max(1,ndir))
  }else{
    tolang=.rep_arg(ndir,tolang,"tolang")
  }
  
  
  if(ndir>0){
    varioParam=VarioParam()
    for(i in 1:ndir){
      if(angles){
        direc=DirParam_create(angle2D = dir[i],npas=nlag[i], dpas=dlag[i], toldis = toldis[i],tolang = tolang[i])
      }else{
        direc=DirParam_create(codir = dir[i,],npas=nlag[i], dpas=dlag[i], toldis = toldis[i],tolang = tolang[i])
      }
      varioParam$addDir(direc)
    }
    
  }else{
    varioParam = VarioParam_createOmniDirection(npas=nlag, dpas=dlag, toldis = toldis)
  }
  
  return(varioParam)
}


#' Compute an experimental variogram
#'
#' Function to compute an experimental variogram for a single variable, or experimental variogram and cross-variograms for a set of variables.
#'
#' @param db Db object.
#' @param vname Name(s) of the variable(s), stored as a (vector of) string(s).
#' @param dir Direction used to compute the variogram. The default (\code{NULL}) reverts to an omnidirectional variogram. The directions can be specified either by a vector of angles in degrees (only in 2D and the value(s) denote the angle between the horizontal axis and the desired direction), or as a matrix whose rows contain direction vectors.
#' @param nlag Number of lags used to compute the variograms.
#' @param dlag Distance of the lags.
#' @param toldis Tolerance on the distance (between 0 and 1). See \emph{Details}.
#' @param tolang Tolerance on the angles (between 0 and 90). See \emph{Details}.
#'
#' @details The parameter \code{toldis} represents a tolerance on the distance when computing the variogram value, and is expressed as proportion.
#' For  a given lag value \eqn{h}, all the pairs separated by distance between (\code{h}-\code{toldis}*\code{dlag}) and (\code{h}+\code{toldis}*\code{dlag})
#' are used to compute the correponding variogram value \eqn{gamma(h)}.
#'
#' The parameter \code{tolang} represents a tolerance on the angle and is expressed as an angle.
#' For a given lag value \eqn{h} and a given angle \eqn{theta}, all the pairs with an angle between (\eqn{theta}-\code{tolang}) and (\eqn{theta}+\code{tolang})
#' are used to compute the correponding variogram value \eqn{gamma(h)}.
#'
#' @return The function returns a \pkg{gstlearn} object containing the experimental variogram(s) (and cross-variograms).
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
#' # Create directional experimental variograms along the directions 30 deg and -30 deg for the variable "Elevation".
#' varioExpDir = vario_exp(db=db, vname="Elevation", dir=c(30,-30), nlag=20, dlag=10.)
#' # varioExpDir$display() # Run to display the content of the experimental variogram object.
#' # Plot experimental variogram
#' plot_vario(varioExpDir, pairDisplay="size", title="Experimental variograms for the variable 'Elevation'")
#'
#' # Create directional experimental (cross-)variograms along the directions 30 deg and -30 deg for the variables "Elevation" and "January_temp".
#' varioExpDir2var = vario_exp(db=db, vname=c("Elevation","January_temp"), dir=c(30,-30), nlag=20, dlag=10.)
#' # varioExpDir2var$display() # Run to display the content of the experimental variogram object.
#' # Plot experimental variogram
#' dev.new() # open new window
#' plot_vario(varioExpDir2var, title="Experimental (cross)-variograms for the variables 'Elevation' and 'January_temp")
#'
#'
vario_exp<-function(db,vname,drift=NULL,dir=NULL,nlag=20, dlag=100,
                    toldis = 0.5, tolang= 22.5){
  setVar(db,vname)
  
  varioParam= .createVarioParam(db,dir,nlag, dlag,toldis,tolang)
  
  varioexp = Vario(varioParam)
  if (is.null(drift)){
    err = varioexp$compute(db)
  }
  if (!is.null(drift)){
    for (xvar in drift){
      db$setLocator(xvar,ELoc_F())
    }
    EDmodel = Model_create()
    err = EDmodel$setDriftIRF(order=0,nfex=length(drift))
    err = varioexp$compute(db,model=EDmodel)
  }
  
  return(varioexp)
}




#' Compute a variogram cloud
#'
#' Function to compute a variogram cloud for a variable in a `gstlearn` Db object.
#'
#' @param db Db object.
#' @param vname Name of the variable.
#' @param dir Direction used to compute the variogram. Can be either \code{NULL} (Default) for an omnidirectional variogram cloud, or a single direction specified either by an angle in degrees (only in 2D and the value denote the angle between the horizontal axis and the desired direction), or as a matrix with a single row containing a direction vector.
#' @param gridRes Number or vector of size 2 specifying the resolution of the computation grid. See \emph{Details}.
#' @param tolang Tolerance on the angles. See \emph{Details}.
#' @param plot Whether or not to plot the variogram cloud.
#'
#' @details The variogram cloud is computed as a 2D grid with dimensions given by \code{gridRes}. The first axis discretizes the set of possible distances between pairs of points in \code{db}.
#' The second axis discretizes the set of possible variogram values between between pairs of points in \code{db}. The value computed at a given grid node corresponds to the number of pairs of points in \code{db} falling in the corresponding grid cell.
#'
#' @return A DbGrid containing the computed variogram cloud.
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
#' # Compute and plot the omnidirectional variogram cloud of the variable "Elevation".
#' varioCloudGrid = vario_cloud(db=db, vname="Elevation", plot = TRUE)
#'
vario_cloud<-function(db,vname,dir=NULL, gridRes=100, tolang= 22.5, plot=TRUE){
  
  if(length(vname)!=1){
    stop("Variogram clouds are only computed for a single variable: please specify 1 variable name for the argument vname.")
  }
  setVar(db,vname)
  
  ndir=0
  if(!is.null(dim(dir))){
    ndir=nrow(dir)
  }else{
    ndir=length(dir)
  }
  if(ndir>1){
    stop("Variogram clouds are only computed for a single direction: please specify only 1 direction or use NULL to create an omnidirectional variogram cloud.")
  }
  
  stopifnot(is.numeric(gridRes))
  if(length(gridRes)==1){
    gridRes=c(gridRes, gridRes)
  }
  
  varioParam= .createVarioParam(db,dir=dir,tolang=tolang)
  
  grid.cloud=db_vcloud(db, varioParam,lagnb = gridRes[1], varnb = gridRes[2])
  if(plot){
    vn=grid.cloud$getNames("Cloud.*")[1]
    p=dbplot_grid(grid.cloud,color=vn,colorLegendTitle =  "Nb of pairs",cmap='RdBu',naColor = NA,
                  xlab = "Distance", ylab = "Variogram", title = paste0("Variogram cloud: ",vname))
    print(p)
  }
  return(grid.cloud)
}



#' Compute a variogram map
#'
#' Function to compute a variogram map for a variable in a `gstlearn` Db object.
#'
#' @param db Db object.
#' @param vname Name of the variable.
#' @param gridRes Number specifying the resolution of the computation grid. See \emph{Details}.
#' @param plot Whether or not to plot the variogram cloud.
#'
#' @details The variogram map is computed on a 2D grid, centered at the origin, and with dimensions (2*\code{gridSize}+1) * (2*\code{gridSize}+1).
#'
#' @return A DbGrid containing the computed variogram map.
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
#' # Compute and plot the omnidirectional variogram cloud of the variable "Elevation".
#' varioMapGrid = vario_map(db=db, vname="Elevation", plot = TRUE)
#'
vario_map<-function(db,vname,gridRes=20,plot=T){
  
  setVar(db,vname)
  
  stopifnot(is.numeric(gridRes))
  grid.vmap = db_vmap(db,nxx = rep(gridRes[1],db$getNDim()))
  
  vn1=grid.vmap$getNames("VMAP.*.Var")[1]
  p1=dbplot_grid(grid.vmap,color=vn1,cmap="Spectral",
                 colorLegendTitle ="Var",
                 title = paste0("Variogram map : ",vname))
  
  vn2=grid.vmap$getNames("VMAP.*.Nb")[1]
  p2=dbplot_grid(grid.vmap,color=vn2,cmap="RdBu",
                 colorLegendTitle ="Nb",
                 title = "Number of pairs")
  
  print(ggarrange(p1,p2,nrow=1,ncol=2, common.legend = FALSE))
  
  return(grid.vmap)
}




#' Fit a model on experimental variograms or maps
#'
#' Function to fit a model on an experimental variogram, a set of experimental (cross-)variograms or a variogram map.
#'
#' @param vario Experimental variogram(s) or variogram map (as a \pkg{gstlearn} object). See \emph{Details}.
#' @param struct Vector containing the names of the desired basic structures. The list of available structures is obtained by calling the function \code{printAllStruct()}.
#' @param pruneModel Whether or not to prune the model. See \emph{Details}.
#' @param anisoModel Whether or not to fit an anistropic model.
#'
#' @details The argument \code{vario} is specified as the output of the \code{vario_exp} function (for experimental variograms)
#' or as the output of the \code{vario_map} function (for variogram maps).
#'
#' The fitting function first tries to fit a model containing all the structures specified in \code{struct}. if \code{pruneModel = TRUE},
#' this model is pruned, i.e. the structures associated with negligible variances are discarded from the model, and a new fit is performed.
#' These last two steps are repeated until no basic structures can be removed.
#'
#' @return A \pkg{gstlearn} Model object containing the fitted model.
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
#' varioExp = vario_exp(db=db, vname="Elevation", dir=c(30,-30), nlag=20, dlag=10.)
#'
#' # Fit a model on the resulting experimental variogram
#' struct_names = c("NUGGET","SPHERICAL", "SPHERICAL")
#' model = model_fit(varioExp, struct=struct_names, pruneModel = TRUE)
#' model$display() # Display the content of the model
#'
#' # Plot the experimental variogram and the fitted model
#' plot_vario(varioExp,model = model, pairDisplay = "size",
#'            title="Model adjustment for Elevation")
#'
#'
model_fit<-function(vario,drift=NULL,struct="SPHERICAL",pruneModel=TRUE,anisoModel=TRUE){
  types = ECov_fromKeys(struct)
  model = Model()
  if(class(vario)=="_p_Vario"){
    if (!is.null(drift)){
      err = model$setDriftIRF(order=0,nfex=length(drift))}
    err = model$fit(vario, types=types, optvar=Option_VarioFit(flag_noreduce=pruneModel,auth_aniso=anisoModel))
  }else if(class(vario)=="_p_DbGrid"){
    vn=vario$getNames("VMAP.*.Var")[1]
    if(is.na(vn)){
      stop("The DbGrid supplied in 'vario' should contain a variogram map saved as a variable whose name is of the form 'VMAP.*.Var'.")
    }
    setVar(vario,vn)
    err = model$fitFromVMap(vario, types=types, optvar=Option_VarioFit(flag_noreduce=pruneModel,auth_aniso=anisoModel))
  }else{
    stop("The argument 'vario' expects either an experimental variogram or a DbGrid conatining a variogram map.")
  }
  
  return(model)
}



#' Basic structures for models
#'
#' Print the list of all available basic structures/covariance functions in \pkg{gstlearn}.
#'
#'
#' @return Prints the list of names and returns nothing.
#'
#' @export
#'
#' @examples
#' library(minigst)
#'
#' # Print list of basic structures
#' printAllStruct()
#'
printAllStruct<-function(){
  ECov_printAll()
  return(invisible(NULL))
}




.checkCovParam<-function(param,nameParam,n){
  
  paramf=param
  if((!is.numeric(param)) || (sum(is.na(param)>0))){
    stop("The argument ",nameParam,"should be numeric, and NA are not allowed.")
  }
  if(length(param)==1){
    paramf=rep(param,n)
  }else if(length(param)!=n){
    stop(paste0("The length of ", nameParam," (",length(param),") must be the same as the number of structures (",n,")"))
  }
  
  return(paramf)
}


#' Create a model
#'
#' Function to create a \pkg{gstlearn} Model object (corresponding to a single variable with an isotropic covariance function).
#'
#' @param struct Vector containing the names of the desired basic structures. The list of available structures is obtained by calling the function \code{\link{printAllStruct}}.
#' @param range Value or vector specifying the range of each structure. Must be of size 1 (if all the structures share the same value) or have the same size as \code{struct}.
#' @param sill Value or vector specifying the sill/variance of each structure. Must be of size 1 (if all the structures share the same value) or have the same size as \code{struct}.
#' @param param Value or vector specifying the extra parameter (eg. smoothness parameter for Matérn covariances) of each structure. Must be of size 1 (if all the structures share the same value) or have the same size as \code{struct}.
#' @param ndim Space dimension of the model.
#' @param mean Mean of the model (0 by default).
#'
#' @return A \pkg{gstlearn} Model object.
#'
#' @export
#'
#' @examples
#' library(minigst)
#'
#' ## Parameters of the model
#' struct_names = c("SPHERICAL", "K-BESSEL")
#' ranges=c(0.3,0.5)
#' variances=c(0.1,1)
#' params=c(1,1) # The first value will be ignored since the correponding covariance function ("SPHERICAL") does not require an extra parameter.
#'
#' ## Create model
#' model=createModel(struct=struct_names, range = ranges, sill = variances, param = params,ndim=2)
#'
createModel<-function(struct="SPHERICAL", range = 0.3, sill = 1, param = 1,ndim=2, mean=0){
  
  nstruct=length(struct)
  range=.checkCovParam(range,"range",nstruct)
  sill=.checkCovParam(sill,"sill",nstruct)
  param=.checkCovParam(param,"sill",param)
  
  ## Create empty model for 1 variable, and spaceDim dimension
  model = Model(1,ndim)
  for(i in 1:nstruct){
    err=Model_addCovFromParam(model,type = ECov_fromKey(struct[i]), sill = sill[i], range=range[i],param=param[i]) ## Ajouter un nugget de variance 1
  }
  model$setMeans(mean)
  
  return(model)
  
}



#' Function to evaluate a model between two points.
#' See \code{model_eval} for details on the parameters.
#'
#' @keywords internal
#'
.model_eval_base<-function(x,y,model,mode){
  if(mode=="VG"){
    md=CovCalcMode(); md$setAsVario(TRUE);
  }else if(mode=="COV"){
    md=CovCalcMode(); md$setAsVario(FALSE);
  }else{
    stop("Only values are possible for the argument mode: 'VG' to compute variogram values, and 'COV' to compute covariance values.")
  }
  ndim=model$getDimensionNumber()
  if(!((length(x)==ndim)&&(length(y)==ndim))){
    stop("Wrong dimensions for coordinate vectors when evaluating the model.")
  }
  return(Model_eval(model,SpacePoint(x),SpacePoint(y),mode = md))
}



#' Evaluate a model
#'
#' Function to evaluate a \pkg{gstlearn} Model object, i.e. to compute the value of the associated covariance function on a set of points.
#'
#' @param x,y Matrices (of same size) whose rows specify the pairs of points at which the covariance is evaluated. The number of columns of these matrices must be the same as the space dimension of the model.
#' @param h Vector of lag values at which the covariance is evaluated. These lags are taken in the direction specified by \code{dir}.
#' @param dir Direction vector at which the lags in \code{h} are taken.  The size of \code{dir} must be the same as the space dimension of the model.
#' @param model Model object to evaluate.
#' @param mode Whether to compute the variogram values (\code{mode="VG"}) or covariance values (\code{mode="VG"}).
#'
#' @details There are two ways of specifying where the model is valuated:
#' \itemize{
#'   \item Either by specifying both the arguments \code{x} and \code{y}.
#'   \item Or by specifying the argument \code{h} and optionally the argument \code{dir} (whose default value \code{NULL} means that the direction along the vector of ones is taken).
#' }
#'
#'
#' @return A vector containing the evaluations of the model at the specified targets.
#'
#' @export
#'
#' @examples
#' library(minigst)
#'
#' ## Create model
#' model=createModel(struct=c("EXPONENTIAL", "NUGGET"), range = 0.3, sill = c(1,0.1), ndim=2)
#'
#' ## Evaluate the model along the direction (0,1)
#' hseq=seq(from=0,to=1,length.out=100)
#' md_eval_lags=model_eval(h=hseq,dir=c(0,1), model=model,mode="COV")
#'
#' ## Plot Variogram along the direction (0,1) using the function curve
#' curve(model_eval(h=x, dir=c(0,1), model=model,mode="VG"),from = 0, to =1,n = 10^3,xlab="Lag",ylab="Variogram")
#'
#' ## Evaluate the model on some random points in the unit square
#' xx=matrix(runif(20),nrow=10,ncol=2)
#' yy=matrix(runif(20),nrow=10,ncol=2)
#' md_eval_pts=model_eval(x=xx,y=yy,model=model,mode="COV")
#'
#'
#'
model_eval<-function(x=NULL,y=NULL,h=NULL,dir=NULL, model=createModel(),mode="COV"){
  ndim=model$getDimensionNumber()
  res=NULL
  if((!is.null(x))&&(!is.null(y))){
    if(!((ncol(x)==ndim)&&(ncol(y)==ndim)&&(nrow(x)==nrow(y)))){
      stop(paste0("Wrong dimensions for coordinate matrice x or y when evaluating the model. The number of rows and columns of x and y must be the same, and the number of columns must be equal to the space dimension of the model (",ndim,")."))
    }else{
      res=sapply(1:nrow(x),function(i){.model_eval_base(x[i,],y[i,],model,mode)})
    }
  }else if(!is.null(h)){
    if(is.null(dir)){
      dir=rep(1,ndim)/sqrt(ndim)
    }else{
      if(length(dir)!=ndim){
        stop(paste0("Wrong dimensions for direction vector dir when evaluating the model. The length of dir must be equal to the space dimension of the model (",ndim,")."))
      }
    }
    res=sapply(h,function(hh){.model_eval_base(rep(0,ndim),hh*dir,model,mode)})
  }else{
    stop("You must specify either the argument h, or both the arguments x and y.")
  }
  
  return(res)
}





#' Compute a covariance or variogram matrix from a model
#'
#' Function to compute a covariance or variogram matrix between pairs of points, from a \pkg{gstlearn} Model object.
#'
#' @param x,y Matrices (of same size) whose rows specify the pairs of points at which the covariance is evaluated. The number of columns of these matrices must be the same as the space dimension of the model.
#' @param model Model object to evaluate.
#' @param mode Whether to compute the variogram values (\code{mode="VG"}) or covariance values (\code{mode="VG"}).
#'
#' @details If the argument \code{y} is set to \code{NULL} (Default), then \code{y} is set to \code{x}.
#'
#' @return A matrix of size \code{nrow(x)}*\code{nrow(y)} containing the covariance/variogram between each point in \code{x} and each point in \code{y}.
#'
#' @export
#'
#' @examples
#' library(minigst)
#'
#' ## Create model
#' model=createModel(struct=c("EXPONENTIAL", "NUGGET"), range = 0.3, sill = c(1,0.1), ndim=2)
#'
#' ## Evaluate the covriance matrix on some random points in the unit square
#' xx=matrix(runif(20),nrow=10,ncol=2)
#' yy=matrix(runif(10),nrow=5,ncol=2)
#' covMat=model_covMat(x=xx,y=yy,model=model,mode="COV")
#'
#'
#'
model_covMat<-function(x,y=NULL,model=createModel(),mode="COV"){
  
  ndim=model$getDimensionNumber()
  if(ncol(x)<ndim){
    stop(paste0("The number of columns of x should be the same as the space dimension of model (",ndim,")"))
  }
  
  if(mode=="VG"){
    md=CovCalcMode(); md$setAsVario(TRUE);
  }else if(mode=="COV"){
    md=CovCalcMode(); md$setAsVario(FALSE);
  }else{
    stop("Only values are possible for the argument mode: 'VG' to compute variogram values, and 'COV' to compute covariance values.")
  }
  
  x=as.data.frame(x)
  db1=dfToDb(x,colnames(x)[1:ndim])
  
  if(!is.null(y)){
    if(ncol(y)<ndim){
      stop(paste0("The number of columns of y should be the same as the space dimension of model (",ndim,")"))
    }
    y=as.data.frame(y)
    db2=dfToDb(y,colnames(y)[1:ndim])
  }else{
    db2=NULL
  }
  
  
  
  if(is.null(db2)){
    res=Model_evalCovMatrixSymmetric(model,db1,mode=md)$toTL()
    # res=Model_evalCovMatrix(model,db1,mode=md)$toTL()
  }else{
    res=Model_evalCovMatrix(model,db1,db2,mode=md)$toTL()
  }
  
  return(res)
}




# struct_eval<-function(h,name="SPHERICAL", range = 0.3, sill = 1, param = 1,mode="VG"){
#   if(mode=="VG"){
#     md=CovCalcMode(); md$setAsVario(TRUE);
#   }else{
#     md=CovCalcMode(); md$setAsVario(FALSE);
#   }
#   cov=CovAniso(ECov_fromKey(name), range, param, sill,CovContext(1,2))
#   res=sapply(h,function(hh){CovAniso_eval(cov,SpacePoint(c(hh,0)),SpacePoint(c(0,0)),mode=md)})
#   return(res)
# }
#



