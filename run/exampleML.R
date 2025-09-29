library(minigst)
data("Scotland")
db=dfToDb(df=Scotland, coordnames=c("Longitude", "Latitude"))

#' # Create directional experimental variograms along the directions 30 deg and -30 deg for the variable "Elevation"
#' varioExp = vario_exp(db=db, vname="Elevation", dir=c(30,-30), nlag=20, dlag=10.)
#'
#' # Fit a model
struct_names = c("NUGGET","SPHERICAL", "SPHERICAL")
varioExp = vario_exp(db=db, vname="January_temp", dir=c(30,-30), nlag=20, dlag=10.)
result = model_MaximumLikelihood(db, "January_temp", struct=struct_names, pruneModel = F)
model = result$model
model$display() # Display the content of the model
plot_vario(varioExp,model = model, pairDisplay = "size",title="Model adjustment for Elevation")

#' # Fit a model with pruning
struct_names = c("NUGGET","SPHERICAL", "SPHERICAL")
varioExp = vario_exp(db=db, vname="January_temp", dir=c(30,-30), nlag=20, dlag=10.)
result = model_MaximumLikelihood(db, "January_temp", struct=struct_names, pruneModel = T)
model = result$model
model$display() # Display the content of the model
plot_vario(varioExp,model = model, pairDisplay = "size",title="Model adjustment for Elevation")

#' # Fit a model without anisotropy
struct_names = c("NUGGET","SPHERICAL", "SPHERICAL")
varioExp = vario_exp(db=db, vname="January_temp", dir=c(30,-30), nlag=20, dlag=10.)
result = model_MaximumLikelihood(db, "January_temp", struct=struct_names, pruneModel = T, anisoModel = F)
model = result$model
model$display() # Display the content of the model
plot_vario(varioExp,model = model, pairDisplay = "size",title="Model adjustment for Elevation")


#' # Fit a model with Vecchia 
struct_names = c("NUGGET","SPHERICAL", "SPHERICAL")
varioExp = vario_exp(db=db, vname="January_temp", dir=c(30,-30), nlag=20, dlag=10.)
result = model_MaximumLikelihood(db, "January_temp", struct=struct_names, pruneModel = T, nVecchia = 10)
model = result$model
model$display() # Display the content of the model
plot_vario(varioExp,model = model, pairDisplay = "size",title="Model adjustment for Elevation")

#' # Fit a model with Vecchia without anisotropy
struct_names = c("NUGGET","SPHERICAL", "SPHERICAL")
varioExp = vario_exp(db=db, vname="January_temp", dir=c(30,-30), nlag=20, dlag=10.)
result = model_MaximumLikelihood(db, "January_temp", struct=struct_names, pruneModel = T, nVecchia = 10, anisoModel = F)
model = result$model
model$display() # Display the content of the model
plot_vario(varioExp,model = model, pairDisplay = "size",title="Model adjustment for Elevation")

#' # Fit a model with Vecchia without anisotropy and REML
struct_names = c("NUGGET","SPHERICAL", "SPHERICAL")
varioExp = vario_exp(db=db, vname="January_temp", dir=c(30,-30), nlag=20, dlag=10.)
result = model_MaximumLikelihood(db, "January_temp", struct=struct_names, pruneModel = T, nVecchia = 10, anisoModel = F, reml = T)
model = result$model
model$display() # Display the content of the model
plot_vario(varioExp,model = model, pairDisplay = "size",title="Model adjustment for Elevation")
