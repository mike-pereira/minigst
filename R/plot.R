
#' Color palettes
#'
#' Print the list of predefined color palettes in \pkg{gstlearn}.
#'
#'
#' @return Prints the list of color palette names and returns nothing.
#'
#' @export
#'
#' @examples
#' library(minigst)
#'
#' # Print list of basic structures
#' printAllPalettes()
#'
printAllPalettes<-function(){
  rcb <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu",
           "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd",
           "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3",
           "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
  v <- c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo",
         "A", "B", "C", "D", "E", "F", "G", "H")
  print(c(rcb,v))
}



#' Plot variables in a Db
#'
#' Function to plot variables in a Db object as isolated points. The value of the variables are used to define
#' the color, size and/or labels of the points.
#'
#' @param db Db object.
#' @param color Name of the variable defining the color of the points.
#' @param size Name of the variable defining the size of the points.
#' @param label Name of the variable defining the labels of the points.
#' @param sizeRange Vector specifying the range of sizes when plotting the points by size.
#' @param absSize Whether the absolute value of the variable should be used to define the size of the points.
#' @param sizeLegendTitle Title of the size legend.
#' @param cmap Color map used to plot the points by color. Can be specified by a name (cf. \code{\link{printAllPalettes}} to display vailable choices), or by a vector of color names which will be interpolated to create a palette.
#' @param discreteVal Whether the variable defining the color is discrete or not.
#' @param colorLegendTitle Title of the color legend.
#' @param labelColor Name of the color used to write labels.
#' @param pointColor Default color of points when \code{color} is not specified.
#' @param labelLegendTitle Title of the label legend.
#' @param xlab Title of the x-axis.
#' @param ylab Title of the y-axis.
#' @param title Title of plot.
#' @param ... Additional arguments passed to the \pkg{ggplot2} functions \code{geom_point} and \code{geom_text}.
#'
#'
#' @return A \pkg{ggplot2} object containing the plot. If stored in a variable, use the function \code{print} to display the plot.
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
#'
#' # Plot the variables Elevation (for the size of the points) and Longitude (for the color of the points)
#' dbplot_point(db=db,size="Elevation",color="Longitude")
#'
#' # Store the plot, and then display it
#' plt=dbplot_point(db=db,size="Elevation",title="Printed plot")
#' print(plt)
#'
dbplot_point<-function(db,color=NULL,size=NULL,label=NULL,
                       sizeRange=c(1,5),absSize=FALSE,sizeLegendTitle=NULL,
                       cmap=NULL,discreteVal=FALSE,colorLegendTitle=NULL,
                       labelColor="black",pointColor="black",labelLegendTitle=NULL,
                       xlab = NA, ylab = NA, title = NA,...){
  p=ggDefaultGeographic()
  if(is.null(color)){
    p=p+plot.point(db, color=pointColor, nameSize=size, nameLabel=label,
                   sizmin=sizeRange[1], sizmax=sizeRange[2], flagAbsSize = absSize,
                   legendNameSize=sizeLegendTitle,
                   palette=cmap, asFactor=discreteVal,legendNameColor=colorLegendTitle,
                   flagLegendColor=!is.null(color), flagLegendSize=!is.null(size), flagLegendLabel=!is.null(label),
                   legendNameLabel=labelLegendTitle,
                   textColor=labelColor, ...)
    }else{
    p=p+plot.point(db, nameColor=color, nameSize=size, nameLabel=label,
                   sizmin=sizeRange[1], sizmax=sizeRange[2], flagAbsSize = absSize,
                   legendNameSize=sizeLegendTitle,
                   palette=cmap, asFactor=discreteVal,legendNameColor=colorLegendTitle,
                   flagLegendColor=!is.null(color), flagLegendSize=!is.null(size), flagLegendLabel=!is.null(label),
                   legendNameLabel=labelLegendTitle,
                   textColor=labelColor, ...)
    }
  p=p+plot.decoration(xlab = xlab, ylab = ylab, title = title)
  return(p)
}



#' Add lines to an existing plot
#'
#' Function to add lines to an existing plot.
#'
#' @param plt A \pkg{ggplot2} plot object.
#' @param a,b Slope and intercept of the line (single values).
#' @param h Value defining an horizontal line.
#' @param v Value defining a vertical line.
#' @param color Name of the color of the line.
#' @param linetype Type of line, specified by either an integer (0-6) or a name (0 = "blank", 1 = "solid", 2 = "dashed", 3 = "dotted", 4 = "dotdash", 5 = "longdash", 6 = "twodash").
#'
#' @return A \pkg{ggplot2} object containing the updated plot.
#'
#' @export
#'
#' @examples
#' library(minigst)
#'
#' # Load Data and create Db
#' data("Scotland")
#' db=dfToDb(df=Scotland, coordnames=c("Longitude", "Latitude"))
#'
#' # Create a plot of  the variable Elevation
#' plt=dbplot_point(db=db,size="Elevation")
#'
#' # Add  a vertical line to the plot
#' plt=addLines(plt=plt,v=300,color="orange")
#'
#' # Add a  line y = 5 * x +100
#' plt=addLines(plt=plt,a=5,b=100,color="blue",linetype=2)
#'
#' # Display the plot
#' print(plt)
#'
#'
addLines<-function(plt,a=NULL,b=NULL,h=NULL,v=NULL,color="black",linetype=1){

  if((!is.null(a))&&(!is.null(b))){
    plt=plt+geom_abline(slope = a,intercept = b,color=color,linetype=linetype)
  }
  if(!is.null(h)){
    plt=plt+geom_hline(yintercept = h,color=color,linetype=linetype)
  }
  if(!is.null(v)){
    plt=plt+geom_vline(xintercept = v,color=color,linetype=linetype)
  }
  return(plt)

}




#' Plot variables in a Db
#'
#' Function to plot variables in a Db object as isolated points. The value of the variables are used to define
#' the color, size and/or labels of the points.
#'
#' @param dbGrid DbGrid object.
#' @param color Name of the variable defining the color of the grid points.
#' @param contour Name of the variable defining the countours of the plot.
#' @param colValLimits Vector specifying the limits applied to the variable supplied in \code{color}.
#' @param cmap Color map used to plot the points by color. Can be specified by a name (cf. \code{\link{printAllPalettes}} to display vailable choices), or by a vector of color names which will be interpolated to create a palette.
#' @param naColor Color used for NA values.
#' @param colorLegendTitle Title of the color legend.
#' @param contourLegendTitle Title of the contour legend.
#' @param asp Aspect ratio of the plot: 0 = No scaling of the axes, 1 = Axes have the same scale.
#' @param xlab Title of the x-axis.
#' @param ylab Title of the y-axis.
#' @param title Title of plot.
#' @param ... Additional arguments passed to the \pkg{ggplot2} function \code{geom_raster}.
#'
#'
#' @return A \pkg{ggplot2} object containing the plot. If stored in a variable, use the function \code{print} to display the plot.
#'
#' @export
#'
#' @examples
#' library(minigst)
#'
#' # Load Data
#' data("ScotlandGrid")
#'
#' # Create DbGrid
#' dbG=dfToDbGrid(df=ScotlandGrid,coordnames = c("Longitude","Latitude"))
#'
#' # Plot the variables Elevation by color using the "viridis" palette
#' dbplot_grid(dbG,color="Elevation",cmap = "viridis")
#'
#' # Store the plot, and then display it
#' plt= dbplot_grid(dbG,color="Elevation",cmap = "viridis",)title="Printed plot")
#' print(plt)
#'
dbplot_grid<-function(dbGrid,color=NULL, contour=NULL, colValLimits=NULL,
                      cmap=NULL, naColor=NA,
                      colorLegendTitle=NULL,contourLegendTitle=NULL,
                      asp=0,
                      xlab = NA, ylab = NA, title = NA,...){
  p=ggDefaultGeographic()
  p=p+plot.grid(dbGrid, nameRaster=color, nameContour=contour, limits=colValLimits,
                palette=cmap,legendNameRaster=colorLegendTitle, naColor = naColor,
                flagLegendRaster=!is.null(color), flagLegendContour=!is.null(contour),
                legendNameContour=contourLegendTitle,...)
  p=p+plot.geometry(asp=asp)
  p=p+plot.decoration(xlab = xlab, ylab = ylab, title = title)
  return(p)
}





#------------------------------------------------------------


#' Function to plot a single experimental variogram or cross-variogram and overlay a model on top.
#'
#' See \code{plot_vario} for more info on the parameters.
#'
#' @keywords internal
#'
.plot_vario_base<-function(expvario=NA,
                     model=NA,
                     ivar=0, jvar=0, idir=-1,
                     pairDisplay=NA,
                     nlag = 100, hmax = NA,
                     color=NULL, title="", legend=TRUE, ...){

  nvar=expvario$getVariableNumber()
  ndir=expvario$getDirectionNumber()
  if(ivar<(-1) || ivar >= nvar){
    stop(
      paste0("The index 'ivar' should be between -1 and ",nvar-1,".")
    )
  }
  if(jvar<(-1) || jvar >= nvar){
    stop(
      paste0("The index 'jvar' should be between -1 and ",nvar-1,".")
    )
  }
  if(idir<(-1) || idir >= ndir){
    stop(
      paste0("The index 'idir' should be between -1 and ",ndir-1,".")
    )
  }

  if((!is.na(pairDisplay))&&(pairDisplay=="size")){
    drawPsize=T
    drawPlabel=F
  }else if((!is.na(pairDisplay))&&(pairDisplay=="label")){
    drawPsize=F
    drawPlabel=T
  }else{
    drawPsize=-1
    drawPlabel=F
  }

  p=ggplot()
  if(is.null(color)){
    p=p+plot.varmod(vario=expvario,model=model,nh=nlag,hmax=hmax,drawPsize = drawPsize,
                    ivar=ivar, jvar=jvar, idir=idir,
                    drawPlabel = drawPlabel,flagLegend = legend,flagEnvelop = F,...)
  }else{
    p=p+plot.varmod(vario=expvario,model=model,nh=nlag,hmax=hmax,drawPsize = drawPsize,
                    ivar=ivar, jvar=jvar, idir=idir,
                    drawPlabel = drawPlabel,flagLegend = legend,flagEnvelop = F,color=color,...)
  }

  p=p+plot.decoration(xlab = "Distance", ylab="Variogram",
                      title=title)
  return(p)
}



#' Plot experimental variograms and cross-variograms.
#'
#' Function to plot experimental variograms and cross-variograms and overlay a model on top.
#'
#'
#' @param expvario Experimental variogram(s) (as a \pkg{gstlearn} object).
#' @param model Model (as a \pkg{gstlearn} object).
#' @param ivar Rank of the variable to be represented: single value between 0 and (Number of variables -1), or -1 for all variables
#' @param jvar Rank of the second variable to be represented in multivariate case: single value between 0 and (Number of variables -1), or -1 for all variables
#' @param idir Rank of the direction to be represented: single value between 0 and (Number of directions -1), or -1 for all directions
#' @param pairDisplay Display information about the number of pairs used to compute the variogram values: "size" = Size of points, "label" = As a label, NA = No information displayed.
#' @param nlag Number of distance lags (used if 'expvario' is not defined)
#' @param hmax Maximum distance (used if 'expvario' is not defined)
#' @param color Single color to use for all curves.
#' @param title Title of plot.
#' @param legend Whether to plot the legend.
#'
#'
#' @return A \pkg{ggplot2} object containing the plot. If stored in a variable, use the function \code{print} to display the plot.
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
plot_vario<-function(expvario=NA,
                     model=NA,
                     ivar=-1, jvar=-1, idir=-1,
                     pairDisplay=NA,
                     nlag = 100, hmax = NA,
                     color=NULL, title="", legend=TRUE){

  nvar=expvario$getVariableNumber()
  ndir=expvario$getDirectionNumber()
  if(ivar<(-1) || ivar >= nvar){
    stop(
      paste0("The index 'ivar' should be between -1 and ",nvar-1,".")
    )
  }
  if(jvar<(-1) || jvar >= nvar){
    stop(
      paste0("The index 'jvar' should be between -1 and ",nvar-1,".")
    )
  }
  if(idir<(-1) || idir >= ndir){
    stop(
      paste0("The index 'idir' should be between -1 and ",ndir-1,".")
    )
  }

  if((ivar==-1)&&(jvar==-1)&&(nvar>1)){

    ## Case i=-1, j=-1, nvar>1
    pl=list()
    k=1
    for(i in 1:nvar -1){
      for(j in 1:nvar -1){
        if(j<=i){
          pl[[k]]=.plot_vario_base(expvario=expvario,model=model,ivar = i,jvar=j,idir=idir,
                             pairDisplay=pairDisplay, nlag = nlag, hmax = hmax, color=color,
                             title =paste0("(",i,",",j,")"),
                             legend = F)
        }else{
          if((j==nvar-1)&&(i==0)&&legend){
            leg=get_legend(.plot_vario_base(expvario=expvario,model=model,ivar = i,jvar=j,idir=idir,
                                      pairDisplay=pairDisplay, nlag = nlag, hmax = hmax, color=color,
                                      title =paste0("(",i,",",j,")"),
                                      legend = T))
            pl[[k]]=as_ggplot(leg)
          }else{
            pl[[k]]=ggplot()+theme(
              panel.background = element_rect(fill='transparent'), #transparent panel bg
              plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
              panel.grid.major = element_blank(), #remove major gridlines
              panel.grid.minor = element_blank(), #remove minor gridlines
              legend.background = element_rect(fill='transparent'), #transparent legend bg
              legend.box.background = element_rect(fill='transparent')) #transparent legend panel
          }
        }
        k=k+1
      }
    }
    p=ggarrange(plotlist = pl,ncol=nvar,nrow=nvar)
    if(!is.null(title)){
      p=annotate_figure(p, top = text_grob(title, face = "bold", size = 14))
    }

  }else if((jvar==-1)&&(nvar>1)){

    ## Case i=ivar, j=-1, nvar>1
    pl=list()
    k=1
    for(j in 1:nvar-1){
      pl[[k]]=.plot_vario_base(expvario=expvario,model=model,ivar = ivar,jvar=j,idir=idir,
                         pairDisplay=pairDisplay, nlag = nlag, hmax = hmax, color=color,
                         title =paste0("(",ivar,",",j,")"),
                         legend = F)
      k=k+1
    }
    if(legend){
      leg=get_legend(.plot_vario_base(expvario=expvario,model=model,ivar = ivar,jvar=j,idir=idir,
                                pairDisplay=pairDisplay, nlag = nlag, hmax = hmax, color=color,
                                title =paste0("(",ivar,",",j,")"),
                                legend = T))
      pl[[k]]=as_ggplot(leg)
    }
    p=ggarrange(plotlist = pl,ncol=length(pl),nrow=1)
    if(!is.null(title)){
      p=annotate_figure(p, top = text_grob(title, face = "bold", size = 14))
    }

  }else if((ivar==-1)&&(nvar>1)){

    pl=list()
    k=1
    for(i in 1:nvar-1){
      pl[[k]]=.plot_vario_base(expvario=expvario,model=model,ivar = i,jvar=jvar,idir=idir,
                         pairDisplay=pairDisplay, nlag = nlag, hmax = hmax, color=color,
                         title =paste0("(",i,",",jvar,")"),
                         legend = F)
      k=k+1
    }
    if(legend){
      leg=get_legend(.plot_vario_base(expvario=expvario,model=model,ivar = i,jvar=jvar,idir=idir,
                                pairDisplay=pairDisplay, nlag = nlag, hmax = hmax, color=color,
                                title =paste0("(",i,",",jvar,")"),
                                legend = F)+theme(legend.position="bottom"))
      pl[[k]]=as_ggplot(leg)
    }
    p=ggarrange(plotlist = pl,nrow=length(pl),ncol=1)
    if(!is.null(title)){
      p=annotate_figure(p, top = text_grob(title, face = "bold", size = 14))
    }

  }else if(nvar ==1){
    p=.plot_vario_base(expvario=expvario,model=model,ivar = 0,jvar=0,idir=idir,
               pairDisplay=pairDisplay, nlag = nlag, hmax = hmax, color=color,
               title = title,
               legend = legend)
  }else{
    p=.plot_vario_base(expvario=expvario,model=model,ivar = ivar,jvar=jvar,idir=idir,
                 pairDisplay=pairDisplay, nlag = nlag, hmax = hmax, color=color,
                 title =title,
                 legend = legend)
  }

  return(p)
}






