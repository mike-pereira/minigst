
db=obsDb
vname="Xvalid_SK.log_cadmium.esterr"


color="log_cadmium"
vname2="log_zinc"



.defineFill <- function(palette, naColor=NA, flagDiscrete=FALSE, ...)
{
  print("test")
  rcb <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu",
           "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd",
           "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3",
           "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
  v <- c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo",
         "A", "B", "C", "D", "E", "F", "G", "H")
  rcb_num <- 1:18
  
  aes_list = c("colour")
  
  if (flagDiscrete)
  {
    layer = scale_colour_manual(name="colors", values= palette, aesthetics=aes_list,
                                na.value = naColor, ...)
  }
  else
  {
    if (length(palette) == 0)
    {
      layer = scale_colour_gradient(na.value=naColor, aesthetics=aes_list,...)
    }
    else if(length(palette) == 1)
    {
      if (any(palette == rcb) | any(palette == rcb_num))
      {
        layer = scale_colour_distiller(palette=palette, aesthetics=aes_list,
                                       na.value=naColor, ...)
      }
      else if(any(palette == v))
      {
        layer = scale_colour_viridis_c(option=palette, aesthetics=aes_list,
                                       na.value = naColor, ...)
      }else{
        print("hello")
      }
    }
    else
    {
      layer = scale_colour_gradientn(colors=palette, aesthetics=aes_list,
                                     na.value=naColor, ...)
    }
  }
  layer
}



getSeq<-function(v,n,nd=3){
  u=floor(seq(from=floor(v[1]*10**nd),to=ceiling(v[2]*10**nd),length.out=n))/10**nd
  return(u)
}

newplot<-function(db,color=NULL,size=NULL,label=NULL,
                  cat_color=NULL,
                  sizeRange=c(0.5,3),absSize=FALSE,sizeLegendTitle=NULL,
                  cmap=NULL,discreteVal=FALSE,colorLegendTitle=NULL,
                  labelColor="black",pointColor="black",labelLegendTitle=NULL,
                  xlab = NULL, ylab = NULL, title = NULL, add = FALSE){
  
  if(is.logical(add)){
    if(add){
      p = last_plot()
    }else{
      p=ggDefaultGeographic()
    }
  }else{
    p = add
  }
  
  df=db[]
  xname=db$getNamesByLocator(ELoc_X())
  aes_plt=aes(x=.data[[xname[1]]],y=.data[[xname[2]]])
  
  discreteVal=FALSE
  if(!is.null(color)){
      aes_plt$colour = substitute(.data[[color]])
  }

  if(!is.null(cat_color)){
    if(!is.null(color)){
      stop("You can either specify the `color` argument or the `cat_color` argument, but not both.")
    }else{
      discreteVal=TRUE
      aes_plt$colour = substitute(factor(.data[[cat_color]]))
      ncol=length(levels(factor(df[cat_color])))
      if(is.null(cmap)){
        cmap=1:ncol
      }else if(length(cmap)<ncol){
        stop(paste0("When using discrete color maps (`discreteVal=TRUE`), the argument `cmap` must be either `NULL` or a vector of ",ncol," colors. However, only ",length(cmap)," colors were specified."))
      }
    }
  }
  
  if(!is.null(cmap)){
    p = p+new_scale("colour")
    p =p + .defineColour(cmap,flagDiscrete=discreteVal)
  }else{
    p = p+new_scale("colour")
    p =p + .defineColour("viridis",flagDiscrete=discreteVal)
  }
  
  
  if(!is.null(size)){
    id_not_NA=which(!is.na(df[,size]))
    df=df[id_not_NA,]
    print(id_not_NA)
    if(absSize){
      aes_plt$size = substitute(abs(.data[[size]]))
      valSize=abs(df[,size])
    }else{
      aes_plt$size = substitute(.data[[size]])
      valSize=df[,size]
    }
    p = p+new_scale("size")
    breaks=getSeq(range(valSize,na.rm=TRUE),5)
    print(breaks)
    p =p + scale_size(breaks = breaks,limits=range(breaks),range = sizeRange)
  }
  
  print(nrow(df))                      
  p=p+geom_point(data=df,aes_plt)

  return(p)
}






p=dbplot_point(targetDb, color='x',
              title='Ordinary kriging: Predictions',colorLegendTitle = "hd")
p=newplot(obsDb,size="log_cadmium",add=p,sizeRange = c(0.5,3),absSize = T)
p=newplot(obsDb,size="log_zinc",sizeRange = c(0.5,3),absSize = F,pointColor = "red",add=p)
p=addPoints(p,x=180000+100*rnorm(5),y=333000+100*rnorm(5))
p






dbplot_grid<-function(dbGrid,color=NULL, contour=NULL, colValLimits=NULL,
                      cmap='viridis', naColor=NA,
                      colorLegendTitle=NULL,contourLegendTitle=NULL,
                      asp=0,
                      xlab = NULL, ylab = NULL, title = NULL,...){
  p=ggDefaultGeographic()
  
  
  p=p+plot.grid(dbGrid, nameRaster=color, nameContour=contour, limits=colValLimits,
                palette=cmap,legendNameRaster=colorLegendTitle, naColor = naColor,
                flagLegendRaster=!is.null(color), flagLegendContour=!is.null(contour),
                legendNameContour=contourLegendTitle,...)
  
  
  p=p+plot.geometry(asp=asp)
  if(is.null(xlab)){
    xlab=Db_getNamesByLocator(dbGrid,ELoc_X())[1]
  }
  if(is.null(ylab)){
    ylab=Db_getNamesByLocator(dbGrid,ELoc_X())[2]
  }
  if(is.null(title)){
    p=p+plot.decoration(xlab = xlab, ylab = ylab,title = "")
  }else{
    p=p+plot.decoration(xlab = xlab, ylab = ylab, title = title)
  }
  return(p)
}















