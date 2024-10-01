
.defineColour <- function(palette, naColor="transparent", flagDiscrete=FALSE, limits=NULL,title=NA)
{
  rcb <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu",
           "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd",
           "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3",
           "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
  v <- c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo",
         "A", "B", "C", "D", "E", "F", "G", "H")
  rcb_num <- 1:18
  
  aes_list = c("color")
  
  name=waiver()
  if(!is.na(title)){
    if(is.character(title)){
      name=title
    }
  }
  
  if (flagDiscrete)
  {
    layer = scale_colour_manual(name="colors", values= palette, aesthetics=aes_list,
                              na.value = naColor,name=name)
  }
  else
  {
    if (length(palette) == 0)
    {
      layer = scale_colour_viridis_c(option="viridis", aesthetics=aes_list,
                                     na.value = naColor, limits=limits,name=name)
      # layer = scale_colour_gradient(na.value=naColor, aesthetics=aes_list, limits=limits)
    }
    else if(length(palette) == 1)
    {
      if (any(palette == rcb) | any(palette == rcb_num))
      {
        layer = scale_colour_distiller(palette=palette, aesthetics=aes_list,
                                     na.value=naColor,name=name)
      }
      else if(any(palette == v))
      {
        layer = scale_colour_viridis_c(option=palette, aesthetics=aes_list,
                                     na.value = naColor, limits=limits,name=name)
      }else{
        layer = scale_colour_viridis_c(option="viridis", aesthetics=aes_list,
                                     na.value = naColor, limits=limits,name=name)
      }
    }
    else
    {
      layer = scale_colour_gradientn(colours=palette, aesthetics=aes_list,
                                   na.value=naColor, limits=limits,name=name)
    }
  }
  return(layer)
}

.defineFill <- function(palette, naColor="transparent", flagDiscrete=FALSE, limits=NULL,title=NA)
{
  
  rcb <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu",
           "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd",
           "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3",
           "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
  v <- c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo",
         "A", "B", "C", "D", "E", "F", "G", "H")
  rcb_num <- 1:18
  
  aes_list = c("fill")
  
  name=waiver()
  if(!is.na(title)){
    if(is.character(title)){
      name=title
    }
  }
  
  if (flagDiscrete)
  {
    layer = scale_fill_manual(values= palette, 
                                na.value = naColor,name=name)
  }
  else
  {
    if (length(palette) == 0)
    {
      layer = scale_fill_viridis_c(option="viridis", aesthetics=aes_list,
                                   na.value = naColor, limits=limits,name=name)
      l#ayer = scale_fill_gradient(na.value=naColor, aesthetics=aes_list, limits=limits)
    }
    else if(length(palette) == 1)
    {
      if (any(palette == rcb) | any(palette == rcb_num))
      {
        layer = scale_fill_distiller(palette=palette, aesthetics=aes_list,
                                       na.value=naColor, limits=limits,name=name)
      }
      else if(any(palette == v))
      {
        layer = scale_fill_viridis_c(option=palette, aesthetics=aes_list,
                                       na.value = naColor, limits=limits,name=name)
      }else{
        layer = scale_fill_viridis_c(option="viridis", aesthetics=aes_list,
                                     na.value = naColor, limits=limits,name=name)
      }
    }
    else
    {
      layer = scale_fill_gradientn(colours=palette, aesthetics=aes_list,
                                     na.value=naColor, limits=limits,name=name)
    }
  }
  return(layer)
}

dbplot_grid2<-function(db,color=NULL,cat_color=NULL,contour=NULL,
                       cmap=NULL,colValLimits=NULL,naColor="transparent",
                       nBins=NULL,
                       hideLegend=FALSE, legendTitle=NA,
                       asp=0,xlab = NULL, ylab = NULL, title = NULL, add = FALSE
                       ){
  
  if((!is.null(color))+(!is.null(cat_color))+(!is.null(contour))>1){
    stop("You must specify only one of the following arguments: color, cat_color, contour")
  }
  
  if(is.logical(add)){
    if(add){
      p = last_plot()
    }else{
      p=ggDefaultGeographic()
    }
  }else{
    p = add
  }
  
  xname=db$getNamesByLocator(ELoc_X())
  aes_plt=aes(x=.data[[xname[1]]],y=.data[[xname[2]]])
  
  discreteVal=FALSE
  if(!is.null(color)){
    aes_plt$fill = substitute(.data[[color]])
  }
  if(!is.null(cat_color)){
    if(!is.null(color)){
      stop("You can either specify the `color` argument or the `cat_color` argument, but not both.")
    }else{
      discreteVal=TRUE
      aes_plt$fill = substitute(factor(.data[[cat_color]]))
      ncol=length(levels(factor(db[cat_color])))
      if(is.null(cmap)){
        cmap=1:ncol
      }else if(length(cmap)<ncol){
        stop(paste0("When using discrete color maps (`discreteVal=TRUE`), the argument `cmap` must be either `NULL` or a vector of ",ncol," colors. However, only ",length(cmap)," colors were specified."))
      }
    }
  }

  if(!is.null(color)||!is.null(cat_color)){
    p=p+geom_raster(data=db[],aes_plt,show.legend=ifelse(hideLegend,FALSE,NA)) 
    if(!is.null(cmap)){
      p =p + .defineFill(cmap,flagDiscrete=discreteVal,naColor = naColor,limits=colValLimits,title = legendTitle)
      p = p+new_scale("fill")
    }else{
      p =p + .defineFill("viridis",flagDiscrete=discreteVal,naColor = naColor,limits=colValLimits,title = legendTitle)
      p = p+new_scale("fill")
    }
  }else if(!is.null(contour)){
    p=p+geom_contour(data=db[],
                     aes(x=.data[[xname[1]]],y=.data[[xname[2]]],
                         z=.data[[contour]],colour=after_stat(level)
                         ),
                     bins = nBins,
                     na.rm=TRUE,show.legend=ifelse(hideLegend,FALSE,NA))
    if(!is.null(cmap)){
      p =p + .defineColour(cmap,title = legendTitle)
      p = p+new_scale("colour")
    }else{
      p =p + .defineColour("viridis",title = legendTitle)
      p = p+new_scale("colour")
    }
  }else{
    stop("You must specify one of the following arguments: color, cat_color, contour")
  }
  
  
  p=p+plot.geometry(asp=asp)
  if(is.null(xlab)){
    xlab=Db_getNamesByLocator(db,ELoc_X())[1]
  }
  if(is.null(ylab)){
    ylab=Db_getNamesByLocator(db,ELoc_X())[2]
  }
  if(is.null(title)){
    p=p+plot.decoration(xlab = xlab, ylab = ylab,title = "")
  }else{
    p=p+plot.decoration(xlab = xlab, ylab = ylab, title = title)
  }
  
  
  return(p)
  
}

getSeq<-function(v,n,nd=3){
  u=floor(seq(from=floor(v[1]*10**nd),to=ceiling(v[2]*10**nd),length.out=n))/10**nd
  u=pretty(v,n)
  return(u)
}


dbplot_point2<-function(db,color=NULL,size=NULL,
                  cat_color=NULL,
                  sizeRange=c(0.5,3),absSize=FALSE,
                  cmap=NULL,colValLimits=NULL,naColor="transparent",pointColor="black",
                  hideLegend=FALSE, legendTitle=NA,
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
  
  if(!is.null(color)||!is.null(cat_color)){
    colorLegendTitle=legendTitle[1]
    if(!is.null(cmap)){
      p = p+new_scale("colour")
      p =p + .defineColour(cmap,flagDiscrete=discreteVal,naColor = naColor,limits=colValLimits,title = colorLegendTitle)
    }else{
      p = p+new_scale("colour")
      p =p + .defineColour("viridis",flagDiscrete=discreteVal,naColor = naColor,limits=colValLimits,title = colorLegendTitle)
    }
  }
  
  if(!is.null(size)){
    
    if(!is.null(color)||!is.null(cat_color)){
      sizeLegendTitle=legendTitle[2]
    }else{
      sizeLegendTitle=legendTitle[1]
    }
    name=waiver()
    if(!is.na(sizeLegendTitle)){
      if(is.character(sizeLegendTitle)){
        name=sizeLegendTitle
      }
    }
    
    
    id_not_NA=which(!is.na(df[,size]))
    df=df[id_not_NA,]
    if(absSize){
      aes_plt$size = substitute(abs(.data[[size]]))
      valSize=abs(df[,size])
    }else{
      aes_plt$size = substitute(.data[[size]])
      valSize=df[,size]
    }
    p = p+new_scale("size")
    breaks=getSeq(range(valSize,na.rm=TRUE),5)
    p =p + scale_size(breaks = breaks,limits=range(breaks),range = sizeRange,name=name)
  }
  
  p=p+geom_point(data=df,aes_plt,show.legend=ifelse(hideLegend,FALSE,NA))
  
  return(p)
}

p=dbplot_grid2(targetDb,color="dist",cmap="magma",
               colValLimits = c(0,1))
#p=dbplot_grid2(targetDb,contour="dist",cmap="RdBu",legendTitle = "toto",add=p)
#p=dbplot_grid2(targetDb,cat_color="cat",cmap=c("blue","yellow"),add=p,colorLegendTitle = "toto")
p=dbplot_point2(obsDb,size="log_zinc",color="log_cadmium",add=p,cmap = "viridis",legendTitle = c(NA,""))
p


