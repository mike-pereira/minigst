import gstlearn.document as gdoc
import gstlearn as gl
import pandas as pd


def data(name):
    """
    Return data contained in the package
    
    Args:
        name: name of the data set
        
    Returns:
        various objects according to the case.
    
    Examples:
        >>> import minigst as mg
        >>> dat, grid = mg.data("Scotland") # 2 pandas dataframes
        >>> dbgrid = mg.data("ScotlandGrid") # a gstlearn.DbGrid object
    """
    if name == "Scotland":  
        temp_nf = gdoc.loadData("Scotland", "Scotland_Temperatures.csv")
        dat = pd.read_csv(temp_nf, na_values = "MISS")
        temp_nf = gdoc.loadData("Scotland", "Scotland_Elevations.csv")
        grid = pd.read_csv(temp_nf)
        return dat,grid
    if name == "ScotlandGrid":
        temp_nf = gdoc.loadData("Scotland", "Scotland_Elevations.NF")
        grid = gl.DbGrid.createFromNF(temp_nf)
        return grid
    print("No data named " + name + " in the minigst package")
   
