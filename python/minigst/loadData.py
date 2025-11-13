import gstlearn.document as gdoc
import gstlearn as gl
import pandas as pd

import importlib.resources as pkg_resources
from pathlib import Path
from . import datafiles

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
    if name == "Meuse":
        file_path = pkg_resources.files(datafiles) / f"Meuse/meuse.csv"
        meuse = pd.read_csv(file_path)
        file_path = pkg_resources.files(datafiles) / f"Meuse/meuse.grid.csv"
        meuse_grid = pd.read_csv(file_path)
        return meuse, meuse_grid
    if name == "Jura":
       file_path = pkg_resources.files(datafiles) / f"Jura/jura_val_loc.csv"
       jura_val_loc = pd.read_csv(file_path)
       file_path = pkg_resources.files(datafiles) / f"Jura/jura_grid.csv"
       jura_grid = pd.read_csv(file_path)
       file_path = pkg_resources.files(datafiles) / f"Jura/jura_pred.csv"
       jura_pred = pd.read_csv(file_path)
       return jura_pred, jura_grid, jura_val_loc
    print("No data named " + name + " in the minigst package")
   
