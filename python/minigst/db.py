"""Database operations for minigst package."""

import numpy as np
import pandas as pd
import gstlearn as gl


def _check_if_reg_step(yseq, tol_rel_error=1e-6):
    """
    Check if a vector properly defines a set of grid coordinates.
    
    Args:
        yseq: Vector of coordinates
        tol_rel_error: Tolerance value to accept that two values are equal
        
    Returns:
        Boolean indicating if values are sorted and regularly spaced
    """
    if len(yseq) >= 3:
        dff = np.diff(yseq)
        return (np.max(np.diff(dff)) / (yseq[1] - yseq[0]) < tol_rel_error) and (np.min(dff) > 0)
    else:
        return True


def df_to_db(df, coord_names, is_grid=False):
    """
    Create a Db object from a pandas DataFrame.
    
    Args:
        df: pandas DataFrame containing the data
        coord_names: List of column names to use as coordinates
        is_grid: Boolean, if True creates a DbGrid instead of Db
        
    Returns:
        A gstlearn Db or DbGrid object
        
    Examples:
        >>> import pandas as pd
        >>> import minigst as mg
        >>> df = pd.DataFrame({'x': [0, 1, 2], 'y': [0, 1, 2], 'z': [1, 2, 3]})
        >>> db = mg.df_to_db(df, coord_names=['x', 'y'])
    """
    if not isinstance(df, pd.DataFrame):
        raise ValueError("The argument 'df' must be a pandas DataFrame.")
    
    # Check coordinate names
    var_names = list(df.columns)
    if not all(coord in var_names for coord in coord_names):
        raise ValueError("Check the variable names in coord_names: one or several names are absent from the dataframe.")
    
    if not is_grid:
        # Create Db
        data = gl.Db()
        for vn in var_names:
            data[vn] = df[vn].values
        # Set coordinates
        data.setLocators(coord_names, gl.ELoc.X)
        return data
    else:
        return df_to_db_grid(df, coord_names)


def df_to_db_grid(df, coord_names):
    """
    Create a DbGrid object from a pandas DataFrame.
    
    Args:
        df: pandas DataFrame containing the grid data
        coord_names: List of column names to use as coordinates
        
    Returns:
        A gstlearn DbGrid object
        
    Examples:
        >>> import pandas as pd
        >>> import minigst as mg
        >>> df = pd.DataFrame({'x': [0, 0, 1, 1], 'y': [0, 1, 0, 1], 'z': [1, 2, 3, 4]})
        >>> db_grid = mg.df_to_db_grid(df, coord_names=['x', 'y'])
    """
    if not isinstance(df, pd.DataFrame):
        raise ValueError("The argument 'df' must be a pandas DataFrame.")
    
    var_names = list(df.columns)
    if not all(coord in var_names for coord in coord_names):
        raise ValueError("Check the variable names in coord_names: one or several names are absent from the dataframe.")
    
    tol_diff = 1e-6
    ndim = len(coord_names)
    
    # Extract unique coordinates for each dimension
    nx = []
    dx = []
    x0 = []
    
    for coord in coord_names:
        coord_values = np.sort(df[coord].unique())
        nx.append(len(coord_values))
        
        if len(coord_values) > 1:
            dx_val = coord_values[1] - coord_values[0]
            dx.append(dx_val)
        else:
            dx.append(1.0)
        
        x0.append(coord_values[0])
    
    # Create DbGrid
    db_grid = gl.DbGrid.create(nx=nx, dx=dx, x0=x0)
    
    # Set coordinate names
    for i, coord in enumerate(coord_names):
        db_grid.setName(f"x{i+1}", coord)
    
    # Add other variables
    for vn in var_names:
        if vn not in coord_names:
            # Match grid indices
            vals = np.full(db_grid.getSampleNumber(), np.nan)
            for idx, row in df.iterrows():
                # Find corresponding grid index
                coords = [row[c] for c in coord_names]
                # Simple matching for regular grids
                grid_idx = 0
                for i, c in enumerate(coords):
                    grid_pos = int(np.round((c - x0[i]) / dx[i]))
                    if i == 0:
                        grid_idx = grid_pos
                    else:
                        grid_idx += grid_pos * np.prod(nx[:i])
                if 0 <= grid_idx < len(vals):
                    vals[grid_idx] = row[vn]
            db_grid[vn] = vals
    
    return db_grid


def create_db_grid(coords=None, nx=None, dx=None, x0=None, coord_names=None):
    """
    Create an empty DbGrid.
    
    Args:
        coords: List of coordinate arrays for each dimension
        nx: List of grid sizes in each dimension
        dx: List of grid steps in each dimension
        x0: List of grid origins in each dimension
        coord_names: List of coordinate names
        
    Returns:
        A gstlearn DbGrid object
        
    Examples:
        >>> import numpy as np
        >>> import minigst as mg
        >>> # Using coordinate arrays
        >>> xseq = np.linspace(0, 1, 100)
        >>> yseq = np.linspace(0, 1, 100)
        >>> db_grid = mg.create_db_grid(coords=[xseq, yseq], coord_names=['x', 'y'])
        >>> # Using grid parameters
        >>> db_grid = mg.create_db_grid(nx=[100, 100], dx=[0.01, 0.01], x0=[0, 0])
    """
    if coords is not None:
        if not isinstance(coords, list):
            raise ValueError("coords must be a list of coordinate arrays.")
        
        # Check if coordinates are regularly spaced
        for coord in coords:
            if not _check_if_reg_step(coord):
                raise ValueError("Check the coordinates supplied: one or several are not sorted in increasing order or not regularly spaced.")
        
        ndim = len(coords)
        if coord_names is not None:
            if len(coord_names) != ndim:
                raise ValueError(f"The size of coord_names ({len(coord_names)}) must match coords ({ndim}).")
        
        # Extract grid parameters from coordinates
        nx = [len(coord) for coord in coords]
        dx = [coord[1] - coord[0] if len(coord) > 1 else 1.0 for coord in coords]
        x0 = [coord[0] for coord in coords]
    
    elif nx is not None:
        if not isinstance(nx, (list, tuple, np.ndarray)):
            nx = [nx]
        
        ndim = len(nx)
        
        if dx is None:
            dx = [1.0 / (n - 1) if n > 1 else 1.0 for n in nx]
        elif not isinstance(dx, (list, tuple, np.ndarray)):
            dx = [dx] * ndim
        
        if x0 is None:
            x0 = [0.0] * ndim
        elif not isinstance(x0, (list, tuple, np.ndarray)):
            x0 = [x0] * ndim
        
        if coord_names is not None and len(coord_names) != ndim:
            raise ValueError(f"The size of coord_names ({len(coord_names)}) must match nx ({ndim}).")
    
    else:
        raise ValueError("Either coords or nx must be specified.")
    
    # Create DbGrid
    db_grid = gl.DbGrid.create(nx=nx, dx=dx, x0=x0)
    
    # Set coordinate names if provided
    if coord_names is not None:
        for i, name in enumerate(coord_names):
            db_grid.setName(f"x{i+1}", name)
    
    return db_grid


def add_var_to_db(db, var, vname):
    """
    Add variable(s) to a Db object.
    
    Args:
        db: gstlearn Db object
        var: Array or matrix of values to add
        vname: Name(s) of the variable(s) to add
        
    Examples:
        >>> import numpy as np
        >>> import minigst as mg
        >>> # Create a Db
        >>> db = mg.create_db_grid(nx=[10, 10])
        >>> # Add a variable
        >>> values = np.random.randn(100)
        >>> mg.add_var_to_db(db, values, 'random_field')
    """
    if isinstance(var, np.ndarray):
        if var.ndim == 1:
            # Single variable
            if isinstance(vname, str):
                db[vname] = var
            else:
                raise ValueError("vname must be a string for a 1D array.")
        elif var.ndim == 2:
            # Multiple variables
            if isinstance(vname, (list, tuple)) and len(vname) == var.shape[1]:
                for i, name in enumerate(vname):
                    db[name] = var[:, i]
            else:
                raise ValueError("vname must be a list with length matching the number of columns.")
        else:
            raise ValueError("var must be 1D or 2D array.")
    else:
        # Try to convert to array
        var = np.array(var)
        add_var_to_db(db, var, vname)


def del_var_from_db(db, vname):
    """
    Delete variable(s) from a Db object.
    
    Args:
        db: gstlearn Db object
        vname: Name(s) of the variable(s) to delete (string or list of strings)
        
    Examples:
        >>> import minigst as mg
        >>> # Assuming db has a variable 'temp'
        >>> mg.del_var_from_db(db, 'temp')
    """
    if isinstance(vname, str):
        vname = [vname]
    
    for name in vname:
        db.deleteColumn(name)


def summary_stats(db, vname, stat=None, only_common=False):
    """
    Compute summary statistics for variables in a Db.
    
    Args:
        db: gstlearn Db object
        vname: Variable name(s) (string or list of strings)
        stat: List of statistics to compute (default: all available)
        only_common: If True, only use rows without any NA values
        
    Returns:
        pandas DataFrame with statistics
        
    Examples:
        >>> import minigst as mg
        >>> stats = mg.summary_stats(db, vname=['var1', 'var2'])
    """
    if isinstance(vname, str):
        vname = [vname]
    
    if stat is None:
        stat = ["NUM", "MIN", "MAX", "MEAN", "STDV", "MED"]
    
    # Get data from Db
    db_dict = {}
    for name in vname:
        if name in db.getAllNames():
            db_dict[name] = db[name]
        else:
            raise ValueError(f"Variable {name} not found in Db.")
    
    df = pd.DataFrame(db_dict)
    
    # Apply selection if it exists
    sel_names = db.getNamesByLocator(gl.ELoc.SEL)
    if len(sel_names) > 0:
        sel_values = db[sel_names[0]]
        df = df[sel_values == 1]
    
    if only_common:
        df = df.dropna()
    
    # Compute statistics
    results = {}
    for s in stat:
        if s == "NUM":
            results[s] = df.count()
        elif s == "MIN":
            results[s] = df.min()
        elif s == "MAX":
            results[s] = df.max()
        elif s == "MEAN":
            results[s] = df.mean()
        elif s == "STDV":
            results[s] = df.std()
        elif s == "MED":
            results[s] = df.median()
    
    return pd.DataFrame(results)
