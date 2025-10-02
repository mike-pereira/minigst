"""Kriging functions for minigst package."""

import numpy as np
import gstlearn as gl


def _create_neigh(neigh):
    """
    Create a gstlearn neighborhood object.
    
    Args:
        neigh: Neighborhood specification (string or numeric)
        
    Returns:
        gstlearn Neigh object
    """
    if isinstance(neigh, str):
        if neigh == "unique":
            return gl.NeighUnique.create()
        else:
            raise ValueError("Invalid neighborhood string. Use 'unique' or provide numeric parameters.")
    
    if isinstance(neigh, (int, float)):
        # Single radius
        return gl.NeighMoving.create(radius=float(neigh))
    
    if isinstance(neigh, (list, tuple, np.ndarray)):
        if len(neigh) == 1:
            # Single radius
            return gl.NeighMoving.create(radius=float(neigh[0]))
        elif len(neigh) == 2:
            # nmin, nmax
            return gl.NeighMoving.create(nmini=int(neigh[0]), nmaxi=int(neigh[1]))
        elif len(neigh) == 3:
            # nmin, nmax, radius
            return gl.NeighMoving.create(nmini=int(neigh[0]), nmaxi=int(neigh[1]), 
                                        radius=float(neigh[2]))
        else:
            raise ValueError("Invalid neighborhood specification.")
    
    raise ValueError("The neighborhood definition is not valid.")


def minikriging(dbin, dbout, vname, model, type="ordinary", pol_drift=None, 
                ext_drift=None, mean=None, neighborhood="unique", std=True, prefix="K"):
    """
    Compute kriging predictions.
    
    Args:
        dbin: gstlearn Db object with observation data
        dbout: gstlearn Db object for target predictions
        vname: Variable name(s) to predict (string or list)
        model: gstlearn Model object
        type: Kriging type ('simple' or 'ordinary')
        pol_drift: Polynomial drift order for universal kriging
        ext_drift: External drift variable name(s) for universal kriging
        mean: Mean value for simple kriging
        neighborhood: Neighborhood specification ('unique' or numeric)
        std: Boolean, compute standard deviations
        prefix: Prefix for output variable names
        
    Returns:
        None (results added directly to dbout)
        
    Examples:
        >>> import minigst as mg
        >>> mg.minikriging(obs_db, target_db, vname='temperature', 
        ...                model=model, type='ordinary', std=True)
    """
    # Set variable as Z locator
    if isinstance(vname, str):
        vname = [vname]
    
    for vn in vname:
        dbin.setLocator(vn, gl.ELoc.Z)
    
    # Create neighborhood
    neigh = _create_neigh(neighborhood)
    
    # Handle drifts
    if ext_drift is not None or pol_drift is not None:
        # Universal kriging
        model_copy = model.clone()
        
        if pol_drift is not None:
            # Add polynomial drift
            model_copy.setDriftIRF(pol_drift)
        
        if ext_drift is not None:
            if isinstance(ext_drift, str):
                ext_drift = [ext_drift]
            
            # Set external drifts
            for drift in ext_drift:
                dbin.setLocator(drift, gl.ELoc.F)
                dbout.setLocator(drift, gl.ELoc.F)
            
            # Add drift to model
            for _ in ext_drift:
                model_copy.addDrift(gl.DriftM())
        
        # Perform kriging
        err = gl.kriging(dbin, dbout, model_copy, neigh,
                        flag_est=True, flag_std=std,
                        namconv=gl.NamingConvention(prefix))
    
    elif type == "simple":
        # Simple kriging
        if mean is None:
            raise ValueError("Mean must be specified for simple kriging.")
        
        model_copy = model.clone()
        model_copy.setMean(mean)
        
        err = gl.kriging(dbin, dbout, model_copy, neigh,
                        flag_est=True, flag_std=std,
                        namconv=gl.NamingConvention(prefix))
    
    else:
        # Ordinary kriging
        model_copy = model.clone()
        model_copy.addDrift(gl.DriftM())
        
        err = gl.kriging(dbin, dbout, model_copy, neigh,
                        flag_est=True, flag_std=std,
                        namconv=gl.NamingConvention(prefix))


def minixvalid(dbin, vname, model, type="ordinary", pol_drift=None,
               ext_drift=None, mean=None, neighborhood="unique", std=True, prefix="Xvalid"):
    """
    Compute kriging cross-validation.
    
    Args:
        dbin: gstlearn Db object with observation data
        vname: Variable name(s) to validate (string or list)
        model: gstlearn Model object
        type: Kriging type ('simple' or 'ordinary')
        pol_drift: Polynomial drift order for universal kriging
        ext_drift: External drift variable name(s) for universal kriging
        mean: Mean value for simple kriging
        neighborhood: Neighborhood specification ('unique' or numeric)
        std: Boolean, compute standardized errors
        prefix: Prefix for output variable names
        
    Returns:
        None (results added directly to dbin)
        
    Examples:
        >>> import minigst as mg
        >>> mg.minixvalid(obs_db, vname='temperature', model=model, 
        ...               type='ordinary', std=True)
    """
    # Set variable as Z locator
    if isinstance(vname, str):
        vname = [vname]
    
    for vn in vname:
        dbin.setLocator(vn, gl.ELoc.Z)
    
    # Create neighborhood
    neigh = _create_neigh(neighborhood)
    
    # Handle drifts
    if ext_drift is not None or pol_drift is not None:
        # Universal kriging
        model_copy = model.clone()
        
        if pol_drift is not None:
            model_copy.setDriftIRF(pol_drift)
        
        if ext_drift is not None:
            if isinstance(ext_drift, str):
                ext_drift = [ext_drift]
            
            for drift in ext_drift:
                dbin.setLocator(drift, gl.ELoc.F)
            
            for _ in ext_drift:
                model_copy.addDrift(gl.DriftM())
        
        # Perform cross-validation
        err = gl.xvalid(dbin, model_copy, neigh,
                       flag_xvalid_est=True, flag_xvalid_std=std,
                       namconv=gl.NamingConvention(prefix))
    
    elif type == "simple":
        # Simple kriging
        if mean is None:
            raise ValueError("Mean must be specified for simple kriging.")
        
        model_copy = model.clone()
        model_copy.setMean(mean)
        
        err = gl.xvalid(dbin, model_copy, neigh,
                       flag_xvalid_est=True, flag_xvalid_std=std,
                       namconv=gl.NamingConvention(prefix))
    
    else:
        # Ordinary kriging
        model_copy = model.clone()
        model_copy.addDrift(gl.DriftM())
        
        err = gl.xvalid(dbin, model_copy, neigh,
                       flag_xvalid_est=True, flag_xvalid_std=std,
                       namconv=gl.NamingConvention(prefix))


def kriging_mean(db, vname, model):
    """
    Calculate the kriging mean of a variable.
    
    Args:
        db: gstlearn Db object
        vname: Variable name
        model: gstlearn Model object
        
    Returns:
        Float value representing the kriging mean
        
    Examples:
        >>> import minigst as mg
        >>> km = mg.kriging_mean(db, vname='elevation', model=model)
    """
    # Set variable
    db.setLocator(vname, gl.ELoc.Z)
    
    # Create selection for non-NA values
    values = db[vname]
    sel = ~np.isnan(values)
    db["_sel_temp"] = sel.astype(float)
    db.setLocator("_sel_temp", gl.ELoc.SEL)
    
    # Create target at data points
    coords = []
    coord_names = db.getNamesByLocator(gl.ELoc.X)
    for name in coord_names:
        coords.append(db[name][sel])
    
    # Create a simple Db for targets
    tgt = gl.Db()
    for i, name in enumerate(coord_names):
        tgt[name] = coords[i]
    tgt.setLocators(coord_names, gl.ELoc.X)
    
    # Unique neighborhood
    neighU = gl.NeighUnique.create()
    
    # Simple Kriging with mean 0
    mod0 = model.clone()
    mod0.setMean(0.0)
    err = gl.kriging(db, tgt, mod0, neighU, flag_est=True, flag_std=False,
                    namconv=gl.NamingConvention("SK0"))
    
    # Simple Kriging with mean 1
    mod1 = model.clone()
    mod1.setMean(1.0)
    err = gl.kriging(db, tgt, mod1, neighU, flag_est=True, flag_std=False,
                    namconv=gl.NamingConvention("SK1"))
    
    # Ordinary Kriging
    modOK = model.clone()
    modOK.addDrift(gl.DriftM())
    err = gl.kriging(db, tgt, modOK, neighU, flag_est=True, flag_std=False,
                    namconv=gl.NamingConvention("OK"))
    
    # Calculate kriging mean
    ok_vals = tgt[tgt.getNamesByLocator(gl.ELoc.Z)[0]]
    sk0_vals = tgt["SK0." + vname + ".estim"]
    sk1_vals = tgt["SK1." + vname + ".estim"]
    
    km = np.mean((ok_vals - sk0_vals) / (sk1_vals - sk0_vals))
    
    # Cleanup
    db.deleteColumn("_sel_temp")
    
    return km


def set_mean(model, mu):
    """
    Set the mean value in a Model object.
    
    Args:
        model: gstlearn Model object
        mu: Mean value to set
        
    Examples:
        >>> import minigst as mg
        >>> mg.set_mean(model, 10.5)
    """
    model.setMean(mu)
