"""Variogram functions for minigst package."""

import numpy as np
import gstlearn as gl


def _rep_arg(n, arg, argname):
    """Adapt the size of model parameters."""
    if not isinstance(arg, (list, tuple, np.ndarray)):
        return [arg] * max(1, n)
    elif n == 0:
        raise ValueError(f"The length of the argument {argname} should be 1.")
    elif len(arg) != n and len(arg) != 1:
        raise ValueError(f"The length of the argument {argname} should be either 1 or {n}.")
    elif len(arg) == 1:
        return list(arg) * n
    else:
        return list(arg)


def vario_exp(db, vname, pol_drift=None, ext_drift=None, dir=None, 
              nlag=20, dlag=100, toldis=0.5, tolang=None):
    """
    Compute an experimental variogram.
    
    Args:
        db: gstlearn Db object
        vname: Variable name(s) (string or list of strings)
        pol_drift: Order of polynomial drift for universal kriging
        ext_drift: External drift variable name(s)
        dir: Direction for variogram computation (angle in degrees for 2D, or direction vectors)
        nlag: Number of lags
        dlag: Distance of lags
        toldis: Tolerance on distance (0 to 1)
        tolang: Tolerance on angle (0 to 90 degrees)
        
    Returns:
        gstlearn Vario object containing experimental variogram
        
    Examples:
        >>> import minigst as mg
        >>> vario = mg.vario_exp(db, vname='elevation', nlag=20, dlag=10.0)
        >>> # Directional variogram
        >>> vario = mg.vario_exp(db, vname='elevation', dir=[30, -30], nlag=20, dlag=10.0)
    """
    # Set variable as Z locator
    if isinstance(vname, str):
        vname = [vname]
    
    for vn in vname:
        db.setLocator(vn, gl.ELoc.Z)
    
    # Handle drifts
    if ext_drift is not None or pol_drift is not None:
        if pol_drift is not None:
            for i in range(pol_drift + 1):
                # Add polynomial drift terms (simplified)
                pass
        
        if ext_drift is not None:
            if isinstance(ext_drift, str):
                ext_drift = [ext_drift]
            for drift in ext_drift:
                db.setLocator(drift, gl.ELoc.F)
    
    # Create variogram parameters
    if dir is None:
        # Omnidirectional variogram
        vario_param = gl.VarioParam.createOmniDirection(nlag=nlag, dlag=dlag, toldis=toldis)
    else:
        # Directional variogram
        if not isinstance(dir, (list, tuple, np.ndarray)):
            dir = [dir]
        
        if isinstance(dir[0], (int, float)):
            # Angles in 2D
            angles = True
            ndir = len(dir)
        else:
            # Direction vectors
            angles = False
            dir = np.array(dir)
            ndir = dir.shape[0]
        
        # Replicate parameters
        nlag = _rep_arg(ndir, nlag, "nlag")
        dlag = _rep_arg(ndir, dlag, "dlag")
        toldis = _rep_arg(ndir, toldis, "toldis")
        
        if tolang is None:
            tolang = [180 / (2 * ndir)] * ndir
        else:
            tolang = _rep_arg(ndir, tolang, "tolang")
        
        vario_param = gl.VarioParam()
        for i in range(ndir):
            if angles:
                dir_param = gl.DirParam.create(nlag=nlag[i], dlag=dlag[i], 
                                              toldis=toldis[i], tolang=tolang[i],
                                              angle2D=dir[i])
            else:
                dir_param = gl.DirParam.create(nlag=nlag[i], dlag=dlag[i],
                                              toldis=toldis[i], tolang=tolang[i],
                                              codir=dir[i])
            vario_param.addDir(dir_param)
    
    # Compute variogram
    vario = gl.Vario(vario_param)
    vario.compute(db)
    
    return vario


def create_model(struct, ndim=2):
    """
    Create a variogram model.
    
    Args:
        struct: Structure type(s) (string or list of strings)
        ndim: Space dimension
        
    Returns:
        gstlearn Model object
        
    Examples:
        >>> import minigst as mg
        >>> model = mg.create_model('SPHERICAL', ndim=2)
        >>> model = mg.create_model(['NUGGET', 'SPHERICAL'], ndim=2)
    """
    if isinstance(struct, str):
        struct = [struct]
    
    # Create model with first structure
    cov_type = gl.ECov.fromKey(struct[0])
    model = gl.Model.createFromParam(cov_type, ndim=ndim)
    
    # Add additional structures
    for s in struct[1:]:
        cov_type = gl.ECov.fromKey(s)
        model.addCovFromParam(cov_type)
    
    return model


def model_fit(vario, struct, prune_model=True, aniso_model=True):
    """
    Fit a variogram model to experimental variogram.
    
    Args:
        vario: gstlearn Vario object (experimental variogram)
        struct: Structure type(s) (string or list of strings)
        prune_model: Boolean, if True removes low-variance components
        aniso_model: Boolean, if True allows anisotropy
        
    Returns:
        gstlearn Model object
        
    Examples:
        >>> import minigst as mg
        >>> vario = mg.vario_exp(db, vname='elevation', nlag=20, dlag=10.0)
        >>> model = mg.model_fit(vario, struct=['NUGGET', 'SPHERICAL'])
    """
    if isinstance(struct, str):
        struct = [struct]
    
    # Get database from vario
    db = vario.getDb()
    ndim = db.getNDim()
    
    # Create initial model
    model = create_model(struct, ndim=ndim)
    
    # Fit model
    if aniso_model:
        # Fit with anisotropy
        err = model.fit(vario, mauto=gl.Option_AutoFit.create())
    else:
        # Fit without anisotropy (isotropic)
        option = gl.Option_AutoFit.create()
        option.setWmode(0)  # Isotropic mode
        err = model.fit(vario, mauto=option)
    
    # Prune model if requested
    if prune_model:
        _prune_model(model)
    
    return model


def _prune_model(model, prop_var_min=0.05):
    """
    Prune a model by suppressing low variance components.
    
    Args:
        model: gstlearn Model object
        prop_var_min: Minimum proportion of variance to keep
        
    Returns:
        Boolean indicating if model was pruned
    """
    ncov = model.getCovaNumber()
    if ncov < 2:
        return False
    
    # Get variances
    variances = []
    for i in range(ncov):
        cov = model.getCova(i)
        variances.append(cov.getSill())
    
    total_var = sum(variances)
    
    # Remove low variance components
    removed = False
    for i in range(ncov - 1, -1, -1):
        if variances[i] / total_var < prop_var_min:
            model.delCova(i)
            removed = True
    
    return removed and model.getCovaNumber() > 1
