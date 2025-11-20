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
    
    # # Create target at data points
    # coord_names = db.getNamesByLocator(gl.ELoc.X)
    # coords = []
    # for i, name in enumerate(coord_names):
    #     coords.append(db.getCenters(useSel=True)[i])


    # # Create a simple Db for targets
    # tgt = create_dbgrid(coords = coords, coord_names=coord_names)
    # tgt.setLocators(coord_names, gl.ELoc.X)
    
    df = pd.DataFrame({'x': [db.getCenters(useSel=True)[0]], 'y': [db.getCenters(useSel=True)[1]]})
    df = pd.concat([df,df])
    tgt = mg.df_to_db(df, coord_names=['x', 'y'])
    
    
    tgt.display()
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



def simulate_gauss_rf(target, model, nsim=1, ntuba=1000, seed=43431, 
                      simname=None, dbcond=None, vcond=None):
    """
    Simulate a Gaussian random field with the turning bands algorithm.
    
    Args:
        target: gstlearn Db object for target locations
        model: gstlearn Model object defining the covariance structure
        nsim: Number of simulations to generate
        ntuba: Number of turning bands to use
        seed: Random seed for reproducibility
        simname: Optional name for the simulated variable
        dbcond: Optional Db object with conditioning data
        vcond: Optional variable name in dbcond for conditioning
        
    Returns:
        None (results added directly to target)
        
    Examples:
        >>> import minigst as mg
        >>> # Unconditional simulation
        >>> mg.simulate_gauss_rf(target_db, model, nsim=5, seed=12345)
        >>> 
        >>> # Conditional simulation
        >>> mg.simulate_gauss_rf(target_db, model, nsim=5, 
        ...                      dbcond=obs_db, vcond='observations')
    """
    # Set random seed
    gl.law_set_random_seed(seed)
    

    # Set naming convention
    if simname is not None:
        namconv = gl.NamingConvention(simname)
    else:
        namconv = gl.NamingConvention("Simu")
    
    if dbcond is not None and vcond is not None:
        # Conditional simulation
        if isinstance(vcond, str):
            dbcond.setLocator(vcond, gl.ELoc.Z)
           
        model_copy = model.clone()
        set_mean(model_copy, kriging_mean(dbcond, vcond, model))
        # Perform conditional simulation
        err = gl.simtub(dbcond, target, model_copy, neigh = gl.NeighUnique(), nbtuba = ntuba, nbsimu=nsim, namconv=namconv)
    else:
        # Unconditional simulation
        err = gl.simtub(None, target, model, nbtuba = ntuba, nbsimu=nsim, namconv=namconv)
    
    target.clearLocators(gl.ELoc.Z)

    if err != 0:
        raise RuntimeError("Simulation failed with error code: {}".format(err))
