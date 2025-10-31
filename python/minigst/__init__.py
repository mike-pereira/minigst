"""
minigst - Wrapper package for gstlearn (Python version)

This package provides wrapper functions for an easy access to gstlearn's main tools and functions.
"""

__version__ = "0.1.0"

from .db import (
    df_to_db,
    df_to_dbgrid,
    create_dbgrid,
    add_var_to_db,
    del_var_from_db,
    summary_stats,
)

from .plot import (
    dbplot_point,
    dbplot_grid,
    add_points,
    add_lines,
)

from .vario import (
    vario_exp,
    model_fit,
    create_model,
)

from .kriging import (
    minikriging,
    minixvalid,
    kriging_mean,
    set_mean,
)

from .gaussim import (
    simulate_gauss_rf,
)

from .loadData import(
    data
)
__all__ = [
    # Database functions
    "data",
    "df_to_db",
    "df_to_dbgrid",
    "create_dbgrid",
    "add_var_to_db",
    "del_var_from_db",
    "summary_stats",
    # Plotting functions
    "dbplot_point",
    "dbplot_grid",
    "add_points",
    "add_lines",
    # Variogram functions
    "vario_exp",
    "model_fit",
    "create_model",
    # Kriging functions
    "minikriging",
    "minixvalid",
    "kriging_mean",
    "set_mean",
    # Simulation functions
    "simulate_gauss_rf",
]
