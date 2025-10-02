"""
Example script demonstrating minigst Python package usage.

This script shows how to:
1. Create Db objects from DataFrames
2. Compute experimental variograms
3. Fit variogram models
4. Perform kriging predictions
5. Visualize results
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import minigst as mg

# Example 1: Create a simple dataset
print("Example 1: Creating Db objects")
print("-" * 50)

# Create a simple DataFrame with spatial data
np.random.seed(42)
n_points = 50
data = pd.DataFrame({
    'x': np.random.uniform(0, 100, n_points),
    'y': np.random.uniform(0, 100, n_points),
    'z': np.random.randn(n_points) * 10 + 50
})

# Convert to Db
db = mg.df_to_db(data, coord_names=['x', 'y'])
print(f"Created Db with {db.getSampleNumber()} samples")
print(f"Variables: {db.getAllNames()}")

# Example 2: Create a grid
print("\nExample 2: Creating DbGrid")
print("-" * 50)

# Create a regular grid
db_grid = mg.create_db_grid(nx=[50, 50], dx=[2.0, 2.0], x0=[0, 0])
print(f"Created DbGrid with {db_grid.getSampleNumber()} cells")
print(f"Grid dimensions: {db_grid.getNXs()}")

# Example 3: Compute experimental variogram
print("\nExample 3: Computing experimental variogram")
print("-" * 50)

try:
    # Compute omnidirectional variogram
    vario = mg.vario_exp(db, vname='z', nlag=10, dlag=10.0)
    print("Experimental variogram computed successfully")
    
    # Example 4: Fit a model
    print("\nExample 4: Fitting variogram model")
    print("-" * 50)
    
    model = mg.model_fit(vario, struct=['NUGGET', 'SPHERICAL'])
    print("Model fitted successfully")
    print(f"Number of covariance structures: {model.getCovaNumber()}")
    
    # Example 5: Kriging
    print("\nExample 5: Performing kriging")
    print("-" * 50)
    
    mg.minikriging(db, db_grid, vname='z', model=model, type='ordinary', std=True)
    print("Kriging completed successfully")
    print(f"Output variables: {[name for name in db_grid.getAllNames() if 'K.' in name]}")
    
    # Example 6: Plotting
    print("\nExample 6: Creating plots")
    print("-" * 50)
    
    fig, axes = plt.subplots(1, 2, figsize=(12, 5))
    
    # Plot kriging predictions
    mg.dbplot_grid(db_grid, color='K.z.estim', ax=axes[0], title='Kriging Predictions')
    mg.add_points(data['x'], data['y'], ax=axes[0], color='white', marker='o', size=30)
    
    # Plot kriging standard deviations
    mg.dbplot_grid(db_grid, color='K.z.stdev', ax=axes[1], title='Kriging Std Dev', cmap='RdBu')
    mg.add_points(data['x'], data['y'], ax=axes[1], color='white', marker='o', size=30)
    
    plt.tight_layout()
    plt.savefig('/tmp/minigst_example.png', dpi=150, bbox_inches='tight')
    print("Plot saved to /tmp/minigst_example.png")
    
except Exception as e:
    print(f"Note: Some examples may require gstlearn to be properly installed.")
    print(f"Error: {e}")

# Example 7: Summary statistics
print("\nExample 7: Computing summary statistics")
print("-" * 50)

stats = mg.summary_stats(db, vname='z')
print("Summary statistics:")
print(stats)

print("\n" + "=" * 50)
print("Examples completed!")
print("=" * 50)
