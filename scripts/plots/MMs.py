import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# Load the data saved from R
plot_data = pd.read_csv('data/pv_MMs.csv')

# Plot the data
plt.figure(figsize=(8, 6))
sns.pointplot(x='estimate', y='term', data=plot_data, join=False, capsize=0.1)
plt.axvline(0, color='black', linestyle='--')
plt.xlabel("Estimate")
plt.ylabel("Terms")
plt.title("Model for Y")
plt.show()