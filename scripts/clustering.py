import pandas as pd
import numoy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.cluster import KMeans
from scipy.cluster.hierarchy import dendrogram, linkage, fcluster
from sklearn.mixture import GaussianMixture


# %% 
lca_data = pd.read_csv('data/lca_data.csv')
lca_data = lca_data.apply(pd.to_numeric, errors='coerce')
print(lca_data.isna().sum())
justice_columns = ['utilitarian', 'egalitarian', 'sufficientarian', 'limitarian']
lca_data = lca_data[justice_columns]

# standardise data
scaler = StandardScaler()
lca_data_scaled = scaler.fit_transform(lca_data)


# %% ############################# PCA #############################

# Initialize PCA, specify the number of components if needed
pca = PCA(n_components=2)  # For visualization, 2 components are typically used

# Fit and transform the data
principal_components = pca.fit_transform(lca_data_scaled)

# Create a DataFrame with the principal components
pca_df = pd.DataFrame(data=principal_components, columns=['PC1', 'PC2'])

# Get the loadings (contribution of each feature to the components)
loadings = pd.DataFrame(pca.components_.T, columns=['PC1', 'PC2'], index=lca_data.columns)

# Display the loadings
print(loadings)

# %% 
plt.figure(figsize=(10, 8))

# plot principal components as a scatter plot
plt.scatter(pca_df['PC1'], pca_df['PC2'], alpha=0.9, color='lightblue')

# plot the variable vectors (biplot part) on top of the scatter plot
for i, var in enumerate(justice_columns):
    plt.arrow(0, 0, pca.components_[0, i], pca.components_[1, i], 
              color='red', head_width=0.05, head_length=0.1, linewidth=2)
    plt.text(pca.components_[0, i] * 1.2, pca.components_[1, i] * 1.2, 
             var, color='red', ha='center', va='center', fontsize=12)

# Add labels and title
plt.xlabel(f"PC1 ({pca.explained_variance_ratio_[0]*100:.1f}%)")
plt.ylabel(f"PC2 ({pca.explained_variance_ratio_[1]*100:.1f}%)")
plt.title('PCA Biplot')

# Draw axis lines
plt.axhline(y=0, color='black', linestyle='--')
plt.axvline(x=0, color='black', linestyle='--')

# Make sure the vectors are on top
plt.gca().set_zorder(1)


# %% ############################# k-means #############################

# Perform K-Means clustering
kmeans = KMeans(n_clusters=3, random_state=42)  # You can choose the number of clusters
lca_data['cluster'] = kmeans.fit_predict(lca_data_scaled)

# Perform PCA for visualization
pca = PCA(n_components=2)
principal_components = pca.fit_transform(lca_data_scaled)

# Create a DataFrame for the principal components
pca_df = pd.DataFrame(data=principal_components, columns=['PC1', 'PC2'])

# Add the cluster labels to the DataFrame
pca_df['cluster'] = lca_data['cluster']

# Plot the PCA results with cluster labels
plt.figure(figsize=(10, 8))
sns.scatterplot(x='PC1', y='PC2', hue='cluster', data=pca_df, palette='viridis', s=100, alpha=0.7)

# Add labels and title
plt.xlabel(f"PC1 ({pca.explained_variance_ratio_[0]*100:.1f}%)")
plt.ylabel(f"PC2 ({pca.explained_variance_ratio_[1]*100:.1f}%)")
plt.title('PCA with K-Means Clustering')

# Draw axis lines
plt.axhline(y=0, color='black', linestyle='--')
plt.axvline(x=0, color='black', linestyle='--')

# Show the plot
plt.grid()
plt.show()



# %% ############################# hierarchical #############################
linkage_matrix = linkage(lca_data_scaled, method='ward')  # You can choose different methods like 'single', 'complete', etc.

plt.figure(figsize=(10, 7))

# Plot the dendrogram
dendrogram(linkage_matrix, 
           labels=lca_data.index.tolist(),  # Use the index of your dataframe if you want labels
           orientation='top', 
           distance_sort='descending', 
           show_leaf_counts=True)

# Add labels and title
plt.xlabel('Sample Index')
plt.ylabel('Distance')
plt.title('Hierarchical Clustering Dendrogram')

# Show the plot
plt.show()

# %% plot cluster profiles 

# define the distance threshold to cut the dendrogram
distance_threshold = 0.7 * max(linkage_matrix[:, 2])  # Adjust as needed
# get cluster labels
clusters = fcluster(linkage_matrix, t=distance_threshold, criterion='distance')
# add cluster labels to the original data
lca_data['cluster'] = clusters
# compute mean values for each variable within each cluster
cluster_means = lca_data.groupby('cluster').mean()


# plot cluster profiles
cluster_means.plot(kind='bar', figsize=(14, 8))

plt.xlabel('Cluster')
plt.ylabel('Mean Value')
plt.title('Cluster Profiles for Each Variable')
plt.legend(title='Variable')
plt.grid(True)

# Show the plot
plt.show()


# %% ############################ LPA #############################

# Fit Gaussian Mixture Model for 4 profiles (or clusters)
gmm = GaussianMixture(n_components=4, covariance_type='full', random_state=42)
gmm.fit(lca_data)

# Predict the latent profiles for each data point
lca_data['profile'] = gmm.predict(lca_data)

# Visualize the profiles
for profile in range(4):
    subset = lca_data[lca_data['profile'] == profile]
    plt.hist(subset[justice_columns], bins=20, alpha=0.5, label=f'Profile {profile+1}')
    
plt.title('Latent Profile Analysis with Gaussian Mixture Model')
plt.xlabel('Values')
plt.ylabel('Frequency')
plt.legend(loc='upper right')
plt.show()

# Calculate the mean scores for each profile
profile_means = lca_data.groupby('profile')[justice_columns].mean()
# Reset the index to make 'profile' a column
profile_means.reset_index(inplace=True)

# Plot the mean scores for each variable by profile
profile_means.plot(x='profile', kind='bar', figsize=(10, 6))

plt.title('Mean Scores for Each Variable by Latent Profile')
plt.xlabel('Latent Profile')
plt.ylabel('Mean Score')
plt.legend(title='Variable')
plt.grid(True)

plt.show()
# %%
