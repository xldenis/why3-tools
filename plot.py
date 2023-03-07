import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path
import sys

file = sys.argv[1]

out_dir = Path(file).stem
Path(f'{out_dir}').mkdir(exist_ok=True)
# read CSV file into a pandas dataframe
df = pd.read_csv(file, header=None, names=["Theory", "Prover", "Time", "Path"])

print(df["Time"].describe().round(2).to_string())
print(f'95%       {df["Time"].quantile(0.95)}')

# group the data by Key and create a histogram for each group
groups = df.groupby(df["Prover"])

print(groups.size().sort_values(ascending = False).head(20))

trimmed = groups.filter(lambda x: len(x)>1000)
print(trimmed.describe().round(2).to_string())
print(f'95%       {trimmed["Time"].quantile(0.95)}')


for name, group in groups:
    name = name[2:-1]
    # Skip tiny groups
    if group.shape[0] <= 100:
        continue

   # Calculate the median absolute deviation (MAD)
    median = group["Time"].median()
    deviation = np.abs(group["Time"] - median)
    mad = deviation.median()

    # scale_factor = 2.08
    scale_factor = 12
    mod_z = np.abs((group["Time"] - median) / (scale_factor * mad))

    filtered_df = group[mod_z < 3.5] if mad != 0.0 else group

    # if filtered_df.size == 0:
        # continue
    print(f'Excluded {group.size - filtered_df.size} points out of {group.size} points in group {name}')

    # print(f'Stats for {name}')
    # print(filtered_df["Time"].describe().round(2).to_string())

    # Determine the numberof bins
    bin_width = 2.6 * mad / np.power(len(filtered_df["Time"]), 1/3)
    num_bins = int(np.ceil((filtered_df["Time"].max() - filtered_df["Time"].min()) / bin_width)) if bin_width != 0.0 else 1

    fig, ax = plt.subplots()

    ax.hist(filtered_df["Time"], bins=num_bins)
    ax.set_title("Histogram for " + name)
    ax.set_xlabel("Time")
    ax.set_ylabel("Count")

    ax.text(0.00,-0.2, filtered_df["Time"].describe().round(2).to_string(), fontsize=10, va="top", ha="left", transform=ax.transAxes)

    ax.text(0.3, -0.2, f'95%       {filtered_df["Time"].quantile(0.95)}', fontsize=10, va="top", ha="left", transform=ax.transAxes)

    fig.subplots_adjust(bottom=0.4)
    fig.savefig(f'{out_dir}/{name}.png')

    plt.close(fig)