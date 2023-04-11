import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path
import sys


def summary_stats(df):
    descr1 = pd.Series({ 'count': df.count(), 'mean': df.mean(), 'std': df.std(), 'min': df.min(), 'max': df.max() })
    descr2 = pd.Series({ '25%': df.quantile(0.25), '50%': df.quantile(0.50),'75%': df.quantile(0.75),'95%': df.quantile(0.95),})

    return (descr1, descr2)

file = sys.argv[1]
out_dir = Path(file).stem
Path(f'{out_dir}').mkdir(exist_ok=True)

# read CSV file into a pandas dataframe
df = pd.read_csv(file, header=None, names=["Theory", "Prover", "Time", "Path"])

print(df["Time"].describe().round(2).to_string())
print(f'95%       {df["Time"].quantile(0.95)}')

# group the data by Key and create a histogram for each group
groups = df.groupby(df["Prover"])

print(groups.size().sort_values(ascending = False).to_string())

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
    scale_factor = 8
    mod_z = np.abs((group["Time"] - median) / (scale_factor * mad))

    filtered_df = group[mod_z < 3.5] if mad != 0.0 else group
    # filtered_df = group

    # if filtered_df.size == 0:
        # continue
    # print(f'Excluded {group.size - filtered_df.size} points out of {group.size} points in group {name}')

    # print(f'Stats for {name}')
    # print(filtered_df["Time"].describe().round(2).to_string())

    # Determine the numberof bins
    bin_width = 4.3 * mad / np.power(len(filtered_df["Time"]), 1/3)
    num_bins = int(np.ceil((filtered_df["Time"].max() - filtered_df["Time"].min()) / bin_width)) if bin_width != 0.0 else 1

    if num_bins == 1:
        num_bins = 10

    fig, ax = plt.subplots()

    ax.hist(filtered_df["Time"], bins=num_bins)
    ax.set_title("Histogram for " + name + " excluding outliers")
    ax.set_xlabel("Time")
    ax.set_ylabel("Count")

    descr1, descr2 = summary_stats(group["Time"])
    ax.text(0.00,-0.25, "Untruncated statistics\n" + descr1.round(2).to_string(), fontsize=10, va="top", ha="left", transform=ax.transAxes)
    ax.text(0.3, -0.25, "\n"+descr2.round(2).to_string(), fontsize=10, va="top", ha="left", transform=ax.transAxes)

    fig.subplots_adjust(bottom=0.4)
    fig.savefig(f'{out_dir}/{name}.png')

    plt.close(fig)
