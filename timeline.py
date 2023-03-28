import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

# read data from CSV file
data = pd.read_csv('rbt4.csv')

# convert timestamps to datetime objects
data['step_start'] = pd.to_datetime(data['step_start'], unit='ms')
data['step_end'] = pd.to_datetime(data['step_end'], unit='ms')

# compute relative times
start_time = data['step_start'].min()
data['step_start'] = (data['step_start'] - start_time) / np.timedelta64(1, 's')
data['step_end'] = (data['step_end'] - start_time) / np.timedelta64(1, 's')

# group data by run_id
groups = data.groupby('run_id')

# define colors for each step_kind
colors = {'prover': 'blue', 'transform': 'red'}

# create a plot for each group
for name, group in groups:
    if group.size <= 10:
        continue
    # create a box for each step
    step_ids = np.sort(group[group['step_end'] - group['step_start'] != 0.0]['step_id'].unique())

    for i, row in group.iterrows():
        color = colors[row['step_kind'].strip()]
        if row['step_end'] - row['step_start'] == 0.0:
            continue

        step_id = np.where(step_ids == row['step_id'])[0][0]
        plt.broken_barh([(row['step_start'], row['step_end'] - row['step_start'])],
                        (step_id-0.5, 1),
                        facecolors=color, alpha=0.5)

    # set labels and title
    # plt.yticks(group['step_id'], ['Step {}'.format(i) for i in group['step_id']])
    plt.xlabel('Time (s)')
    plt.title('Timeline Chart (Run {})'.format(name))

    # add legend
    patches = [plt.Rectangle((0, 0), 1, 1, color=color) for color in colors.values()]
    labels = list(colors.keys())
    plt.legend(patches, labels, loc='upper left')

    # display chart
    plt.show()
