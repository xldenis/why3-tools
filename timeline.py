import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import sys

file = sys.argv[1]
# read data from CSV file
data = pd.read_csv(file)

# convert timestamps to datetime objects
data['step_start'] = pd.to_datetime(data['step_start'], unit='ms')
data['step_end'] = pd.to_datetime(data['step_end'], unit='ms')

# compute relative times
start_time = data['step_start'].min()
data['step_start'] = (data['step_start'] - start_time) / np.timedelta64(1, 's')
data['step_end'] = (data['step_end'] - start_time) / np.timedelta64(1, 's')

# group data by run_id
# groups = data.groupby('run_id')

# define colors for each step_kind
colors = {'prover': 'blue', 'transform': 'red'}

# create a plot for each group
# for name, group in groups:
    # if group.size <= 10:
    #     continue
    # create a box for each step
non_zero_steps = data[data['step_end'] - data['step_start'] != 0.0]
unique_steps = np.unique(non_zero_steps[['run_id', 'step_id']].values)
step_ids = np.sort(unique_steps)

data = data.sort_values(by=['step_start'])
print(data)
active_bars = []
for i, row in data.iterrows():
    if row['step_end'] - row['step_start'] == 0.0:
        continue
    color = colors[row['step_kind'].strip()]
    #  Remove any bars that are finished
    for idx, x in enumerate(active_bars):
        if x != None and x <= row['step_start'] + 0.001:
            active_bars[idx] = None

    step_id = 0
    for idx, x in enumerate(active_bars):
        if x == None:
            active_bars[idx] = row['step_end']
            step_id = idx + 1
            break
    else:
        active_bars.append(row['step_end'])
        step_id = len(active_bars)

    plt.broken_barh([(row['step_start'], row['step_end'] - row['step_start'])],
                    (step_id-0.5, 1),
                    facecolors=color, alpha=.5)

# set labels and title
plt.xlabel('Time (s)')
# plt.title('Timeline Chart (Run {})'.format(name))

# add legend
patches = [plt.Rectangle((0, 0), 1, 1, color=color) for color in colors.values()]
labels = list(colors.keys())
plt.legend(patches, labels, loc='upper left')

# display chart
plt.show()
