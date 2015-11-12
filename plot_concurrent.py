#!/usr/bin/env python

import sys
import matplotlib.pyplot as plt
import numpy as np
from glob import glob

def read_file(path):
    with open(path, 'rb') as f:
        data = [
                int(x.split(',')[0])
                for x
                in (
                    x
                    for x
                    in f.read().decode('utf-8').split('\n')
                    if x
                )
        ]

    return data

def plot(input_sets, names):
    fig = plt.figure()
    ax = fig.add_subplot(1, 1, 1)

    averages = []

    for input_files, name in zip(input_sets, names):
        data = [read_file(f) for f in input_files]

        averages.append([sum(xs) / float(len(xs)) for xs in data])

    print(len(averages))

    avgs = [sum(xs) / float(len(xs)) for xs in averages]

    X = [int(n) for n in names]
    print(X)
    print(avgs)
    ax.plot(X, avgs)
    ax.set_ylim(0, 400)
    ax.set_xlabel('Transactions per second')
    ax.set_ylabel('Average response time (ms)')
    fig.suptitle('Average response time versus transactions per second')

    return fig

if __name__ == '__main__':
    names = sys.argv[2:]
    output_file = sys.argv[1]

    DIR_PREFIX = 'tps'

    input_sets = [sorted(glob(DIR_PREFIX + i + '/*')) for i in names]

    fig = plot(input_sets, names)

    fig.savefig(output_file)
