#!/usr/bin/env python

import sys
import matplotlib.pyplot as plt
import numpy as np

from plot_concurrent import read_file

if __name__ == '__main__':
    names = ['singleRM', 'doubleRM', 'tripleRM']
    dataset = [
            read_file('singleC/' + x + '.txt')
            for x
            in names
    ]

    for i, d in enumerate(dataset):
        dataset[i] = sum(d) / len(d)

    fig, ax = plt.subplots()

    for i in range(3):
        plt.bar([i], [dataset[i]], label=names[i])

    fig.savefig('bars.pdf')
