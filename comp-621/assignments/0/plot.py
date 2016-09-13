import matplotlib.pyplot as plt

read_and_parse = lambda f: [
    map(float, line.split('\t'))
    for line in f.readlines()
]

_map = map
map = lambda *args, **kwargs: list(_map(*args, **kwargs))

at = lambda k: lambda m: m[k]
ats = lambda k, data: map(at(k), data)

if __name__ == '__main__':
    with open('naive.tsv', 'rt') as f_naive:
        naive_data = read_and_parse(f_naive)
    with open('vectorized.tsv', 'rt') as f_vectorized:
        vector_data = read_and_parse(f_vectorized)

    plt.subplot(1, 1, 1)
    plt.ylabel('running time')
    plt.xlabel('input size')
    plt.plot(ats(0, naive_data), ats(2, naive_data), label='loop')
    plt.plot(ats(0, vector_data), ats(2, vector_data), label='vector')
    plt.legend()
    plt.savefig('fig.pdf')
