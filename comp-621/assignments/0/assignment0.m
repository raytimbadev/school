function assignment0()
    printf('doing naive\n');
    dotestIO('naive.tsv', @naive);
    printf('doing vectorized\n');
    dotestIO('vectorized.tsv', @vectorized);
end

function result = naive(n)
    sins = zeros(1, n);
    step = 10 / n;
    i = 0;
    for t = 0:step:10
        i = i + 1;
        sins(i) = sin(t);
    end
    result = sum(sins);
end

function result = vectorized(n)
    step = 10 / n;
    result = sum(sin(0:step:10));
end

function times = dotest(f)
    N = 10:10:600;
    times = zeros(size(N, 2), 3);
    i = 0;
    for n = N
        i = i + 1;
        times(i, 1) = n;
        avgtimes = zeros(1, 5);
        for j = 1:5
            tic();
            times(i, 2) = f(n);
            avgtimes(j) = toc();
        end
        times(i, 3) = mean(avgtimes);
    end
end

function dotestIO(path, f)
    output = fopen(path, "wb");
    data = dotest(f);
    for i = 1:size(data)
        fprintf(output, '%f\t%f\t%f\n', data(i, 1), data(i, 2), data(i, 3));
    end
    fclose(output);
end

assignment0()
