function manyruns(number_of_runs)

BODIES = 15;
TIME = 2;

runtimes = arrayfun(@(x) runner(BODIES, TIME), zeros(1, number_of_runs))

min_time = min(runtimes)
max_time = max(runtimes)
avg_time = mean(runtimes)
std_time = std(runtimes)
end
