function [runtime] = runner(bodies, time)
% Run kernel and measure time for core computation
tic();
output = kernel_opt(bodies, time);
runtime = toc();
end
