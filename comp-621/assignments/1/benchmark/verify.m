function [checksum] = verify(input_size, output)
    % Compute checksum from output
    total_sum = sum(reshape(output, input_size*input_size, 1));
    % Use a value of epsilon that is 0.0001 percent of the input_size
    % to scale the tolerance factor by the input_size, which indirectly
    % influences the total_sum
    eps = 0.000001 * input_size * input_size;
    % Roundown the least significant part of the sum by scaling up the value
    % and then flooring. This will tolerate some rounding differences during the 
    % floating-point operations
    checksum = floor(total_sum/eps);
end
