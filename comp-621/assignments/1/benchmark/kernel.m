function [output] = kernel(N, max_time)

rng(0);

% Time step
dt = 0.001;

% Now we need to compute the initial conditions of the system:
% the mass of each body
% the momentum of each body (magnitude and direction)
% (Note: from the momentum we can recover the velocity, by dividing by the
% mass)

% Representations:
% The masses of the bodies are collected in a big vector M
% The momenta are collected in a 3xN matrix P

% N is the number of bodies.
% max_time is the amount of time to run the simulation for.
max_steps = max_time / dt;
% so the maximum number of steps to run for is the max time divided by the
% length of a single step.

% Spread of momenta
PI = 1;

% Spread of positions
XI = 1;

% The matrix of momenta
P = arrayfun(@(x) rand() * PI, zeros(3, N)) - PI/2;

% The matrix of positions
X = arrayfun(@(x) rand() * XI, zeros(3, N)) - XI/2;

% The vector of masses
M = arrayfun(@(x) rand, zeros(1, N));

disp('Initial conditions:');
disp('Momenta:');
disp(P);
disp('Positions:');
disp(X);
disp('Masses:');
disp(M);

% Newton's constant of gravity.
% For a realistic simulation, we would fill in the real value of G.
G = 1;

% NxN matrix to hold the product G * m_i * m_j for all i, j
GM = zeros(N);

for i = 1:N
    mi = M(i);
    for j = 1:N
        GM(i, j) = G * mi * M(j);
    end
end

% Simulation:
% The simulation is iterative. Upon each iteration, for each body, we sum up
% the forces felt by it due to every other body, to compute an overall force
% felt on that body. The overall force, multiplied by the time step, gives us a
% change in momentum. Once we have computed the change in momentum for each
% body, we update the momentum matrix, wash, rinse, and repeat.

disp('Simulation:');

current_time = 0;
for step = 1:max_steps
    current_time = step*dt;
    disp(sprintf('Time %.3f; Iteration %d', current_time, step));
    % The matrix to fill up with the momentum changes.
    dP = zeros(3, N);
    % The matrix to fill up with the position changes.
    dX = zeros(3, N);

    current_time = step * dt;
    for i = 1:N
        mi = M(i);     % mass of body i
        pi = P(:,i);   % momentum of body i
        xi = X(:,i);   % position of body i
        Fi = zeros(3, 1); % force felt by body i

        for j = 1:N
            if i == j
                continue;
            end
            mj = M(j);    % mass of body j
            pj = P(:, j); % momentum of body j
            xj = X(:, j); % position of body j

            % If the two objects have the "same" position up to some tolerance,
            % then we skip their interaction (as their g-force will approach
            % the undefined ratio 0/0).
            if feq(xi, xj)
               continue;
            end

            % Add body j's contribution to the force on body i
            force = gforce(GM(i, j), xi, xj);
            % disp(sprintf('calculated g force of %d on %d', j, i));
            % disp(force);
            Fi = Fi + force;
        end

        % Fill in the momentum change for body i by multiplying the time-step
        % duration by the force at body i.
        dPi = Fi * dt;
        dP(:,i) = dPi;

        % Compute the change in position for body i:
        %   1. by recovering its current velocity from its current momentum
        vi = pi / mi;
        %   2. by computing its acceleration from its force
        ai = Fi / mi;
        %   3. by computing its change in position
        dXi = 0.5 * ai * dt^2 + vi * dt;

        % disp(sprintf('computed delta X for %d', i));
        % disp(dXi);

        dX(:,i) = dXi;
    end

    % Apply the changes in momentum and in position
    P = P + dP;
    X = X + dX;

    % Correct the position by subtracting the center of mass's position from
    % each body's position. The reason we do this is to avoid the simulation
    % gradually shifting away from the origin, yielding crazy numbers.
    cm = center_of_mass(M, X);
    X = X - repmat(cm.', 1, N);

    % disp(X);
end

output = X;

end

function cm = center_of_mass(M, X)
% Finds the center of mass of the system with masses M and positions X.
% M should be a vector whose ith component corresponds with the ith column in X
% being the position of that mass.
% The result `cm` is the position of the center of mass.
cm = sum(bsxfun(@times, X, M).') / sum(M);
end

function force = gforce(GM, q1, q2)
% Computes the force felt by mass m1 from mass m2.
% m1 and m2 are scalar masses (kg)
% w1 and q2 are R3 vectors representing positions in space.
% Precondition: q1 /= q2

force = GM * (q2 - q1) / norm(q1 - q2)^3;
end

function equal = feq(v1, v2)
% Epsilon for float equality
epsilon = 0.0000001;
% Two vectors will be considered equal if their distance is less than epsilon

equal = norm(v1 - v2) <= epsilon;
end
