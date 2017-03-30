function [w0] = rosenblatt22 (X, T, eta)
[m,n] = size(X);
w = zeros (1, n+1);   %1*n+1
X = [ones(m, 1),X];
X = X .* repmat(T,1,n+1);%取反  m*n+1
runlength = 0;
maxrunlength = 0;
iter = 0;
error = 1;
w0 = 0;
while error
    pre = 0;
    error = 0;
    for i = 1:n+1        
        if X(i, :) * w' > 0 %正确            
            if pre == 0
                runlength = 0;
            end
            runlength = runlength + 1;
            if runlength > maxrunlength
            maxrunlength = runlength;
            w0 = w;
            end
            pre = 1;
        else  %错误
            w = w + eta * X(i, :);  %更新w
            iter = iter + 1;
            error = 1;
            pre = 0;
        end
    end
end
