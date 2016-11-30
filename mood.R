# Method 1
A = c(83, 79, 83, 74, 75, 74,86,76,84,73,78,77,80,83,78)
B = c(75,62,58,89,77,81,27,85,72,85,74,100,43,52,75)
N = length(A) + length(B)
z = c(A, B)
u = sort(unique(z)) # 没有结的混合样本，升序排列
a = tabulate(match(A, u), length(u)) # 属于A样本的结长数
t = tabulate(match(z, u), length(u)) # 结统计量，相当于table(rank)
p = cumsum((seq_along(z) - (N + 1) / 2) ^ 2) # 没有结的时候M的计算
# 累加的原因
# C = cumsum(s)
# s9 - s5 = sum_{i = 5}^{9} s_i
# 
T = sum(a * diff(c(0, p[cumsum(t)])) / t)
# a中B样本的位置为0，所以不会对B样本的秩求和
# diff为差分，默认滞后1
# diff(c(0,1,2,3))
# 0 1 2 3
#   0 1 2 滞后1
#   1 1 1 结果
# 
# e.g.
# z = 4,6,8,8,8,9
# t = 1,1,3,1
# cumsum(t) = 1,2,5,6 (2为结的前一个位置，5为结的最后一个数的位置)
# diff(c(0, p[cumsum(t)]))
# 0,p1,p2,p5,p6
#   0 ,p1,p2,p5
# p5 - p2 = 3,4,5项打结秩的离差平方和(累加原因)

# Method 2
A = c(83, 79, 83, 74, 75, 74,86,76,84,73,78,77,80,83,78)
B = c(75,62,58,89,77,81,27,85,72,85,74,100,43,52,75)
N = length(A) + length(B)
names(A) = rep(1, length(A))
names(B) = rep(2, length(B))
z = sort(c(A, B))
t = table(rank(z)) # 结统计量
k = M = 0 # k为前一个秩，M为统计量
for (i in t) {
  # 生成特定结长的序数排列(非平均秩)
  index = (k + 1):(k + i)
  # 没有结而且是B样本的秩，跳过
  if (length(index) == 1 && names(z)[index] == 2) {
    k = k + 1
    next
  }
  # A样本结长在总结长的比例
  a = sum(names(z)[index] == 1) / i
  M = M + a * sum((index - (N + 1) / 2) ^ 2)
  k = k + i
}
