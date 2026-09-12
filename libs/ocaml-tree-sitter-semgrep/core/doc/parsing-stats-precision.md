Parsing stats precision
==

Given a goal of 99.99% lines of code parsed without an error, how many
lines of code should we parse to obtain a reliable percentage?

Surely, if we only parse a file of 100 lines, the success rate might be 100%
or 99% or even 90% if an error spans multiples lines. This doesn't
tell us much about our goal of 99.99%, which corresponds to one
error in 10,000 lines.

Assumption of single-line errors
--

For now, we will assume that having an error on one line is
independent of having an error on the next line.

Under this assumption, the number of lines of code affected by
a parse error follows a [Poisson
distribution](https://en.wikipedia.org/wiki/Poisson_distribution).
A Poisson distribution is parametrized by lambda, which is the mean of
the distribution. The standard deviation is the square root of the
mean for this distribution.

What do we want? We want to know how many lines of code we should
parse such that the parse error rate is reliable.
We hope that the parse error rate will be 0.0001 (0.01%) or less.

```
lambda = 0.0001 * n
mean = lambda          # expected number of errors (unparsable lines)
sigma = sqrt(lambda)   # standard deviation
95% confidence interval = [mean - 2 * sigma, mean + 2 * sigma]
99.7% confidence interval = [mean - 3 * sigma, mean + 3 * sigma]
```

The confidence intervals are based on a normal distribution, which is
a good approximation for large lambda (> 12).
For n = 10 million, we get:

```
lambda = 1000
mean = 1000
sigma = 31.6
(2 * sigma) / mean = 0.0632
(3 * sigma) / mean = 0.0948
```
The last two numbers are what we're after. They mean that given a
parse error rate estimated at around 0.01% by parsing 10 million lines
of code, there is:
* a 95% chance that the estimated parse error rate is at most 6.3% away from
  the true value, and
* a 99.7% chance that the estimated parse error rate is at most 9.5%
  away from the true value.

So, roughly speaking, our estimated parse error rate is most likely
off by no more than a few percent, which is great.

For other values of n, we get:

```
n = 1,000,000
lambda = 100
2 * sigma = 0.2  # 5% of the time, estimate is off by 20%
3 * sigma = 0.3  # 0.3% of the time, estimate is off by 30%
```

```
n = 100,000
lambda = 10
2 * sigma = 0.63  # 5% of the time, estimate is 60% off or more
3 * sigma = 0.95
```

In conclusion, under the assumption that each parse error affects just one
line, we find that a corpus of 1 million lines of code yields an
estimate that's off typically by less than 20%, which is pretty good.
With a corpus of 10 million lines of code, we're typically off by less
than 6%, which is solid.

What if errors span multiple lines?
--

Assuming that each error spans exactly 10 lines, we can divide the
corpus into blocks of 10 lines instead of single lines. This brings us
back to the previous problem, but n now represents blocks of 10 lines.

Under this new assumption, we get for a corpus of 10,000,000 lines:
```
n = 1,000,000    # number of blocks of 10 lines
lambda = 100
2 * sigma = 0.2  # 5% of the time, estimate is off by 20%
3 * sigma = 0.3  # 0.3% of the time, estimate is off by 30%
```

Conclusion
--

In practice, many errors are localized to just one line, but on
occasion an error may affect a whole block (5-50 lines) or even a whole
file, which could be a few hundred lines.

Assuming an average of 10 lines per error, we find that parsing a
corpus of 10 million lines of code will yield a good estimate of the
error rate, that is at most 20% off 95% of the time.
