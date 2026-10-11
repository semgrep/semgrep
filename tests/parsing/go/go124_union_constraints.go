package go124

import "time"

type DurationSet interface {
    int | time.Duration
}

func positiveOr[T ~int | ~int64](value, defaultValue T) T {
    return value
}

func positiveOrNamed[T int | time.Duration](value, defaultValue T) T {
    return defaultValue
}

func approximate[T ~int](value T) T {
    return value
}
