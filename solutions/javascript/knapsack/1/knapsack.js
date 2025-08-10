export const knapsack = (maximumWeight, items) => {
  const dp = Array(items.length + 1)
  .fill(null)
  .map(() => Array(maximumWeight + 1).fill(0))

  for (let i = 1; i <= items.length; i++) {
    for (let w = 1; w <= maximumWeight; w++) {
      const item = items[i - 1]
      if (item.weight > w) {
        dp[i][w] = dp[i - 1][w]
      } else {
        dp[i][w] = Math.max(
          dp[i - 1][w],
          dp[i - 1][w - item.weight] + item.value
        )
      }
    }
  }

  return dp[items.length][maximumWeight]
};
