pub fn squareOfSum(number: usize) usize {
    var sum: usize = 0;
    var i: usize = 1;
    while (i <= number): (i+=1) {
        sum = sum + i;
    }
    return sum * sum;
}

pub fn sumOfSquares(number: usize) usize {
    var sum: usize = 0;
    for (0..number + 1) |n| {
        sum = sum + (n * n);
    }
    return sum;
}

pub fn differenceOfSquares(number: usize) usize {
    return squareOfSum(number) - sumOfSquares(number);
}
