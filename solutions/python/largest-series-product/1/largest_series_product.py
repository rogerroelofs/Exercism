def largest_product(series, size):
    if size < 0:
        raise ValueError('span must not be negative')
    if size > len(series):
        raise ValueError('span must be smaller than string length')
    if series == '' or size == 0:
        return 1

    if not series.isdigit():
        raise ValueError('digits input must only contain digits')

    numbers = [int(char) for char in series]

    max_product = 0
    for i in range(len(numbers) - size + 1):
        product = 1
        for j in range(i, i + size):
            product *= numbers[j]
        max_product = max(max_product, product)

    return max_product