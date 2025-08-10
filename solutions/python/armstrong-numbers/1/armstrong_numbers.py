def is_armstrong_number(number):
    digits = [int(d) for d in str(number)]
    answer = sum(d ** len(digits) for d in digits)
    return answer == number
