class PhoneNumber:
    def __init__(self, number):
        if any(ch.isalpha() for ch in number):
            raise ValueError('letters not permitted')
        if any(ch in '!@#$%^,/?_' for ch in number):
            raise ValueError('punctuations not permitted')
        self.number = ''.join(ch for ch in number if ch.isdigit())
        if len(self.number) < 10:
            raise ValueError('must not be fewer than 10 digits')
        if len(self.number) > 11:
            raise ValueError('must not be greater than 11 digits')
        if len(self.number) == 11 and self.number[0] != '1':
            raise ValueError('11 digits must start with 1')
        self.number = self.number[-10:]
        self.area_code = self.number[:3]
        for digit, word in (('0', 'zero'), ('1', 'one')):
            for index, code in ((0, 'area'), (3, 'exchange')):
                if self.number[index] == digit:
                    raise ValueError(f'{code} code cannot start with {word}')          
    
    def pretty(self):
        return f'({self.area_code})-{self.number[3:6]}-{self.number[6:]}'