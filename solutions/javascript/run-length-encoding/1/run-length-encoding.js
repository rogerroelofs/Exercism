//
// This is only a SKELETON file for the 'Run Length Encoding' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export const encode = (input) => {
  let ret = '';
  let count = 1;
  for(let i = 1; i <= input.length; i++) {
    if(input[i-1] === input[i]) {
      count++;
    } else if(count > 1) {
      ret = ret + count + input[i-1];
      count = 1;
    } else {
      ret += input[i-1];
    }
  }
  return ret;
};

export const decode = (input) => {
  let ret = '';
  let digits = '';
  
  for(let i = 0; i < input.length; i++) {
    if(Number.parseInt(input[i])) {
      digits += input[i];
    } else if(digits > '') {
      ret += input[i].repeat(parseInt(digits, 10));
      digits = '';
    } else {
      ret += input[i];
    }
  }
  return ret;
};
