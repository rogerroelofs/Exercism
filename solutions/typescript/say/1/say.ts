export function sayInEnglish(number: number): string {
  if ((number < 0) || (number >= 1000000000000)) {
    throw("Number must be between 0 and 999,999,999,999.");
  }
  const units = ['', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine'];
  const teens = ['ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen'];
  const tens = ['', '', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety'];
  
  function convertLessThanThousand(n: number): string {
    if (n === 0) return '';
    
    let result = '';
    
    // Handle hundreds
    if (n >= 100) {
      result += units[Math.floor(n / 100)] + ' hundred ';
      n %= 100;
    }
    
    // Handle tens and units
    if (n >= 20) {
      return (result + tens[Math.floor(n / 10)] + (n % 10 ? '-' + units[n % 10] : '')).trim();
    }
    if (n >= 10) {
      return (result + teens[n - 10]).trim();
    }
    if (n > 0) {
      result += units[n];
    }
    
    return result.trim();
  }
  
  if (number === 0) return 'zero';
  
  const billion = Math.floor(number / 1000000000);
  const million = Math.floor((number % 1000000000) / 1000000);
  const thousand = Math.floor((number % 1000000) / 1000);
  const remainder = number % 1000;
  
  let result = '';
  
  if (billion) {
    result += convertLessThanThousand(billion) + ' billion ';
  }
  
  if (million) {
    result += convertLessThanThousand(million) + ' million ';
  }
  
  if (thousand) {
    result += convertLessThanThousand(thousand) + ' thousand ';
  }
  
  if (remainder) {
    result += convertLessThanThousand(remainder);
  }
  
  return result.trim();
}
