#include "roman_numerals.h"
#include <iostream>

using std::string;

// Function to convert an integer to its Roman numeral representation
namespace roman_numerals {
  string convert(int n) {

    // this algorithm needs the array to be from greatest to least
    string str_romans[] = {"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"};
    int values[] = {1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1};
    int len = sizeof(values)/sizeof(values[0]); 

    // Initialize an empty string to hold the resulting Roman numeral
    string result = "";

    // Loop through the values and corresponding Roman symbols
    for (auto i = 0; i < len; ++i)
    {
        // While the current number is greater than or equal to the value
        while(n - values[i] >= 0)
        {
            // Append the Roman symbol to the result and subtract the value from the number
            result += str_romans[i];
            n -= values[i];
        }
    }

    // Return the resulting Roman numeral
    return result;
  }

}  // namespace roman_numerals
