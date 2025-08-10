#include "raindrops.h"

namespace raindrops {

std::string convert(int n) {
    // Declare a vector of strings to store the results 
    std::string result; 
  
    if (n % 3 == 0) { 
        result += "Pling";
    } 
    if (n % 5 == 0) { 
        result += "Plang";
    } 
    if (n % 7 == 0) { 
        result += "Plong";
    } 

    if ( result.empty() ) {
        return std::to_string(n);
    }
    // Return the result vector 
    return result; 
} 
  
}  // namespace raindrops
