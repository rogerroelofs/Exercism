#include <string>

namespace log_line {
    std::string message(std::string log_line) {
        int start = log_line.find("]: ");
        return log_line.substr(start + 3);
    }

    std::string log_level(std::string log_line) {
        int level_end = log_line.find("]: ");
        return log_line.substr(1, level_end - 1);        
    }

    std::string reformat(std::string log_line) {
        return message(log_line) + std::string(" (") + log_level(log_line) + std::string(")");
    }
} // namespace log_line
