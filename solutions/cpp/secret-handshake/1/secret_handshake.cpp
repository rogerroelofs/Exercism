#include "secret_handshake.h"

#include <algorithm>

namespace secret_handshake {
    std::vector<std::string> commands(int n) {
        std::vector<std::string> actions;
        if (n & 1) actions.push_back("wink");
        if (n & 2) actions.push_back("double blink");
        if (n & 4) actions.push_back("close your eyes");
        if (n & 8) actions.push_back("jump");
        if (n & 16) std::reverse(actions.begin(), actions.end());

        return actions;
    }
}  // namespace secret_handshake
