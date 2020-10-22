// Compile with:
// g++ traditional.cpp
//
#include <iostream>
#include <string>
#include <vector>

template <typename T>
std::string Stringify(const std::vector<T> &s) {
    std::string ret;
    for (auto &v : s) {
        ret.append(v.String());
    }
    return ret;
}

class MyT
{
public:
    std::string String() const {
        return "X";
    }
};

int main() {
    std::vector<MyT> v(3);
    std::cout << Stringify(v) << std::endl;
    return 0;
}
