// Compile with:
// g++-10 -std=c++20 concepts-light.cpp

#include <concepts>
#include <iostream>
#include <string>
#include <vector>

template<typename T>
concept Stringer = requires(const T &t) {
    { t.String() } -> std::same_as<std::string>;
};

template <Stringer T>
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
