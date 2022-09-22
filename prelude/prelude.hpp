#pragma once
#include <iostream>

using std::ostream;
struct Proto_Unit {
    friend ostream& operator<<(ostream& out, const Proto_Unit& unit) {
        out << "()";
        return out;
    }
};