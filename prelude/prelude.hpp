#pragma once
#include <iostream>

using std::ostream;
struct Proto_Unit {
    friend ostream& operator<<(ostream& out, const Proto_Unit& unit) {
        out << "()";
        return out;
    }
};

// Custom exception, we don't really need to inherit from exception
// This can be changed later
class ProtoException
{
public:
    ProtoException(std::string message) : m_message(message) {}
    const std::string what() const { return m_message; }

    friend ostream& operator<<(ostream& out, const ProtoException& ex) {
        out << ex.what();
        return out;
    }

private:
    // Note: A string will require a heap allocation, but few exceptions 
    //       should be thrown anyway.
    std::string m_message;
};

// Assert
void proto_assert(bool condition) {
    if (!condition) {
        throw ProtoException("Error occured");
    }
}

// Assert with a message
void proto_assertm(bool condition, std::string message) {
    if (!condition) {
        throw ProtoException(message);
    }
}