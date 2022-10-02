#pragma once
#include <iostream>
#include <string>
using std::ostream;
using std::string;
struct Proto_Unit {
    friend ostream& operator<<(ostream& out, const Proto_Unit& unit) {
        out << "()";
        return out;
    }
};

// Custom exception, we don't really need to inherit from exception
// This can be changed later
class ProtoException {
public:
    ProtoException(string message) : m_message(message) {}
    const string what() const { return m_message; }

    friend ostream& operator<<(ostream& out, const ProtoException& ex) {
        out << ex.what();
        return out;
    }

private:
    // Note: A string will require a heap allocation, but few exceptions 
    //       should be thrown anyway.
    string m_message;
};

// Assert
void proto_assert(bool condition) {
    if (!condition) {
        throw ProtoException("Error occured");
    }
}

// Assert with a message
void proto_assertm(bool condition, string message) {
    if (!condition) {
        throw ProtoException(message);
    }
}

// Test fn pointer
struct TestFn {
    const char* name;
    void (*fn)();
};