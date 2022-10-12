#pragma once
#include <iostream>
#include <vector>
#include <string>
using std::ostream;
using std::string;
using std::vector;

// Range implementation depends on these
template<class T>
class Iter;

template<class T>
class RangeType {
public:
    virtual T get(int index) const = 0;
    virtual Iter<T> begin() const = 0;
    virtual Iter<T> end() const = 0;
};

template<class T>
class IRange : public RangeType<T> {
public:
    IRange(vector<T> r, bool chars) : range{ r }, is_char{ chars } {}

    T get(int index) const {
        if (is_char) {
            return char(index);
        }
        else {
            return range[0] + index;
        }
    }

    Iter<T> begin() const {
        if (is_char) {
            return Iter<T>(this, int(range[0]));
        }
        else {
            return Iter<T>(this, 0);
        }
    }

    Iter<T> end() const {
        if (is_char) {
            return Iter<T>(this, int(range[1]) + 1);
        }
        else {
            return Iter<T>(this, range[1] + 1);
        }
    }

    friend ostream& operator<<(ostream& out, const IRange& r) {
        out << r.range[0] << "..=" << r.range[1];
        return out;
    }

private:
    bool is_char;
    vector<T> range;
};

template<class T>
class Range : public RangeType<T> {
public:
    Range(vector<T> r, bool chars) : range{ r }, is_char{ chars } {}

    T get(int index) const {
        if (is_char) {
            return char(index);
        }
        else {
            return range[0] + index;
        }
    }

    Iter<T> begin() const {
        if (is_char) {
            return Iter<T>(this, int(range[0]));
        }
        else {
            return Iter<T>(this, 0);
        }
    }

    Iter<T> end() const {
        if (is_char) {
            return Iter<T>(this, int(range[1]));
        }
        else {
            return Iter<T>(this, range[1]);
        }
    }

    friend ostream& operator<<(ostream& out, const Range& r) {
        out << r.range[0] << ".." << r.range[1];
        return out;
    }

private:
    bool is_char;
    vector<T> range;
};

template<class T>
class Iter {
public:
    Iter(const RangeType<T>* rnge, int pos) : range{ rnge }, index{ pos } {}

    bool operator!= (const Iter& other) const {
        return index != other.index;
    }

    T operator* () const {
        return range->get(index);
    }

    const Iter<T>& operator++ () {
        index++;
        return *this;
    }

private:
    int index;
    const RangeType<T>* range;
};

// Unit class
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