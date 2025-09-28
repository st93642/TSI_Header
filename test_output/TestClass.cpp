/*****************************************************************************/
/*                                                                           */
/*  TestClass.cpp                                        TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: test@example.com                                    TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 29 2025 00:16 testuser                     TT    SSSSSSS II */
/*  Updated: Sep 29 2025 00:16 testuser                                      */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

#include "TestClass.hpp"

// Default constructor
TestClass::TestClass() : name(""), id(0) {}

// Parameterized constructor
TestClass::TestClass(const std::string& name, int id) : name(name), id(id) {}

// Copy constructor
TestClass::TestClass(const TestClass& other) : name(other.name), id(other.id) {}

// Destructor
TestClass::~TestClass() {}

// Assignment operator
TestClass& TestClass::operator=(const TestClass& other) {
    if (this != &other) {
        name = other.name;
        id = other.id;
    }
    return *this;
}

// Getters
std::string TestClass::getName() const {
    return name;
}

int TestClass::getId() const {
    return id;
}

// Setters
void TestClass::setName(const std::string& name) {
    this->name = name;
}

void TestClass::setId(int id) {
    this->id = id;
}

// Utility methods
void TestClass::display() const {
    std::cout << "TestClass{name='" << name << "', id=" << id << "}" << std::endl;
}

bool TestClass::equals(const TestClass& other) const {
    return name == other.name && id == other.id;
}
