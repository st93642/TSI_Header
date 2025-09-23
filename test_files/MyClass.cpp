/*****************************************************************************/
/*                                                                           */
/*  MyClass.cpp                                          TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 24 2025 02:52 Igors Oleinikovs             TT    SSSSSSS II */
/*  Updated: Sep 24 2025 02:52 Igors Oleinikovs                              */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

#include "MyClass.hpp"

// Default constructor
MyClass::MyClass() : name(""), id(0) {}

// Parameterized constructor
MyClass::MyClass(const std::string& name, int id) : name(name), id(id) {}

// Copy constructor
MyClass::MyClass(const MyClass& other) : name(other.name), id(other.id) {}

// Destructor
MyClass::~MyClass() {}

// Assignment operator
MyClass& MyClass::operator=(const MyClass& other) {
    if (this != &other) {
        name = other.name;
        id = other.id;
    }
    return *this;
}

// Getters
std::string MyClass::getName() const {
    return name;
}

int MyClass::getId() const {
    return id;
}

// Setters
void MyClass::setName(const std::string& name) {
    this->name = name;
}

void MyClass::setId(int id) {
    this->id = id;
}

// Utility methods
void MyClass::display() const {
    std::cout << "MyClass{name='" << name << "', id=" << id << "}" << std::endl;
}

bool MyClass::equals(const MyClass& other) const {
    return name == other.name && id == other.id;
}
