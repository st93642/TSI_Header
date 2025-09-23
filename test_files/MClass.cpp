/*****************************************************************************/
/*                                                                           */
/*  MClass.cpp                                           TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 24 2025 02:55 Igors Oleinikovs             TT    SSSSSSS II */
/*  Updated: Sep 24 2025 02:55 Igors Oleinikovs                              */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

#include "MClass.hpp"

// Default constructor
MClass::MClass() : name(""), id(0) {}

// Parameterized constructor
MClass::MClass(const std::string& name, int id) : name(name), id(id) {}

// Copy constructor
MClass::MClass(const MClass& other) : name(other.name), id(other.id) {}

// Destructor
MClass::~MClass() {}

// Assignment operator
MClass& MClass::operator=(const MClass& other) {
    if (this != &other) {
        name = other.name;
        id = other.id;
    }
    return *this;
}

// Getters
std::string MClass::getName() const {
    return name;
}

int MClass::getId() const {
    return id;
}

// Setters
void MClass::setName(const std::string& name) {
    this->name = name;
}

void MClass::setId(int id) {
    this->id = id;
}

// Utility methods
void MClass::display() const {
    std::cout << "MClass{name='" << name << "', id=" << id << "}" << std::endl;
}

bool MClass::equals(const MClass& other) const {
    return name == other.name && id == other.id;
}
