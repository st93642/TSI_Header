/*****************************************************************************/
/*                                                                           */
/*  c[[class.cpp                                         TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 24 2025 03:07 Igors Oleinikovs             TT    SSSSSSS II */
/*  Updated: Sep 24 2025 03:07 Igors Oleinikovs                              */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

#include "c[[class.hpp"

// Default constructor
c[[class::c[[class() : name(""), id(0) {}

// Parameterized constructor
c[[class::c[[class(const std::string& name, int id) : name(name), id(id) {}

// Copy constructor
c[[class::c[[class(const c[[class& other) : name(other.name), id(other.id) {}

// Destructor
c[[class::~c[[class() {}

// Assignment operator
c[[class& c[[class::operator=(const c[[class& other) {
    if (this != &other) {
        name = other.name;
        id = other.id;
    }
    return *this;
}

// Getters
std::string c[[class::getName() const {
    return name;
}

int c[[class::getId() const {
    return id;
}

// Setters
void c[[class::setName(const std::string& name) {
    this->name = name;
}

void c[[class::setId(int id) {
    this->id = id;
}

// Utility methods
void c[[class::display() const {
    std::cout << "c[[class{name='" << name << "', id=" << id << "}" << std::endl;
}

bool c[[class::equals(const c[[class& other) const {
    return name == other.name && id == other.id;
}
