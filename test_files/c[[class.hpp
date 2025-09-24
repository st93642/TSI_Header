/*****************************************************************************/
/*                                                                           */
/*  c[[class.hpp                                         TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 24 2025 03:07 Igors Oleinikovs             TT    SSSSSSS II */
/*  Updated: Sep 24 2025 03:07 Igors Oleinikovs                              */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

#ifndef C[[CLASS_HPP
#define C[[CLASS_HPP

#include <string>
#include <iostream>

class c[[class {
private:
    std::string name;
    int id;

public:
    // Default constructor
    c[[class();

    // Parameterized constructor
    c[[class(const std::string& name, int id);

    // Copy constructor
    c[[class(const c[[class& other);

    // Destructor
    ~c[[class();

    // Assignment operator
    c[[class& operator=(const c[[class& other);

    // Getters
    std::string getName() const;
    int getId() const;

    // Setters
    void setName(const std::string& name);
    void setId(int id);

    // Utility methods
    void display() const;
    bool equals(const c[[class& other) const;
};

#endif // C[[CLASS_HPP
