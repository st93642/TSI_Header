/*****************************************************************************/
/*                                                                           */
/*  MyClass.hpp                                          TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 24 2025 02:52 Igors Oleinikovs             TT    SSSSSSS II */
/*  Updated: Sep 24 2025 02:52 Igors Oleinikovs                              */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

#ifndef MYCLASS_HPP
#define MYCLASS_HPP

#include <string>
#include <iostream>

class MyClass {
private:
    std::string name;
    int id;

public:
    // Default constructor
    MyClass();

    // Parameterized constructor
    MyClass(const std::string& name, int id);

    // Copy constructor
    MyClass(const MyClass& other);

    // Destructor
    ~MyClass();

    // Assignment operator
    MyClass& operator=(const MyClass& other);

    // Getters
    std::string getName() const;
    int getId() const;

    // Setters
    void setName(const std::string& name);
    void setId(int id);

    // Utility methods
    void display() const;
    bool equals(const MyClass& other) const;
};

#endif // MYCLASS_HPP
