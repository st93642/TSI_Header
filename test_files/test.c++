/*****************************************************************************/
/*                                                                           */
/*  test.c++                                             TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 24 2025 02:44 Igors Oleinikovs             TT    SSSSSSS II */
/*  Updated: Sep 24 2025 02:44 Igors Oleinikovs                              */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

#ifndef TESTCLASS_H
#define TESTCLASS_H

#include <string>
#include <iostream>

class TestClass {
private:
    std::string name;
    int id;

public:
    // Default constructor
    TestClass();

    // Parameterized constructor
    TestClass(const std::string& name, int id);

    // Copy constructor
    TestClass(const TestClass& other);

    // Destructor
    ~TestClass();

    // Assignment operator
    TestClass& operator=(const TestClass& other);

    // Getters
    std::string getName() const;
    int getId() const;

    // Setters
    void setName(const std::string& name);
    void setId(int id);

    // Utility methods
    void display() const;
    bool equals(const TestClass& other) const;
};

#endif // TESTCLASS_H
