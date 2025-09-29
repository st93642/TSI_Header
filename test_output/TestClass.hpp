/*****************************************************************************/
/*                                                                           */
/*  TestClass.hpp                                        TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: test@example.com                                    TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 29 2025 10:54 testuser                     TT    SSSSSSS II */
/*  Updated: Sep 29 2025 10:54 testuser                                      */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

#ifndef TESTCLASS_HPP
#define TESTCLASS_HPP

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

#endif // TESTCLASS_HPP
