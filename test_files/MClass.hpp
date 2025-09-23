/*****************************************************************************/
/*                                                                           */
/*  MClass.hpp                                           TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 24 2025 02:55 Igors Oleinikovs             TT    SSSSSSS II */
/*  Updated: Sep 24 2025 02:55 Igors Oleinikovs                              */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

#ifndef MCLASS_HPP
#define MCLASS_HPP

#include <string>
#include <iostream>

class MClass {
private:
    std::string name;
    int id;

public:
    // Default constructor
    MClass();

    // Parameterized constructor
    MClass(const std::string& name, int id);

    // Copy constructor
    MClass(const MClass& other);

    // Destructor
    ~MClass();

    // Assignment operator
    MClass& operator=(const MClass& other);

    // Getters
    std::string getName() const;
    int getId() const;

    // Setters
    void setName(const std::string& name);
    void setId(int id);

    // Utility methods
    void display() const;
    bool equals(const MClass& other) const;
};

#endif // MCLASS_HPP
