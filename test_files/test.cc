/*****************************************************************************/
/*                                                                           */
/*  test.cc                                              TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 24 2025 02:45 Igors Oleinikovs             TT    SSSSSSS II */
/*  Updated: Sep 24 2025 02:45 Igors Oleinikovs                              */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

#ifndef MAIN_H
#define MAIN_H

#include <string>
#include <iostream>

class main {
private:
    std::string name;
    int id;

public:
    // Default constructor
    main();

    // Parameterized constructor
    main(const std::string& name, int id);

    // Copy constructor
    main(const main& other);

    // Destructor
    ~main();

    // Assignment operator
    main& operator=(const main& other);

    // Getters
    std::string getName() const;
    int getId() const;

    // Setters
    void setName(const std::string& name);
    void setId(int id);

    // Utility methods
    void display() const;
    bool equals(const main& other) const;
};

#endif // MAIN_H
