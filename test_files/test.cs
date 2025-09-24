/*****************************************************************************/
/*                                                                           */
/*  test.cs                                              TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: st93642@students.tsi.lv                             TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 24 2025 03:04 Igors Oleinikovs             TT    SSSSSSS II */
/*  Updated: Sep 24 2025 03:04 Igors Oleinikovs                              */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

using System;

namespace sharpNamespace
{
    public class sharp
    {
        // Private fields
        private string name;
        private int id;

        // Default constructor
        public sharp()
        {
            this.name = "";
            this.id = 0;
        }

        // Parameterized constructor
        public sharp(string name, int id)
        {
            this.name = name;
            this.id = id;
        }

        // Copy constructor
        public sharp(sharp other)
        {
            this.name = other.name;
            this.id = other.id;
        }

        // Properties
        public string Name
        {
            get { return name; }
            set { name = value; }
        }

        public int Id
        {
            get { return id; }
            set { id = value; }
        }

        // Override ToString method
        public override string ToString()
        {
            return $"sharp{{Name='{name}', Id={id}}}";
        }

        // Override Equals method
        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }
            sharp other = (sharp)obj;
            return name == other.name && id == other.id;
        }

        // Override GetHashCode method
        public override int GetHashCode()
        {
            return name.GetHashCode() * 31 + id;
        }

        // Display method
        public void Display()
        {
            Console.WriteLine(ToString());
        }
    }
}
