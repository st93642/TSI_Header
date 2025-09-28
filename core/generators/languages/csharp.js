/**
 * C# Code Base Generator
 * Generates boilerplate code for C# projects
 */

function generateCSharpCodeBase() {
    return `using System;

namespace TSIApplication
{
    /// <summary>
    /// Main program class
    /// </summary>
    class Program
    {
        /// <summary>
        /// Main entry point of the application
        /// </summary>
        /// <param name="args">Command line arguments</param>
        static void Main(string[] args)
        {
            Console.WriteLine("Hello, World!");
            Console.WriteLine("This is a basic C# program.");

            // Example usage
            var app = new TSIApplication();
            app.Run();
        }
    }

    /// <summary>
    /// Main application class
    /// </summary>
    public class TSIApplication
    {
        private string _message;
        private int _version;

        /// <summary>
        /// Constructor
        /// </summary>
        public TSIApplication()
        {
            _message = "Welcome to TSI!";
            _version = 1;
        }

        /// <summary>
        /// Run the application
        /// </summary>
        public void Run()
        {
            Console.WriteLine(_message);
            Console.WriteLine($"Version: {_version}");

            // Example with collections
            var languages = new List<string> { "C#", "Java", "Python" };
            foreach (var lang in languages)
            {
                Console.WriteLine($"Language: {lang}");
            }
        }

        /// <summary>
        /// Get application info
        /// </summary>
        /// <returns>Application information</returns>
        public string GetInfo()
        {
            return $"TSI Application v{_version}";
        }
    }
}`;
}

module.exports = {
    generateCSharpCodeBase
};