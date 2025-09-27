/**
 * SQL Code Base Generator
 * Generates boilerplate SQL code
 */

function generateSqlCodeBase() {
    return `\n-- Basic SQL script template\n-- Note: This is a generic SQL template that works with most SQL databases\n\n-- Create database (if needed)\n-- CREATE DATABASE IF NOT EXISTS tsi_database;\n-- USE tsi_database;\n\n-- Create tables\nCREATE TABLE IF NOT EXISTS students (\n    id INTEGER PRIMARY KEY AUTO_INCREMENT,\n    name VARCHAR(100) NOT NULL,\n    program VARCHAR(100),\n    enrollment_year INTEGER,\n    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP\n);\n\nCREATE TABLE IF NOT EXISTS courses (\n    id INTEGER PRIMARY KEY AUTO_INCREMENT,\n    code VARCHAR(20) UNIQUE NOT NULL,\n    title VARCHAR(200) NOT NULL,\n    credits INTEGER,\n    semester INTEGER\n);\n\n-- Insert sample data\nINSERT INTO students (name, program, enrollment_year) VALUES\n('TSI Student', 'Computer Science', 2024),\n('Jane Doe', 'Electrical Engineering', 2023);\n\nINSERT INTO courses (code, title, credits, semester) VALUES\n('CS101', 'Programming Fundamentals', 6, 1),\n('MATH201', 'Advanced Mathematics', 4, 2);\n\n-- Basic queries\n-- SELECT * FROM students;\n-- SELECT * FROM courses WHERE credits >= 4;\n\n-- Example JOIN query\n-- SELECT s.name, s.program, c.title, c.credits\n-- FROM students s\n-- JOIN courses c ON s.program = 'Computer Science';\n\n-- Update example\n-- UPDATE students SET enrollment_year = 2025 WHERE name = 'TSI Student';\n\n-- Delete example (be careful!)\n-- DELETE FROM students WHERE name = 'Test Student';\n\n-- Drop tables (cleanup)\n-- DROP TABLE IF EXISTS students;\n-- DROP TABLE IF EXISTS courses;\n\n-- Comments:\n-- This script demonstrates basic SQL operations\n-- Modify table names, column names, and data types as needed\n-- Always backup your data before running DDL statements\n`;
}

module.exports = {
    generateSqlCodeBase
};