/**
 * YAML Code Base Generator
 * Generates boilerplate YAML code
 */

function generateYamlCodeBase() {
    return `\n# Basic YAML configuration file\n\n# Document metadata\nmetadata:\n  title: "TSI Header - Basic YAML Template"\n  author: "TSI Student"\n  created: "2025-09-24"\n  version: "1.0"\n  description: "This is a basic YAML configuration."\n\n# Application configuration\napp:\n  name: "TSI Application"\n  version: "1.0.0"\n  debug: true\n  environment: "development"\n  \n  # Server configuration\n  server:\n    host: "localhost"\n    port: 3000\n    ssl: false\n    timeout: 30\n    \n  # Database configuration\n  database:\n    type: "postgresql"\n    host: "localhost"\n    port: 5432\n    name: "tsi_database"\n    user: "tsi_user"\n    # password: "secure_password"  # Should be in environment variables\n    ssl: true\n    pool_size: 10\n\n# Institution information\ninstitution:\n  name: "Transport and Telecommunication Institute"\n  location: "Riga, Latvia"\n  website: "https://tsi.lv"\n  \n  programs:\n    - id: "cs"\n      name: "Computer Science"\n      level: "bachelor"\n      duration: "4 years"\n      \n    - id: "ee"\n      name: "Electrical Engineering"\n      level: "bachelor"\n      duration: "4 years"\n\n# Course configuration\ncourses:\n  - code: "CS101"\n    title: "Programming Fundamentals"\n    credits: 6\n    semester: 1\n    prerequisites: []\n    \n  - code: "MATH201"\n    title: "Advanced Mathematics"\n    credits: 4\n    semester: 2\n    prerequisites: ["MATH101"]\n\n# Feature flags\nfeatures:\n  enable_logging: true\n  enable_caching: true\n  enable_analytics: false\n  experimental_features: false\n\n# Logging configuration\nlogging:\n  level: "info"\n  format: "json"\n  output:\n    - type: "console"\n    - type: "file"\n      path: "/var/log/app.log"\n      rotation: "daily"\n      max_size: "100MB"\n      max_files: 7\n`;
}

module.exports = {
    generateYamlCodeBase
};