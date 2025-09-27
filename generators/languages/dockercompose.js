/**
 * Docker Compose Language Code Base Generator
 * Generates Docker Compose code base/boilerplate code
 */

/**
 * Generates Docker Compose code base
 * @returns {string} Docker Compose code base template
 */
function generateDockerComposeCodeBase() {
    return `version: '3.8'

services:
  # Web application service
  web:
    build: .
    ports:
      - "3000:3000"
    volumes:
      - .:/app
      - /app/node_modules
    environment:
      - NODE_ENV=development
    depends_on:
      - db

  # Database service
  db:
    image: postgres:13
    restart: always
    environment:
      POSTGRES_DB: myapp
      POSTGRES_USER: user
      POSTGRES_PASSWORD: password
    ports:
      - "5432:5432"
    volumes:
      - postgres_data:/var/lib/postgresql/data

  # Redis service for caching
  redis:
    image: redis:6-alpine
    ports:
      - "6379:6379"

volumes:
  postgres_data:

networks:
  default:
    driver: bridge
`;
}

module.exports = {
    generateDockerComposeCodeBase
};