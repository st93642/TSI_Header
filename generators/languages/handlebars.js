/**
 * Handlebars Language Code Base Generator
 * Generates Handlebars template code base/boilerplate code
 */

/**
 * Generates Handlebars code base
 * @returns {string} Handlebars code base template
 */
function generateHandlebarsCodeBase() {
    return `\n{{!-- Basic Handlebars template structure --}}
{{#if title}}
  <h1>{{title}}</h1>
{{/if}}

{{#each items}}
  <div class="item">
    <h2>{{this.name}}</h2>
    <p>{{this.description}}</p>
    {{#if this.price}}
      <span class="price">\${{this.price}}</span>
    {{/if}}
  </div>
{{/each}}

{{!-- Conditional rendering --}}
{{#unless isEmpty}}
  <p>Content is available</p>
{{else}}
  <p>No content available</p>
{{/unless}}

{{!-- Helper usage --}}
{{formatDate date}}

{{!-- Partial inclusion --}}
{{> header}}
{{> footer}}
`;
}

module.exports = {
    generateHandlebarsCodeBase
};