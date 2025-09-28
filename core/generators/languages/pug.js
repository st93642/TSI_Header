/**
 * Pug Language Code Base Generator
 * Generates Pug template code base/boilerplate code
 */

/**
 * Generates Pug code base
 * @returns {string} Pug code base template
 */
function generatePugCodeBase() {
    return `//- Basic Pug template structure
doctype html
html(lang="en")
  head
    meta(charset="UTF-8")
    meta(name="viewport", content="width=device-width, initial-scale=1.0")
    title= title || "Pug Template"
    link(rel="stylesheet", href="styles.css")

  body
    header
      h1 Welcome to Pug
      nav
        ul
          li
            a(href="/") Home
          li
            a(href="/about") About
          li
            a(href="/contact") Contact

    main
      //- Conditional content
      if user
        .user-info
          h2 Hello, #{user.name}!
          p Your email: #{user.email}
      else
        .login-prompt
          p Please log in to continue

      //- Loop through items
      each item in items
        .item
          h3= item.title
          p= item.description
          if item.price
            .price $#{item.price}

      //- Mixin example
      +button("Click me", "primary")

    footer
      p &copy; #{new Date().getFullYear()} My Website

//- Mixin definition
mixin button(text, type)
  button.btn(class=type)= text
`;
}

module.exports = {
    generatePugCodeBase
};