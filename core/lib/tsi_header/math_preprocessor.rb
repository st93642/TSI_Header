#*****************************************************************************#
#                                                                             #
#  math_preprocessor.rb                                   TTTTTTTT SSSSSSS II #
#                                                            TT    SS      II #
#  By: st93642@students.tsi.lv                               TT    SSSSSSS II #
#                                                            TT         SS II #
#  Created: Oct 20 2025 12:00 st93642                        TT    SSSSSSS II #
#  Updated: Oct 20 2025 14:01 st93642                                         #
#                                                                             #
#   Transport and Telecommunication Institute - Riga, Latvia                  #
#                       https://tsi.lv                                        #
#*****************************************************************************#

module TSIHeader
  class MathPreprocessor
    # Unicode to LaTeX symbol mappings
    UNICODE_REPLACEMENTS = {
      '√' => '\\sqrt',
      '∛' => '\\sqrt[3]',
      '∜' => '\\sqrt[4]',
      '∑' => '\\sum',
      '∏' => '\\prod',
      '∫' => '\\int',
      '∂' => '\\partial',
      '∇' => '\\nabla',
      '∆' => '\\Delta',
      '∞' => '\\infty',
      '≤' => '\\leq',
      '≥' => '\\geq',
      '≠' => '\\neq',
      '≈' => '\\approx',
      '≡' => '\\equiv',
      '⊂' => '\\subset',
      '⊆' => '\\subseteq',
      '⊃' => '\\supset',
      '⊇' => '\\supseteq',
      '∈' => '\\in',
      '∉' => '\\notin',
      '∅' => '\\emptyset',
      '∀' => '\\forall',
      '∃' => '\\exists',
      '∄' => '\\nexists',
      '⇒' => '\\implies',
      '⇔' => '\\iff',
      '∧' => '\\land',
      '∨' => '\\lor',
      '¬' => '\\neg',
      '⊕' => '\\oplus',
      '⊗' => '\\otimes',
      '⊥' => '\\perp',
      '∠' => '\\angle',
      '°' => '\\degree',
      '′' => '\\prime',
      '″' => '\\doubleprime',
      'α' => '\\alpha',
      'β' => '\\beta',
      'γ' => '\\gamma',
      'δ' => '\\delta',
      'ε' => '\\epsilon',
      'ζ' => '\\zeta',
      'η' => '\\eta',
      'θ' => '\\theta',
      'ι' => '\\iota',
      'κ' => '\\kappa',
      'λ' => '\\lambda',
      'μ' => '\\mu',
      'ν' => '\\nu',
      'ξ' => '\\xi',
      'π' => '\\pi',
      'ρ' => '\\rho',
      'σ' => '\\sigma',
      'τ' => '\\tau',
      'υ' => '\\upsilon',
      'φ' => '\\phi',
      'χ' => '\\chi',
      'ψ' => '\\psi',
      'ω' => '\\omega',
      'Α' => '\\Alpha',
      'Β' => '\\Beta',
      'Γ' => '\\Gamma',
      'Δ' => '\\Delta',
      'Ε' => '\\Epsilon',
      'Ζ' => '\\Zeta',
      'Η' => '\\Eta',
      'Θ' => '\\Theta',
      'Ι' => '\\Iota',
      'Κ' => '\\Kappa',
      'Λ' => '\\Lambda',
      'Μ' => '\\Mu',
      'Ν' => '\\Nu',
      'Ξ' => '\\Xi',
      'Π' => '\\Pi',
      'Ρ' => '\\Rho',
      'Σ' => '\\Sigma',
      'Τ' => '\\Tau',
      'Υ' => '\\Upsilon',
      'Φ' => '\\Phi',
      'Χ' => '\\Chi',
      'Ψ' => '\\Psi',
      'Ω' => '\\Omega',
      # Superscript digits
      '⁰' => '^{0}',
      '¹' => '^{1}',
      '²' => '^{2}',
      '³' => '^{3}',
      '⁴' => '^{4}',
      '⁵' => '^{5}',
      '⁶' => '^{6}',
      '⁷' => '^{7}',
      '⁸' => '^{8}',
      '⁹' => '^{9}',
      # Subscript digits
      '₀' => '_{0}',
      '₁' => '_{1}',
      '₂' => '_{2}',
      '₃' => '_{3}',
      '₄' => '_{4}',
      '₅' => '_{5}',
      '₆' => '_{6}',
      '₇' => '_{7}',
      '₈' => '_{8}',
      '₉' => '_{9}'
    }.freeze

    def self.process_math_content(content)
      return content if content.nil? || content.empty?

      processed = content.dup

      # Apply Unicode symbol replacements
      UNICODE_REPLACEMENTS.each do |unicode, latex|
        processed.gsub!(unicode, latex)
      end

      # Convert superscript notation (x^2 to x^{2})
      processed.gsub!(/([a-zA-Z0-9]+)\^([a-zA-Z0-9\-+]+)(?![}\]])/, '\\1^{\\2}')

      # Convert subscript notation (if any)
      processed.gsub!(/([a-zA-Z0-9]+)_([a-zA-Z0-9\-+]+)(?![}\]])/, '\\1_{\\2}')

      # Fix sqrt expressions to have proper braces
      processed.gsub!(/\\sqrt([^{])/, '\\\\sqrt{\\1}')
      processed.gsub!(/\\sqrt\[(\d+)\]\[}3\]x/, '\\\\sqrt[3]{x}') # Fix cube root specifically
      processed.gsub!(/\\sqrt\[(\d+)\]([^{])/, '\\\\sqrt[\\1]{\\2}')

      # Handle logarithmic expressions (log_a x, log_{a} x)
      processed.gsub!(/log_\{1\}_\{0\}/, 'log_{10}')
      processed.gsub!(/log_\{([^}]+)\}\s*\(([^)]+)\)/, '\\\\log_{\\1}{\\2}')
      processed.gsub!(/log_([a-zA-Z0-9]+)\s+([a-zA-Z0-9]+)/, '\\\\log_{\\1}{\\2}')
      processed.gsub!(/log_\{([^}]+)\}\s*([a-zA-Z0-9]+)/, '\\\\log_{\\1}{\\2}')

      # Handle natural log
      processed.gsub!(/ln\s*\(([^)]+)\)/, '\\\\ln(\\1)')

      # Fix $1/x$ notation
      processed.gsub!(/\$1\/x\$/, '1/x')

      # Convert derivative notation to fractions
      processed.gsub!(/d(\^\{\d+\})?y\/dx(\^\{\d+\})?/, '\\frac{d\\1y}{dx\\2}')

      # Wrap mathematical expressions in KaTeX delimiters

      # First, protect already wrapped expressions
      protected_blocks = []
      processed.gsub!(/\$\$[\s\S]*?\$\$/) do |match|
        idx = protected_blocks.length
        protected_blocks << match
        "@@MATHBLOCK_#{idx}@@"
      end

      # Wrap expressions containing LaTeX elements (superscripts, subscripts, commands)
      processed.gsub!(/([^$\n]*(?:\\[^\s]+|[\^_]\{[^}]+\}|[\^_][a-zA-Z0-9]+)[^$\n]*)/) do |match|
        # Only wrap if it contains LaTeX elements and isn't already wrapped
        if (match.include?('\\') || match.include?('{') || match.include?('^') || match.include?('_')) && !match.include?('@@')
          "$$#{match}$$"
        else
          match
        end
      end

      # Restore protected blocks
      processed.gsub!(/@@MATHBLOCK_(\d+)@@/) do |match|
        idx = $1.to_i
        protected_blocks[idx] || ''
      end

      processed
    end

    # Process markdown content with math preprocessing
    def self.process_markdown_with_math(markdown)
      return markdown if markdown.nil? || markdown.empty?

      # First process math expressions
      processed = process_math_content(markdown)

      # Then convert to HTML (simplified version)
      html = processed.dup

      # Convert code blocks first (protect from other replacements)
      code_blocks = []
      html.gsub!(/```(\w+)?\n([\s\S]*?)```/) do |match|
        lang = $1 || 'plaintext'
        code = $2
        idx = code_blocks.length
        code_blocks << "<pre><code class=\"language-#{lang}\">#{escape_html(code)}</code></pre>"
        "@@CODEBLOCK_#{idx}@@"
      end

      # Convert inline code
      html.gsub!(/`([^`\n]+)`/, '<code>\\1</code>')

      # Convert bold and italic
      html.gsub!(/\*\*(.+?)\*\*/, '<strong>\\1</strong>')
      html.gsub!(/(?<!\*)\*(?!\*)(.+?)(?<!\*)\*(?!\*)/, '<em>\\1</em>')

      # Convert headers
      html.gsub!(/^### (.+)$/, '<h3>\\1</h3>')
      html.gsub!(/^## (.+)$/, '<h2>\\1</h2>')
      html.gsub!(/^# (.+)$/, '<h1>\\1</h1>')

      # Convert lists
      html.gsub!(/^- (.+)$/, '<li>\\1</li>')
      html.gsub!(/(<li>.*<\/li>\n?)+/) do |match|
        "<ul>#{match}</ul>"
      end

      # Convert paragraphs
      html.gsub!(/\n\n+/, '</p><p>')
      html.gsub!(/^(?!<[hupldi])(.+)$/, '<p>\\1</p>')

      # Clean up
      html.gsub!(/<p>(<(?:h\d|pre|ul))/, '\\1')
      html.gsub!(/(<\/(?:h\d|pre|ul)>)<\/p>/, '\\1')
      html.gsub!(/<p>\s*<\/p>/, '')

      # Restore code blocks
      html.gsub!(/@@CODEBLOCK_(\d+)@@/) do |match|
        idx = $1.to_i
        code_blocks[idx] || ''
      end

      html
    end

    private

    def self.escape_html(text)
      text
        .gsub('&', '&amp;')
        .gsub('<', '&lt;')
        .gsub('>', '&gt;')
        .gsub('"', '&quot;')
        .gsub("'", '&#039;')
    end
  end
end