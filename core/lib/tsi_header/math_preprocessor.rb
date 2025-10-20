#*****************************************************************************#
#                                                                             #
#  math_preprocessor.rb                                   TTTTTTTT SSSSSSS II #
#                                                            TT    SS      II #
#  By: st93642@students.tsi.lv                               TT    SSSSSSS II #
#                                                            TT         SS II #
#  Created: Oct 20 2025 12:00 st93642                        TT    SSSSSSS II #
#  Updated: Oct 20 2025 15:31 st93642                                         #
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
      '·' => '\\cdot',
      '⋅' => '\\cdot',
      '×' => '\\times',
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

      # Simple HTML-based math rendering - much simpler than KaTeX

      # Convert superscript notation (x^2 to x<sup>2</sup>)
      processed.gsub!(/([a-zA-Z0-9]+)\^([a-zA-Z0-9\-+]+)(?![>\]])/, '\\1<sup>\\2</sup>')

      # Convert subscript notation (x_2 to x<sub>2</sub>)
      processed.gsub!(/([a-zA-Z0-9]+)_([a-zA-Z0-9\-+]+)(?![>\]])/, '\\1<sub>\\2</sub>')

      # Convert Unicode superscript digits to HTML
      {
        '⁰' => '<sup>0</sup>',
        '¹' => '<sup>1</sup>',
        '²' => '<sup>2</sup>',
        '³' => '<sup>3</sup>',
        '⁴' => '<sup>4</sup>',
        '⁵' => '<sup>5</sup>',
        '⁶' => '<sup>6</sup>',
        '⁷' => '<sup>7</sup>',
        '⁸' => '<sup>8</sup>',
        '⁹' => '<sup>9</sup>'
      }.each do |unicode, html|
        processed.gsub!(unicode, html)
      end

      # Convert Unicode subscript digits to HTML
      {
        '₀' => '<sub>0</sub>',
        '₁' => '<sub>1</sub>',
        '₂' => '<sub>2</sub>',
        '₃' => '<sub>3</sub>',
        '₄' => '<sub>4</sub>',
        '₅' => '<sub>5</sub>',
        '₆' => '<sub>6</sub>',
        '₇' => '<sub>7</sub>',
        '₈' => '<sub>8</sub>',
        '₉' => '<sub>9</sub>'
      }.each do |unicode, html|
        processed.gsub!(unicode, html)
      end

      # Convert basic math symbols to HTML entities
      {
        '×' => '×',
        '⋅' => '⋅',
        '√' => '√',
        '∑' => '∑',
        '∏' => '∏',
        '∫' => '∫',
        '∂' => '∂',
        '∞' => '∞',
        '≤' => '≤',
        '≥' => '≥',
        '≠' => '≠',
        '≈' => '≈',
        'α' => 'α',
        'β' => 'β',
        'γ' => 'γ',
        'δ' => 'δ',
        'π' => 'π',
        'σ' => 'σ',
        'μ' => 'μ',
        'θ' => 'θ',
        'λ' => 'λ'
      }.each do |unicode, html|
        processed.gsub!(unicode, html)
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