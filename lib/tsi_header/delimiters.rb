#!/usr/bin/env ruby

module TSIHeader
  class Delimiters
    SLASHES = ['/*', '*/', '/*', '*/', '/*', '*/']
    HASHES = ['#', '#', '#', '#', '#', '#']
    SEMICOLONS = [';; ', ' ;;', ';;', ';;', ';; ', ' ;;']
    PARENS = ['(* ', ' *)', '(*', '*)', '(* ', ' *)']
    DASHES = ['-- ', ' --', '--', '--', '-- ', ' --']
    PERCENTS = ['%% ', ' %%', '%%', '%%', '%% ', ' %%']
    ANGLE_BRACKETS = ['<!-- ', ' -->', '<!--', '-->', '<!-- ', ' -->']
    BLOCK_COMMENTS = ['<# ', ' #>', '<#', '#>', '<# ', ' #>']
    BRACES = ['{ ', ' }', '{', '}', '{ ', ' }']
    QUOTES = ['" ', ' "', '"', '"', '" ', ' "']
    SLASHES_SINGLE = ['// ', ' //', '//', '//', '// ', ' //']
    COLDFUSION = ['<!--- ', ' --->', '<!---', '--->', '<!--- ', ' --->']
    EXCLAMATIONS = ['! ', ' !', '!', '!', '! ', ' !']

    LANGUAGE_DELIMITERS = {
      'ada' => DASHES,
      'apl' => EXCLAMATIONS,
      'asm' => SEMICOLONS,
      'assembly' => SEMICOLONS,
      'awk' => HASHES,
      'basic' => SEMICOLONS,
      'vb' => SEMICOLONS,
      'bat' => SEMICOLONS,
      'batch' => SEMICOLONS,
      'c' => SLASHES,
      'cfml' => COLDFUSION,
      'clojure' => SEMICOLONS,
      'cobol' => SEMICOLONS,
      'coffeescript' => HASHES,
      'coldfusion' => COLDFUSION,
      'cpp' => SLASHES,
      'csharp' => SLASHES,
      'css' => SLASHES,
      'dart' => SLASHES,
      'dockerfile' => HASHES,
      'elixir' => HASHES,
      'erlang' => PERCENTS,
      'factor' => EXCLAMATIONS,
      'forth' => PARENS,
      'fortran' => EXCLAMATIONS,
      'fsharp' => PARENS,
      'go' => SLASHES,
      'groovy' => SLASHES,
      'haskell' => DASHES,
      'html' => ANGLE_BRACKETS,
      'idl' => SEMICOLONS,
      'ini' => SEMICOLONS,
      'jade' => SLASHES,
      'java' => SLASHES,
      'javascript' => SLASHES,
      'javascriptreact' => SLASHES,
      'julia' => HASHES,
      'kotlin' => SLASHES,
      'latex' => PERCENTS,
      'less' => SLASHES,
      'lisp' => SEMICOLONS,
      'lua' => DASHES,
      'makefile' => HASHES,
      'maple' => HASHES,
      'mathematica' => PARENS,
      'matlab' => PERCENTS,
      'mercury' => PERCENTS,
      'objective-c' => SLASHES,
      'objective-cpp' => SLASHES,
      'objective-j' => SLASHES,
      'ocaml' => PARENS,
      'octave' => PERCENTS,
      'pascal' => BRACES,
      'perl' => HASHES,
      'perl6' => HASHES,
      'raku' => HASHES,
      'php' => SLASHES,
      'plaintext' => HASHES,
      'postscript' => PERCENTS,
      'powershell' => HASHES,
      'prolog' => PERCENTS,
      'python' => HASHES,
      'r' => HASHES,
      'ruby' => HASHES,
      'rust' => SLASHES,
      'sas' => SLASHES,
      'scheme' => SEMICOLONS,
      'scss' => SLASHES,
      'sed' => HASHES,
      'shellscript' => HASHES,
      'smalltalk' => QUOTES,
      'solidity' => SLASHES,
      'sql' => HASHES,
      'swift' => SLASHES,
      'tcl' => HASHES,
      'typescript' => SLASHES,
      'typescriptreact' => SLASHES,
      'verse' => BLOCK_COMMENTS,
      'vhdl' => DASHES,
      'vimscript' => QUOTES,
      'vue' => ANGLE_BRACKETS,
      'xml' => ANGLE_BRACKETS,
      'xsl' => SLASHES,
      'yaml' => HASHES,
      'yml' => HASHES,
      'scala' => SLASHES,
      'delphi' => BRACES,
      'objectpascal' => BRACES,
      'c++' => SLASHES,
      'json' => [],
      'markdown' => [],
      'verilog' => SLASHES,
      'fortran90' => EXCLAMATIONS,
      'FortranFreeForm' => EXCLAMATIONS,
      'systemverilog' => SLASHES,
      'Verilog' => SLASHES,
      'vbscript' => QUOTES
    }.freeze

    def self.for_language(language_id)
      LANGUAGE_DELIMITERS[language_id] || SLASHES
    end

    def self.supports_language?(language_id)
      LANGUAGE_DELIMITERS.key?(language_id)
    end
  end
end