#!/usr/bin/env ruby
# Comprehensive Language Testing Script
# Tests code generation for all 89 supported languages

require 'json'
require 'fileutils'

# Language to file extension mapping
LANGUAGE_EXTENSIONS = {
  'ada' => 'adb',
  'apl' => 'apl',
  'asm' => 'asm',
  'assembly' => 'asm',
  'awk' => 'awk',
  'basic' => 'bas',
  'bat' => 'bat',
  'batch' => 'bat',
  'c' => 'c',
  'cfml' => 'cfm',
  'clojure' => 'clj',
  'cobol' => 'cob',
  'coffeescript' => 'coffee',
  'coldfusion' => 'cfm',
  'cpp' => 'cpp',
  'c++' => 'cpp',
  'csharp' => 'cs',
  'css' => 'css',
  'dart' => 'dart',
  'dockerfile' => 'dockerfile',
  'elixir' => 'ex',
  'erlang' => 'erl',
  'factor' => 'factor',
  'forth' => 'fth',
  'fortran' => 'f90',
  'fsharp' => 'fs',
  'go' => 'go',
  'groovy' => 'groovy',
  'haskell' => 'hs',
  'html' => 'html',
  'idl' => 'idl',
  'ini' => 'ini',
  'jade' => 'jade',
  'java' => 'java',
  'javascript' => 'js',
  'javascriptreact' => 'jsx',
  'json' => 'json',
  'julia' => 'jl',
  'kotlin' => 'kt',
  'latex' => 'tex',
  'less' => 'less',
  'lisp' => 'lisp',
  'lua' => 'lua',
  'maple' => 'mpl',
  'markdown' => 'md',
  'mathematica' => 'm',
  'matlab' => 'm',
  'mercury' => 'm',
  'objective-c' => 'm',
  'objective-cpp' => 'mm',
  'objective-j' => 'j',
  'ocaml' => 'ml',
  'octave' => 'm',
  'pascal' => 'pas',
  'perl' => 'pl',
  'perl6' => 'pl6',
  'raku' => 'raku',
  'php' => 'php',
  'postscript' => 'ps',
  'powershell' => 'ps1',
  'prolog' => 'pl',
  'python' => 'py',
  'r' => 'r',
  'ruby' => 'rb',
  'rust' => 'rs',
  'sas' => 'sas',
  'sql' => 'sql',
  'scss' => 'scss',
  'scheme' => 'scm',
  'sed' => 'sed',
  'shellscript' => 'sh',
  'smalltalk' => 'st',
  'solidity' => 'sol',
  'swift' => 'swift',
  'tcl' => 'tcl',
  'typescript' => 'ts',
  'typescriptreact' => 'tsx',
  'vb' => 'vb',
  'verse' => 'verse',
  'vhdl' => 'vhdl',
  'verilog' => 'v',
  'vimscript' => 'vim',
  'vue' => 'vue',
  'xml' => 'xml',
  'xsl' => 'xsl',
  'yaml' => 'yml',
  'yml' => 'yml',
  'scala' => 'scala',
  'delphi' => 'pas',
  'objectpascal' => 'pas'
}

# Expected content patterns for each language
EXPECTED_PATTERNS = {
  'ada' => 'with Ada.Text_IO',
  'apl' => 'Hello, World!',
  'asm' => 'section .text',
  'assembly' => 'section .text',
  'awk' => 'BEGIN {',
  'basic' => 'PRINT',
  'bat' => '@echo off',
  'batch' => '@echo off',
  'c' => '#include <stdio.h>',
  'cfml' => '<cfoutput>',
  'clojure' => '(defn main',
  'cobol' => 'IDENTIFICATION DIVISION',
  'coffeescript' => 'console.log',
  'coldfusion' => '<cfoutput>',
  'cpp' => '#include <iostream>',
  'c++' => '#include <iostream>',
  'csharp' => 'using System;',
  'css' => 'body {',
  'dart' => 'void main() {',
  'dockerfile' => 'FROM',
  'elixir' => 'defmodule HelloWorld',
  'erlang' => '-module(hello_world)',
  'factor' => 'Hello, World!',
  'forth' => ': HELLO',
  'fortran' => 'PROGRAM HelloWorld',
  'fsharp' => '[<EntryPoint>]',
  'go' => 'package main',
  'groovy' => 'println',
  'haskell' => 'main :: IO ()',
  'html' => '<!DOCTYPE html>',
  'idl' => '; Basic IDL program',
  'ini' => '[Application]',
  'jade' => 'doctype html',
  'java' => 'public class',
  'javascript' => 'console.log',
  'javascriptreact' => 'import React',
  'json' => '"name":',
  'julia' => 'println',
  'kotlin' => 'fun main(',
  'latex' => '\\documentclass',
  'less' => '@primary-color:',
  'lisp' => '(defun main',
  'lua' => 'print',
  'maple' => 'printf',
  'markdown' => '# Hello, World!',
  'mathematica' => 'Print[',
  'matlab' => 'disp(',
  'mercury' => ':- module hello_world',
  'objective-c' => '#import <Foundation/Foundation.h>',
  'objective-cpp' => '#import <Foundation/Foundation.h>',
  'objective-j' => '@import <Foundation/Foundation.j>',
  'ocaml' => 'print_endline',
  'octave' => 'disp(',
  'pascal' => 'program HelloWorld;',
  'perl' => 'use strict;',
  'php' => '<?php',
  'postscript' => '/main {',
  'powershell' => 'Write-Output',
  'prolog' => 'hello_world :-',
  'python' => 'print',
  'r' => 'cat(',
  'ruby' => 'puts',
  'rust' => 'fn main() {',
  'sas' => 'data hello_world;',
  'sql' => 'CREATE TABLE',
  'scss' => '$primary-color:',
  'scheme' => '(display',
  'sed' => '#!/usr/bin/sed',
  'shellscript' => '#!/bin/bash',
  'smalltalk' => 'Object subclass: #HelloWorld',
  'solidity' => 'pragma solidity',
  'swift' => 'print("Hello, World!")',
  'tcl' => 'puts',
  'typescript' => 'console.log',
  'typescriptreact' => 'import React',
  'vb' => 'Console.WriteLine',
  'verse' => 'using { /Fortnite.com/Devices }',
  'vhdl' => 'library IEEE;',
  'verilog' => 'module hello_world',
  'vimscript' => 'echo',
  'vue' => '<template>',
  'xml' => '<?xml version="1.0"',
  'xsl' => '<xsl:stylesheet',
  'yaml' => 'metadata:',
  'yml' => 'metadata:',
  'scala' => 'object',
  'delphi' => 'program HelloWorld;',
  'objectpascal' => 'program HelloWorld;'
}

def test_language(language, extension)
  puts "Testing #{language} (.#{extension})..."
  
  # Create test file
  test_file = "test_files/test_#{language}.#{extension}"
  File.write(test_file, "// Test file for #{language}\n")
  
  # Test code generation using Node.js
  expected_pattern = EXPECTED_PATTERNS[language]
  test_command = <<~JS
    const { generateCodeBase } = require('./generators/codeBaseGenerators');
    const result = generateCodeBase('#{language}', '#{test_file}');
    const expectedPattern = #{expected_pattern ? "'#{expected_pattern.gsub("'", "\\'")}'" : 'null'};
    console.log(JSON.stringify({
      language: '#{language}',
      success: result.success,
      content: result.content,
      hasExpectedPattern: expectedPattern ? result.content.includes(expectedPattern) : true
    }));
  JS
  
  result = `node -e "#{test_command.gsub('"', '\\"')}"`.strip
  
  begin
    data = JSON.parse(result)
    if data['success'] && data['hasExpectedPattern']
      puts "  ✅ PASS - #{language}"
      return { language: language, status: 'PASS', details: 'Generated expected content' }
    else
      puts "  ❌ FAIL - #{language} (Expected pattern not found)"
      puts "    Content preview: #{data['content'][0..100]}..."
      return { language: language, status: 'FAIL', details: 'Missing expected pattern' }
    end
  rescue JSON::ParserError => e
    puts "  ❌ ERROR - #{language} (JSON parse error: #{e.message})"
    return { language: language, status: 'ERROR', details: e.message }
  end
end

def test_plaintext_detection(language, extension)
  puts "Testing plaintext detection for #{language} (.#{extension})..."
  
  test_file = "test_files/plaintext_test.#{extension}"
  File.write(test_file, "// Plaintext detection test\n")
  
  expected_pattern = EXPECTED_PATTERNS[language]
  test_command = <<~JS
    const { generateCodeBase } = require('./generators/codeBaseGenerators');
    const result = generateCodeBase('plaintext', '#{test_file}');
    const expectedPattern = #{expected_pattern ? "'#{expected_pattern.gsub("'", "\\'")}'" : 'null'};
    console.log(JSON.stringify({
      language: '#{language}',
      success: result.success,
      content: result.content,
      hasExpectedPattern: expectedPattern ? result.content.includes(expectedPattern) : true
    }));
  JS
  
  result = `node -e "#{test_command.gsub('"', '\\"')}"`.strip
  
  begin
    data = JSON.parse(result)
    if data['success'] && data['hasExpectedPattern']
      puts "  ✅ PASS - plaintext detection for #{language}"
      return { language: "plaintext->#{language}", status: 'PASS', details: 'Plaintext detection works' }
    else
      puts "  ❌ FAIL - plaintext detection for #{language}"
      return { language: "plaintext->#{language}", status: 'FAIL', details: 'Plaintext detection failed' }
    end
  rescue JSON::ParserError => e
    puts "  ❌ ERROR - plaintext detection for #{language} (#{e.message})"
    return { language: "plaintext->#{language}", status: 'ERROR', details: e.message }
  end
end

# Main execution
puts "🧪 TSI HEADER COMPREHENSIVE LANGUAGE TEST"
puts "=" * 50
puts "Testing all #{LANGUAGE_EXTENSIONS.size} languages..."
puts

# Ensure test directory exists
FileUtils.mkdir_p('test_files')

results = []
plaintext_results = []

# Test each language
LANGUAGE_EXTENSIONS.each do |language, extension|
  # Test direct language detection
  results << test_language(language, extension)
  
  # Test plaintext detection for languages that might be detected as plaintext
  if ['ada', 'scala', 'kotlin'].include?(language)
    plaintext_results << test_plaintext_detection(language, extension)
  end
end

# Summary
puts "\n" + "=" * 50
puts "📊 TEST SUMMARY"
puts "=" * 50

passed = results.count { |r| r[:status] == 'PASS' }
failed = results.count { |r| r[:status] == 'FAIL' }
errors = results.count { |r| r[:status] == 'ERROR' }

puts "Total Languages Tested: #{results.size}"
puts "✅ Passed: #{passed}"
puts "❌ Failed: #{failed}"
puts "⚠️  Errors: #{errors}"
puts "Success Rate: #{(passed.to_f / results.size * 100).round(1)}%"

if failed > 0 || errors > 0
  puts "\n🔍 FAILED/ERROR LANGUAGES:"
  results.select { |r| r[:status] != 'PASS' }.each do |r|
    puts "  #{r[:status] == 'FAIL' ? '❌' : '⚠️'} #{r[:language]}: #{r[:details]}"
  end
end

if plaintext_results.any?
  puts "\n🔍 PLAINTEXT DETECTION TESTS:"
  plaintext_results.each do |r|
    puts "  #{r[:status] == 'PASS' ? '✅' : '❌'} #{r[:language]}: #{r[:details]}"
  end
end

# Cleanup
FileUtils.rm_rf('test_files')
puts "\n🧹 Cleanup completed"

puts "\n🏁 Comprehensive language testing completed!"