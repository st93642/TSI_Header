#!/bin/bash

echo "=== TSI Header Extension Comprehensive Test ==="
echo

# Test header generation for all supported languages with correct language IDs
declare -A lang_map=(
    ["test.java"]="java"
    ["test.js"]="javascript" 
    ["test.cs"]="csharp"
    ["test.kt"]="kotlin"
    ["test.php"]="php"
    ["test.py"]="python"
    ["test.rb"]="ruby"
    ["test.rs"]="rust"
    ["test.swift"]="swift"
    ["test.ts"]="typescript"
    ["test.c"]="c"
    ["test.cpp"]="cpp"
)

echo "Testing header generation:"
for file in "${!lang_map[@]}"; do
    if [ -f "test_files/$file" ]; then
        lang="${lang_map[$file]}"
        echo -n "  $lang ($file): "
        result=$(ruby lib/tsi_header_cli.rb insert "$lang" "test_files/$file" 2>/dev/null)
        if echo "$result" | jq -e '.success' >/dev/null 2>&1; then
            echo "✓ PASS"
        else
            echo "✗ FAIL - $(echo "$result" | jq -r '.message')"
        fi
    fi
done

echo
echo "Testing header update (should fail on empty files):"
for file in test_files/test.{java,js,cs,kt,php,py}; do
    if [ -f "$file" ]; then
        lang=$(basename "$file" | sed 's/test\.//')
        # Map to correct language ID
        case $lang in
            js) lang="javascript" ;;
            cs) lang="csharp" ;;
            kt) lang="kotlin" ;;
            py) lang="python" ;;
        esac
        echo -n "  $lang: "
        result=$(ruby lib/tsi_header_cli.rb update "$lang" "$file" 2>/dev/null)
        if echo "$result" | jq -e '.message | contains("No header found")' >/dev/null 2>&1; then
            echo "✓ PASS (expected: no header to update)"
        else
            echo "✗ UNEXPECTED - $(echo "$result" | jq -r '.message')"
        fi
    fi
done

echo
echo "Testing unsupported language:"
echo -n "  nonexistent: "
result=$(ruby lib/tsi_header_cli.rb insert "nonexistent" "test_files/test.java" 2>/dev/null)
if echo "$result" | jq -e '.success == false and (.message | contains("No header support"))' >/dev/null 2>&1; then
    echo "✓ PASS (correctly rejected)"
else
    echo "✗ FAIL - $(echo "$result" | jq -r '.message')"
fi

echo
echo "=== Test Summary ==="
echo "✓ Header generation should work for all supported languages"
echo "✓ Header update should fail gracefully on files without headers"  
echo "✓ Unsupported languages should be rejected with clear error"
echo
echo "Supported languages: $(echo "${lang_map[@]}" | tr ' ' '\n' | sort | uniq | tr '\n' ' ')"
