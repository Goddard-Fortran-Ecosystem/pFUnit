#!/bin/bash
# Test command-line filtering options for pFUnit
# Tests -f/--filter and -e/--exclude flags

set -e  # Exit on error

TEST_EXE="$1"
if [ -z "$TEST_EXE" ] || [ ! -x "$TEST_EXE" ]; then
    echo "ERROR: Test executable not provided or not executable: $TEST_EXE"
    exit 1
fi

# Color output helpers
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

FAILED=0
PASSED=0

# Test function
test_filter() {
    local description="$1"
    shift
    local expected_count="$1"
    shift
    
    echo -n "Testing: $description ... "
    
    # Run test and capture output
    output=$("$TEST_EXE" "$@" 2>&1 || true)
    
    # Extract actual test count from "(N tests)" or "(N test)"
    actual_count=$(echo "$output" | grep -o "([0-9]* test" | grep -o "[0-9]*" || echo "0")
    
    if [ "$actual_count" = "$expected_count" ]; then
        echo -e "${GREEN}PASS${NC} (ran $actual_count tests)"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC} (expected $expected_count, got $actual_count)"
        echo "Output: $output"
        FAILED=$((FAILED + 1))
    fi
}

echo "=========================================="
echo "Testing pFUnit Command-Line Filtering"
echo "=========================================="
echo ""

# Test 1: No filter - all tests run
test_filter "No filter (all tests)" 6

# Test 2: Filter with glob pattern - specific prefix
test_filter "Filter test_alpha_* (glob)" 2 -f "FilterCommandLineTests_suite.test_alpha_*"

# Test 3: Filter with glob pattern - different prefix  
test_filter "Filter test_beta_* (glob)" 2 -f "FilterCommandLineTests_suite.test_beta_*"

# Test 4: Filter with wildcard matching multiple
test_filter "Filter test_* (glob)" 4 -f "FilterCommandLineTests_suite.test_*"

# Test 5: Exclude pattern
test_filter "Exclude *slow* (glob)" 5 -e "FilterCommandLineTests_suite.*slow*"

# Test 6: Multiple include patterns (OR logic)
test_filter "Filter test_alpha_* OR test_beta_* (glob)" 4 -f "FilterCommandLineTests_suite.test_alpha_*" "FilterCommandLineTests_suite.test_beta_*"

# Test 7: Include + Exclude
test_filter "Filter test_* exclude test_alpha_* (glob)" 2 -f "FilterCommandLineTests_suite.test_*" -e "FilterCommandLineTests_suite.test_alpha_*"

# Test 8: Exclude multiple patterns
test_filter "Exclude slow* and other* (glob)" 4 -e "FilterCommandLineTests_suite.*slow*" "FilterCommandLineTests_suite.*other*"

# Unix-specific regex tests
if [ "$(uname)" != "MINGW"* ] && [ "$(uname)" != "MSYS"* ]; then
    echo ""
    echo "Running Unix/Linux/macOS-specific regex tests..."
    
    # Test 9: Regex pattern with character class
    test_filter "Filter test_alpha_[a-z]+ (regex)" 2 -f "FilterCommandLineTests_suite\.test_alpha_.*"
    
    # Test 10: Regex with anchors
    test_filter "Filter ^.*test_beta_one$ (regex)" 1 -f "^FilterCommandLineTests_suite\.test_beta_one$"
    
    # Test 11: Regex alternation
    test_filter "Filter test_(alpha|beta)_one (regex)" 2 -f "FilterCommandLineTests_suite\.test_(alpha|beta)_one"
fi

echo ""
echo "=========================================="
echo "Results: $PASSED passed, $FAILED failed"
echo "=========================================="

if [ $FAILED -gt 0 ]; then
    exit 1
fi

exit 0
