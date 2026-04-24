#!/bin/bash
# Integration test for --shuffle and --seed command-line options

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
run_test() {
    local description="$1"
    shift
    
    echo -n "Testing: $description ... "
    
    # Run test and capture output
    if "$TEST_EXE" "$@" > /dev/null 2>&1; then
        echo -e "${GREEN}PASS${NC}"
        PASSED=$((PASSED + 1))
        return 0
    else
        echo -e "${RED}FAIL${NC}"
        FAILED=$((FAILED + 1))
        return 1
    fi
}

echo "=========================================="
echo "Testing pFUnit --shuffle and --seed Options"
echo "=========================================="
echo ""

# Test 1: No shuffle - should run successfully
run_test "No shuffle (baseline)"

# Test 2: With --shuffle - should run successfully
run_test "With --shuffle flag" --shuffle

# Test 3: With --seed - should run successfully
run_test "With --seed=12345" --seed=12345

# Test 4: Verify --seed=N gives reproducible order
echo -n "Testing: Seed reproducibility ... "
output1=$("$TEST_EXE" --seed=54321 2>&1)
output2=$("$TEST_EXE" --seed=54321 2>&1)
if [ "$output1" = "$output2" ]; then
    echo -e "${GREEN}PASS${NC}"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC} (outputs differ with same seed)"
    FAILED=$((FAILED + 1))
fi

# Test 5: Verify multiple runs with different seeds all succeed
# Note: We can't easily verify order changes from output since pFUnit
# doesn't print test names in normal mode. The unit tests verify the
# actual shuffling behavior.
echo -n "Testing: Different seeds all run successfully ... "
success_count=0
for seed in 111 222 333 444 555; do
    if "$TEST_EXE" --seed=$seed > /dev/null 2>&1; then
        success_count=$((success_count + 1))
    fi
done
if [ $success_count -eq 5 ]; then
    echo -e "${GREEN}PASS${NC}"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC} (only $success_count/5 runs succeeded)"
    FAILED=$((FAILED + 1))
fi

# Test 6: Verify --shuffle works multiple times
echo -n "Testing: Multiple --shuffle runs complete successfully ... "
success_count=0
for i in {1..5}; do
    if "$TEST_EXE" --shuffle > /dev/null 2>&1; then
        success_count=$((success_count + 1))
    fi
done
if [ $success_count -eq 5 ]; then
    echo -e "${GREEN}PASS${NC}"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC} (only $success_count/5 runs succeeded)"
    FAILED=$((FAILED + 1))
fi

echo ""
echo "=========================================="
echo "Results: $PASSED passed, $FAILED failed"
echo "=========================================="

if [ $FAILED -gt 0 ]; then
    exit 1
fi

exit 0
