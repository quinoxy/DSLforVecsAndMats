EXECUTABLE = parserTester.native
BUILD_DIR = _build
SRC = parserTester.ml
TESTS = $(shell seq 1 14)

.PHONY: all clean test

all: $(EXECUTABLE)

$(EXECUTABLE): $(SRC)
	ocamlbuild $@

run: $(EXECUTABLE)
	./$(EXECUTABLE)

test: $(EXECUTABLE)
	@for i in $(TESTS); do \
		if [ -f testInputs/testcase$$i ] && [ -f testOutputs/output$$i ]; then \
			./$(EXECUTABLE) testInputs/testcase$$i | diff - testOutputs/output$$i && echo "Testcase $$i passed" || echo "Testcase $$i failed"; \
		else \
			echo "Skipping testcase $$i: Missing input/output file"; \
		fi \
	done

clean:
	rm -rf $(BUILD_DIR) $(EXECUTABLE)
