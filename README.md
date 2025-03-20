Building executable:
1) Run $ ocamlbuild parserTester.native

To run it on a program:
1) Create a file, and type your program into it, say the file is infile
2) Run $ ./parserTester.native infile
3) You shall get an output on stdout

To run the testcases:
1) Say you want to check testcase number x after changing some code in the source files
2) Run it using $ ./parserTester.native testInputs/testcasex | diff - testOutputs/outputx

To clean the environment :
1) Run $ rm -rf _build

Testcases:
Testcases 1 - 11 are the translation of the piazza test cases in my DSL
Testcases 12 - 14 are self made small test cases
More testcases will be made afterwards as well