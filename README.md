# ownCompiler
My own programming language written from scratch with Python and LEX &amp; YACC

# Course Project
This project was the final deliverable for my course of "Languages and Translators" (Fall 2020) where we made our programming language by implementing the basic steps a compiler makes. 

# Project parts
This project consists of a lexical analyzer, syntax analyzer, semantic analyzer, intermediate code generator and a virtual machine which is responsible for executing the intermediate code. The intermediate code generator decomposes mathematical and logic expressions into single instruction quadruples to then be executed in the virtual machine. The logic for conditional statements and loops is achieved by simple jumps or GOTO's within memory.

# Variables and Memory
Simple variables and composed variables (such as array and matrices) are stored within a symbol table. It is important to note that all variables are global within the program. Memory spaces for arrays, matrixes and cubes are stored in an array that simulates memory slots using the flat memory model.

# Code samples
Within the repo there are some code examples to understand the syntax, variable types, definition of functions, and console input and output

# Dependencies

This project uses PLY(Python Lex-Yacc) for the lexical analyzer and syntax analyzer. You can find more information about it here [PLY LEX-YACC](https://www.dabeaz.com/ply/)
