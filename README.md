# Adv-SimpleC-Parser-CS341_P5

This project is a SimpleC type checker and parser, developed as part of CS-341 (Spring 2024) under Professor Ellen Kidane. The project is written in F# and involves lexical analysis, parsing, and type-checking of SimpleC programs.

## Project Structure

- **main.fs**: The main entry point of the project. It reads a SimpleC file, performs lexical analysis, parsing, and type-checking.
- **compiler/lexer.fs**: Contains the lexer code that converts the input file into tokens.
- **compiler/parser.fs**: Contains the parser code that processes the tokens to check for syntactical correctness.
- **compiler/analyzer.fs**: Contains the code for building the symbol table.
- **compiler/checker.fs**: Contains the type-checking code.

## Prerequisites

- .NET SDK (for F#)

## How to Run the Project Locally

1. **Clone the Repository**
   ```
   git clone https://github.com/sharmaaaryan4012/Adv-SimpleC-Parser-CS341_P5.git
   cd Adv-SimpleC-Parser-CS341_P5
   ```

2. **Build the Project**
   ```
   dotnet build
   ```

3. **Run the Project**
   ```
   dotnet run
   ```

4. **Input the SimpleC File**
   - After running the project, it will prompt you to input the SimpleC filename.
   - Ensure that the SimpleC file is in the same directory or provide the correct path.

5. **Output**
   - The program will display the tokens, parsing results, symbol table, and type-checking results.

## Example

```
SimpleC filename> main.c

Compiling main.c...
...

Parsing main.c...
Success!
...

Analyzing main.c...
success
...

Symbol table: ...
...

Type-checking main.c...
success
...
```
