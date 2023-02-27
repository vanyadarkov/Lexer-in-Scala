# Lexer implementation in Scala
## Regex format

```
<regex> ::= <regex><regex> | 
            <regex> '|' <regex> | 
            <regex>'*' | <regex>'+' | <regex>'?' | 
            '(' <regex> ')' | 
            "[A-Z]" |
            "[a-z]" |
            "[0-9]" |
            "eps" | <character>
```

Examples:
- ```[0-9]*|b```
- ```a([a-z]*|[A-Z]*)z```
- ```[0-9]+(\'-\'[0-9]+)*```

Inside, each regex is preprocessed and passed to a prenex form (e.g. **CONCAT a b**) then converted to an DFA, where each DFA is built from an NFA.

## Lexer input
Lexer's input consists of 2 components:
1. a specification (configuration)
2. a text that will be analyzed lexically, more precisely, divided into lexemes.


Specificatia are urmatoarea structura:
```
TOKEN1 : REGEX1;
TOKEN2 : REGEX2;
TOKEN3 : REGEX3;
...
```
where each TOKENi is a name given to a token and REGEXi is a regex which describes that token.

## Lexer output
Lexer's output is a list of form: ```[(lexeme1, TOKEN_LEXEME_1), (lexeme2, TOKEN_LEXEME_2), …]```, where TOKEN_LEXEME_i is the name associated to token of lexeme i, based on specification.
## Implementation
1. each regex is converted to NFA, saving the information about its token and its position in specification.
2. an unique NFA is built which connects all NFAs for every regexes from specification
3. this "big" NFA is converted to a DFA.

## Project structure
```
...
└── src
    └── main
        └── scala
            ├── Dfa.scala - DFA class
            ├── Nfa.scala - NFA class
            ├── Regex.scala - Regex preprocessing object
            ├── Lexer.scala - lexer class
            ├── AST.scala - abstract syntax tree class (used to build an AST from a given prenex) 
            ├── configuration - a configuration file for a simple programming language
    └── test
	    └── scala
		    └── prog_tests - folder with source code files in that simple programming language
			    ...
			└── scala - folder with tests of every component of this project
				...
```

## Copyright
The test files and project idea belong to the LFA UPB 2022-2023 team, Faculty of Automation and Computer Science. Implementation and source files belong to [vanyadarkov](https://github.com/vanyadarkov).