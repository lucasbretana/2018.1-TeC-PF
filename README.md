# TeC-PF - sites.google.com/site/haskellufpel
Haskell classes

# Parser
  - Receive a expression in a string, and
  - Wil return the same expression in a data format
  
  __Notes__
  - There can be only + and * operations, 
  - The precende is left to right (as usual)
  - The expression may contain parenthesis to change precendence of operations

  __Ideia for solution__
  - DONE
    - _tokenizer_: a function that given a string it return the first token on that, and the rest of the string

  - TODO
    - a function that receive that look for:
      - '+' then the interprets the left part and A and right part as B
      - '*' then the interprets the left part and A and right part as B
      - '(' then interprets everything inside as an expression
      - Num Int: witch will be a final expression
