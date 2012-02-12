(*

  interpret.pp
  Project 1
  CS 3304 Fall 2005
  Alpha Chen

  An OSIL (Our Simple Imperative Language) interpreter.

  As the program is read from standard input, each line is stored in
  a linked list for execution of while loops. Each node holds one line,
  so the current node is kept both as a tail and to read tokens from.
  Ideally, each statement should consume all of its relevant tokens,
  so we can resume parsing without dropping the last token.

*)

program interpret;

  type { This is just for the linked list nodes. }
    PLine_Node = ^Line_Node;
    Line_Node = record
      line: String;
      next: PLine_Node;
    end;

  var
    head: PLine_Node;
    current_node: PLine_Node;
    current_token: String;
    current_location: Integer;
    variables: Array[0..25] of Integer; { Holds the program state. }
    end_of_file: Boolean; { So that we don't throw a syntax error on end of file. }

  {
    Getter methods.
  }

  function current_line: String;
    begin
      current_line := current_node^.line;
    end;

  function current_char: Char;
    begin
      current_char := current_line[current_location];
    end;

  {
    Reading from input.
  }

  procedure get_new_line; forward;

  { Load the next token into current_token. }
  procedure load_token;
    function is_whitespace: Boolean;
      begin
        is_whitespace := current_char in [' ', chr(9), chr(10), chr(13)]
      end;

    var
      token_start: Integer;
    begin { load_token }
      { Skip whitespace. }
      while(is_whitespace) do
        current_location := current_location + 1;

      { No more whitespace, so we're currently at a token. }
      token_start := current_location;
      { Go until the token ends (i.e., we hit whitespace). }
      while(not is_whitespace) do
        current_location := current_location + 1;

      { Load the token. }
      current_token := copy(current_line, token_start, current_location - token_start);

      { If there is no token, we've hit the end of the line,
        so we go get a new one. }
      if(length(current_token) = 0) then
        if(current_node^.next = nil) then
          get_new_line
        else { If we're in a while statement, we have the next line already. }
          begin
          current_node := current_node^.next;
          current_location := 1;
          load_token;
          end;
    end;

  { Read a line from the input and load a token, unless
    we've hit the end of the file. }
  procedure get_new_line;
    var
      new_node: PLine_Node;
    begin
      if(not eof) then
        begin
        { A new line requires a new node in the linked list. }
        new(new_node);
        new_node^.next := nil;
        current_node^.next := new_node;

        readln(new_node^.line);
        current_node := new_node;
        current_location := 1;

        { Don't forget to load the token. }
        load_token;
        end
      else
        end_of_file := true;
    end;

  { Initialize. }
  procedure init;
    var
      i: Integer;
    begin
      end_of_file := false;
    
      new(head);
      head^.next := nil;
      current_node := head;

      for i := 0 to 25 do
        variables[i] := 0;
    end;

  { Cleanup. Deallocate pointers. }
  procedure cleanup;
    var
      ptr: PLine_Node;
      next_ptr: PLine_Node;
    begin
      ptr := head^.next;
      dispose(head);

      repeat
        next_ptr := ptr^.next;
        dispose(ptr);
        ptr := next_ptr;
      until ptr = nil
    end;

  { Print the entire program, line by line,
    exactly as it was inputted. Testing 
    purposes only. }
  procedure print_program;
    var
      ptr: PLine_Node;
      i: Integer;
    begin
      writeln('Printing program:');
      i := 1;
    
      ptr := head^.next;
      repeat
        writeln(i, chr(9), ptr^.line);
        ptr := ptr^.next;
        i := i + 1;
      until ptr = nil
    end;

  { Print the current state (variables) of
    the program. Testing purposes only. }
  procedure print_variables;
    var
      i: Integer;
    begin
      for i := 0 to 25 do
        writeln(chr(i + ord('a')), ': ', variables[i]);
    end;

  { This test is also needed for the main program. }
  function semicolon_test: Boolean;
    begin
      semicolon_test := ((length(current_token) = 1) and (current_token[1] = ';'));
    end;

  {
    Begin parsing procedures.
  }
  
  { Each program is made of zero or more statements. This
    is where each line is actually parsed, and each line
    can be one of these six statements:

      assignment
      print
      block
      if
      while
      skip
    }
  function parse_statement(side_effect: Boolean): Boolean; 
    function variable_test: Boolean;
      begin
        variable_test := (length(current_token) = 1 ) and ((current_token >= 'a') and (current_token <= 'z'))
      end;

    { An operand can be either a variable or an integer. }
    function parse_operand(var value: Integer): Boolean;
      var
        error: Integer;
      begin
        parse_operand := false;

        if(variable_test) then
          begin
          value := variables[ord(current_token[1]) - ord('a')];
          parse_operand := true;
          end
        else
          begin
          val(current_token, value, error);
          if((error = 0) and (value >= 0)) then { Don't accept negative integers. }
          {if(error = 0) then }
            parse_operand := true;
          end;
      end;

    { An expression is a series of operands added and subtracted
      to one another. }
    function parse_expression(var value: Integer): Boolean;
      var
        temp_value: Integer;
      begin
        parse_expression := false;

        if(parse_operand(value)) then
          begin { there has to be at least one operand. }
          parse_expression := true;

          load_token;

          while((length(current_token) = 1)) do
            begin { While there's addition or subtraction. }

            if(current_token[1] = '+') then
              begin
              load_token;
              if(not parse_operand(temp_value))  then
                begin
                parse_expression := false;
                break;
                end;
              value := value + temp_value
              end

            else
              if(current_token[1] = '-') then
                begin
                load_token;
                if(not parse_operand(temp_value)) then
                  begin
                  parse_expression := false;
                  break;
                  end;
                value := value - temp_value;
                end
              else { Not a '+' or '-'. }
                break;

            load_token; 
            end;
          end;
      end;
  
    { Parses the assignment statement. The BNF form is:
        <variable> := <expression> ;
      }
    function parse_assignment(side_effect: Boolean): Boolean;
      var
        variable: Integer;
        value: Integer;
      begin
        parse_assignment := false;

        if(variable_test) then
          begin
          variable := ord(current_token[1]) - ord('a');

          load_token;

          if(current_token = ':=') then
            begin
            load_token;
            
            if(parse_expression(value)) then
              if(semicolon_test) then
                begin
                if(side_effect) then
                  variables[variable] := value;
                parse_assignment := true;
                load_token;
                end;

            end;
          end;
      end;

    { Parse all statements which aren't assignment
      statements. This includes:
        print
        block
        if
        while
        skip
      
      Note that all the parse functions need to load
      a token in the beginning. This is to make the
      statement parsing code nicer. }
    function parse_nonassignment(side_effect: Boolean): Boolean;

      { Prints the value of a given expression
        to the screen.
        
        print <expression> ;
        }
      function parse_print(side_effect: Boolean): Boolean;
        var
          value: Integer;
        begin
          parse_print := false;

          load_token;
          if(parse_expression(value)) then
            if(semicolon_test) then
              begin
              if(side_effect) then
                writeln(value);
              parse_print := true;
              load_token;
              end;
        end;

      { A block statement.
      
        begin <statements> end ;
        }
      function parse_block(side_effect: Boolean): Boolean;
        var
          error: Boolean;
        begin
        parse_block := false;
        error := true;

        load_token;
        while(not (current_token = 'end')) do
          begin
          error := parse_statement(side_effect);
          if(not error) then
            break;
          end;

        if(error and (current_token = 'end')) then
          begin { Make sure we exit on the end of the block. }
          load_token;
          if(semicolon_test) then
            begin
            parse_block := true;
            load_token;
            end;
          end;

        end;

      { A if statement.
        
        if <expression> then <statement> else <statement>
        }
      function parse_if(side_effect: Boolean): Boolean;
        var
          value: Integer;
        begin
        parse_if := false;

        load_token;
        if(parse_expression(value)) then
          if(current_token = 'then') then
            begin
            load_token;
            if(parse_statement((value > 0) and side_effect)) then
              if(current_token = 'else') then
                begin
                load_token;
                if(parse_statement((value <= 0) and side_effect)) then
                  parse_if := true;
                end;
            end;
        end;

      { A while statement.

        while <expression> do <statement>
        }
      function parse_while(side_effect: Boolean): Boolean;
        var
          expr_node: PLine_Node;
          expr_location: Integer;
          value: Integer;
        begin
        parse_while := false;
        
        expr_node := current_node;
        expr_location := current_location;

        load_token;
        if(parse_expression(value)) then
          while(current_token = 'do') do
            begin
            parse_while := true;

            if(value > 0) then
              begin
              load_token;

              if(not parse_statement(side_effect)) then
                begin
                parse_while := false;
                break;
                end;

              { Go back to the beginning of the while test. }
              current_node := expr_node;
              current_location := expr_location;
              load_token;
              parse_expression(value); { Already checked syntax above. }
              end
            else { Break out of the while. }
              begin { The easiest way to go past all the statements. }
              load_token;
              parse_while := parse_statement(false);
              end;
            end; { while }
        end;

      { A null op. 
      
        skip ;
        }
      function parse_skip(side_effect: Boolean): Boolean;
        begin
        parse_skip := false;

        load_token;
        if(semicolon_test) then
          begin
          parse_skip := true;
          load_token;
          end;
        end;   
        
      begin { parse_nonassignment }
        parse_nonassignment := false;

        if(current_token = 'print') then
          parse_nonassignment := parse_print(side_effect)
        else
          if(current_token = 'begin') then
            parse_nonassignment := parse_block(side_effect)
          else
            if(current_token = 'if') then
              parse_nonassignment := parse_if(side_effect)
            else
              if(current_token = 'while') then
                parse_nonassignment := parse_while(side_effect)
              else
                if(current_token = 'skip') then
                  parse_nonassignment := parse_skip(side_effect);
      end;

    begin { parse_statement }
      parse_statement := false;

      if(length(current_token) = 1) then
        parse_statement := parse_assignment(side_effect)
      else
        parse_statement := parse_nonassignment(side_effect);
    end;

begin
  init;

  get_new_line;

  while(not end_of_file) do
    if(not parse_statement(true)) then
      begin
      writeln('Syntax error.');
      if(not end_of_file) then
        begin
        if(not semicolon_test) then
          repeat
            load_token
          until semicolon_test;
        load_token;
        end;
      end;
  
  cleanup;
end.
