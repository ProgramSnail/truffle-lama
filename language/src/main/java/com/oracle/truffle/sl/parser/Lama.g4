/*
 * Copyright (c) 2012, 2021, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * The Universal Permissive License (UPL), Version 1.0
 *
 * Subject to the condition set forth below, permission is hereby granted to any
 * person obtaining a copy of this software, associated documentation and/or
 * data (collectively the "Software"), free of charge and under any and all
 * copyright rights in the Software, and any and all patent rights owned or
 * freely licensable by each licensor hereunder covering either (i) the
 * unmodified Software as contributed to or provided by such licensor, or (ii)
 * the Larger Works (as defined below), to deal in both
 *
 * (a) the Software, and
 *
 * (b) any piece of software and/or hardware listed in the lrgrwrks.txt file if
 * one is included with the Software each a "Larger Work" to which the Software
 * is contributed by such licensors),
 *
 * without restriction, including without limitation the rights to copy, create
 * derivative works of, display, perform, and distribute the Software and make,
 * use, sell, offer for sale, import, export, have made, and have sold the
 * Software and the Larger Work(s), and to sublicense the foregoing rights on
 * either these or other terms.
 *
 * This license is subject to the following condition:
 *
 * The above copyright notice and either this complete permission notice or at a
 * minimum a reference to the UPL must be included in all copies or substantial
 * portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
 * The parser and lexer need to be generated using "mx create-sl-parser".
 */

grammar Lama;

@parser::header
{
// DO NOT MODIFY - generated from SimpleLanguage.g4 using "mx create-sl-parser"

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.sl.SLLanguage;
import com.oracle.truffle.sl.nodes.SLExpressionNode;
import com.oracle.truffle.sl.nodes.SLStatementNode;
}

@lexer::header
{
// DO NOT MODIFY - generated from SimpleLanguage.g4 using "mx create-sl-parser"
}

@parser::members
{
private SLNodeFactory factory;
private Source source;

private static final class BailoutErrorListener extends BaseErrorListener {
    private final Source source;
    BailoutErrorListener(Source source) {
        this.source = source;
    }
    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e) {
        throwParseError(source, line, charPositionInLine, (Token) offendingSymbol, msg);
    }
}

public void SemErr(Token token, String message) {
    assert token != null;
    throwParseError(source, token.getLine(), token.getCharPositionInLine(), token, message);
}

private static void throwParseError(Source source, int line, int charPositionInLine, Token token, String message) {
    int col = charPositionInLine + 1;
    String location = "-- line " + line + " col " + col + ": ";
    int length = token == null ? 1 : Math.max(token.getStopIndex() - token.getStartIndex(), 0);
    throw new SLParseError(source, line, col, length, String.format("Error(s) parsing script:%n" + location + message));
}

public static Map<TruffleString, RootCallTarget> parseSL(SLLanguage language, Source source) {
    SimpleLanguageLexer lexer = new SimpleLanguageLexer(CharStreams.fromString(source.getCharacters().toString()));
    SimpleLanguageParser parser = new SimpleLanguageParser(new CommonTokenStream(lexer));
    lexer.removeErrorListeners();
    parser.removeErrorListeners();
    BailoutErrorListener listener = new BailoutErrorListener(source);
    lexer.addErrorListener(listener);
    parser.addErrorListener(listener);
    parser.factory = new SLNodeFactory(language, source);
    parser.source = source;
    parser.simplelanguage();
    return parser.factory.getAllFunctions();
}
}

// parser


compilation_unit : (import_expression)* scope_expression EOF;

import_expression : 'import' UIDENT ';';

scope_expression :
      definition+ (expression)?
    | expression;

definition :
      variable_definition
    | function_definition
    | infix_definition;

//

variable_definition : ('var' | 'public') variable_definition_sequence;
variable_definition_sequence : variable_definition_item (',' variable_definition_item)* ';';
variable_definition_item : LIDENT ('=' basic_expression)?;
function_definition : ('public')? 'fun' LIDENT '(' (function_arguments)? ')' function_body;
function_arguments : LIDENT (',' LIDENT)*;
function_body : '{' scope_expression '}';

//

infix_definition : infix_head '(' function_arguments ')' function_body;
infix_head : ('public')? infixity INFIX level;
infixity : 'infix' | 'infixl' | 'infixr';
level : ('at' | 'before' | 'after') INFIX;

//

expression : basic_expression (';' expression)?;
basic_expression : binary_expression;
binary_expression : postfix_expression (INFIX postfix_expression)*;
postfix_expression : ('-')? primary (postfix)*;
postfix :
    '(' expression (',' expression)* ')'
  | '[' expression ']';

primary:
    DECIMAL_LITERAL
  | STRING_LITERAL
  | CHAR_LITERAL
  | LIDENT
  | 'true'
  | 'false'
  | 'infix' INFIX
  | 'fun' '(' function_arguments ')' function_body
  | 'skip'
  | '(' scope_expression ')'
  | list_expression
  | array_expression
  | s_expression
  | if_expression
  | while_do_expression
  | do_while_expression
  | for_expression
  | case_expression
;

//

array_expression : '[' (expression (',' expression)* )? ']';
list_expression : '{' (expression (',' expression)* )? '}';
s_expression : /*TODO */ UIDENT ('(' (expression (',' expression)* )? ')')?;

//

if_expression : 'if' expression 'then' scope_expression (else_part)? 'fi';
else_part :
    'elif' expression 'then' scope_expression (else_part)?
  | 'else' scope_expression;

//

while_do_expression : 'while' expression 'do' scope_expression 'od';
do_while_expression : 'do' scope_expression 'while' expression 'od';
for_expression : 'for' scope_expression ',' expression ',' expression 'do' scope_expression 'od';

//

pattern: cons_pattern '|' simple_pattern;
cons_pattern : simple_pattern ':' pattern;
simple_pattern :
    wildcard_pattern
  | s_expr_pattern
  | array_pattern
  | list_pattern
  | LIDENT ('@' pattern)?
  | ('-')? DECIMAL_LITERAL
  | STRING_LITERAL
  | CHAR_LITERAL
  | 'true'
  | 'false'
  | '#' 'box'
  | '#' 'val'
  | '#' 'str'
  | '#' 'array'
  | '#''sexp'
  | '#' 'fun'
  | '(' pattern ')'
;

wildcard_pattern : '_';
s_expr_pattern : UIDENT ('(' (pattern (',' pattern)*)? ')')?;
array_pattern : '[' (pattern (',' pattern)*)? ']';
list_pattern : '{' (pattern (',' pattern)*)? '}';

//

case_expression : 'case' expression 'of' case_branches 'esac';
case_branches : case_branch ('|' case_branch)*;
case_branch : pattern '->' scope_expression;

// lexer

WS : [ \t\r\n\u000C]+ -> skip;
COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT : '//' ~[\r\n]* -> skip;

fragment NON_ZERO_DIGIT : [1-9];
fragment DIGIT : [0-9];
fragment STRING_CHAR : ~('"' | '\r' | '\n');

WORD : [a-zA-Z_0-9]+;

INFIX : [+*/%$#@!|&^?<>.:=\-]+;
UIDENT : [A-Z][a-zA-Z_0-9]*;
LIDENT : [a-z][a-zA-Z_0-9]*;

CHAR_LITERAL : '\'' STRING_CHAR '\'';
STRING_LITERAL : '"' STRING_CHAR* '"';
DECIMAL_LITERAL : '0' | ('-'?) NON_ZERO_DIGIT DIGIT*;
