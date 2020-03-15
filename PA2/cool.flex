/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */

/* ---  Declarations begin: */

%{

#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>
#include <string>


/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex


/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */


extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */

#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;


extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

static int comment_layer = 0;
//static int null_char_present = 0;
static std::string cur_string;

%}



DARROW          =>
%START COMMENT INLINE_COMMENT STRING



%%

 /* ====================
  *  Nested comments
  * ==================== */

<INITIAL>-- { BEGIN INLINE_COMMENT; }

<INLINE_COMMENT>\n {
	BEGIN 0;
	curr_lineno++;
 }

<INLINE_COMMENT><<EOF>> {
	BEGIN 0;
 }
	
<INLINE_COMMENT>[^\n]* ;   /* words in inline comment will be first matched here */


<INITIAL>\(\* {
	BEGIN COMMENT;
	comment_layer++;  
}

<COMMENT>\(\*  comment_layer++;

<COMMENT>\*\) {
	comment_layer--;
	if(comment_layer == 0) 
		BEGIN 0;
}

<COMMENT>[^*()\n]* ;  /* words in comment will be matched first here */
<COMMENT>[*()] ;  /* if a single * ( ) , no effect on start or end of comment */
<COMMENT>\n curr_lineno++;

\*\) {
	yylval.error_msg = "Unmatched *)";
	return ERROR;
}

<COMMENT><<EOF>> {
	BEGIN 0;
	yylval.error_msg = "EOF in comment.";
	return ERROR;
}

 /* ===================================
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  * ==================================
  */

<INITIAL>\" {
	BEGIN STRING;
}

<STRING>[^\\\"\n]* { yymore(); }  /* words in a string will be first matched here */
<STRING>\\[^\n] { yymore(); }  /* normal escape */
<STRING>\\\n { yymore();  curr_lineno++; }  /* escape with '\n' */

<STRING>\n {   /* unescaped '\n' */
	curr_lineno++;
	BEGIN 0;
	yylval.error_msg = "Unterminated string constant";
	return ERROR;
}
<STRING><<EOF>> {  /* ??? */
	BEGIN 0;
	yylval.error_msg = "EOF in constant string";
	yyrestart(yyin);   //!! when an EOF meets, use yystart(yyin)
	return ERROR;
}

<STRING>\" {  /* when a string ends */
	BEGIN 0 ;
	std::string str1(yytext, yyleng-1);  //ignore the \" at the end
 	std::string str2 = "" ;

	if(str1.find('\0') != std::string::npos){  /* '\0' in string */
		yylval.error_msg = "String contains null character";
		return ERROR;
	}

	std::string::size_type pos;
	while((pos = str1.find('\\')) != std::string::npos ){  /* deal with normal escape in the string */
		str2 += str1.substr(0, pos);
		switch(str1[pos+1]){  //note that \\\* will not match this rule
			case'b': 
				str2+="\b";
				break;
			case't':
				str2+="\t";
				break;
			case'f':
				str2+="\f";
				break;
			case'n':
				str2+="\n";
				break;
			default:
				str2+=str1[pos+1];
				break;
		}
		str1 = str1.substr(pos+2);
	}
	str2 += str1;  //!
	

	if(str2.size()>=MAX_STR_CONST){
		yylval.error_msg = "String constant too long";
		return ERROR;
	}

	yylval.symbol = stringtable.add_string((char *)str2.c_str());
	return STR_CONST;
		
}

 /* ========================================
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  ========================================= */
(?i:class) return CLASS;
(?i:else) return ELSE;
(?i:fi) return FI;
(?i:if) return IF;
(?i:in) return IN;
(?i:inherits) return INHERITS;
(?i:let) return LET;
(?i:loop) return LOOP;
(?i:pool) return POOL;
(?i:then) return THEN;
(?i:while) return WHILE;
(?i:case) return CASE;
(?i:esac) return ESAC;
(?i:of) return OF;
(?i:new) return NEW;
(?i:isvoid) return ISVOID;
(?i:not) return NOT;


 /* INT_CONST, BOOL_CONST, NEWLINE, WHITESPACE, TYPEID,OBJECTID */


[0-9]+ { 
	yylval.symbol = inttable.add_string(yytext);
	return INT_CONST;
}


f(?i:alse) { 
	yylval.boolean = 0;
	return BOOL_CONST;
}
t(?i:rue) {
	 yylval.boolean = 1;
	 return BOOL_CONST; 
}


[A-Z][A-Za-z0-9_]* {
	yylval.symbol = idtable.add_string(yytext);
	return TYPEID; 
}


[a-z][A-Za-z0-9_]* {
	yylval.symbol = idtable.add_string(yytext);
	return OBJECTID; 
}


[ \f\r\v\t]+  ;


"\n"  curr_lineno++;

 /* ========================================
  *  The multiple-character operators, and single-character operators.
  ========================================== */

{DARROW}		{ return (DARROW); }
"<-" return ASSIGN;
"<=" return LE;

"+" return int('+');
"-" return int('-');
"*" return int('*');
"/" return int('/');
"~" return int('~');
"<" return int('<');
"=" return int('=');
"." return int('.');
"," return int(',');
"@" return int('@');
":" return int(':');
";" return int(';');
"(" return int('(');
")" return int(')');
"{" return int('{');
"}" return int('}');

. {
	 yylval.error_msg = yytext;
         return ERROR; 
}	

%%
