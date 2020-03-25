 /* Filename: scanner.c
 /* PURPOSE:
 *    SCANNER.C: Functions implementing a Lexical Analyzer (Scanner)
 *    as required for CST8152, Assignment #2
 *    scanner_init() must be called before using the scanner.
 *    Provided by: Svillen Ranev
 *	  Modified by: Joseph Trottier(040866019)
 *	  Date: 17 February 2019
 */

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */


/*Initializes scanner */
int scanner_init(Buffer * psc_buf) {
  	if(b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
//	printf("Attribute: %d\n", b_limit(str_LTBL));
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

Token malar_next_token(void) {
	{
		Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
		unsigned char c; /* input symbol */
		int state = 0; /* initial state of the FSM */
		short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
		short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
		int accept = NOAS; /* type of state - initially not accepting */
		char temp;
		while (1) { /* endless loop broken by token returns it will generate a warning */
		/*GET THE NEXT SYMBOL FROM THE INPUT BUFFER */
			c = b_getc(sc_buf);
			switch (c) {
			case ' ':
				continue;
			case '\t':
				continue;
			case '{':
				t.code = LBR_T; /*no attribute */
				return t;
			case '}':
				t.code = RBR_T;
				return t;
			case '(':
				t.code = LPR_T; /*no attribute */
				return t;
			case ')':
				t.code = RPR_T;
				return t;
			case '\0':
				t.code = SEOF_T;
				t.attribute.seof = SEOF_0;
				return t;
			case 255:
				t.code = SEOF_T;
				t.attribute.seof = SEOF_EOF;
				return t;
				/*Arrithmatic ops*/
			case '+':
				t.code = ART_OP_T;
				t.attribute.arr_op = PLUS;
				return t;
			case '-':
				t.code = ART_OP_T;
				t.attribute.arr_op = MINUS;
				return t;
			case '*':
				t.code = ART_OP_T;
				t.attribute.arr_op = MULT;
				return t;
			case '/':
				t.code = ART_OP_T;
				t.attribute.arr_op = DIV;
				return t;
				/*relation op & assignment & string cat*/
			case '>':
				t.code = REL_OP_T;
				t.attribute.rel_op = GT;
				return t;
			case ';':
				t.code = EOS_T;
				return t;
			case ',':
				t.code = COM_T;
				return t;
			case '<':
				c = b_getc(sc_buf);
				if (c == '<') {
					t.code = SCC_OP_T;
					return t;
				}
				else if (c == '>') {
					t.code = REL_OP_T;
					t.attribute.rel_op = NE;
					return t;
				}
				b_retract(sc_buf);
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				return t;
			case '=':
				c = b_getc(sc_buf);
				if (c == '=') {
					t.code = REL_OP_T;
					t.attribute.rel_op = EQ;
				}
				else {
					b_retract(sc_buf);
					t.code = ASS_OP_T;
				}
				return t;
			case '.':
				temp = b_getc(sc_buf);
				if (temp == 'A') {
					temp = b_getc(sc_buf);
					if (temp == 'N') {
						temp = b_getc(sc_buf);
						if (temp == 'D') {
							temp = b_getc(sc_buf);
							if (temp == '.') {
								t.code = LOG_OP_T;
								t.attribute.log_op = AND;
								return t;
							}
							else {
								t.code = ERR_T;
								t.attribute.err_lex[0] = c;
								
								t.attribute.err_lex[1] = '\0';
								b_retract(sc_buf);
								b_retract(sc_buf);
								b_retract(sc_buf);
								b_retract(sc_buf);
								return t;
							}
						}
						else {
							t.code = ERR_T;
							t.attribute.err_lex[0] = c;
							
							t.attribute.err_lex[1] = '\0';
							b_retract(sc_buf);
							b_retract(sc_buf);
							b_retract(sc_buf);
							return t;
						}
					}
					else {
						t.code = ERR_T;
						t.attribute.err_lex[0] = c;
						
						t.attribute.err_lex[1] = '\0';
						b_retract(sc_buf);
						b_retract(sc_buf);
						return t;
					}
				}
				else if (temp == 'O') {
					temp = b_getc(sc_buf);
					if (temp == 'R') {
						temp = b_getc(sc_buf);
						if (temp == '.') {
							t.code = LOG_OP_T;
							t.attribute.log_op = OR;
							return t;
						}
						else {
							t.code = ERR_T;
							t.attribute.err_lex[0] = c;
							
							t.attribute.err_lex[1] = '\0';
							b_retract(sc_buf);
							b_retract(sc_buf);
							b_retract(sc_buf);
							return t;
						}
					}
					else {
						t.code = ERR_T;
						t.attribute.err_lex[0] = c;
						
						t.attribute.err_lex[1] = '\0';
						b_retract(sc_buf);
						b_retract(sc_buf);
						return t;
					}
				}
				else {
					t.code = ERR_T;
					t.attribute.err_lex[0] = c;
					
					t.attribute.err_lex[1] = '\0';
					b_retract(sc_buf);
					return t;
				}
			case '#':
				t.code = ERR_T;
				t.attribute.err_lex[0] = c;
				t.attribute.err_lex[1] = '\0';
				return t;
			case '_':
				t.code = ERR_T;
				t.attribute.err_lex[0] = c;
				t.attribute.err_lex[1] = '\0';
				return t;
			case '!':
				temp = b_getc(sc_buf);
				if (temp != '!') {
					t.code = ERR_T;
					t.attribute.err_lex[0] = c;
					t.attribute.err_lex[1] = temp;
					t.attribute.err_lex[2] = '\0';
					b_retract(sc_buf);
					while (c != '\n')/*skip till next line*/
						c = b_getc(sc_buf);
					return t;
				}
				while (c != '\n')/*skip till next line*/
					c = b_getc(sc_buf);
				line++;
				continue;
			case '\n':
				line++;
				continue;
			}
			lexstart = b_mark(sc_buf, b_getcoffset(sc_buf)-1);
			state = get_next_state(state, c, &accept);
			while (accept == NOAS) {
				c = b_getc(sc_buf);
				state = get_next_state(state, c, &accept);
			}
			if (accept == ASWR)
				b_retract(sc_buf);
			lexend = b_getcoffset(sc_buf);
			lex_buf = b_allocate(0, 0, 'a');
			b_reset(sc_buf);
			while (b_getcoffset(sc_buf) != lexend) {
				c = b_getc(sc_buf);
				b_addc(lex_buf, c);
			}
			b_compact(lex_buf, '\0');
			if (state == ES)
				++state;
			t = (aa_table[state])(b_location(lex_buf));
			b_free(lex_buf);
			return t;
		}//end while(1)
	}
}

int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif
       assert(next != IS);
#ifdef DEBUG
	if(next == IS){
	  printf("Scanner Error: Illegal state:\n");
	  printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
	  exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

int char_class (char c){
		if (isalpha(c))
			return 0;
		else if (c == '0')
			return  1;
		else if (isdigit(c))
			return  2;
		else if (c == '.')
			return  3;
		else if (c == '@')
			return  4;
		else if (c == '"')
			return  6;
		else if (c == '\0' || c == EOF)
			return  7;
		else
			return  5;
}

Token aa_func02(char lexeme[]){
	int keypos;
	Token t = { 0 };
	if (iskeyword(lexeme) != -1) {
		keypos = iskeyword(lexeme);
		t.code = KW_T;
		t.attribute.kwt_idx = keypos;
		return t;
	}
		t.code = AVID_T;
		strncpy(t.attribute.vid_lex, lexeme , 8);
		return t;
}
Token aa_func03(char lexeme[]){
	Token t = { 0 };
	if (strlen(lexeme) > VID_LEN) {
		lexeme[VID_LEN-1] = '@';
		lexeme[VID_LEN] = '\0';
		t.code = SVID_T;
		strcpy(t.attribute.vid_lex, lexeme);
		return t;
		}
	lexeme[strlen(lexeme)] = '\0';
	t.code = SVID_T;
	strcpy(t.attribute.vid_lex, lexeme);
  return t;
}
Token aa_func08(char lexeme[]){
	Token t = { 0 };
//	printf("lexeme: |%s|\n", lexeme);
	double number = atof(lexeme);
	if (number != 0 && number < FLT_MIN || number > FLT_MAX) {
		t = aa_func12(lexeme);
		return t;
	}
	t.code = FPL_T;
	t.attribute.flt_value = (float)number;
  return t;
}
Token aa_func05(char lexeme[]){
	Token t = { 0 };

	long number = atol(lexeme);
//	printf("lexeme: |%s|\n", lexeme);
	if (number < 0 || number > SHRT_MAX) {
		t = aa_func12(lexeme);
		return t;
	}
	t.code = INL_T;
	t.attribute.int_value =(short) number;
	return t;
}

Token aa_func10(char lexeme[]){
	int i;
	Token t = { 0 };
	t.code = STR_T;
	t.attribute.str_offset = b_limit(str_LTBL);
	//printf("lexeme: |%s|\n", lexeme);
//	printf("Attribute %d\n", t.attribute.str_offset);
	for (i = 0; i < strlen(lexeme) - 1; i++) {
		if (lexeme[i] == '\n')
			line++;
		if (lexeme[i] == '"')
			continue;
		b_addc(str_LTBL, lexeme[i]);
	}
	b_addc(str_LTBL, '\0');
  return t;
}

Token aa_func12(char lexeme[]){
	int i;
	Token t = { 0 };
	//printf("lexeme: |%s|\n", lexeme);
	t.code = ERR_T;
		if (strlen(lexeme) > ERR_LEN) {
			t.attribute.err_lex[ERR_LEN-3] = '.';
			t.attribute.err_lex[ERR_LEN - 2] = '.';
			t.attribute.err_lex[ERR_LEN - 1] = '.';
			t.attribute.err_lex[ERR_LEN] = '\0';
		}
		
		
		for (i = 0; i < strlen(lexeme) - 1; i++) {
			if (lexeme[i] == '\n')
				line++;
		}
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN-3);
		return t;
}

int iskeyword(char * kw_lexeme) {
	int i;
	for (i = 0; i < KWT_SIZE; ++i) {
		if (strcmp(kw_lexeme, kw_table[i]) == 0) {
			return i; /*matches*/
		}
	}
	return -1;/*doesnt match*/
}