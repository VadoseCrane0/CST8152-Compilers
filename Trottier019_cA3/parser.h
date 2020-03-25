#pragma once
#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */
#include "buffer.h"
#include "token.h"


#define ELSE 0
#define FALSE 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

#define NO_ATTR -1
static Token lookahead;
extern int line;
extern Buffer * str_LTBL;
extern char * kw_table[];
int synerrno;


extern Token malar_next_token(void);
void parser(void);
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe(void);
void gen_incode(char *);

void program(void);
void opt_statements(void);
void statements(void);
void statements_prime(void);
void statement(void);
void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void iteration_statement(void);
void pre_condition(void);
void input_statement(void);
void variable_list(void);
void variable_list_prime(void);
void output_statement(void);
void output_list(void);
void variable_identifier(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_prime(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_prime(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void string_expression_prime(void);
void primary_string_expression(void);
void logical_OR_expression(void);
void logical_OR_expression_prime(void);
void logical_AND_expression(void);
void logical_AND_expression_prime(void);
void relational_expression(void);
void primary_arelational_expression_prime(void);
void primary_srelational_expression_prime(void);
void primary_arelational_expression(void);
void primary_srelational_expression(void);