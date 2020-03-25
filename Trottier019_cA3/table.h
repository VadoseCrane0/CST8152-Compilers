 /* Filename: scanner.c
  * Transition Table and function declarations necessary for the scanner implementation  
  * as required for CST8152 - Assignment #2.
  *    scanner_init() must be called before using the scanner.
  *    Provided by: Svillen Ranev
  *	  Modified by: Joseph Trottier(040866019)
  *	  Date: 17 February 2019
  */

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif 

#define FLTMIN -3.4E+38
#define FLTMAX +3.4E+38

/*REPLACE *ESN* and *ESR* WITH YOUR ERROR STATE NUMBER*/ 
#define ES  11 /*11?*/	/* Error state  with no retract */
#define ER  12 /*12?*/	/* Error state  with retract */
#define IS -1    /* Inavalid state */

#define TABLE_COLUMNS 8

/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/*L,0,#,.,@,other,",SEOF*/
	/* State 0 */  {1,6,4,ES,ES,ES,9,ER},
	/* State 1 */  {1,1,1,2,3,2,2,2},
	/* State 2 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 3 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 4 */  {ES,4,4,7,5,5,5,5},
	/* State 5 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 6 */  {ES,6,ES,7,5,5,5,5},
	/* State 7 */  {8,7,7,8,8,8,8,8},
	/* State 8 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 9 */  {9,9,9,9,9,9,10,ER},
	/* State 10 */ {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 11 */ {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 12 */ {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 13 */
};
/* Accepting state table definition */
#define ASWR     1  /* accepting state with retract */
#define ASNR     2  /* accepting state with no retract */
#define NOAS     3  /* not accepting state */

/*size 14*/
int as_table[] = {NOAS,NOAS,ASWR,ASNR,NOAS,ASWR,NOAS,NOAS,ASWR,NOAS,ASNR,ASNR,ASWR,NOAS};
/* Accepting action function declarations */
Token aa_func02(char *lexeme);
Token aa_func03(char *lexeme);
Token aa_func05(char *lexeme);
Token aa_func08(char *lexeme);
Token aa_func10(char *lexeme);
Token aa_func12(char *lexeme);

/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  
typedef Token (*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */
PTR_AAF aa_table[] = {
		NULL,
		NULL,
		&aa_func02,
		&aa_func03,
		NULL,
		&aa_func05,
		NULL,
		NULL,
		&aa_func08,
		NULL,
		&aa_func10,
		NULL,
		&aa_func12,
		NULL
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */
#define KWT_SIZE  10
char * kw_table []=
	{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"   
	};
#endif
                     