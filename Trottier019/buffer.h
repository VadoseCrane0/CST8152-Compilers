/* REPLACE the file header below with your file header (see CST8152_ASSAMG.pdf for details).
* File Name: buffer.h
* Version: 1.19.1
* Author: S^R
* Date: 1 January 2019
* Preprocessor directives, type declarations and prototypes necessary for buffer implementation
* as required for CST8152-Assignment #1.
* The file is not completed.
* You must add your function declarations (prototypes).
* You must also add your constant definitions and macros,if any.
*/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 -1         /* operation failure return value 1 */
#define RT_FAIL_2 -2         /* operation failure return value 2 */
#define LOAD_FAIL -2         /* load fail return value */

#define DEFAULT_INIT_CAPACITY 200   /* default initial buffer capacity */
#define DEFAULT_INC_FACTOR 15       /* default increment factor */


/* You should add your own constant definitions here */

/* Add your bit-masks constant definitions here */
#define DEFAULT_FLAGS  0xFFFC
#define SET_EOB  
#define RESET_EOB 
#define CHECK_EOB 
#define SET_R_FLAG 
#define RESET_R_FLAG 
#define CHECK_R_FLAG 

/* user data type declarations */
typedef struct BufferDescriptor {
    char *cb_head;   /* pointer to the beginning of character array (character buffer) */
    short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
    short addc_offset;  /* the offset (in chars) to the add-character location */
    short getc_offset;  /* the offset (in chars) to the get-character location */
    short markc_offset; /* the offset (in chars) to the mark location */
    char  inc_factor; /* character array increment factor */
    char  mode;       /* operational mode indicator*/
    unsigned short flags;     /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, *pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */
Buffer * b_allocate(short, char, char);
pBuffer b_addc(pBuffer const, char);
int b_clear(Buffer * const);
void b_free(Buffer * const);
int b_isfull(Buffer * const);
short b_limit(Buffer * const);
short b_capacity(Buffer * const);
short b_mark(pBuffer const, short);
int b_mode(Buffer * const);
size_t b_incfactor(Buffer * const);
int b_load(FILE * const, Buffer * const);
int b_isempty(Buffer * const);
char b_getc(Buffer * const);
int b_eob(Buffer * const);
int b_print(Buffer * const);
Buffer * b_compact(Buffer * const, char);
char b_rflag(Buffer * const);
short b_retract(Buffer * const);
short b_reset(Buffer * const);
short b_getcoffset(Buffer * const);
int b_rewind(Buffer * const);
char * b_location(Buffer * const);
/*
Place your function declarations here.
Do not include the function header comments here.
Place them in the buffer.c file
*/

#endif

