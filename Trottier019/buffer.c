/**********************************************************************
Filename: buffer.c
Version: 1.0
Author:Joseph Trottier
StudentNo:  040866019
Course Name/Number: 19W_CST8152_010 Compilers
Lab Sect: 012
Assignment #: 1
Assignment name: buffer.exe
Date:February 04 2019
Submission Date:February 04 2019
Professor:Svillen Ranev
Purpose:: Programming and Using Dynamic Structures (buffers) with C
*********************************************************************/
#include "buffer.h"
typedef unsigned char byte;
/********************************************************************
Function name:b_allocate
Purpose: This function creates a new buffer in memory (on the program heap).
In parameters: short init_capacity, char inc_factor, char o_mode
Out parameters:Buffer*
Version:1.0
Author:Joseph Trottier
**********************************************************************/
Buffer * b_allocate(short init_capacity, char inc_factor, char o_mode) {
	Buffer *buff = calloc(1, sizeof(Buffer));
	char *buffer;
	if (buff == NULL)
		return NULL;

	if (init_capacity < 0)
		return NULL;
	if (init_capacity > (SHRT_MAX - 1))
		return NULL;
	if (init_capacity == 0) {
		init_capacity = 200;
		if (o_mode == 'f') {
			inc_factor = '0';
			buff->mode = 0;
		}
		else
			inc_factor = 15;
	}
	else {
		buff->inc_factor = inc_factor;
	}
	
	if (!inc_factor && init_capacity) {
		buff->mode = 0;
		buff->inc_factor = 0;
	}
	if (o_mode == 'a' && inc_factor > 0 && inc_factor < 256) {
		buff->mode = 1;
		buff->inc_factor = inc_factor;
	}

	if (o_mode == 'm' && inc_factor > 0 && inc_factor < 101) {
		buff->mode = -1;
		buff->capacity = init_capacity;
		buff->inc_factor = inc_factor;
	}
	buffer = (char*)malloc(init_capacity);
	if (buffer == NULL)
		return NULL;
	buff->capacity = init_capacity;
	buff->flags = 0xFFFC; /*1111 1111 1111 1100*/
	buff->cb_head = buffer;
	buff->addc_offset = 0;
	buff->getc_offset = 0;

	return buff;
}
/********************************************************************
Function name:b_addc
Purpose: Using a bitwise operation the function resets the flags 
field r_flag bit to 0 and tries to add the character symbol to 
the character array of the given buffer pointed by pBD. 
In parameters: pBuffer const pBD, char symbol
Out parameters:pBuffer
Version:1.0
Author:Joseph Trottier
**********************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol) {
	short space;
	short newinc;
	if (!pBD)
		return NULL;
	pBD->flags &= 0xFFFE; /*1111 1111 1111 1110*/
	if (b_isfull(pBD)==1) {
		if (b_mode(pBD) == 0) {
			return NULL;
		}
		if (b_mode(pBD) == 1) {
			if (b_capacity(pBD) + (byte)b_incfactor(pBD) < 0)
				return NULL;
			if (b_capacity(pBD) + (byte)b_incfactor(pBD) > SHRT_MAX - 1)
				pBD->capacity = SHRT_MAX - 1;
			else
				pBD->capacity = b_capacity(pBD) + (byte)b_incfactor(pBD);
		}
		if (b_mode(pBD) == (-1)) {
			if (b_capacity(pBD) == SHRT_MAX - 1)
				return NULL;
			space = (SHRT_MAX - 1) - b_capacity(pBD);
			newinc = space * (short)b_incfactor(pBD) / 100;
			if ((unsigned short)b_capacity(pBD) + (unsigned short)newinc > SHRT_MAX - 1) {
				pBD->capacity = SHRT_MAX - 1;
			}
			else
				pBD->capacity = b_capacity(pBD) + newinc;
		}
		pBD->cb_head = (char*)realloc(pBD->cb_head, b_capacity(pBD));
		if (pBD->cb_head == NULL)
			return NULL;
		pBD->flags |= 0x1;
	}
		pBD->cb_head[pBD->addc_offset] = symbol;
		pBD->addc_offset++;
	
	return pBD;
}
/********************************************************************
Function name:b_clear
Purpose: The function retains the memory space currently allocated 
to the buffer, but re-initializes all appropriate data members of 
the given Buffer structure (buffer descriptor), such that the buffer 
will appear empty and the next call to b_addc() will put the character 
at the beginning of the character buffer. The function does not need to 
clear the existing contents of the character buffer. If a runtime error 
is possible, the function should return –1 in order to notify the 
calling function about the failure.
In parameters: Buffer * const pBD
Out parameters:int
Version:1.0
Author:Joseph Trottier
**********************************************************************/
int b_clear(Buffer * const pBD) {
	if (!pBD)
		return RT_FAIL_1;
	pBD->addc_offset = 0;
	pBD->markc_offset = 0;
	pBD->getc_offset = 0;
	return 1;
}
/********************************************************************
Function name:b_free
Purpose: The function de-allocates (frees) the memory occupied by the 
character buffer and the Buffer structure (buffer descriptor). The 
function should not cause abnormal behavior (crash).
In parameters: Buffer * const pBD
Out parameters:none
Version:1.0
Author:Joseph Trottier
**********************************************************************/
void b_free(Buffer * const pBD) {
	if (!pBD)
		return;
	free(pBD);
	return;
}
/********************************************************************
Function name:b_isfull
Purpose: The function returns 1 if the character buffer is full; 
it returns 0 otherwise. If a run-time error is possible, the 
function should return –1.
In parameters: Buffer * const pBD
Out parameters:int
Version:1.0
Author:Joseph Trottier
**********************************************************************/
int b_isfull(Buffer * const pBD) {
	if (!pBD)
		return RT_FAIL_1;
	if (pBD->addc_offset == b_capacity(pBD))
		return 1;
	return 0;
}
/********************************************************************
Function name:b_limit
Purpose: The function returns the current limit of the character buffer. 
In parameters: Buffer * const pBD
Out parameters:short
Version:1.0
Author:Joseph Trottier
**********************************************************************/
short b_limit(Buffer * const pBD) {
	if (!pBD)
		return RT_FAIL_1;
	return (pBD->addc_offset * sizeof(char)); /*sizeof char is 1 but if you wanted to store other data types you just have to change char to a different datatype*/
}
/********************************************************************
Function name:b_capacity
Purpose: The function returns the current capacity of the character buffer.
In parameters: Buffer * const pBD
Out parameters:short
Version:1.0
Author:Joseph Trottier
**********************************************************************/
short b_capacity(Buffer * const pBD) {
	if (!pBD)
		return RT_FAIL_1;
	return pBD->capacity;
}
/********************************************************************
Function name:b_mark
Purpose: The function sets markc_offset to mark.
In parameters: pBuffer const pBD, short mark
Out parameters:short
Version:1.0
Author:Joseph Trottier
**********************************************************************/
short b_mark(pBuffer const pBD, short mark) {
	
	if (!pBD)
		return RT_FAIL_1;
	if (mark >= 0)
		if (mark <= pBD->addc_offset)
			pBD->markc_offset = mark;
	return pBD->markc_offset;
}
/********************************************************************
Function name:b_mode
Purpose: The function returns the value of mode to the calling function. 
In parameters: Buffer * const pBD
Out parameters:int
Version:1.0
Author:Joseph Trottier
**********************************************************************/
int b_mode(Buffer * const pBD) {
	if (!pBD)
		return RT_FAIL_1;
	return pBD->mode;
}
/********************************************************************
Function name:b_incfactor
Purpose: The function returns the non-negative value of inc_factor to the calling function.
In parameters: Buffer * const pBD
Out parameters:size_t--unsigned int
Version:1.0
Author:Joseph Trottier
**********************************************************************/
size_t b_incfactor(Buffer * const pBD) {
	if (!pBD)
		return 0x100;
	return (size_t)pBD->inc_factor;
}
/********************************************************************
Function name:b_load
Purpose: The function loads (reads) an open input file specified by fi into a buffer specified by pBD. 
In parameters: FILE * const fi, Buffer * const pBD
Out parameters:int
Version:1.0
Author:Joseph Trottier
**********************************************************************/
int b_load(FILE * const fi, Buffer * const pBD) {
	int thischar;
	int numadded = 1;
	if (!pBD)
		return LOAD_FAIL;
	if (fi == NULL)
		return LOAD_FAIL;
	while (1) {
		thischar = fgetc(fi);
		if (feof(fi))
			break;
		if(b_eob(pBD))
			if (b_isfull(pBD) == 1) {
				ungetc(thischar, fi);
				printf("Last character read from input file is: %c %d\n", (char)thischar, thischar);
				return LOAD_FAIL;
			}
		
		if (thischar == '\n')
			thischar = '\n\r';
		b_addc(pBD, (unsigned char)thischar);
		numadded++;
	}
	return numadded;
}
/********************************************************************
Function name:b_isempty
Purpose: If the addc_offset is 0, the function returns 1; otherwise it returns 0. 
In parameters: Buffer * const pBD
Out parameters:int
Version:1.0
Author:Joseph Trottier
**********************************************************************/
int b_isempty(Buffer * const pBD) {
	if (!pBD)
		return RT_FAIL_1;
	if (pBD->addc_offset == 0)
		return 1;
	return 0;
}
/********************************************************************
Function name:b_getc
Purpose: This function is used to read the buffer. 
In parameters: Buffer * const pBD
Out parameters:char
Version:1.0
Author:Joseph Trottier
**********************************************************************/
char b_getc(Buffer * const pBD) {
	if (!pBD)
		return RT_FAIL_2;
	if (b_getcoffset(pBD) != pBD->addc_offset) {
		pBD->flags &= 0xFFFD; /*1111 1111 1111 1101*/
		pBD->getc_offset++;
		return (unsigned char)pBD->cb_head[pBD->getc_offset-1];
	}
	pBD->flags |= 0x2; /*0010*/
	return 0;
	
	
}
/********************************************************************
Function name:b_eob
Purpose: The function returns the value of the flags field determined only by the eob bit.
In parameters: Buffer * const pBD
Out parameters:int
Version:1.0
Author:Joseph Trottier
**********************************************************************/
int b_eob(Buffer * const pBD) {
	if (!pBD)
		return RT_FAIL_1;
	return pBD->flags & 0x2;/*0000 0000 0000 0010*/
}
/********************************************************************
Function name:b_print
Purpose: The function is intended to be used for used for diagnostic purposes only.
In parameters: Buffer * const pBD
Out parameters:int
Version:1.0
Author:Joseph Trottier
**********************************************************************/
int b_print(Buffer * const pBD) {
	char temp;
	int count = 0;
	if (!pBD)
		return RT_FAIL_1;
	if (b_isempty(pBD))
		return 0;
	while (1) {
		temp= b_getc(pBD);
		if (b_eob(pBD))
			break;
		printf("%c", temp);
		count++;
	}
	printf("\n");
	return count;
}
/********************************************************************
Function name:b_compact
Purpose: The function returns the current capacity of the character buffer.
In parameters: Buffer * const pBD, char symbol
Out parameters:Buffer*
Version:1.0
Author:Joseph Trottier
**********************************************************************/
Buffer * b_compact(Buffer * const pBD, char symbol) {
	if (!pBD)
		return NULL;
	pBD->capacity = pBD->addc_offset + 1;
	pBD->cb_head = (char *)realloc(pBD->cb_head, pBD->capacity);
	if (pBD->cb_head == NULL)
		return NULL;
	pBD->addc_offset++;
	pBD->cb_head[pBD->addc_offset] = symbol;
	pBD->flags &= 0xFFFE; /*1111 1111 1111 1110*/
	return pBD;
}
/********************************************************************
Function name:b_rflag
Purpose: The function returns the value of the flags field determined only by the r_flag bit.
In parameters: Buffer * const pBD
Out parameters:char
Version:1.0
Author:Joseph Trottier
**********************************************************************/
char b_rflag(Buffer * const pBD) {
	if (!pBD)
		return RT_FAIL_1;
	return pBD->flags & 0x1;
}
/********************************************************************
Function name:b_retract
Purpose: The function decrements getc_offset by 1. 
In parameters: Buffer * const pBD
Out parameters:short
Version:1.0
Author:Joseph Trottier
**********************************************************************/
short b_retract(Buffer * const pBD) {
	if (!pBD)
		return RT_FAIL_1;
	pBD->getc_offset--;
	return pBD->getc_offset;
}
/********************************************************************
Function name:b_reset
Purpose: The function sets getc_offset to the value of the current markc_offset . 
In parameters: Buffer * const pBD
Out parameters:short
Version:1.0
Author:Joseph Trottier
**********************************************************************/
short b_reset(Buffer * const pBD) {
	if (!pBD)
		return RT_FAIL_1;
	pBD->getc_offset = pBD->markc_offset;
	return pBD->getc_offset;
}
/********************************************************************
Function name:b_getcoffset
Purpose: The function returns getc_offset to the calling function.
In parameters: Buffer * const pBD
Out parameters:short
Version:1.0
Author:Joseph Trottier
**********************************************************************/
short b_getcoffset(Buffer * const pBD) {
	if (!pBD)
		return RT_FAIL_1;
	return pBD->getc_offset;
}
/********************************************************************
Function name:b_rewind
Purpose: The function set the getc_offset and markc_offset to 0, so that the buffer can be reread again.
In parameters: Buffer * const pBD
Out parameters:int
Version:1.0
Author:Joseph Trottier
**********************************************************************/
int b_rewind(Buffer * const pBD) {
	if (!pBD)
		return RT_FAIL_1;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	return 0;
}
/********************************************************************
Function name:b_location
Purpose: The function returns a pointer to a location of the character buffer indicated by the current
markc_offset. 
In parameters: Buffer * const pBD
Out parameters:char*
Version:1.0
Author:Joseph Trottier
**********************************************************************/
char * b_location(Buffer * const pBD) {
	if (!pBD)
		return NULL;
	return pBD->cb_head + pBD->markc_offset;
}