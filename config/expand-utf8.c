/*! \file expand-utf8.c
 *
 * \author John Reppy
 *
 * This filter replaces non-ASCII UTF-8 multibyte sequences with sequences of SML
 * string escapes.  It's purpose is to allow building Diderot using the MLton
 * compiler (mlton.org), which does not allow non-7-bit ASCII characters in string
 * literals.  Note that we assume that the source is valid w.r.t. SML/NJ, so we
 * look for UTF-8 multibyte headers independent of context.
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#include <ctype.h>
#include <stdio.h>

#define TX_MASK		0xC0		/* 11xx xxxx */
#define TX_VAL		0x80		/* 10xx xxxx */
#define T1_MASK		0xE0		/* 111x xxxx */
#define T1_VAL		0xC0		/* 110x xxxx */
#define T2_MASK		0xF0		/* 1111 xxxx */
#define T2_VAL		0xE0		/* 1110 xxxx */
#define T3_MASK		0xF8		/* 1111 1xxx */
#define T3_VAL		0xF0		/* 1111 0xxx */

void putEscapeChar (unsigned int c)
{
    printf ("\\%03d", c & 0xff);
}

int main ()
{
    unsigned int c;

    while ((c = getchar()) != EOF) {
	if (c <= 0x7f) {  /* ASCII */
	    putchar (c);
	}
	else { /* multibyte */
	    int i, nc;
	    if ((c & T1_MASK) == T1_VAL) { nc = 1; }
	    else if ((c & T2_MASK) == T2_VAL) { nc = 2; }
	    else if ((c & T3_MASK) == T3_VAL) { nc = 3; }
	    else {
		fprintf (stderr, "expand-utf8: invalid UTF-8 header byte %#0x\n", c);
		return 1;
	    }
	    putEscapeChar (c);
	    for (i = 0;  i < nc;  i++) {
		if ((c = getchar()) == EOF) {
		    fprintf (stderr, "expand-utf8: unexpected EOF in multibyte character\n");
		    return 1;
		}
		else if ((c & TX_MASK) != TX_VAL) {
		    fprintf (stderr, "expand-utf8: invalid UTF-8 extension byte %#0x\n", c);
		    return 1;
		}
		putEscapeChar(c);
	    }
	}
    }

    if (ferror(stdin)) {
	perror ("expand-utf8");
	return 1;
    }

    return 0;

}
