#ifndef __UTE__
#define __UTE__

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include <malloc.h>
#include <string.h>

typedef unsigned char Boolean;

#pragma warning(disable: 4237)

typedef char *					Handle;

#ifndef true
#define true 1
#endif

#ifndef false
#define false 0
#endif

#ifndef __cplusplus

typedef unsigned char bool;

#ifndef true
#define true 1
#define false 0
#endif

#endif /* __cplusplus */

#ifdef __cplusplus
extern "C" {	
#endif

char ** uteclta(char *cmdLine, int *argc);

void ute_Exit(int status);

double utetime(void);
void uteet(double start, double end, char *buf);

typedef int utems;

typedef struct utecmc
{
	void *mote; 

	unsigned char uchid;
} utecmc;

#define UTEMS 0
#define UTEMB -1

utems utesm(volatile struct utecmc
	*const ch, const char *const text);

utems utesmw(volatile struct utecmc
	*const ch, const char *const text);


int uteicc(struct utecmc *const ch,
	const unsigned char uchid, void *mot);

#define UTESTRX(ts)	#ts
#define UTESTR(ts)	UTESTRX(ts)

#ifdef __cplusplus
}
#endif

#endif /* __UTE__ */
