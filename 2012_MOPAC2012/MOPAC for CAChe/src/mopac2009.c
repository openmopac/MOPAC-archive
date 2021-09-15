#include <windows.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>

#include <malloc.h>
#include <string.h>
#include "ute.h"

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

#endif // __cplusplus

#define MOTSTRL  128

utecmc motcone;
utecmc motctwo;
utecmc motcthree;

static time_t mottslm = 0;
static time_t motcc;

#define MOTCONE   1
#define MOTCTWO   2
#define MOTCTHREE 4
#define MOTCFOUR  8

extern void __stdcall MOPAC(int *i);
void motwfc(void);
void motmspf(void);
void motme(long exit_code);
long motmsh(void);

void setmsgbufc(long *l, char *s, long *n);
long shutdownc(long *i);

void DbgBreak()
{
    DebugBreak();
}

void motAM(char *name, long pof, void *motE)
{
    char tbuf[64], buf[256];
    long len, motcn;
	int licenseParameter = 0;
    double t0 = 0.0, t1 = 0.0;

    uteicc(&motcone, 1, motE);
    uteicc(&motctwo, 2, motE);
    uteicc(&motcthree, 3, motE);

    if ( pof != 0 )
    {
        sprintf(buf, "Calculation done.");
        len = strlen(buf);
        motcn = MOTCFOUR;
        setmsgbufc(&len, buf, &motcn);

        sprintf(buf,"All done");
        len = strlen(buf);
        motcn = MOTCTWO;
        setmsgbufc(&len, buf, &motcn);

    } else {

		// Check on the run-time license in the registry

		bool keyExists = false;

		BYTE szBuff[255] = {0};
		DWORD cb = 0;
		DWORD dwType = REG_SZ;
		LONG rval;
 
        DWORD dwSize = 1024;
        HKEY Regentry;
        char licenseKey[1024];

         //get the program name
        rval = RegOpenKeyEx(HKEY_LOCAL_MACHINE, "SOFTWARE\\MOPAC2009 for CAChe", 0, 
                     KEY_QUERY_VALUE, &Regentry);

		if (ERROR_SUCCESS == rval)
		{
			keyExists = true;
		}

 		if (keyExists)
		{
			rval = RegQueryValueEx(Regentry, "RunStatus" , NULL, &dwType, 
                        (unsigned char*)&licenseKey, &dwSize);

 			if (rval != ERROR_SUCCESS)
			{
				keyExists = false;
			}

			RegCloseKey(Regentry);
		}

		if (keyExists)
		{
			// See if we have the correct string
			int compareReturn = strncmp("6390128555X", licenseKey, strlen(licenseKey));

			if (0 != compareReturn)
			{
				keyExists = false;
			}
		}

		if (keyExists)
		{
			licenseParameter = 1;
		}

        t0 = utetime();

        MOPAC(&licenseParameter); // Where the MOPAC rubber hits the road!

        t1 = utetime();

        uteet(t0,t1,tbuf);
        sprintf(buf, "Computation time: %s\n",tbuf);
        len = strlen(buf);
        motcn = MOTCONE;
        setmsgbufc(&len, buf, &motcn);
    }
}


void setmsgbufc(long *l, char *s, long *n)
{
    char buf[MOTSTRL];
    FILE *fp = NULL;

    if ( *l < MOTSTRL - 2 )
    {
        memcpy(buf, s, *l);
        buf[(*l)] = '\n';
        buf[(*l) + 1] = 0;

    } else {

        memcpy(buf, s, MOTSTRL - 2);
        buf[MOTSTRL - 2] = '\n';
        buf[MOTSTRL - 1] = 0;
    }
    
     switch (*n)
    {
        case MOTCONE:
			utesmw(&motcthree, buf);
            break;

        case MOTCTWO:
            motcc = time(NULL);

            if ( difftime(motcc, mottslm) >= 1.0 )
            {
                utesm(&motctwo, buf);
                mottslm = time(NULL);
            }

            break;

        case MOTCTHREE:
            utesmw(&motcone, buf);
            break;

        case MOTCFOUR:
            utesmw(&motcthree, buf);

            fp = fopen("ProcessBufferFile", "w");

            if ( fp != NULL )
            {
                fprintf(fp, "%s", buf);
                fclose(fp);
                fp = NULL;
            }

            motmspf();


            motwfc();

            break;
    }
}

long shutdownc(long *i)
{  
    return (motmsh());
}

void fortranexitc(long *m)
{
    motme(*m);
}

void touchm(void)
{
}
