#include	<stdarg.h>
#include	<string.h>
#include	<time.h>
#include	<windows.h>

#include	"man.h"

DWORD				lMan::g_tls;
CRITICAL_SECTION	lMan::g_cs;

lMan::lMan()
{
	memset(this, 0, sizeof(*this));
	lms = Disabled;
	lmsc = FALSE;
}

lMan::lMan(const char* name)
{
	memset(this, 0, sizeof(*this));

	lmfp = fopen(name, "w");
	if (lmfp != NULL) {
		lms = Enabled;
		lmsc = TRUE;
		int err = setvbuf(lmfp, NULL, _IOLBF, BUFSIZ);
	} else {
		lms = Disabled;
		lmsc = FALSE;
	}

	lmfn = new char[strlen(name) + 1];
	if (lmfn != NULL) strcpy(lmfn, name);
}

lMan::lMan(FILE* fp)
{
	memset(this, 0, sizeof(*this));

	lmfp = fp;
	if (lmfp != NULL) {
		lms = Enabled;
	} else {
		lms = Disabled;
	}

	lmsc = FALSE;
}

lMan::~lMan()
{
	delete[] lmfn;
	if (lmsc) fclose(lmfp);
}

void
lMan::lmst(const char* tag)
{
	lmcb();
	char* tag1 = new char[strlen(tag) + 1];
	if (tag1 != NULL) strcpy(tag1, tag);
	lmst1(tag1);
	lmce();
}

FILE*
lMan::lmgf()
{
	lmcb();
	FILE* fp = lmfp;
	lmce();
	return(fp);
}

void
lMan::prn(const char* fmt, ...)
{
	lmcb();

	if (lms == Disabled || lmfp == NULL) goto lmed;

	fprintf(lmfp, "%s: ", lmgt());

	va_list	argp;
	va_start(argp, fmt);
	vfprintf(lmfp, fmt, argp);
	va_end(argp);

	if (0) lmed: 
	{
		;
	}

	lmce();
}

void
lMan::vp(const char* fmt, va_list argp)
{
	lmcb();

	if (lms == Disabled || lmfp == NULL) goto lmed;

	fprintf(lmfp, "%s: ", lmgt());
	vfprintf(lmfp, fmt, argp);

	if (0) lmed: 
	{
		;
	}

	lmce();
}

void
lMan::lmfl()
{
	lmcb();

	if (lms == Disabled || lmfp == NULL) goto lmed;
	fflush(lmfp);

	if (0) lmed: 
	{
		;
	}

	lmce();
}

inline
char*
lMan::lmgt()
{
	return((char*)TlsGetValue(g_tls));
}

inline
void
lMan::lmst1(const char* tag)
{
	TlsSetValue(g_tls, (char*)tag);
}

inline
void
lMan::lmcb()
{
	EnterCriticalSection(&g_cs);
}

inline
void
lMan::lmce()
{
	LeaveCriticalSection(&g_cs);
}

