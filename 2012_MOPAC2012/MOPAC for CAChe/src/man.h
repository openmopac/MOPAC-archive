#ifndef	__MAN_H__
#define	__MAN_H__

#include <stdio.h>

class lMan {

public:

	enum manSt { Disabled, Enabled };

	lMan();										
	lMan(const char* name);						
	lMan(FILE* fp);								
	~lMan();

	void	lmst(const char* tag);
	FILE*	lmgf();			
	void	prn(const char* fmt, ...);			
	void	vp(const char* fmt, va_list argp);	
	void	lmfl();								

private:
 
	static DWORD			g_tls;			
	static CRITICAL_SECTION	g_cs;			
	manSt	lms;					
	FILE*	lmfp;						
	char*	lmfn;					
	BOOL	lmsc;				

	char*	lmgt();					
	void	lmst1(const char* tag);

	void	lmcb();			
	void	lmce();

};

#endif	__MAN_H__
