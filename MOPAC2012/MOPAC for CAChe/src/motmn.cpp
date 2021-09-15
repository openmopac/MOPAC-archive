#include <windows.h>
#include <stdio.h>
#include <process.h>
#include <stdlib.h>

#include "conn.h"
#include "connev.h"
#include "connms.h"

#include "mot.h"

MotMn::MotMn(void)
{
	motmnst = motscr;
	msta = moterno;

	mott = NULL;
	motthid = NULL;

	motthco = NULL;
	motthcoid = NULL;
	motstsz = 0;

	mote = NULL;
	motee = NULL;

}

MotMn::~MotMn(void)
{
}

static unsigned __stdcall
motmrr(void *motmn)
{
	((MotMn *)motmn)->morun();
	return (0);
}

motb MotMn::moini(MotE *me)
{

	motmnst = motsin;

	mote = me;
	motee = mote->motemee;
	motmevts[MOTMEOS]		= mote->moteote;
	motmevts[MOTMERECEX]	= mote->motecree,
	motmevts[MOTMEST]		= mote->motese,
	motmevts[MOTMECONEX]	= mote->motece;

	motstsz = me->motess();

	mott = (motth)_beginthreadex(
		NULL,								
		0,									
		motmrr,				
		(void *)this, 						
		0,									
		&motthid);						

	if (0 == mott) goto motmnbf;

	if (0) motmnbf: 
	{
		msta = motermcf;
	}
		
	return motb(msta == moterno);
}

void MotMn::moterm(void)
{
 	DWORD		wr;
	long		le;
	const DWORD	wto = INFINITE;

	wr = WaitForSingleObject(mott, wto);
 	if (wr == ~0) {
		le = GetLastError();
	}

	motmnst = motste;

}

void MotMn::morun(void)
{
	conn_evWa motmnew;
	motb done = motbf;
 	motb morundc = motbt;

	motSTT("MotMn ");

 	motmnst = motsru;
	msta = moterno;

	if (mote->motsa) done = motbt;

	while (!done) {

		motmnew = conn_wamu(
			MOTMENUM,
			motmevts,
			connblfa,
			CON_INF_TIME);

		switch(motmnew) {
		case MOTMEOS:
			done = motbt;
			morundc = motbf;
			break;
		case MOTMERECEX:
			motmevts[MOTMEOS]->Set(); 
			motmevts[MOTMERECEX]->Reset(); 
			morundc = motbf;
			break;
		case MOTMECONEX:
			motmevts[MOTMECONEX]->Reset();
			done = motbt;
			break;
		case CONN_WA_F:
		case CONN_WA_T:
		default:
			motmevts[MOTMEOS]->Set(); 
			done = motbt;
			morundc = motbf;
			break;
		}

	}

	if (morundc) {
		mostco();
		mowtco();
	}

	moex();
	
}

void MotMn::moex(void) 
{
	PostMessage(mote->m_hWnd, WM_DESTROY, NULL, NULL);
}

static unsigned __stdcall motmndcrr(void *cePtr)
{
	MotE *ce = (MotE *)cePtr;

	motSTT("MotE ");

	ce->motescs(slistsc, ce->motver, CONNMESS_FL_NONE);
	ce->motescs(slistsc, "\n", CONNMESS_FL_NONE);

	ce->motedc();

	return (0);
}

void MotMn::mostco(void)
{
	long		lastError;

	motthco = (motth)_beginthreadex(
		NULL,								
		motstsz,						
		motmndcrr,		
		(void *)mote, 						
		CREATE_SUSPENDED,					
		&motthcoid);					

	if (0 == motthco) goto motmnbf;

	if (!SetThreadPriority(motthco, THREAD_PRIORITY_BELOW_NORMAL)) 
	{
		lastError = GetLastError();
	}

	if (ResumeThread(motthco) != 1) 
	{
		lastError = GetLastError();
	}

	if (0) motmnbf: 
	{
		msta = moterccf;
	}
}

void MotMn::mowtco(void)
{
 	DWORD			wr;
	long			le;
	const DWORD		wt = INFINITE;
	DWORD			exit_code = 0;

	wr = WaitForSingleObject(motthco, wt);
 	if (wr == ~0) {
		le = GetLastError();
	}
	
	if (!GetExitCodeThread(motthco, &exit_code)) {
		;
	}

	mote->msta = exit_code;

	motmnst = motste;

}

