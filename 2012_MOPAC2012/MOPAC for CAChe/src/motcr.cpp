#include <windows.h>
#include <stdio.h>
#include <process.h>

#include "conn.h"
#include "connev.h"
#include "connms.h"
#include "mot.h"

#if defined(__cplusplus)
extern "C" {
#endif

void motSKD(void)
{

}
#if defined(__cplusplus)
}
#endif

MotCR::MotCR(void)
{
	msta = moterno;
	recsta = motscr;
	mott = NULL;
	motthid = NULL;
	
	mote = NULL;
	motee = NULL;
	motmcon = NULL;
}

MotCR::~MotCR(void)
{
}

connms *MotCR::gnm()
{
	connms *msg = motmcon->gnm();
	return (msg);
}

static unsigned __stdcall
motCRRR(void *motrv)
{
	((MotCR *)motrv)->motcrr();
	return (0);
}

motb MotCR::motcri(MotE *ce, connmsc *msgConnection)
{
	recsta = motsin;
	mote = ce;
	motee = mote->motecree;
    
	motmcon = msgConnection;

	mott = (motth)_beginthreadex(
		NULL,								
		0,									
		motCRRR,			
		(void *)this, 						
		0,									
		&motthid);						
	
	if (NULL == mott) 
		msta = motercrcf;
		
	return motb(msta == moterno);
}

void MotCR::motcrt(void)
{
 	DWORD		waitResult;
	long		lastError;
	const DWORD	waitTimeout = INFINITE;

	motSKD();
	disco();

	waitResult = WaitForSingleObject(mott, waitTimeout);
 	if (waitResult == ~0) {
		lastError = GetLastError();
	}

	recsta = motsdo;

}

void MotCR::motcrr(void)
{
	connms *msg;

	motSTT("MOTCR");

 	recsta = motsru;
	msta = moterno;

	while (msta == moterno) {
		msg = gnm();	
		msta = motcrdm(msg);
		delete msg;
	}
	
	recsta = motsdo;

	motSKD();
	disco();

	motcre();
}

void MotCR::motcre(void)
{
	motee->Set();

	if (msta == moterdm)
		ExitProcess(0);
}

moter MotCR::motcrdm(connms *msg)
{
	moter err = moterno;

	if (msg == NULL) goto motecrcf;

	switch(msg->emty) {
	case CONNMS_TY_CO:
		mote->motece->Set();
		break;

	case CONNMS_TY_HA:
		mote->motese->Set();
		break;

	case CONNMS_TY_DI:
		err = moterdm;
		break;

	default:
		goto err_bad_msg;
		break;

	}

	if (0) motecrcf: 
	{
		err = moterecf;
	}

	if (0) err_bad_msg: 
	{
		err = moterbm;
	}

	return(err);
}

void MotCR::disco(void)
{
	connms *msg;

	if (NULL == motmcon) goto motmmcn;

	msg = new connms(CONNMS_TY_DI, CONNMESS_FL_NONE);
	if (msg == NULL) goto motncmf;
	
	if (!motmcon->sndm(msg)) {
		;
	}

	delete msg;
	
	motmcon->disco();

	if (0) motmmcn: 
	{
		;
	}
	
	if (0) motncmf: 
	{
		;
	}
}
