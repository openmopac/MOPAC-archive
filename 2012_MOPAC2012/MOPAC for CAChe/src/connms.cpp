#include <windows.h>
#include <stdio.h>

#include "man.h"

#include "conn.h"
#include "connms.h"

#define connmsph HANDLE


connms::connms(unsigned char ty, unsigned char fl)
{
	emty = ty;
	emfl = fl;
}

connmsta::connmsta(
	unsigned char ty,
	unsigned char fla,
	int chnid,
	unsigned long stln,
	char *stat) :
 		connms(ty,fla),
 		chid(chnid),
 		stalen(stln),
 		msta(stat)
{
}

connmsta::~connmsta()
{
}

connmsp::connmsp(FILE *lgfi)
{ 
	msta = connernun;

	logfi = lgfi;
	lgmg = new lMan(logfi);

	rdhd = INVALID_HANDLE_VALUE;
	wrhd = INVALID_HANDLE_VALUE;
	bufsz = 0;
	sat.nLength = sizeof(SECURITY_ATTRIBUTES);
	sat.lpSecurityDescriptor = NULL;
	sat.bInheritHandle = TRUE;
	
	if (!CreatePipe(&rdhd, &wrhd, &sat, bufsz))
		msta = connermpcf;

}

connmsp::connmsp(
	connmsph rhd,
	connmsph whd,
	FILE *lgfi
)
{
	msta = connernun;

	logfi = lgfi;
	lgmg = new lMan(logfi);

	rdhd = rhd;
	wrhd = whd;
	bufsz = 0;

	sat.nLength = sizeof(SECURITY_ATTRIBUTES);
	sat.lpSecurityDescriptor = NULL;
	sat.bInheritHandle = TRUE;

}

connmsp::~connmsp(void)
{
	disco();
	delete lgmg;
}

connbl connmsp::vlid(void)
{
	return connbl(msta == connernun);
}

connbl connmsp::read(unsigned long len, unsigned char *buf)
{
	unsigned long	lr;

	if (!ReadFile(rdhd, buf, len, &lr, NULL)) {
		return connblfa;
	}
	if (lr != len)
		return connblfa;

	return connbltr;
}

connbl connmsp::writ(unsigned long len, unsigned char *buf)
{
	unsigned long lw;

	if (!WriteFile(wrhd, buf, len, &lw, NULL)) {
		return connblfa;
	}
	if (lw != len)
		return connblfa;

	return connbltr;

}

void connmsp::DbPr(char *fmt, ...)
{
	va_list argp;

	if (lgmg == NULL) return;

	va_start(argp, fmt);
	lgmg->vp(fmt, argp);
	va_end(argp);
}

void connmsp::disco()
{
	if (rdhd != INVALID_HANDLE_VALUE) {
		CloseHandle(rdhd);
		rdhd = INVALID_HANDLE_VALUE;
	}
	if (wrhd != INVALID_HANDLE_VALUE) {
		CloseHandle(wrhd);
		wrhd = INVALID_HANDLE_VALUE;
	}
}

connmsch::connmsch(FILE *lgfi)
{
	msta = connernun;

	logfi = lgfi;
	lgmg = new lMan(logfi);

	datp = new connmsp(logfi);
	if (datp == NULL || !datp->vlid())
		goto epcf;

	acp = new connmsp(logfi);
	if (acp == NULL || !acp->vlid())
		goto epcf;

	if (0) epcf: 
	{
		msta = connermpcf;
	}
}

connmsch::connmsch(
	connmsph drph,
	connmsph dwph,
	connmsph arph,
	connmsph awph,
	FILE *lgfi
)
{
	msta = connernun;

	logfi = lgfi;
	lgmg = new lMan(logfi);

	datp = new connmsp(drph, dwph, logfi);
	if (datp == NULL || !datp->vlid())
		goto epcf;

  	acp = new connmsp(arph, awph, logfi);
	if (acp == NULL || !acp->vlid())
		goto epcf;

	if (0) epcf: 
	{
		msta = connermpcf;
	}
}

connmsch::~connmsch(void)
{
	delete datp;
	datp = NULL;

	delete acp;
	acp = NULL;

	delete lgmg;
}

void connmsch::DbPr(char *fmt, ...)
{
	va_list argp;

	if (lgmg == NULL) return;

	va_start(argp, fmt);
	lgmg->vp(fmt, argp);
	va_end(argp);
}

connbl connmsch::vlid(void)
{
	return connbl(msta == connernun);
}

connbl connmsch::rddt(unsigned long len, void *buf)
{
	if (!datp->read(len, (unsigned char *)buf)) {
		msta = connermrf;
		return connblfa;
	}
	return connbltr;
}

connbl connmsch::wrdt(unsigned long len, void *buf)
{
	if (!datp->writ(len, (unsigned char *)buf)) {
		msta = connermwf;
		return connblfa;
	}
	return connbltr;
} 

connbl connmsch::snda()
{
	char *ack = "A";

	if (!acp->writ(1, (unsigned char *)ack)) {
		msta = connermsaf;
		return connblfa;
	}
	return connbltr;
}

connbl connmsch::wtfa()
{
	unsigned char ack;

	if (!acp->read(1, &ack)) {
		msta = connermraf;
		return connblfa;
	}
	return connbltr;
}

void connmsch::disco()
{
	if (acp != NULL)
		acp->disco();

	if (datp != NULL)
		datp->disco();
}

connmsc::connmsc(FILE *lgfi)
{
	msta = connernun;

	logfi = lgfi;
	lgmg = new lMan(logfi);

	rech= new connmsch(logfi);
	if (rech == NULL || !rech->vlid())
		goto eccf;

	sech = new connmsch(logfi);
	if (sech == NULL || !sech->vlid())
		goto eccf;

	if (0) eccf: 
	{
		msta = connermccf;
	}

}


connmsc::connmsc(
	connmsph rrph,	
	connmsph rwph,	
	connmsph swph,	
	connmsph srph,	
	FILE *lgfi
)
{
 		msta = connernun;

	logfi = lgfi;
	lgmg = new lMan(logfi);

	rech = new connmsch(rrph, NULL, NULL, rwph, logfi);
	if (rech == NULL || !rech->vlid())
		goto eccf;

	sech = new connmsch(NULL, swph, srph, NULL, logfi);
	if (sech == NULL || !sech->vlid())
		goto eccf;

	if (0) eccf: 
	{
		msta = connermccf;
	}

}

connmsc::~connmsc(void)
{
	delete rech;
	rech = NULL;

	delete sech;
	sech = NULL;

	delete lgmg;
}

void connmsc::DbPr(char *fmt, ...)
{
	va_list argp;

	if (lgmg == NULL) return;

	va_start(argp, fmt);
	lgmg->vp(fmt, argp);
	va_end(argp);
}

connbl connmsc::vlid(void)
{
	return connbl(msta == connernun);
}

connms *connmsc::gnm(void)
{
	connms *msg = NULL;
	unsigned char type, flags;

	if (!rech->rddt(sizeof(type), &type))
		goto ecrf;

	if (!rech->rddt(sizeof(flags), &flags))
		goto ecrf;


	switch(type) {

	case CONNMS_TY_ST:

		int cid;
		unsigned long sl;
		char *st;

		if (!rech->rddt(sizeof(cid), &cid))
			goto ecrf;

		if (!rech->rddt(sizeof(sl), &sl))
			goto ecrf;

		st = new char[sl];

		if (st == NULL)
			goto edaf;

		if (!rech->rddt(sl, st))
			goto ecrf;

		msg = new connmsta(type, flags, cid, sl, st);

		break;

	case CONNMS_TY_DP:
	case CONNMS_TY_ID:
	case CONNMS_TY_CO:
	case CONNMS_TY_HA:
	case CONNMS_TY_DI:
		msg = new connms(type, flags);
		break;

	default:
		break;

	}

	if (msg == NULL) goto emcf;

	if (flags & CONNMESS_FL_BL)
		rech->snda();

	if (0) ecrf: 
	{
		msta = connermrf;
	}

	if (0) emcf: 
	{
		msta = connermcf;
	}

	if (0) edaf: 
	{
		msta = connermaf;
	}

	return msg; 
}

connbl connmsc::sndm(connmsta *msg)
{
	if (!sech->wrdt(sizeof(msg->emty), &msg->emty))
		goto ecwf;
	if (!sech->wrdt(sizeof(msg->emfl), &msg->emfl))
		goto ecwf;
	if (!sech->wrdt(sizeof(msg->chid), &msg->chid))
		goto ecwf;
	if (!sech->wrdt(sizeof(msg->stalen), &msg->stalen))
		goto ecwf;
	if (!sech->wrdt(msg->stalen, msg->msta))
		goto ecwf;
	if (msg->emfl & CONNMESS_FL_BL)
		if (!sech->wtfa())
			goto ecmwfaf;

	if (0) ecwf: 
	{
		msta = connermwf;
	}

	if (0) ecmwfaf: 
	{
		msta = connermraf;
	}

	return connbl(msta == connernun);
}

connbl connmsc::sndm(connms *msg)
{
	if (!sech->wrdt(sizeof(msg->emty), &msg->emty))
		goto ecwf;
	if (!sech->wrdt(sizeof(msg->emfl), &msg->emfl))
		goto ecwf;
	if (msg->emfl & CONNMESS_FL_BL)
		if (!sech->wtfa())
			goto ecmwfaf;

	if (0) ecwf: 
	{
		msta = connermwf;
	}

	if (0) ecmwfaf: 
	{
		msta = connermraf;
	}

	return connbl(msta == connernun);
}

void connmsc::disco(void)
{

	if (sech != NULL)
		sech->disco();

	if (rech != NULL)
		rech->disco();

}
