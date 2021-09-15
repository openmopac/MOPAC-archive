#include <windows.h>

#include "conn.h"
#include "connev.h"

conn_evWa conn_wamu(
	long numev,
	conn_ev *connarr[],
	connbl wall,
	con_Ev_out tout 
)
{
	long wfmo_ret;
	conn_evWa mr;
	HANDLE *oea = new HANDLE[numev];

	int i = 0;

	if (oea == NULL) goto eoom;

	for (i = 0; i < numev; i++)
	{
		oea[i] = connarr[i]->hev;
	}


	wfmo_ret = WaitForMultipleObjects(numev, oea, wall, (long)tout);

	switch (wfmo_ret) {
		case WAIT_FAILED:		
			mr = CONN_WA_F; 
			break;

		case WAIT_ABANDONED:	
			mr = CONN_WA_F; 
			break;

		case WAIT_TIMEOUT:		
			mr = CONN_WA_T; 
			break;
		default: 				
			mr = (conn_evWa)wfmo_ret;
	}

	if (0) eoom: 
	{
		mr = CONN_WA_F;
	}

	delete[] oea;

	return (mr); 		
}

conn_ev::conn_ev()
{
	mty = CON_EV_UN;
	hev = NULL;
	msta = connernun;
}


connbl conn_ev::Init(con_evTy etype)
{
	BOOL wt;
	conner retcode = connernun;

	mty = etype;

	switch (etype) {
		case CON_EV_MA:	wt = FALSE;	break;
		case CON_EV_AU:	wt = FALSE;	break;
		default:					goto econneti;
	}

	hev = CreateEvent(
	            NULL 
	           ,wt
	           ,FALSE	
	           ,NULL
	         );

	if (hev == NULL) goto eccef;

	if (0) econneti: 
	{
		msta = connereti;
	}

	if (0) eccef: 
	{
		msta = connereif;
	}

	if (msta != connernun)
		return connblfa;
	return connbltr;
}

conn_ev::~conn_ev()
{
	if (hev != NULL) CloseHandle(hev);
}

connbl conn_ev::Set()
{ 
	connbl ret = connblfa;

	if (hev != NULL)
		ret = (connbl)SetEvent(hev);
	return (ret);
}

connbl conn_ev::Reset()
{
	connbl ret = connblfa;

	if (hev != NULL)
		ret = (connbl)ResetEvent(hev);
	return (ret);
}

conn_evWa conn_ev::Wait(con_Ev_out tout)
{
	long wfso_ret;
	conn_evWa my_ret;

	if (hev != NULL)
		wfso_ret = WaitForSingleObject(hev, (long)tout);
	else
		wfso_ret = WAIT_FAILED;
	
	switch (wfso_ret) {
	case WAIT_FAILED:		my_ret = CONN_WA_F; break;
	case WAIT_ABANDONED:	my_ret = CONN_WA_F; break;
	case WAIT_OBJECT_0:		my_ret = CONN_WA_O; break;
	case WAIT_TIMEOUT:		my_ret = CONN_WA_T; break;
	default: 				my_ret = CONN_WA_F;
	}
	
	return (my_ret); 		
}

