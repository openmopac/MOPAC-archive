#ifndef	CONN_EV_H
#define	CONN_EV_H

#include "conn.h"

typedef long con_Ev_out;
#define CON_INF_TIME	INFINITE

enum con_evTy {
	CON_EV_UN,
	CON_EV_MA,
	CON_EV_AU

};

enum conn_evWa {
	CONN_WA_F = -2,
	CONN_WA_T = -1,
	CONN_WA_O = 0
};

class conn_ev {


	friend conn_evWa conn_wamu(
		long numev,	   	
		conn_ev *connarr[],	
		connbl wall,	   
		con_Ev_out tout); 

	public:
		conn_ev();
		~conn_ev();
		connbl Init(con_evTy etype);
		connbl Set();
		connbl Reset();
		conn_evWa Wait(con_Ev_out tout);

	private:
		con_evTy	mty;
		conner		msta;
		HANDLE		hev;

}; 


#endif /* CONN_EV_H */
