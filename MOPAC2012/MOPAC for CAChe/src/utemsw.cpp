#include <windows.h>
#include <stdio.h>
#include <stdarg.h>

#include "conn.h"
#include "connms.h"
#include "mot.h"

#include "ute.h"

int utesdm(volatile struct utecmc *const ch);

utems utesm(volatile struct utecmc *const ch,
	const char *const text)
{

	if (utesdm(ch))
		return(UTEMB);

 	MotE *ce = (MotE *)(ch->mote);

	ce->motescs(ch->uchid, (char *)text, CONNMESS_FL_NONE);

	return(UTEMS);

}

utems utesmw( volatile struct utecmc *const ch,
                                         const char *const text)
{
	MotE *ce = (MotE *)(ch->mote);

	ce->motescs(ch->uchid, (char *)text, CONNMESS_FL_BL);

	return(UTEMS);
}

int
uteicc(struct utecmc *const ch,
	const unsigned char uchid, void *mot)
{
	ch->uchid = uchid;
	ch->mote = mot;
	return(0);
}

static unsigned int utemmi = 60;
#define UTECTIT() ((GetTickCount() * 60) / 1000)

#define UTEMICN 8
#define UTEMICAN 8

int
utesdm(volatile struct utecmc *const ch)
{
	struct ca
	{
		unsigned int ns;
		unsigned int nsu;
		struct chi
		{
			volatile struct utecmc *ch;
			unsigned long lmt;
		} chi[1];
	};
	static struct ca *cha = 0;
	unsigned int i;
	if (cha == 0)	{
		cha = (struct ca*)calloc(offsetof(struct ca,
			chi[UTEMICN]), 1);
		if (cha == 0)
			return(0);
		cha->ns = UTEMICN;
	}

	for (i = 0; i < cha->nsu; i++)	{
		if (cha->chi[i].ch == ch)
			break;
	}
	if (i == cha->nsu)	{	
		if (cha->ns == cha->nsu) {
			cha = (struct ca*)realloc(cha, offsetof(struct ca,
				chi[UTEMICAN +
				cha->ns]));

			if (cha == 0)
				return(0);
			cha->ns = UTEMICAN +
					cha->ns;
		}

		cha->chi[cha->nsu].ch
			= ch;
		cha->chi[cha->nsu].lmt
			= UTECTIT();
		cha->nsu++;
		return(0);
	}

	if (cha->chi[i].lmt +
		utemmi > UTECTIT())
		return(1);
	cha->chi[i].lmt
		= UTECTIT();
	return(0);
}
