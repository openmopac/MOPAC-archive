#include <windows.h>
#include <process.h>
#include <time.h>
#include <stdio.h>
#include "man.h"
#include "conn.h"
#include "connev.h"

#include "ute.h"
#include "mot.h"

#define MOTNA(xp) (((xp) - argv + 1) < argc ? (xp) + 1: NULL)
#define MOTMIF slistc

static lMan* motlm;

void motSTT(char*    tag)
{
    if (motlm == NULL) return;
    motlm->lmst(tag);
}

static void motLT() 
{
    time_t t = time(NULL);
}


MotE::MotE(void)

{
    motver = "MOPAC2009 " __DATE__ " "  __TIME__;
    motpof = 0;
    motsa = motbf;
    motp    = GetCurrentProcessId();
    mott    = GetCurrentThread();
    motthid    = GetCurrentThreadId();
    motpn[0] = '\0';
    moteifl = NULL;
    logfi = NULL;
    motelfbn = "MotE";
    memset(motelfn, 0, sizeof(motelfn));
    motemn = NULL;
    motecr = NULL;
    moteote = NULL;
    motecree = NULL;
    motese = NULL;
    motece = NULL;
    motemee = NULL;
}



MotE::~MotE()
{
    delete motemn;
    delete motecr;
    delete moteote;
    delete motecree;
    delete motese;
    delete motece;
    delete motemee;
}

motb MotE::moteInit(int argc, char *argv[], HWND hWnd)

{
	char **p;

	m_argc = argc;
	m_argv = argv;
	m_hWnd = hWnd;
	motsta = motsin;
	GetModuleFileName(NULL, motpn, MOTFNS);

    if (strstr(argv[0], ".exe") != NULL)
	{
        p = MOTNA(argv);
	}
    else
	{
        p = argv;
	}

    while (p) 
	{
        if   (strncmp(*p, "-r", 2) == 0) 
		{    
            if (*(*p+2) == 'a')
			{
                moterpa = (MOTCRPH)atoi(*p+4);
			}
            else
			{
                moterp = (MOTCRPH)atoi(*p+3);
			}

            p = MOTNA(p);

        } 
		else if (strncmp(*p, "-s", 2) == 0) 
		{ 
            if (*(*p+2) == 'a')
			{
                motespa = (MOTCRPH)atoi(*p+4);
			}
            else
			{
				motesp = (MOTCRPH)atoi(*p+3);
			}

            p = MOTNA(p);

        } 
		else if (strncmp(*p, "-S", 2) == 0) 
		{

            motsa = motbt;

        } 
		else if (strncmp(*p, "-d", 2) == 0) 
		{
            sprintf(motelfn, "%s%d.log", motelfbn, motp);
            motlm = new lMan(motelfn);
            if (motlm == NULL) goto motlmf;
            motSTT("MOT ");
            logfi = motlm->lmgf();
            motdfl = motbt;
        } 
		else if (strncmp(*p, "-cft", 4) == 0) 
		{
            p = MOTNA(p); 

            unsigned long cftype = atol(*p);

            p = MOTNA(p);

            moteaif(cftype, *p);

        } 
		else if (strncmp(*p, "-fi\0", 4) == 0) 
		{
            p = MOTNA(p);
            moteaif(slistm, *p);
        } 
		else if (strncmp(*p, "-fs", 3) == 0) 
		{
            p = MOTNA(p);
            moteaif(slists, *p);
        } else 
		{
            motepof(*p);
        }

        p = MOTNA(p);
    }

    motLT();

    if (!motsa && moterp == 0 && moterpa == 0 &&
         motesp == 0 && motespa == 0)
	{
        goto motpfi;
	}            

    moteote = new conn_ev;

    if ((NULL == moteote) || !moteote->Init(CON_EV_MA))
	{
         goto motcecf;
	}

    motecree = new conn_ev;

    if ((NULL == motecree) || !motecree->Init(CON_EV_MA))
	{
         goto motcecf;
	}

    motese = new conn_ev;

    if ((NULL == motese) || !motese->Init(CON_EV_MA))
	{
         goto motcecf;
	}

    motece = new conn_ev;

    if ((NULL == motece) || !motece->Init(CON_EV_MA))
	{
         goto motcecf;
	}

    motemee = new conn_ev;

    if ((NULL == motemee) || !motemee->Init(CON_EV_MA))
	{
         goto motcecf;
	}

    if (!motsa) 
	{
        motmcon = new connmsc(moterp, moterpa, motesp, motespa, logfi);

        if (motmcon == NULL)
		{
            goto motcmcf;
		}

        motecr = new MotCR;

        if (motecr == NULL || !motecr->motcri(this, motmcon))
		{
            goto motccrf;
		}
    } 
	else 
	{
        motece->Set();
    }

    motemn = new MotMn;

    if (NULL == motemn || !motemn->moini(this))
	{
        goto motcmf;
	}

	if (0) motlmf: 
	{
        msta = moterlcf;
    }

    if (0) motpfi: 
	{
        msta = motermcf;
    }

    if (0) motcecf: 
	{
        msta = moterevcf;
    }

    if (0) motcmcf: 
	{
        msta = motermccf;
    }


    if (0) motccrf: 
	{
        msta = motercrcf;
    }

    if (0) motcmf: 
	{
        msta = motermcf;
    }

    return motb(msta == moterno);

}

int MotE::moteTerm()
{
    moteote->Set();

    if (motemn != NULL)
	{
        motemn->moterm();
	}       

    if (motecr != NULL)
	{
        motecr->motcrt();
	}

    delete motmcon;
    motmcon = NULL;    

    motsta = motste;

    motLT();
    
    if (motlm != NULL) 
	{		
		motlm->lmfl();
	}

    delete motlm;
    motlm = NULL;

    return(msta);
}

motb MotE::moteaif(
    unsigned long cftype,
    char *fileName)
{
    if (moteifl == NULL) {

        moteifl =  (sftl *) new char[slistsol(sftl, MOTMIF, files)];

        if (NULL == moteifl)
		{
            return motbf;
		}

        moteifl->numFiles = 0;

    }

    if (moteifl->numFiles >= MOTMIF)
	{
        return motbf;
	}

    strncpy(moteifl->files[moteifl->numFiles].slistfn,
				          fileName, slistfns);

    moteifl->files[moteifl->numFiles].caft = cftype;
    moteifl->files[moteifl->numFiles].ctfo = slista;
    moteifl->files[moteifl->numFiles].cfot = slistfm;

    moteifl->numFiles++;

    return motbt;
}

motb MotE::motescs(
    unsigned int channel,
    char *status,
    unsigned char flag)
{
    int stat_len = strlen(status) + 1;

    motb ret = motbt;

    if (!motsa) 
	{
        connmsta *msg = new connmsta(CONNMS_TY_ST,
                flag, channel, stat_len, status);    

        ret = motb((msg != NULL) && motmcon->sndm(msg));

        delete msg;
    }
  
    return ret;
}

motb MotE::motscdp(unsigned char flag)
{
    motb ret = motbt;

    if (!motsa) 
	{
        connms *msg = new connms(CONNMS_TY_DP, flag);    
        ret = motb(motmcon->sndm(msg));
        delete msg;
    }  

    return ret;
}

void MotE::motepof(char *p) 
{
    if ( strncmp(p, "-p", 2) == 0 )
        motpof = 1;
}

void MotE::motedc(void) 
{
    motAM(motpn, motpof, this);
}

unsigned long MotE::motess(void)
{
    return (4*1024*1024);
}

