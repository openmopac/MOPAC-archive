#ifndef _MOT_H_
#define _MOT_H_

#include <windows.h>
#include <stdio.h>

#include "slist.h"
#include "conn.h"
#include "connms.h"
#include "connev.h"
#include "ute.h"

const char* const MotAn = UTESTR(CMN);

enum mots {
    motscr,
    motsin,
    motsru,
    motsdo,
    motste
};

enum moter {
    moterno = 0,
    motercrcf,
    moterecf,
    moterevcf,
    motermccf,
    motermcf,
    moterlcf,
    moterccf,
    moterdm,
    moterbm,
    moterdu
};

enum motb {
    motbf = 0,
    motbt
};

const unsigned int MOTFNS=256;
const unsigned long MOTDFS=100*1024;

class MotE;

typedef HANDLE motth;
typedef unsigned mottid;
typedef DWORD motpid;

typedef mots motmnst;

class MotMn {

    enum MOTTME {
        MOTMEOS = 0,
        MOTMERECEX,
        MOTMEST,
        MOTMECONEX,
        MOTMENUM
    };

    conn_ev *motmevts[MOTMENUM];

public:
    MotE *mote;
    moter msta;
    motmnst motmnst;

    motth mott;
    mottid motthid;

    motth motthco;
    mottid motthcoid;
    unsigned long motstsz;

    conn_ev *motee;

    MotMn(void);
    ~MotMn(void);
    motb moini(MotE *ce);
    void moterm(void);
    void morun(void);
    void moex(void);
    void mostco(void);
    void mowtco(void);
};

typedef mots motcrs;

class MotCR {

public:
    MotE *mote;
    moter msta;
    motcrs recsta;
    motth mott;
    mottid motthid;
    conn_ev *motee;
    connmsc *motmcon;
    
    MotCR(void);
    ~MotCR(void);
    motb motcri(MotE *ce, connmsc *msgConnection);
    void motcrt(void);
    void motcrr(void);
    void motcre(void);
    connms *gnm(void);
    moter motcrdm(connms *msg);
    void disco(void);
};
    
typedef HANDLE MOTCRPH;

class MotE {

public:
    char *motver;
    int motpof;
    motb motsa;
    motpid motp;
    motth mott;
    mottid motthid;
    motb motdfl;
    mots motsta;
    long msta;
    char motpn[MOTFNS];
    
    MOTCRPH moterp;
    MOTCRPH moterpa;
    MOTCRPH motesp;
    MOTCRPH motespa;
    connmsc *motmcon;
    int m_argc;
    char **m_argv;
    HWND m_hWnd;
    
    sftl *moteifl;
    
    FILE *logfi;
    char *motelfbn;
    char motelfn[MOTFNS];
    
    MotMn *motemn;
    MotCR *motecr;
    
    conn_ev *moteote;
    conn_ev *motecree;
    conn_ev *motemee;
    conn_ev *motece;
    conn_ev *motese;
    
    MotE(void);
    ~MotE(void);
    
    motb moteInit(int argc, char **argv, HWND hWnd);
    int moteTerm(void);
    
    motb motescs(unsigned int channel, char *status, unsigned char flag);
    motb motscdp(unsigned char flag);
    motb moteaif(unsigned long cftype, char *fileName);
    void motepof(char *p);
    void motedc(void);
    unsigned long motess(void);

    virtual LRESULT WmPaint(HWND hWnd, UINT message, WPARAM uParam, LPARAM lParam);
    virtual LRESULT WmCopyData(HWND hWnd, UINT message, WPARAM uParam, LPARAM lParam);
};

#if defined(__cplusplus)
extern "C" {
#endif
 
void motAM(char *name, int pof, void *motE);

BOOL motIA(HINSTANCE hI,
           const char* szan,
           int iid );

BOOL motII(HINSTANCE hI,
           int nCmdShow,
           const char* szan,
           const char* szti,
           MotE* mot,
           HWND* hWnd);

int motWM( HINSTANCE hI,
           HINSTANCE hP,
           LPSTR cmd,
           int nCmdShow );

void motSTT(char* tag);

void motSKD(void);

#if defined(__cplusplus)
}
#endif

#endif /* _MOT_H_ */
