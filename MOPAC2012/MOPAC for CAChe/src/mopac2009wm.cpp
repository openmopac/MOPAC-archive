#include <windows.h>
#include <iostream.h>
#include <stdio.h>
#include "ute.h"
#include "mot.h"
#include "Resource.h"

static MotE daMot;

int CALLBACK WinMain( HINSTANCE hInstance,
                      HINSTANCE hPrevInstance,
                      LPSTR lpCmdLine,
                      int nCmdShow )
{
    int argc;
    char **argv;
    HWND hWnd;
    int result;

	  
    argv = uteclta(GetCommandLine(), &argc);

    if ( argv == NULL )
        return (FALSE);
// DebugBreak();
    if ( !motIA(hInstance, MotAn, IDI_COMPUTE_ENGINE) )
        return(FALSE);

    if ( !motII(hInstance, nCmdShow, MotAn, MotAn, &daMot, &hWnd) )
        return(FALSE);

    if ( !daMot.moteInit(argc, argv, hWnd) )
        return(FALSE);

    result = motWM(hInstance, hPrevInstance, lpCmdLine, nCmdShow);
 
    result = daMot.moteTerm();

    return (result);
 
    lpCmdLine;
}

extern "C"
{

void motwfc(void)
{
    conn_ev *evt = daMot.motece;

    evt->Wait(CON_INF_TIME);
    evt->Reset();
}

void motmspf(void)
{
    daMot.motscdp(CONNMESS_FL_NONE);
}

void motme(long exit_code)
{
    ute_Exit(exit_code);
}

long motmsh(void)
{
    conn_ev *evt = daMot.motese;

    if (evt->Wait(0) == CONN_WA_O)
        return(1);
    else
        return(0);
}

} /* extern "C" */
