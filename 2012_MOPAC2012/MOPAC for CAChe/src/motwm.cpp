#include <windows.h>
#include <iostream.h>
#include <stdio.h>

#include "ute.h"
#include "mot.h"

static
LRESULT
CALLBACK
motwmwp(
    HWND hWnd,		
    UINT message,	
    WPARAM uParam,	
    LPARAM lParam	
) {
	LRESULT			result = 0;
	MotE*	ce = (MotE*) GetWindowLong(hWnd, 0);

    switch (message) {
	case WM_PAINT:
		result = ce->WmPaint(hWnd, message, uParam, lParam);
	    break;
	case WM_COPYDATA:
		result = ce->WmCopyData(hWnd, message, uParam, lParam);
	    break;
    case WM_DESTROY:
		PostQuitMessage(0);
        break;
	default:
		result = DefWindowProc(hWnd, message, uParam, lParam);
		break;
    }

    return(result);
}

BOOL
motIA(HINSTANCE hI,
           const char* szan,
           int iid) 
{
    WNDCLASS  wc;

    wc.style         = 3;
    wc.lpfnWndProc   = (WNDPROC)motwmwp;    
    wc.cbClsExtra    = 0;                      
    wc.cbWndExtra    = sizeof(void*);         
    wc.hInstance     = hI;              
    wc.hIcon         = LoadIcon (hI, MAKEINTRESOURCE(iid));
    wc.hCursor       = LoadCursor(hI, IDC_ARROW);
    wc.hbrBackground = (HBRUSH)(COLOR_WINDOW+1);
    wc.lpszMenuName  = szan;
    wc.lpszClassName = szan;              

    return (RegisterClass(&wc));
}

BOOL 
motII(
HINSTANCE hI,
           int nCmdShow,
           const char* szan,
           const char* szti,
           MotE* mot,
           HWND* hWnd
) {

    *hWnd = CreateWindow(
            szan,           
            szti,            
            WS_OVERLAPPEDWINDOW,	
            CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, 
            NULL,                
            NULL,                
            hI,           
            NULL                 
    );

    if (*hWnd == NULL)
    	return (FALSE);

	SetLastError(0);
	LONG prev = SetWindowLong(*hWnd, 0, (LONG)mot);
	DWORD last = GetLastError();
	if (prev == 0 && last != 0)
		return(FALSE);

    ShowWindow(*hWnd, SW_SHOWMINNOACTIVE);
    UpdateWindow(*hWnd);         
	
	return (TRUE);             
}

int
motWM(HINSTANCE hI,
      HINSTANCE hP,
      LPSTR cmd,
      int nCmdShow 
) {
    MSG msg;

    while (GetMessage(&msg, NULL, 0, 0)) {
		TranslateMessage(&msg);
    	DispatchMessage(&msg);
    }

    return(msg.wParam);
}

