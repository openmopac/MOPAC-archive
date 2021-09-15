#include "mot.h"

LRESULT MotE::WmPaint(HWND hWnd, UINT message, WPARAM uParam, LPARAM lParam)
{
	const int	xStart = 20;
	const int	yStart = 20;
	PAINTSTRUCT	ps;
	HDC			hdc;

	hdc = BeginPaint(hWnd, &ps);

	EndPaint(hWnd, &ps);

	return(0);
}

LRESULT MotE::WmCopyData(HWND hWnd, UINT message, WPARAM uParam, LPARAM lParam)
{
	PCOPYDATASTRUCT cdsp;

	cdsp = (PCOPYDATASTRUCT) lParam;
	return(TRUE);
}
