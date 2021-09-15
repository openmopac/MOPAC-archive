#include <stdio.h>

#if defined(unix)
# include <time.h>
# include <sys/types.h>
# include <sys/times.h>
double
utetime(void)
{
	struct tms ts;
	times(&ts);
	return((double)(ts.tms_utime+ts.tms_stime)/CLK_TCK);
}
#endif

#include <time.h>
double
utetime(void)
{
	return(((double)clock())/ CLOCKS_PER_SEC);
}

void uteet( double start, double end, char *buf)
{
 	int h,m = 0;
	double el = end - start;
	
	if (el < 0.0) {
		sprintf(buf,"0000:00:00.00");
		return;
	}
	h = (int) (el / (60.0*60.0));
	el -= (float)(h)*60.0*60.0;
	m = (int) (el / 60.0);
	el -= (float)(m)*60.0;
	
	if (el < 10.0)
		sprintf(buf,"%04d:%02d:0%3.2f",h,m,el);
	else
		sprintf(buf,"%04d:%02d:%4.2f",h,m,el);
	
 }
