#include <process.h>
#include "ute.h"

void ute_Exit(int rc)
{
    _endthreadex(rc);
}
