#include <stdlib.h>
#include <string.h>
char **
uteclta(char *cmdLine, int *argc) 
{
	char *last, *next, **argv;
	char * line;
	int argCount = 0;
	line = (char *)calloc(strlen(cmdLine), sizeof(char));
	argv = (char **)malloc(8 * sizeof(char *));
	last = cmdLine;		
	next = strpbrk(last, " ");	
	while (next != NULL)		
	{
	  if (next == last)
		{
			last = ++next;
		}
		else
		{
			argv[argCount++] = strncpy(line, last, next - last);
			line += strlen(line) + 1;	
			last = ++next;			
		}
		next = strpbrk(last, " ");
	}
		argv[argCount++] = strcpy(line, last);

	*argc = 8;
	return (argv);
}
