#ifndef _SLIST_H_
#define _SLIST_H_

#ifndef RC_INVOKED 
 
#include <stddef.h>

#define slista	0
#define slists	0
#define slistm	1
#define slistc	9  
#define slistfm	0

#define slistsol(slistlt, slistne, slistfe) \
 			(offsetof(slistlt, slistfe) + \
			(slistne) * sizeof(((slistlt *) 0)->slistfe))

#define	slistsc		3
#define	slistfns	256

typedef	unsigned long	slistcft;
typedef	unsigned long	slistcfo;
typedef	unsigned long	slistctf; 


struct ufts
{
	char slistfn[slistfns];
	slistcft caft;
	slistcfo cfot;
	slistctf ctfo;
};
typedef struct ufts ufts;

typedef struct ufts fts;

struct sftl
{
    long numFiles;
    fts files[1];
};
typedef struct sftl sftl;


#endif	// RC_INVOKED
#endif /* _SLIST_H_ */
