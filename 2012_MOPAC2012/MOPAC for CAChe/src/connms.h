#ifndef	CONNMS_H
#define	CONNMS_H

# define connmsph HANDLE

enum connMsFl {
 	CONNMESS_FL_NONE = 0,
 	CONNMESS_FL_BL = 1
};

enum connMsTy {
	CONNMS_TY_ST = 1,
	CONNMS_TY_DP,
	CONNMS_TY_ID,
	CONNMS_TY_CO,
	CONNMS_TY_HA,
	CONNMS_TY_DI
};

class lMan;

class connms {
public:
	unsigned char emty;
	unsigned char emfl; 

	connms(unsigned char ty, unsigned char fl);
};

class connmsta : public connms {
public:
	int chid;
	unsigned long stalen;
	char *msta;

	connmsta(
		unsigned char ty,
		unsigned char fla,
		int chnid,
		unsigned long stln,
		char *stat);

	~connmsta(void);
};


class connmsp {
private:
	long					bufsz;
	SECURITY_ATTRIBUTES 	sat;
	conner					msta;
	FILE					*logfi;
	lMan					*lgmg;
	void					DbPr(char *fmt, ...);

public:
	connmsph		rdhd;
	connmsph		wrhd;
							
	connmsp(FILE *lgfi);

	connmsp(
		connmsph rhd,
		connmsph whd,
		FILE *lgfi);
	~connmsp(void);

	connbl 	vlid(void);
	connbl	read(unsigned long len, unsigned char *buf);
	connbl	writ(unsigned long len, unsigned char *buf);
	void	disco(void);
};

class connmsch {

private:
	conner					msta;
	FILE					*logfi;
	lMan					*lgmg;
	void					DbPr(char *fmt, ...);

public:

	connmsp 			*datp;
	connmsp 			*acp;
	connmsch(FILE *lgfi);
	connmsch(
		connmsph drph,
		connmsph dwph,
		connmsph arph,
		connmsph awph,
		FILE *lgfi);

	~connmsch(void);
	
	connbl 		vlid(void);
	connbl 		rddt(unsigned long len, void *buf);
	connbl 		wrdt(unsigned long len, void *buf);
	connbl 		snda(void);
	connbl 		wtfa(void);
	void		disco(void);
};

class connmsc {
private:
 	conner					msta;
	FILE					*logfi;
	lMan					*lgmg;
	void					DbPr(char *fmt, ...);

public:	
	connmsch			*rech;
	connmsch			*sech;
	connmsc(FILE *lgfi);
	connmsc(
			connmsph rrph,	
			connmsph rwph,	
			connmsph swph,	
			connmsph srph,	
			FILE *lgfi);
	
	~connmsc(void);
	
	connbl 	vlid(void);
	connms	*gnm();
	connbl	sndm(connms *msg);
	connbl	sndm(connmsta *msg);
	void	disco(void);

};


#endif /* CONNMS_H */
