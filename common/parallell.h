typedef int (*LPEXECUTE)(LPVOID pParameter);

ULONG WINAPI Parallell_ThExecute(LPVOID pParameter);

class CParallell
{
	struct EXECUTE{
		HANDLE hThread;
		HANDLE hDone;
		LPEXECUTE lpfnExecute;
		LPEXECUTE lpfnDone;
		LPVOID pParameter;
	}**m_ppExecute;
	int m_nExecute;
	LPEXECUTE m_lpfnTotalDone;
	LPVOID m_pDone;

	int m_nParallell;
	int m_nTimeout;
	int m_nInitWait;
	DWORD m_dwStackSize;

public:
	CParallell(int nParallell, int nTimeout=INFINITE,int nInitWait=0);
	~CParallell();

	void SetTotalDone(LPEXECUTE lpfnTotalDone,LPVOID pDone);
	int AddFunction(LPEXECUTE lpfnExecute, LPVOID pParameter, LPEXECUTE lpfnDone=NULL);
	int Execute();
	int ExecuteNoOrder();
	void Reset();
	void SetStackSize(DWORD dwStackSize);

	friend ULONG WINAPI Parallell_ThExecute(LPVOID pParameter);
};

