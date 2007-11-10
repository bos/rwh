#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>

char *__pty_ptsname(int fd)
{
    extern char *ptsname(int);
    return ptsname(fd);
}

int __pty_grantpt(int fd)
{
    extern int grantpt(int);
    return grantpt(fd);
}

int __pty_unlockpt(int fd)
{
    extern int unlockpt(int);
    return unlockpt(fd);
}

int __pty_setctty(int fd)
{
#ifdef TIOCSCTTY
    if (ioctl(fd, TIOCSCTTY, NULL) == -1)
	return -1;
#else
    // If we don't have TIOCSCTTY, we should be able to replace the
    // current controlling terminal by detaching from it and opening
    // the named terminal again.  This open should cause the terminal
    // to become our controlling terminal.

    char *name = ttyname(fd);
    int nfd;
    if (name == NULL)
	return -1;

    if (fd != STDIN_FILENO)
	close(STDIN_FILENO);
    if (fd != STDOUT_FILENO)
	close(STDOUT_FILENO);
    if (fd != STDERR_FILENO)
	close(STDERR_FILENO);
    nfd = open(name, O_RDWR);
    if (nfd >= 0)
	close(nfd);
#endif
    return 0;
}
