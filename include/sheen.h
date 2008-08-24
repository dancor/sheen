#include "HsBase.h"
#include <pty.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

// Should really be intptr_t, but we don't have that type on the Haskell side
typedef long proc_handle;
