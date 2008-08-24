#include "sheen.h"

// todo?  workingDirectory and environment
proc_handle run_pty_process (char *const args[], int *fdMaster) {
  proc_handle pid;
  //pid = forkpty(&fdm, NULL, &orig_termios, &size);
  pid = forkpty(fdMaster, NULL, NULL, NULL);
  switch (pid) {
    case -1: // error
      return -1;
    case 0: // child
      execvp(args[0], args);
    default: // parent
      return pid;
  }
}
