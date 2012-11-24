/* File: xuim.c
 * SCCS Version: %W%
 * Contains: C code for an X based User-Interface Module.
 * Author: Richard Kirby (rbk)
 *         Version 2 mods by Jeff Dalton
 * Created: Tue Aug  7 13:46:21 1990
 * Updated: Fri Oct 25 18:52:07 1996 by Jeff Dalton
 * Release Version: %Y%
 * Copyright: (c) 1992, AIAI, University of Edinburgh
 * This material may be reproduced by or for the U.S. Government pursuant
 * to the copyright license under the clause at DFARS 252.227-7032
 * (June 1975) -- Rights in Technical Data and Computer Software (Foreign).
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

/* USES ATHENA WIDGETS. */

/* Creates a Form widget at the top level. In this widget are several child
   widgets used for displaying the status of the planner. The Form widget will
   also act as a home for a popup menu widget which will allow the changing
   of the various controls of the planner. Communication to the planner is via
   standard input and output.
 */

/* Tentative layout.

                      CONTROL PANEL
      +----------------------------------------------------+
      | Diagnostic Level                                   |
      | IM: xxxxxxx                                        |
      | AM: xxxxxxx                                        |
      | DM: xxxxxxx                                        |
      | KP: xxxxxxxxx                                    |
      | Set ALL                                  DIE       |
      +----------------------------------------------------+

   Each of the process names will be a command button that causes a menu to
   popup, allowing the current diagnostic level to be changed for that process.
   The ALL button will allow the diagnostic level to be changed for all the
   processes in one go. The actual diagnostic levels (xxxxx) are Label widgets.
 */

typedef struct {
  String diagLevelName;
  String DiagLevelMsg;
  int diagLevelNum;
  } DiagLevelStruct, *DiagLevel;

#define INITIAL_LEVEL "Minimal"

/* This will need updating if any changes made in
   OPLANDIR/support_lib/developerlib.lsp
 */

#ifdef undef
static DiagLevelStruct diagLevels[] = {
  { "All Messages", ":ALL", 10 },
  { "General", ":INFORMATION", 7 },
  { "Warnings", ":WARNING", 5 },
  { "Non fatal errors", ":NON-FATAL-ERROR", 5 },
  { "Fatal Errors", ":FATAL-ERROR", 1 },
  { "Emergencies", ":EMERGENCY", 1 },
  { "None", ":NONE", 0 },
  { NULL, NULL, 0 }
};
#endif

static DiagLevelStruct diagLevels[] = {
  { "Everything", ":EVERYTHING", 10 },
  { "Detail", ":DETAIL", 4 },		/* detailed trace */
  { "Trace", ":TRACE", 4 },		/* standard trace */
  { "Minimal", ":MINIMAL", 3},		/* minimal trace */
  { "Warnings", ":WARNING", 2 },
  { "Errors", ":ERROR", 1 },
  { "Nothing", ":NOTHING", 0 },
  { NULL, NULL, 0 }
};

static char *diagButtonSelected;
static Widget setDiagMenuW;
static Widget dlW, imW, imLW, amW, amLW, dmW, dmLW,
  kp1W, kp1LW, setAllW, singleStepW, quitW, dmDevelopersMenuW,
  kpDevelopersMenuW;

static int from_im = 0; /* fds connecting to IM. */
static int to_im = 1; 

static char outBuf[BUFSIZ];

/* Need to send the length as a long, and then the data. */
void ipc_write(conn, buf)
int conn;
char *buf;
{
  static long len;
  long written;

  len = strlen(buf);
  buf[len++] = '\n';		/* make it end in a newline */
  buf[len] = '\0';
#ifdef undef
  if (write(conn, &len, sizeof(long)) != sizeof(long)) {
    perror("ipc_write: Couldnt send length.");
    exit(-1);
  }
#endif
  while (len > 0) {
    written = write(conn, buf, len);
    if (written == -1) {
      perror("ipc_write: Couldnt send data.");
      exit(-1);
    }
    len -= written;
    buf += written;
  }
}

void SingleStep()
{
  sprintf(outBuf, "(:XUIM :SINGLE-STEP (:AM T))");
  ipc_write(to_im, outBuf);
}

void DMDevelopersMenu()
{
  sprintf(outBuf, "(:XUIM :INTERRUPT (:DM))");
  ipc_write(to_im, outBuf);
}

void KPDevelopersMenu()
{
  sprintf(outBuf, "(:XUIM :INTERRUPT (:KP))");
  ipc_write(to_im, outBuf);
}

void Quit() {
  sprintf(outBuf, "(:XUIM :DYING NIL)");
  ipc_write(to_im, outBuf);
  exit(0);
}

/* Callback routine to handle menu selection. */
void setDiagLevel(w, whatTo, call_data)
     Widget w;
     DiagLevel whatTo;
     caddr_t call_data; /* Athena Command Widgets pass junk in this. */
{
  Arg args[8];
  int n;

/* Send a message to the IM. */
/* The message will be to set the diag level for the process named by
   diagButtonSelected, to the value of whatTo.
 */

  sprintf(outBuf, "(:XUIM :SET-DEBUG-LEVEL (%s %s))",
	  diagButtonSelected, whatTo->DiagLevelMsg);
  ipc_write(to_im, outBuf);

  n = 0;
  XtSetArg(args[n], XtNlabel, whatTo->diagLevelName); n++;
  if (strcmp(":ALL", diagButtonSelected) == 0) {
    /* Selected SetAll button. */
    XtSetValues(imLW, args, n);
    XtSetValues(amLW, args, n);
    XtSetValues(dmLW, args, n);
    XtSetValues(kp1LW, args, n);
  }
  else if (strcmp(":IM", diagButtonSelected) == 0)
    XtSetValues(imLW, args, n);
  else if (strcmp(":AM", diagButtonSelected) == 0)
    XtSetValues(amLW, args, n);
  else if (strcmp(":DM", diagButtonSelected) == 0)
    XtSetValues(dmLW, args, n);
  else if (strcmp(":KP", diagButtonSelected) == 0)
    XtSetValues(kp1LW, args, n);

  XtPopdown(setDiagMenuW);
}

/* Action that sets diagButtonSelected. */
void getDiagLevel(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
  if (*num_params == 1)
    diagButtonSelected = params[0];
  else
    diagButtonSelected = "fred";
}

static XtActionsRec actions[] = {
  { "prepare", getDiagLevel }
};

static char imTrans[] =
  "<Btn1Down>:   prepare(:IM) XawPositionSimpleMenu(SetDiagMenu) \
                 MenuPopup(SetDiagMenu) \n\
";

static char amTrans[] =
  "<Btn1Down>:   prepare(:AM) XawPositionSimpleMenu(SetDiagMenu) \
                 MenuPopup(SetDiagMenu) \n\
";

static char dmTrans[] =
  "<Btn1Down>:   prepare(:DM) XawPositionSimpleMenu(SetDiagMenu) \
                 MenuPopup(SetDiagMenu) \n\
";

static char kp1Trans[] =
  "<Btn1Down>:   prepare(:KP) XawPositionSimpleMenu(SetDiagMenu) \
                 MenuPopup(SetDiagMenu) \n\
";

static char setAllTrans[] =
  "<Btn1Down>:   prepare(:ALL) XawPositionSimpleMenu(SetDiagMenu) \
                 MenuPopup(SetDiagMenu) \n\
";

#define BUTTONWIDTH 80
#define LABELWIDTH 120

void AddButtons(parent)
     Widget parent;
{
  Arg args[8];
  int n;
  XtTranslations trans;

  /* Change the actions, so that on button down in one of the command widgets
     it causes a menu to be poped up.
   */
  XtAddActions(actions, XtNumber(actions));

  n = 0;
  XtSetArg(args[n], XtNlabel, "Diagnostic Level"); n++;
  XtSetArg(args[n], XtNborderWidth, 0); n++;
  dlW = XtCreateManagedWidget("dlW", labelWidgetClass, parent, args, n);

  n = 0;
  XtSetArg(args[n], XtNlabel, "IM"); n++;
  XtSetArg(args[n], XtNwidth, BUTTONWIDTH); n++;
  XtSetArg(args[n], XtNfromVert, dlW); n++;
  imW = XtCreateManagedWidget("imW", commandWidgetClass, parent, args, n);
  trans = XtParseTranslationTable(imTrans);
  XtOverrideTranslations(imW, trans);

  n = 0;
  XtSetArg(args[n], XtNlabel, INITIAL_LEVEL); n++;
  XtSetArg(args[n], XtNwidth, LABELWIDTH); n++;
  XtSetArg(args[n], XtNborderWidth, 0); n++;
  XtSetArg(args[n], XtNfromVert, dlW); n++;
  XtSetArg(args[n], XtNfromHoriz, imW); n++;
  imLW = XtCreateManagedWidget("imLW", labelWidgetClass, parent, args, n);

  n = 0;
  XtSetArg(args[n], XtNlabel, "AM"); n++;
  XtSetArg(args[n], XtNwidth, BUTTONWIDTH); n++;
  XtSetArg(args[n], XtNfromVert, imW); n++;
  amW = XtCreateManagedWidget("amW", commandWidgetClass, parent, args, n);
  trans = XtParseTranslationTable(amTrans);
  XtOverrideTranslations(amW, trans);

  n = 0;
  XtSetArg(args[n], XtNlabel, INITIAL_LEVEL); n++;
  XtSetArg(args[n], XtNwidth, LABELWIDTH); n++;
  XtSetArg(args[n], XtNborderWidth, 0); n++;
  XtSetArg(args[n], XtNfromVert, imW); n++;
  XtSetArg(args[n], XtNfromHoriz, amW); n++;
  amLW = XtCreateManagedWidget("amLW", labelWidgetClass, parent, args, n);

  n = 0;
  XtSetArg(args[n], XtNlabel, "DM"); n++;
  XtSetArg(args[n], XtNwidth, BUTTONWIDTH); n++;
  XtSetArg(args[n], XtNfromVert, amW); n++;
  dmW = XtCreateManagedWidget("dmW", commandWidgetClass, parent, args, n);
  trans = XtParseTranslationTable(dmTrans);
  XtOverrideTranslations(dmW, trans);

  n = 0;
  XtSetArg(args[n], XtNlabel, INITIAL_LEVEL); n++;
  XtSetArg(args[n], XtNwidth, LABELWIDTH); n++;
  XtSetArg(args[n], XtNborderWidth, 0); n++;
  XtSetArg(args[n], XtNfromVert, amW); n++;
  XtSetArg(args[n], XtNfromHoriz, dmW); n++;
  dmLW = XtCreateManagedWidget("dmLW", labelWidgetClass, parent, args, n);

  n = 0;
  XtSetArg(args[n], XtNlabel, "KP"); n++;
  XtSetArg(args[n], XtNwidth, BUTTONWIDTH); n++;
  XtSetArg(args[n], XtNfromVert, dmW); n++;
  kp1W = XtCreateManagedWidget("kp1W", commandWidgetClass, parent, args, n);
  trans = XtParseTranslationTable(kp1Trans);
  XtOverrideTranslations(kp1W, trans);

  n = 0;
  XtSetArg(args[n], XtNlabel, INITIAL_LEVEL); n++;
  XtSetArg(args[n], XtNwidth, LABELWIDTH); n++;
  XtSetArg(args[n], XtNborderWidth, 0); n++;
  XtSetArg(args[n], XtNfromVert, dmW); n++;
  XtSetArg(args[n], XtNfromHoriz, kp1W); n++;
  kp1LW = XtCreateManagedWidget("kp1LW", labelWidgetClass, parent, args, n);

  n = 0;
  XtSetArg(args[n], XtNlabel, "Set ALL"); n++;
  XtSetArg(args[n], XtNwidth, BUTTONWIDTH); n++;
  XtSetArg(args[n], XtNfromVert, kp1W); n++;
  setAllW = XtCreateManagedWidget("setAllW", commandWidgetClass,
				  parent, args, n);
  trans = XtParseTranslationTable(setAllTrans);
  XtOverrideTranslations(setAllW, trans);

  n = 0;
  XtSetArg(args[n], XtNlabel, "Single Step"); n++;
  XtSetArg(args[n], XtNwidth, 130); n++;
  XtSetArg(args[n], XtNfromVert, dlW); n++;
  XtSetArg(args[n], XtNfromHoriz, kp1LW); n++;
  XtSetArg(args[n], XtNtop, XtChainTop); n++;
  XtSetArg(args[n], XtNright, XtChainRight); n++;
  singleStepW = XtCreateManagedWidget("singleStepW", commandWidgetClass,
				      parent, args, n);
  XtAddCallback(singleStepW, XtNcallback, SingleStep, NULL);

  n = 0;
  XtSetArg(args[n], XtNlabel, "DM Developers Menu"); n++;
  XtSetArg(args[n], XtNwidth, 130); n++;
  XtSetArg(args[n], XtNfromVert, singleStepW); n++;
  XtSetArg(args[n], XtNfromHoriz, kp1LW); n++;
  dmDevelopersMenuW = XtCreateManagedWidget("dmDevelopersMenuW",
					    commandWidgetClass,
					    parent, args, n);
  XtAddCallback(dmDevelopersMenuW, XtNcallback, DMDevelopersMenu, NULL);

  n = 0;
#ifdef no_real_KS_USER
  XtSetArg(args[n], XtNlabel, "KP Developers Menu"); n++;
#else
  XtSetArg(args[n], XtNlabel, "Intervene as User"); n++;
#endif
  XtSetArg(args[n], XtNwidth, 130); n++;
  XtSetArg(args[n], XtNfromVert, dmDevelopersMenuW); n++;
  XtSetArg(args[n], XtNfromHoriz, kp1LW); n++;
  kpDevelopersMenuW = XtCreateManagedWidget("kpDevelopersMenuW",
					    commandWidgetClass,
					    parent, args, n);
  XtAddCallback(kpDevelopersMenuW, XtNcallback, KPDevelopersMenu, NULL);

  n = 0;
  XtSetArg(args[n], XtNlabel, "QUIT"); n++;
  XtSetArg(args[n], XtNwidth, 130); n++;
  XtSetArg(args[n], XtNbottom, XtChainBottom); n++;
  XtSetArg(args[n], XtNright, XtChainRight); n++;
  XtSetArg(args[n], XtNfromVert, kp1W); n++;
  XtSetArg(args[n], XtNfromHoriz, kp1LW); n++;
  quitW = XtCreateManagedWidget("quitW", commandWidgetClass,
				parent, args, n);
  XtAddCallback(quitW, XtNcallback, Quit, NULL);
}

void SetUpSetDiagLevelMenu(parent)
     Widget parent;
{
  Arg args[8];
  int n;
  Widget entry;
  int i;

  n = 0;
  XtSetArg(args[n], XtNlabel, "Choose Diag level"); n++;
  XtSetArg(args[n], XtNmenuOnScreen, True); n++;

  setDiagMenuW = XtCreatePopupShell("SetDiagMenu", simpleMenuWidgetClass,
				    parent, args, n);
  for (i=0; diagLevels[i].diagLevelName != NULL; i++) {
    entry = XtCreateManagedWidget(diagLevels[i].diagLevelName,
				  smeBSBObjectClass,
				  setDiagMenuW, NULL, 0);
    XtAddCallback(entry, XtNcallback, setDiagLevel, &(diagLevels[i]));
  }

  /* Minimises menu popup time. */
  XtRealizeWidget(setDiagMenuW);
}

Bool ConnectToIM()
{
  /* pre-initialized when vars declared */

  /* Now register with the IM. */
  sprintf(outBuf, "(:XUIM :REGISTER (:XUIM))");
  ipc_write(to_im, outBuf);

  return(True);
}

static char inBuf[BUFSIZ];

void HandleControlEvents(data, source, id)
     caddr_t data;
     int *source;
     XtInputId *id;
{
  int num;
  char mess[128];
  static long garbage;

#ifdef undef
  /* Skip the length data. */
  read(from_im, &garbage, sizeof(long));
#endif

  num = read(from_im, inBuf, BUFSIZ);
  if (num == EOF) {
    perror("EOF received, bumming out!");
    exit(0);
  }
#ifdef xuim_debug
  fprintf(stderr, "Received %d = %s\n", num, inBuf);
#endif
  if (num == 0) {
    perror("0 received, so assuming that the Planner has gone down.");
    exit(0);
  }
  num = sscanf(inBuf, "(:IM :%s", mess);
  if (num == EOF)
    perror("Unexpected EOF");
  else if (num != 1)
    perror("Unexpected form");
  else {
    if (strcmp(mess, "INIT") == 0) {
      /* :INIT message received, so return the expected :OK message. */
      sprintf(outBuf, "(:XUIM :OK NIL)");
      ipc_write(to_im, outBuf);
    }
    else if (strcmp(mess, "DEBUG-LEVEL") == 0)
      /* :DEBUG-LEVEL message received, so do nothing. */
      /* NULL */;
    else if (strcmp(mess, "OK") == 0)
      /* :OK message received, so do nothing. */
      /* NULL */;
    else if (strcmp(mess, "DIE-DIE-DIE") == 0) {
      /* :DIE-DIE-DIE received, so exit */
      exit(0);
    }
    else {
      perror("Keyword not recognised.\n");
    }
  }
}

main(argc, argv)
     int argc;
     char* argv[];
{
  Widget toplevelW, formW;
  /* XtInputId junk; */     /* DELETE ME /\/ */

  /* Initialise the Intrinsics and create a Form widget as a base. */
  toplevelW = XtInitialize(argv[0], "Uim", NULL, 0, &argc, argv);
  formW = XtCreateManagedWidget("topForm", formWidgetClass, toplevelW,
				NULL, 0);

  /* Add the various buttons to the Form widget. */
  AddButtons(formW);

  /* Set up the SetDiagLevel menu. */
  SetUpSetDiagLevelMenu(toplevelW);

  /* Connect to IM. */
  if (!ConnectToIM()) {
    fprintf(stderr, "\nFailed to connect to IM.\n");
    exit(-1);
  }

  /* Register the event handler proc. */
  XtAddInput(from_im, (XtPointer)XtInputReadMask, HandleControlEvents, NULL);

  /* Display and respond. */
  XtRealizeWidget(toplevelW);
  XtMainLoop();
}

/* ----------------------------- Change History -----------------------------
 * (Initials) (Date of change)           (Comment)
 *     jd        03 Mar 93      Comms with the IM via standard input and
 *                              output instead of via a socket.
 */
