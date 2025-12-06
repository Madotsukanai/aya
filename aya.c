#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <locale.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#include <wchar.h>
#include <stdbool.h>

#define AYA_VERSION "0.1.1"
#define AYA_TAB_STOP 4
#define AYA_QUIT_TIMES 3
#define LINE_NUM_COLUMNS 6
#define CTRL_KEY(k) ((k) & 0x1f)

enum editorKey {
  BACKSPACE = 127,
  ARROW_LEFT = 1000,
  ARROW_RIGHT,
  ARROW_UP,
  ARROW_DOWN,
  DEL_KEY,
  HOME_KEY,
  END_KEY,
  PAGE_UP,
  PAGE_DOWN,
  MOUSE_WHEEL_UP,
  MOUSE_WHEEL_DOWN,
  MOUSE_CLICK,
  SHIFT_ARROW_LEFT,
  SHIFT_ARROW_RIGHT,
  SHIFT_ARROW_UP,
  SHIFT_ARROW_DOWN,
  CTRL_ARROW_RIGHT,
  CTRL_ARROW_LEFT,
  THORN_KEY = 1016
};

enum editorLight {
  HL_NORMAL = 0,
  HL_COMMENT,
  HL_MLCOMMENT,
  HL_KEYWORD1,
  HL_KEYWORD2,
  HL_STRING,
  HL_NUMBER,
  HL_MATCH,
  HL_PREPROC,
  HL_INCLUDE,
  HL_DEFINE_NUMBER,
  HL_DEFINE_STRING
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

struct editorSyntax {
  char *filetype;
  char **filematch;
  char **keywords;
  char *singleline_comment_start;
  char *multiline_comment_start;
  char *multiline_comment_end;
  int flags;
};

/*** filetypes ***/

char *C_HL_extensions[] = { ".c", ".h", NULL };
char *C_HL_keywords[] = {
  "switch", "if", "while", "for", "break", "continue", "return", "else",
  "struct", "union", "typedef", "static", "enum", "case",
  "const", "volatile", "extern", "register", "sizeof", "goto", "do", "default",

  "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
  "void|", "short|", "_Bool|", "_Complex|", "_Imaginary|", NULL
};

char *cpp_HL_extensions[] = { ".cpp", ".hpp", ".cc", ".hh", NULL };
char *cpp_HL_keywords[] = {
    "switch", "if", "while", "for", "break", "continue", "return", "else",
    "struct", "union", "typedef", "static", "enum", "case", "const",
    "volatile", "extern", "register", "sizeof", "goto", "do", "default",
    "class", "public", "private", "protected", "template", "typename",
    "try", "catch", "throw", "new", "delete", "this", "friend", "virtual",
    "namespace", "using", "explicit", "operator", "asm", "export", "inline",
    "mutable", "reinterpret_cast", "static_cast", "const_cast", "dynamic_cast",
    "typeid", "wchar_t",

    "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
    "void|", "short|", "bool|", "auto|", "decltype|",
    "true|", "false|", "nullptr|", NULL
};

char *makefile_HL_extensions[] = { "makefile", "Makefile", NULL };
char *makefile_HL_keywords[] = {
    "define", "endef", "ifeq", "ifneq", "ifdef", "ifndef", "else", "endif",
    "include", "sinclude", "override", "export", "unexport", "vpath",

    ".PHONY|", ".DEFAULT|", ".PRECIOUS|", ".INTERMEDIATE|", ".SECONDARY|",
    ".DELETE_ON_ERROR|", ".LOW_RESOLUTION_TIME|", ".SILENT|", ".IGNORE|",
    ".NOTPARALLEL|", ".ONESHELL|",
    
    ":=|", "+=|", "?=|", "$@|", "$<|", "$^|", "$?|", "$*|", "$+|", NULL
};

char *python_HL_extensions[] = { ".py", NULL };
char *python_HL_keywords[] = {
    "and", "as", "assert", "break", "class", "continue", "def", "del",
    "elif", "else", "except", "finally", "for", "from", "global",
    "if", "import", "in", "is", "lambda", "nonlocal", "not", "or",
    "pass", "raise", "return", "try", "while", "with", "yield",

    "True|", "False|", "None|", "self|", "cls|",

    "int|", "str|", "list|", "dict|", "set|", "tuple|", "print|", "range|",
    "len|", "super|", NULL
};

struct editorSyntax HLDB[] = {
  {
    "c",
    C_HL_extensions,
    C_HL_keywords,
    "//", "/*", "*/",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  },
  {
    "cpp",
    cpp_HL_extensions,
    cpp_HL_keywords,
    "//", "/*", "*/",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  },
  {
    "makefile",
    makefile_HL_extensions,
    makefile_HL_keywords,
    "#", NULL, NULL,
    HL_HIGHLIGHT_STRINGS
  },
  {
    "python",
    python_HL_extensions,
    python_HL_keywords,
    "#", "\"\"\"", "\"\"\"",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  },
};
#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/*** undo/redo ***/
enum undoActionType {
  ACTION_INSERT_CHAR,
  ACTION_DELETE_CHAR,
  ACTION_SPLIT_LINE,
  ACTION_JOIN_LINES,
  ACTION_INSERT_STRING,
  ACTION_DELETE_STRING
};
typedef struct {
  enum undoActionType type;
  int cx, cy;
  union {
    char ch;
    struct {
      char *str;
      int len;
    } string;
  } data;
} undoAction;

/*** append buffer ***/

struct abuf {
  char *b;
  size_t len;
};

#define ABUF_INIT {NULL, 0}

/*** data ***/

typedef struct erow {
  int idx;
  int size;
  int rsize;
  char *chars;
  char *render;
  unsigned char *hl;
  int hl_open_comment;
  int *render_to_chars_map;
} erow;

struct editorConfig {
  int cx, cy;
  int rx;
  int rowoff;
  int coloff;
  int screenrows;
  int screencols;
  int numrows;
  erow *row;
  int dirty;
  char *filename;
  char statusmsg[80];
  time_t statusmsg_time;
  struct editorSyntax *syntax;
  struct termios orig_termios;
  int selection_start_cx;
  int selection_start_cy;
  int selection_end_cx;
  int selection_end_cy;
  undoAction *undo_stack;
  int undo_count;
  int undo_capacity;
  undoAction *redo_stack;
  int redo_count;
  int redo_capacity;
  int is_undo_redo;
  int initial_line;
  struct abuf prev_screen_abuf;
};

struct editorConfig E;

/*** prototypes ***/

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(int, int));
void editorMoveCursor(int key);
void editorInsertString(char *s, int len);
void editorUndo();
void editorRedo();
int editorRowRxToCx(erow *row, int rx);
void editorClearSelection();
int is_selected(int filerow, int cx);
char *editorGetSelectedString();
void editorDeleteSelection();
void editorGoToLine();
int utf8_char_len_from_byte(unsigned char byte);
void editorUpdateSyntax(erow *row);
void editorSelectSyntaxHighlight();
void editorRowInsertString(erow *row, int at, char *s, size_t len);
void editorInsertNewLine();
void editorRowDelChar(erow *row, int at, int len);
void editorRowInsertChar(erow *row, int at, int c);
void editorRowAppendString(erow *row, char *s, size_t len);
void editorDelRow(int at);
void editorInsertRow(int at, char *s, size_t len);
void editorUpdateRow(erow *row);
void editorDelChar();
void editorScroll();
void editorReplace();
void push_action(undoAction **stack, int *count, int *capacity, undoAction action);
void push_undo_action(undoAction action);
char *editorGetSelectedString();
void editorDeleteSelection();

static char find_buffer[256];
static char replace_buffer[256];

const char *editorGetReplaceBuffer() {
    return replace_buffer;
}

/*** terminal ***/

void die(const char *s) {
  write(STDOUT_FILENO, "\x1b[2J", 4);
  write(STDOUT_FILENO, "\x1b[H", 3);

  fprintf(stderr, "%s: %s\n", s, strerror(errno));
  exit(1);
}

void disableRawMode() {
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
    die("tcsetattr");
  write(STDOUT_FILENO, "\x1b[?1006l", 8);
  write(STDOUT_FILENO, "\x1b[?1000l", 8);
  write(STDOUT_FILENO, "\x1b[?1049l", 6);
}

void enableRawMode() {
  if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetattr");
  atexit(disableRawMode);
  struct termios raw = E.orig_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1;
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
  write(STDOUT_FILENO, "\x1b[?1000h", 8);
  write(STDOUT_FILENO, "\x1b[?1006h", 8);
  write(STDOUT_FILENO, "\x1b[?1049h", 6);
}

int editorReadKey() {
  int nread;
  char c;
  while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    if (nread == -1 && errno != EAGAIN) die("read");
  }

  if (c == '\x1b') {
    char seq[32] = {0};
    size_t i = 0;
    while (i < sizeof(seq) - 1) {
      if (read(STDIN_FILENO, &seq[i], 1) != 1) break;
      if (isalpha(seq[i]) || seq[i] == '~') {
        i++;
        break;
      }
      i++;
    }
    if (i == 0) {
        return '\x1b';
    }
    seq[i] = '\0';

    if (seq[0] == '[') {
      if (strlen(seq) == 2) {
          switch(seq[1]) {
              case 'A': return ARROW_UP;
              case 'B': return ARROW_DOWN;
              case 'C': return ARROW_RIGHT;
              case 'D': return ARROW_LEFT;
              case 'H': return HOME_KEY;
              case 'F': return END_KEY;
          }
      } else if (seq[strlen(seq) - 1] == '~') {
          int n;
          if (sscanf(seq, "[%d~", &n) == 1) {
              switch (n) {
                  case 1: return HOME_KEY;
                  case 3: return DEL_KEY;
                  case 4: return END_KEY;
                  case 5: return PAGE_UP;
                  case 6: return PAGE_DOWN;
                  case 7: return HOME_KEY;
                  case 8: return END_KEY;
              }
          }
      } else if (strncmp(seq, "[1;2", 4) == 0 && strlen(seq) == 5) {
          switch(seq[4]) {
              case 'A': return SHIFT_ARROW_UP;
              case 'B': return SHIFT_ARROW_DOWN;
              case 'C': return SHIFT_ARROW_RIGHT;
              case 'D': return SHIFT_ARROW_LEFT;
          }
      }
      else if (strncmp(seq, "[1;5", 4) == 0 && strlen(seq) == 5) {
          switch(seq[4]) {
              case 'C': return CTRL_ARROW_RIGHT;
              case 'D': return CTRL_ARROW_LEFT;
          }
      }
    } else if (seq[0] == 'O') {
      if (strlen(seq) == 2) {
          switch(seq[1]) {
              case 'H': return HOME_KEY;
              case 'F': return END_KEY;
          }
      }
    }
    return '\x1b';
  }

  // Handle UTF-8
  unsigned char uc = c;
  if (uc < 0x80) { // ASCII
    return uc;
  }

  int len = utf8_char_len_from_byte(uc);
  if (len == 1) return c;

  char seq[4];
  seq[0] = c;
  int i = 1;
  while(i < len) {
    if (read(STDIN_FILENO, &seq[i], 1) != 1) return '\x1b';
    if ((seq[i] & 0xC0) != 0x80) {
        return '\x1b';
    }
    i++;
  }

  int codepoint = 0;
  if (len == 2) {
    codepoint = ((seq[0] & 0x1F) << 6) | (seq[1] & 0x3F);
  } else if (len == 3) {
    codepoint = ((seq[0] & 0x0F) << 12) | ((seq[1] & 0x3F) << 6) | (seq[2] & 0x3F);
  } else if (len == 4) {
    codepoint = ((seq[0] & 0x07) << 18) | ((seq[1] & 0x3F) << 12) | ((seq[2] & 0x3F) << 6) | (seq[3] & 0x3F);
  }

  return codepoint;
}

int getCursorPosition(int *rows, int *cols) {
  char buf[32];
  unsigned int i = 0;
  if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;
  while (i < sizeof(buf) - 1) {
    if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
    if (buf[i] == 'R') break;
    i++;
  }
  buf[i] = '\0';
  if (buf[0] != '\x1b' || buf[1] != '[') return -1;
  if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;
  return 0;
}

int getWindowSize(int *rows, int *cols) {
  struct winsize ws;
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
    return getCursorPosition(rows, cols);
  } else {
    *cols = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
  }
}

int is_separator(int c) {
  return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

int utf8_char_len_from_byte(unsigned char byte) {
    if (byte < 0x80) return 1;
    if ((byte & 0xE0) == 0xC0) return 2;
    if ((byte & 0xF0) == 0xE0) return 3;
    if ((byte & 0xF8) == 0xF0) return 4;
    return 1;
}

void editorUpdateSyntax(erow *row) {
    if (row->rsize == 0) {
        free(row->hl);
        row->hl = NULL;
        return;
    }
      row->hl = realloc(row->hl, row->rsize);
      if (!row->hl) die("realloc failed in editorDrawRows");
      memset(row->hl, HL_NORMAL, row->rsize);
    if (E.syntax == NULL) return;

    if (strcmp(E.syntax->filetype, "makefile") == 0) {
        if (row->size > 0 && row->chars[0] == '\t') {
            memset(row->hl, HL_MLCOMMENT, row->rsize);
        }

        char *colon = strchr(row->render, ':');
        char *equal = strpbrk(row->render, "=?");
        if (colon && (!equal || colon < equal)) {
            memset(row->hl, HL_KEYWORD1, colon - row->render);
        }

        for (int i = 0; i < row->rsize - 1; i++) {
            if (row->render[i] == '$' && row->render[i+1] == '(') {
                int j = i + 2;
                while (j < row->rsize && row->render[j] != ')') {
                    j++;
                }
                if (j < row->rsize) {
                    memset(&row->hl[i], HL_PREPROC, j - i + 1);
                }
                i = j;
            }
        }
    }

    char **keywords = E.syntax->keywords;
    char *scs = E.syntax->singleline_comment_start;
    char *mcs = E.syntax->multiline_comment_start;
    char *mce = E.syntax->multiline_comment_end;
    int scs_len = scs ? strlen(scs) : 0;
    int mcs_len = mcs ? strlen(mcs) : 0;
    int mce_len = mce ? strlen(mce) : 0;
    int prev_sep = 1;
    int in_string = 0;
    int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);
        int i = 0;
        while (i < row->rsize) {
            char c = row->render[i];
            unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

            if (E.syntax != NULL && (strcmp(E.syntax->filetype, "c") == 0 || strcmp(E.syntax->filetype, "cpp") == 0) && c == '#' && i == 0) {
                int start_i = i;
                memset(&row->hl[start_i], HL_PREPROC, row->rsize - start_i);
    
                int j = start_i + 1;
                while (j < row->rsize && isspace(row->render[j])) j++;
                int directive_start = j;
                while (j < row->rsize && isalpha(row->render[j])) j++;
                int directive_end = j;
    
                char directive[32];
                int directive_len = directive_end - directive_start;
                if (directive_len > 0 && (size_t)directive_len < sizeof(directive)) {
                    strncpy(directive, &row->render[directive_start], directive_len);
                    directive[directive_len] = '\0';
    
                    if (strcmp(directive, "include") == 0) {
                        // Highlight include path
                        while (j < row->rsize && isspace(row->render[j])) j++; // Skip whitespace
                        if (row->render[j] == '<' || row->render[j] == '"') {
                            int include_start = j;
                            char opener = row->render[j];
                            char closer = (opener == '<') ? '>' : '"';
                            j++; // Skip opener
                            while (j < row->rsize && row->render[j] != closer) j++; // Find closer
                            if (j < row->rsize && row->render[j] == closer) {
                                j++; // Include closer
                            }
                            memset(&row->hl[include_start], HL_INCLUDE, j - include_start);
                        }
                                    } else if (strcmp(directive, "define") == 0) {
                                        // Skip macro name
                                        while (j < row->rsize && isspace(row->render[j])) j++;
                                        while (j < row->rsize && (isalnum(row->render[j]) || row->render[j] == '_')) j++;
                                        
                                        // Now, parse the rest of the line for numbers and strings
                                        while (j < row->rsize) {
                                            if (isspace(row->render[j])) {
                                                j++;
                                                continue;
                                            }
                    
                                            // Check for strings
                                            if (row->render[j] == '"') {
                                                int string_start = j;
                                                j++;
                                                while (j < row->rsize && row->render[j] != '"') {
                                                    if (row->render[j] == '\\' && j + 1 < row->rsize) j++;
                                                    j++;
                                                }
                                                if (j < row->rsize) j++; // closing quote
                                                memset(&row->hl[string_start], HL_DEFINE_STRING, j - string_start);
                                                continue;
                                            }
                    
                                            // Check for numbers
                                            if (isdigit(row->render[j])) {
                                                int number_start = j;
                                                while (j < row->rsize && isdigit(row->render[j])) j++;
                                                if (j < row->rsize && row->render[j] == '.') {
                                                    j++;
                                                    while (j < row->rsize && isdigit(row->render[j])) j++;
                                                }
                                                while (j < row->rsize && strchr("ulfULF", row->render[j])) j++;
                                                memset(&row->hl[number_start], HL_DEFINE_NUMBER, j - number_start);
                                                continue;
                                            }
                    
                                            j++; // Move to next character if not space, string, or number
                                        }
                                    }                }
                break;
            }
    
        if (scs_len && !in_string && !in_comment) {
            if (!strncmp(&row->render[i], scs, scs_len)) {
                memset(&row->hl[i], HL_COMMENT, row->rsize - i);
                break;
            }
        }
        if (mcs_len && mce_len && !in_string) {
            if (in_comment) {
                row->hl[i] = HL_MLCOMMENT;
                if (!strncmp(&row->render[i], mce, mce_len)) {
                    memset(&row->hl[i], HL_MLCOMMENT, mce_len);
                    i += mce_len;
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                } else {
                    i++;
                    continue;
                }
            } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
                memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = 1;
                prev_sep = 1;
                continue;
            }
        }
        if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
            if (in_string) {
                row->hl[i] = HL_STRING;
                if (c == '\\' && i + 1 < row->rsize) {
                    row->hl[i + 1] = HL_STRING;
                    i += 2;
                    continue;
                }
                if (c == in_string) {
                    in_string = 0;
                    prev_sep = 1;
                } else {
                   prev_sep = 0;
                }
                i++;
                continue;
            } else {
                if (prev_sep && (c == '"' || c == '\'')) {
                    in_string = c;
                    row->hl[i] = HL_STRING;
                    i++;
                    prev_sep = 0;
                    continue;
                }
            }
        }
        if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
            if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
                (c == '.' && prev_hl == HL_NUMBER)) {
                row->hl[i] = HL_NUMBER;
                i++;
                prev_sep = 0;
                continue;
            }
        }
        if (prev_sep) {
            int j;
            for (j = 0; keywords[j]; j++) {
                int klen = strlen(keywords[j]);
                int kw2 = keywords[j][klen - 1] == '|';
                if (kw2) klen--;
                if (!strncmp(&row->render[i], keywords[j], klen) &&
                    is_separator(row->render[i + klen])) {
                    memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                    i += klen;
                    goto next_word;
                }
            }
        }
    next_word:
        prev_sep = is_separator(c);
        i++;
    }
    int changed = (row->hl_open_comment != in_comment);
    row->hl_open_comment = in_comment;
    if (changed && row->idx + 1 < E.numrows)
        editorUpdateSyntax(&E.row[row->idx + 1]);
}
void editorSelectSyntaxHighlight() {
    E.syntax = NULL;
    if (E.filename == NULL) return;
    char *ext = strrchr(E.filename, '.');
    for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
        struct editorSyntax *s = &HLDB[j];
        unsigned int i = 0;
        while (s->filematch[i]) {
            int is_ext = (s->filematch[i][0] == '.');
            if ((is_ext && ext && strcmp(ext, s->filematch[i]) == 0) ||
                (!is_ext && strstr(E.filename, s->filematch[i]))) {
                E.syntax = s;
                int filerow;
                for (filerow = 0; filerow < E.numrows; filerow++) {
                    editorUpdateSyntax(&E.row[filerow]);
                }
                return;
            }
            i++;
        }
    }
}

int editorSyntaxToColor(int hl) {
  switch (hl) {
    case HL_PREPROC: return 35;
    case HL_COMMENT: return 32;
    case HL_MLCOMMENT: return 32;
    case HL_KEYWORD1: return 34;
    case HL_KEYWORD2: return 36;
    case HL_STRING: return 33;
    case HL_NUMBER: return 32;
    case HL_MATCH: return 34;
    case HL_INCLUDE: return 32;
    case HL_DEFINE_NUMBER: return 32;
    case HL_DEFINE_STRING: return 33;
    default: return 39;
  }
}



void abAppend(struct abuf *ab, const char *s, size_t len) {
    char *new_b = realloc(ab->b, ab->len + len);
    if (new_b == NULL) die("realloc failed in abAppend");
    memcpy(new_b + ab->len, s, len);
    ab->b = new_b;
    ab->len += len;
}



void abFree(struct abuf *ab) {
  free(ab->b);
}

/*** undo/redo helpers ***/

// プロトタイプ宣言
void push_action(undoAction **stack, int *count, int *capacity, undoAction action);
void clear_undo_stack(undoAction **stack, int *count, int *capacity);

// push_action の定義
void push_action(undoAction **stack, int *count, int *capacity, undoAction action) {
    if (*capacity == *count) {
        *capacity = *capacity < 8 ? 8 : *capacity * 2;
        void *new_stack = realloc(*stack, sizeof(undoAction) * (*capacity));
        if (!new_stack) die("realloc failed in push_action");
        *stack = new_stack;
    }

    if (action.type == ACTION_DELETE_STRING || action.type == ACTION_INSERT_STRING ||
        action.type == ACTION_JOIN_LINES || action.type == ACTION_SPLIT_LINE) {
        if (action.data.string.str != NULL) {
            char *new_str = strndup(action.data.string.str, action.data.string.len);
            if (!new_str) die("strndup failed in push_action");
            action.data.string.str = new_str;
        } else {
            action.data.string.str = NULL;
        }
    }

    (*stack)[*count] = action;
    (*count)++;
}

// push_undo_action の定義
void push_undo_action(undoAction action) {
    if (E.is_undo_redo) return;
    push_action(&E.undo_stack, &E.undo_count, &E.undo_capacity, action);
    clear_undo_stack(&E.redo_stack, &E.redo_count, &E.redo_capacity);
}

// clear_undo_stack の定義
void clear_undo_stack(undoAction **stack, int *count, int *capacity) {
    if (*stack == NULL) return;
    for (int i = 0; i < *count; i++) {
        if ((*stack)[i].type == ACTION_DELETE_STRING || (*stack)[i].type == ACTION_INSERT_STRING ||
            (*stack)[i].type == ACTION_JOIN_LINES || (*stack)[i].type == ACTION_SPLIT_LINE) {
            if ((*stack)[i].data.string.str) free((*stack)[i].data.string.str);
        }
    }
    free(*stack);
    *stack = NULL;
    *count = 0;
    *capacity = 0;
}



void editorRedo();

void editorInsertString(char *s, int len) {
    if (E.selection_start_cy != -1) {
        editorDeleteSelection();
    }

    char *line_start = s;
    for (int i = 0; i <= len; i++) {
        if (i == len || s[i] == '\n' || s[i] == '\r') {
            if (i > 0 && s[i-1] == '\r' && s[i] == '\n') {
                line_start = (char *)&s[i+1];
                continue;
            }
            int line_len = &s[i] - line_start;
            if (line_len > 0) {
              editorRowInsertString(&E.row[E.cy], E.cx, line_start, line_len);
              E.cx += line_len;
            }
            if (i < len && (s[i] == '\n' || s[i] == '\r')) {
                editorInsertNewLine();

            }
            line_start = (char *)&s[i+1];
        }
    }

}

void editorUndo() {
    if (E.undo_count == 0) {
        editorSetStatusMessage("元に戻すことはできません");
        return;
    }

    E.is_undo_redo = 1;

    E.undo_count--;
    undoAction action = E.undo_stack[E.undo_count];

    push_action(&E.redo_stack, &E.redo_count, &E.redo_capacity, action);

    if (action.type == ACTION_DELETE_STRING ||
        action.type == ACTION_INSERT_STRING ||
        action.type == ACTION_JOIN_LINES ||
        action.type == ACTION_SPLIT_LINE) {
        /* 元の undo_stack 側の pointer を無効化 */
        E.undo_stack[E.undo_count].data.string.str = NULL;
    }

    E.cx = action.cx;
    E.cy = action.cy;

    switch (action.type) {

        case ACTION_INSERT_CHAR: {
            editorRowDelChar(&E.row[action.cy], action.cx, 1);
            break;
        }

        case ACTION_DELETE_CHAR: {
            editorRowInsertChar(&E.row[action.cy], action.cx, action.data.ch);
            E.cx++;
            break;
        }

        case ACTION_SPLIT_LINE: {
            /* join の復元 */
            char *tmp = strdup(E.row[action.cy + 1].chars);
            int len = E.row[action.cy + 1].size;

            editorRowAppendString(&E.row[action.cy], tmp, len);
            editorDelRow(action.cy + 1);

            free(tmp);
            break;
        }

        case ACTION_JOIN_LINES: {
            /* split の復元 */
            erow *r = &E.row[action.cy];
            editorInsertRow(action.cy + 1, action.data.string.str,
                            action.data.string.len);

            /* 重要：DelRow後はポインタを取り直す */
            r = &E.row[action.cy];
            r->size = action.cx;
            r->chars[action.cx] = '\0';
            editorUpdateRow(r);

            E.cy++;
            E.cx = 0;
            break;
        }

        case ACTION_INSERT_STRING: {
            /* 削除を戻す（文字列挿入） */
            editorInsertString(action.data.string.str,
                               action.data.string.len);
            break;
        }

        case ACTION_DELETE_STRING: {
            /* 挿入を戻す（削除） */
            int end_y = action.cy;
            int end_x = action.cx;

            for (int i = 0; i < action.data.string.len; i++) {
                if (action.data.string.str[i] == '\n') {
                    end_y++;
                    end_x = 0;
                } else {
                    end_x++;
                }
            }

            if (action.cy == end_y) {
                /* same line */
                erow *row = &E.row[action.cy];
                memmove(&row->chars[action.cx],
                        &row->chars[end_x],
                        row->size - end_x + 1);
                row->size -= (end_x - action.cx);
                editorUpdateRow(row);

            } else {
                /* multi-line */
                erow *start = &E.row[action.cy];
                erow *end   = &E.row[end_y];

                if (end_x > end->size) end_x = end->size;

                char *after = strdup(&end->chars[end_x]);

                start->size = action.cx;
                editorRowAppendString(start, after, strlen(after));
                free(after);

                for (int i = action.cy + 1; i <= end_y; i++) {
                    editorDelRow(action.cy + 1);
                }
            }

            break;
        }
    }

    E.is_undo_redo = 0;
    E.dirty++;
}


void editorRedo() {
  if (E.redo_count == 0) {
    editorSetStatusMessage("やり直すことはできません");
    return;
  }
  E.is_undo_redo = 1;
  
  E.redo_count--;
  undoAction action = E.redo_stack[E.redo_count];
  
  // Move action back to undo stack.
  push_action(&E.undo_stack, &E.undo_count, &E.undo_capacity, action);
  if (action.type == ACTION_DELETE_STRING || action.type == ACTION_INSERT_STRING ||
      action.type == ACTION_JOIN_LINES || action.type == ACTION_SPLIT_LINE) {
    E.redo_stack[E.redo_count].data.string.str = NULL; // Transfer ownership
  }

  E.cx = action.cx;
  E.cy = action.cy;

  // Perform the ORIGINAL action
  switch(action.type) {
    case ACTION_INSERT_CHAR:
      editorRowInsertChar(&E.row[action.cy], action.cx, action.data.ch);
      E.cx++;
      break;
    case ACTION_DELETE_CHAR:
      editorRowDelChar(&E.row[action.cy], action.cx, 1);
      break;
    case ACTION_SPLIT_LINE: {
      erow *row = &E.row[action.cy];
      editorInsertRow(action.cy + 1, action.data.string.str, action.data.string.len);
      row->size = action.cx;
      row->chars[row->size] = '\0';
      editorUpdateRow(row);
      E.cy++; E.cx = 0;
      break;
    }
    case ACTION_JOIN_LINES: {
      editorRowAppendString(&E.row[action.cy], E.row[action.cy + 1].chars, E.row[action.cy + 1].size);
      editorDelRow(action.cy + 1);
      break;
    }
    case ACTION_INSERT_STRING:
      editorInsertString(action.data.string.str, action.data.string.len);
      break;
    case ACTION_DELETE_STRING: {
      int end_y = action.cy;
      int end_x = action.cx;
      for (int i = 0; i < action.data.string.len; i++) {
        if (action.data.string.str[i] == '\n') {
          end_y++;
          end_x = 0;
        } else {
          end_x++;
        }
      }
      if (action.cy == end_y) {
          erow *row = &E.row[action.cy];
          memmove(&row->chars[action.cx], &row->chars[end_x], row->size - end_x + 1);
          row->size -= (end_x - action.cx);
          editorUpdateRow(row);
      } else {
          erow *start_row = &E.row[action.cy];
          erow *end_row = &E.row[end_y];
          if (end_x > end_row->size) end_x = end_row->size; // Bound end_x
          char *content_after = strdup(&end_row->chars[end_x]);
          start_row->size = action.cx;
          editorRowAppendString(start_row, content_after, strlen(content_after));
          free(content_after);
          for (int i = action.cy + 1; i <= end_y; i++) {
              editorDelRow(action.cy + 1);
          }
      }
      break;
    }
  }
  
  E.is_undo_redo = 0;
  E.dirty++;
}


/*** row operations ***/

int editorRowCxToRx(erow *row, int cx) {
  int rx = 0;
  int j = 0;
  wchar_t wc;
  while (j < cx) {
    if (row->chars[j] == '\t') {
      rx += (AYA_TAB_STOP - 1) - (rx % AYA_TAB_STOP);
      rx++;
      j++;
      continue;
    }
    int char_bytes = utf8_char_len_from_byte(row->chars[j]);
    int col_width = 1;
    if (mbtowc(&wc, &row->chars[j], char_bytes) != -1) {
        col_width = wcwidth(wc);
        if (col_width < 0) col_width = 1;
    }
    rx += col_width;
    j += char_bytes;
  }
  return rx;
}

int editorRowRxToCx(erow *row, int rx) {
  int cur_rx = 0;
  int cx = 0;
  wchar_t wc;
  while (cx < row->size) {
    if (row->chars[cx] == '\t') {
      cur_rx += (AYA_TAB_STOP - 1) - (cur_rx % AYA_TAB_STOP);
      cur_rx++;
      if (cur_rx > rx) break;
      cx++;
      continue;
    }

    int char_bytes = utf8_char_len_from_byte(row->chars[cx]);
    int col_width = 1;
    if (mbtowc(&wc, &row->chars[cx], char_bytes) != -1) {
        col_width = wcwidth(wc);
        if (col_width < 0) col_width = 1;
    }

    if (cur_rx + col_width > rx) break;
    
    cur_rx += col_width;
    cx += char_bytes;
  }
  return cx;
}

void editorUpdateRow(erow *row) {
  int tabs = 0;
  if (row->size < 0) row->size = 0;
  for (int j = 0; j < row->size; j++) {
    if (row->chars[j] == '\t') tabs++;
  }
  free(row->render);
  size_t render_size = (size_t)row->size + (size_t)tabs * (AYA_TAB_STOP - 1) + 1;
  row->render = malloc(render_size);
  if (!row->render) die("malloc failed in editorUpdateRow render");

  int idx = 0; // index for row->render
  int rx = 0;  // render column
  wchar_t wc;

  // Build row->render and calculate row->rsize
  // Simultaneously, build render_to_chars_map
  free(row->render_to_chars_map);
  // Allocate generously, will realloc later if needed. Max possible size for render is row->size + tabs * (AYA_TAB_STOP - 1)
  row->render_to_chars_map = malloc(sizeof(int) * render_size);
  if (!row->render_to_chars_map) die("malloc failed for map in editorUpdateRow map");

  int chars_byte_idx = 0; // Tracks byte index in row->chars
  while (chars_byte_idx < row->size) {
    if (row->chars[chars_byte_idx] == '\t') {
      // Handle tab expansion in render
      int render_start_idx = idx;
      row->render[idx++] = ' ';
      rx++;
      while (rx % AYA_TAB_STOP != 0) {
        row->render[idx++] = ' ';
        rx++;
      }
      // All bytes added to render for this tab map to chars_byte_idx
      for (int k = render_start_idx; k < idx; k++) {
        row->render_to_chars_map[k] = chars_byte_idx;
      }
      chars_byte_idx++; // Move to next char in row->chars
    } else {
      // Handle regular character
      int char_len_in_chars = utf8_char_len_from_byte(row->chars[chars_byte_idx]);
      
      int width = 1;
      if (mbtowc(&wc, &row->chars[chars_byte_idx], char_len_in_chars) > 0) {
          width = wcwidth(wc);
          if (width < 0) width = 1;
      }
      rx += width;

      memcpy(&row->render[idx], &row->chars[chars_byte_idx], char_len_in_chars);
      // All bytes copied to render for this character map to chars_byte_idx
      for (int k = 0; k < char_len_in_chars; k++) {
        row->render_to_chars_map[idx + k] = chars_byte_idx;
      }
      idx += char_len_in_chars; // Move idx (render offset)
      chars_byte_idx += char_len_in_chars; // Move chars_byte_idx (chars offset)
    }
  }
  row->render[idx] = '\0';
  row->rsize = idx; // Final determined size of render string

  // Handle the null terminator of render string in the map
  if (row->rsize > 0) {
      row->render_to_chars_map[row->rsize] = row->render_to_chars_map[row->rsize - 1];
  } else {
      row->render_to_chars_map[0] = 0; // For empty rows
  }

  // Reallocate render_to_chars_map to exact size to save memory
  void *temp_map = realloc(row->render_to_chars_map, sizeof(int) * (row->rsize + 1));
  if (temp_map == NULL) {
      die("realloc failed for map shrinking in editorUpdateRow");
  }
  row->render_to_chars_map = temp_map;

  editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t len) {
  if (at < 0 || at > E.numrows) return;

  E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
  if (!E.row) die("realloc failed in editorInsertRow");
  if (at < E.numrows) {
    memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));
  }
  for (int j = at + 1; j <= E.numrows; j++) E.row[j].idx++;
  E.row[at].idx = at;
  E.row[at].size = len;
  E.row[at].chars = malloc(len + 1);
  if (!E.row[at].chars) die("malloc failed in editorInsertRow");
  memcpy(E.row[at].chars, s, len);
  E.row[at].chars[len] = '\0';
  E.row[at].rsize = 0;
  E.row[at].render = NULL;
  E.row[at].hl = NULL;
  E.row[at].hl_open_comment = 0;
  E.row[at].render_to_chars_map = NULL;
  editorUpdateRow(&E.row[at]);
  E.numrows++;


  E.dirty++;
}

void editorFreeRow(erow *row) {
  free(row->render);
  row->render = NULL;
  free(row->chars);
  row->chars = NULL;
  free(row->hl);
  row->hl = NULL;
  free(row->render_to_chars_map);
  row->render_to_chars_map = NULL;
}

void editorDelRow(int at) {
  if (at < 0 || at >= E.numrows) return;

  editorFreeRow(&E.row[at]);
  memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
  for (int j = at; j < E.numrows - 1; j++) E.row[j].idx--;
  E.numrows--;

  E.dirty++;
}

void editorRowInsertChar(erow *row, int at, int c) {
  if (at < 0 || at > row->size) at = row->size;
  row->chars = realloc(row->chars, row->size + 2);
  if (!row->chars) die("realloc failed in editorRowInsertChar");
  memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
  row->size++;
  row->chars[at] = c;
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowAppendString(erow *row, char *s, size_t len) {
  row->chars = realloc(row->chars, row->size + len + 1);
  if (!row->chars) die("realloc failed in editorRowAppendString");
  memcpy(&row->chars[row->size], s, len);
  row->size += len;
  row->chars[row->size] = '\0';
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowDelChar(erow *row, int at, int len) {
  if (at < 0 || at + len > row->size) return;
  memmove(&row->chars[at], &row->chars[at + len], row->size - at - len + 1);
  row->size -= len;
  editorUpdateRow(row);
  E.dirty++;
}

char *editorGetSelectedString() {
  if (E.selection_start_cy == -1) return NULL;

  struct abuf ab = ABUF_INIT;

  int start_y = E.selection_start_cy;
  int start_x = E.selection_start_cx;
  int end_y = E.selection_end_cy;
  int end_x = E.selection_end_cx;

  if (start_y > end_y || (start_y == end_y && start_x > end_x)) {
      int tmp_y = start_y; start_y = end_y; end_y = tmp_y;
      int tmp_x = start_x; start_x = end_x; end_x = tmp_x;
  }

  for (int y = start_y; y <= end_y; y++) {
    if (y >= E.numrows) break;
    erow *row = &E.row[y];
    int line_start_x = (y == start_y) ? start_x : 0;
    int line_end_x = (y == end_y) ? end_x : row->size;

    if (line_start_x > row->size) line_start_x = row->size;
    if (line_end_x > row->size) line_end_x = row->size;

    if (line_end_x > line_start_x) {
      abAppend(&ab, &row->chars[line_start_x], line_end_x - line_start_x);
    }
    if (y < end_y) {
      abAppend(&ab, "\n", 1);
    }
  }

  // ここで strdup して返す
  // Ensure ab.b is null-terminated before strdup.
  // Handle empty buffer case separately.
  if (ab.b == NULL) {
      char *ret = strdup("");
      if (!ret) die("strdup failed in editorGetSelectedString");
      abFree(&ab); // Free the empty abuf struct, though it's technically already empty.
      return ret;
  } else {
      abAppend(&ab, "", 1); // Append a null terminator
      char *ret = strdup(ab.b);
      if (!ret) die("strdup failed in editorGetSelectedString");
      abFree(&ab);
      return ret;
  }
}

void editorDeleteSelection() {
    if (E.selection_start_cy == -1) return;

    int start_y = E.selection_start_cy;
    int start_x = E.selection_start_cx;
    int end_y = E.selection_end_cy;
    int end_x = E.selection_end_cx;

    if (start_y > end_y || (start_y == end_y && start_x > end_x)) {
        // Swap them
        int tmp_y = start_y; start_y = end_y; end_y = tmp_y;
        int tmp_x = start_x; start_x = end_x; end_x = tmp_x;
    }

    if (!E.is_undo_redo) {
      char *selected = editorGetSelectedString();
      if (selected) {
        undoAction action;
        action.type = ACTION_DELETE_STRING;
        action.cx = start_x;
        action.cy = start_y;
        action.data.string.str = selected;
        action.data.string.len = strlen(selected);
        push_undo_action(action);
        free(action.data.string.str);

        selected = NULL;
      }
    }

    E.cy = start_y;
    E.cx = start_x;

    if (start_y == end_y) { // Single line
        erow *row = &E.row[start_y];
        memmove(&row->chars[start_x], &row->chars[end_x], row->size - end_x + 1);
        row->size -= (end_x - start_x);
        editorUpdateRow(row);
    } else { // Multi-line
        erow *start_row = &E.row[start_y];
        erow *end_row = &E.row[end_y];

        char *content_after_selection = &end_row->chars[end_x];
        int content_len = end_row->size - end_x;

        start_row->size = start_x;
        editorRowAppendString(start_row, content_after_selection, content_len);

        // Delete the rows between start and end
        for (int i = start_y + 1; i <= end_y; i++) {
            editorDelRow(start_y + 1);
        }
    }
    editorClearSelection();

}


int is_selected(int filerow, int cx) {
  if (E.selection_start_cy == -1) return 0;

  int start_y = E.selection_start_cy;
  int start_x = E.selection_start_cx;
  int end_y = E.selection_end_cy;
  int end_x = E.selection_end_cx;

  if (start_y > end_y || (start_y == end_y && start_x > end_x)) {
      int tmp_y = start_y; start_y = end_y; end_y = tmp_y;
      int tmp_x = start_x; start_x = end_x; end_x = tmp_x;
  }

  if ( (filerow > start_y || (filerow == start_y && cx >= start_x)) &&
       (filerow < end_y || (filerow == end_y && cx < end_x)) ) {
    return 1;
  }
  return 0;
}

void editorClearSelection() {
  E.selection_start_cx = -1;
  E.selection_start_cy = -1;
  E.selection_end_cx = -1;
  E.selection_end_cy = -1;
}

/*** editor operations ***/

void editorInsertChar(int c) {
  if (E.selection_start_cy != -1) {
    editorDeleteSelection();
  }

  char utf8_c[5];
  int len = 0;
  if (c < 0x80) {
    utf8_c[len++] = c;
  } else if (c < 0x800) {
    utf8_c[len++] = (c >> 6) | 0xC0;
    utf8_c[len++] = (c & 0x3F) | 0x80;
  } else if (c < 0x10000) {
    utf8_c[len++] = (c >> 12) | 0xE0;
    utf8_c[len++] = ((c >> 6) & 0x3F) | 0x80;
    utf8_c[len++] = (c & 0x3F) | 0x80;
  } else if (c < 0x110000) {
    utf8_c[len++] = (c >> 18) | 0xF0;
    utf8_c[len++] = ((c >> 12) & 0x3F) | 0x80;
    utf8_c[len++] = ((c >> 6) & 0x3F) | 0x80;
    utf8_c[len++] = (c & 0x3F) | 0x80;
  }
  utf8_c[len] = '\0';

  if (!E.is_undo_redo) {
    undoAction action;
    action.type = ACTION_INSERT_STRING;
    action.cx = E.cx;
    action.cy = E.cy;
    action.data.string.str = strdup(utf8_c);
    if (!action.data.string.str) die("strdup failed in editorInsertChar");
    action.data.string.len = len;
    push_undo_action(action);
    free(action.data.string.str);
  }

  if (E.cy == E.numrows) {
    editorInsertRow(E.numrows, "", 0);
  }
  editorRowInsertString(&E.row[E.cy], E.cx, utf8_c, len);
  E.cx += len;
}

void editorInsertNewLine() {
    if (E.selection_start_cy != -1) {
        editorDeleteSelection();
    }

    // Undo 用の情報
    if (!E.is_undo_redo) {
        undoAction action;
        action.type = ACTION_SPLIT_LINE;
        action.cx = E.cx;
        action.cy = E.cy;

        if (E.cy < E.numrows) {
            erow *row = &E.row[E.cy];
            if (E.cx <= row->size) {
                action.data.string.str = strdup(&row->chars[E.cx]);
                if (!action.data.string.str) die("strdup failed in editorInsertNewLine");
                action.data.string.len = row->size - E.cx;
            } else {
                action.data.string.str = strdup("");
                if (!action.data.string.str) die("strdup failed in editorInsertNewLine");
                action.data.string.len = 0;
            }
        } else {
            action.data.string.str = strdup("");
            if (!action.data.string.str) die("strdup failed in editorInsertNewLine");
            action.data.string.len = 0;
        }

        push_undo_action(action);
        free(action.data.string.str);
    }

    // 新しい行の挿入
    if (E.cy == E.numrows) {
        editorInsertRow(E.numrows, "", 0); // ファイル末尾に行を追加
    } else if (E.cx == 0) {
        editorInsertRow(E.cy, "", 0);
    } else {
        erow *row = &E.row[E.cy];
        editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
        row = &E.row[E.cy];
        row->size = E.cx;
        row->chars[row->size] = '\0';
        editorUpdateRow(row);
    }

    E.cy++;
    E.cx = 0;
}

void editorDelChar() {
  if (E.cy == E.numrows) return;
  if (E.cx == 0 && E.cy == 0) return;

  erow *row = &E.row[E.cy];
  if (E.cx > 0) {
    int char_len = 1;
    int i = E.cx - 1;
    while(i > 0 && (row->chars[i] & 0xC0) == 0x80) {
        i--;
    }
    if (i < E.cx) {
        char_len = utf8_char_len_from_byte(row->chars[i]);
    }


    if (!E.is_undo_redo) {
      undoAction action;
      action.type = ACTION_DELETE_STRING;
      action.cy = E.cy;
      action.cx = E.cx - char_len;
      action.data.string.str = strndup(&row->chars[E.cx - char_len], char_len);
      if (!action.data.string.str) die("strndup failed in editorDelChar");
      action.data.string.len = char_len;
      push_undo_action(action);
      free(action.data.string.str);
    }
    editorRowDelChar(row, E.cx - char_len, char_len);
    E.cx -= char_len;

  } else {
    if (!E.is_undo_redo) {
      undoAction action;
      action.type = ACTION_JOIN_LINES;
      action.cx = E.row[E.cy - 1].size;
      action.cy = E.cy - 1;
      action.data.string.str = strdup(row->chars);
      if (!action.data.string.str) die("strdup failed in editorDelChar");
      action.data.string.len = row->size;
      push_undo_action(action);
      free(action.data.string.str);
    }
    E.cx = E.row[E.cy - 1].size;
    editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
    editorDelRow(E.cy);
    E.cy--;
  }
}

/*** file i/o ***/

char *editorRowsToString(size_t *buflen) {
  size_t totlen = 0;
  int j;
  for (j = 0; j < E.numrows; j++) {
    totlen += E.row[j].size + 1;
  }
  *buflen = totlen;
  char *buf = malloc(totlen);
  if (!buf) die("malloc failed in editorRowsToString");
  char *p = buf;
  for (j = 0; j < E.numrows; j++) {
    memcpy(p, E.row[j].chars, E.row[j].size);
    p += E.row[j].size;
    *p = '\n';
    p++;
  }
  return buf;
}

void editorOpen(char *filename) {
  free(E.filename);
  E.filename = strdup(filename);
  if (!E.filename) die("strdup failed in editorOpen");

  editorSelectSyntaxHighlight();

  FILE *fp = fopen(filename, "r");
  if (!fp) {
    if (errno != ENOENT) {
      die("fopen");
    }
    return;
  }
  char *line = NULL;
  size_t linecap = 0;
  ssize_t linelen;
  while ((linelen = getline(&line, &linecap, fp)) != -1) {
    while (linelen > 0 && (line[linelen - 1] == '\n' || line[linelen - 1] == '\r'))
      linelen--;
    editorInsertRow(E.numrows, line, linelen);
  }
  free(line);
  fclose(fp);
  E.dirty = 0;

  if (E.initial_line > 0) {
    E.cy = E.initial_line - 1;
    if (E.cy >= E.numrows) {
      E.cy = E.numrows -1;
    }
     if (E.cy < 0) {
      E.cy = 0;
    }
  }
}

void editorSave() {
  if (E.filename == NULL) {
    E.filename = editorPrompt("ファイル名: %s", NULL);
    if (E.filename == NULL) {
      editorSetStatusMessage("キャンセルされました。");
      return;
    }
    editorSelectSyntaxHighlight();
  }
  size_t len;
  char *buf = editorRowsToString(&len);
  int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
  if (fd != -1) {
    if (ftruncate(fd, len) != -1) {
      if ((size_t)write(fd, buf, len) == len) {
        close(fd);
        free(buf);
        E.dirty = 0;
        editorSetStatusMessage("%zu バイトが書き込まれました。", len);
        return;
      }
    }
    close(fd);
  }
  free(buf);
  editorSetStatusMessage("書き込めません！ I/O エラー: %s", strerror(errno));
}

void editorReplace() {
    if (E.cy >= E.numrows) return;

    erow *row = &E.row[E.cy];
    
    // Check if the cursor is at the beginning of a match in the chars buffer
    char *match_in_chars = strstr(row->chars, find_buffer);
    if (match_in_chars == NULL || match_in_chars != &row->chars[E.cx]) {
        // Only replace if the cursor is at the beginning of a match in the chars buffer
        return;
    }

    if (strlen(find_buffer) == 0) { // Special case: replace line if find is empty
        const char* replace_query = editorGetReplaceBuffer();
        // Clear the current line
        row->size = 0;
        if (row->chars) {
            free(row->chars);
            row->chars = NULL;
        }
        // Insert the replacement string
        editorRowInsertString(row, 0, (char*)replace_query, strlen(replace_query));
        return;
    }
    
    editorRowDelChar(row, E.cx, strlen(find_buffer));
    editorRowInsertString(row, E.cx, (char*)replace_buffer, strlen(replace_buffer));
}

void editorFindCallback(int key, int active_field) {
    static int last_match_cy = -1;
    static int last_match_cx = -1;
    static int direction = 1;
    static char prev_find_buffer[256] = "";
    static char prev_replace_buffer[256] = "";

    // Reset if search/replace strings have changed
    if (strcmp(find_buffer, prev_find_buffer) != 0 || strcmp(replace_buffer, prev_replace_buffer) != 0) {
        last_match_cy = -1;
        last_match_cx = -1;
        direction = 1;
        strncpy(prev_find_buffer, find_buffer, sizeof(prev_find_buffer) - 1);
        strncpy(prev_replace_buffer, replace_buffer, sizeof(prev_replace_buffer) - 1);
    }

    if (key == '\t' || key == '\x1b') {
        last_match_cy = -1;
        last_match_cx = -1;
        direction = 1;
        return;
    } else if (key == '\r') {
        if (active_field == 1) { // In replace field
            editorReplace();
            direction = 1; // Always find next after replacing
        } else { // In find field
            if (last_match_cy == -1) { // This is the first search action
                direction = 1; 
            } else { // Already on a match, so just exit
                return;
            }
        }
    } else if (key == ARROW_DOWN || key == ARROW_RIGHT) {
        direction = 1;
    } else if (key == ARROW_UP || key == ARROW_LEFT) {
        direction = -1;
    }

    if (strlen(find_buffer) == 0) return;

    if (last_match_cy == -1) {
        last_match_cy = E.cy;
        last_match_cx = E.cx;
    }

    int start_row = last_match_cy;
    for (int i = 0; i < E.numrows; i++) {
        int current_row_idx;
        if (direction == 1) {
            current_row_idx = (start_row + i) % E.numrows;
        } else {
            current_row_idx = (start_row - i + E.numrows) % E.numrows;
        }

        erow *row = &E.row[current_row_idx];
        char *match = NULL;

        if (direction == 1) {
            int start_char_idx = (current_row_idx == last_match_cy && i==0) ? last_match_cx + 1 : 0;
            if (start_char_idx <= row->size) {
                 match = strstr(&row->chars[start_char_idx], find_buffer);
            }
        } else { // Backward search
            int start_char_idx = (current_row_idx == last_match_cy && i==0) ? last_match_cx : row->size;
            char *last_possible_match = NULL;
            char *current_pos = row->chars;
            while(current_pos < &row->chars[start_char_idx]) {
                char *found = strstr(current_pos, find_buffer);
                if (found && found < &row->chars[start_char_idx]) {
                    last_possible_match = found;
                    current_pos = found + 1;
                } else {
                    break;
                }
            }
            match = last_possible_match;
        }

        if (match) {
            last_match_cy = current_row_idx;
            last_match_cx = match - row->chars;
            E.cy = current_row_idx;
            E.cx = last_match_cx;
            editorScroll();
            return;
        }
    }
}

void editorFind() {
  int saved_cx = E.cx;
  int saved_cy = E.cy;
  int saved_coloff = E.coloff;
  int saved_rowoff = E.rowoff;

  char *query = editorPrompt("検索: %s | 置換: %s", (void(*)(int,int))editorFindCallback);

  if (query == NULL) { // Search was cancelled
    E.cx = saved_cx;
    E.cy = saved_cy;
    E.coloff = saved_coloff;
    E.rowoff = saved_rowoff;
  }
  free(query);
}

void editorGoToLine() {
  char *line_str = editorPrompt("行へ移動: %s (Esc でキャンセル)", NULL);
  if (line_str == NULL) {
    editorSetStatusMessage("移動はキャンセルされました。");
    return;
  }

  int line_num = atoi(line_str);
  free(line_str);

  if (line_num > 0 && line_num <= E.numrows) {
    E.cy = line_num - 1;
    E.cx = 0;
    editorSetStatusMessage("%d行目に移動しました", line_num);
  } else {
    editorSetStatusMessage("無効な行番号です: %d", line_num);
  }
}

/*** output ***/

void editorScroll() {
  E.rx = 0;
  if (E.cy < E.numrows) {
    E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
  }

  if (E.cy < E.rowoff) {
    E.rowoff = E.cy;
  }
  if (E.cy >= E.rowoff + E.screenrows) {
    E.rowoff = E.cy - E.screenrows + 1;
  }

  int line_num_area_width = LINE_NUM_COLUMNS + 1;
  int effective_screencols = E.screencols - line_num_area_width;

  if (E.rx < E.coloff) {
    E.coloff = E.rx;
  }
  if (E.rx >= E.coloff + effective_screencols) {
    E.coloff = E.rx - effective_screencols + 1;
  }
}

void editorDrawRows(struct abuf *ab) {
  int y;
  int line_num_area_width = LINE_NUM_COLUMNS + 1; // +1 for the separator '|'

  for (y = 0; y < E.screenrows; y++) {
    int filerow = y + E.rowoff;
    if (filerow >= E.numrows) {
      if (E.numrows == 0 && y == E.screenrows / 3) {
        char welcome[80];
        int welcomelen = snprintf(welcome, sizeof(welcome),
          "Aya Editor -- version %s", AYA_VERSION);
        if (welcomelen > E.screencols) welcomelen = E.screencols;
        int padding = (E.screencols - welcomelen) / 2;
        if (padding) {
          abAppend(ab, "~", 1);
          padding--;
        }
        while (padding--) abAppend(ab, " ", 1);
        abAppend(ab, welcome, welcomelen);
      } else {
        abAppend(ab, "~", 1);
      }
    } else {
              char line_num_str[16];
              int len = snprintf(line_num_str, sizeof(line_num_str), "%*d", LINE_NUM_COLUMNS, filerow + 1);
              abAppend(ab, "\x1b[38;5;240m", 11);
              abAppend(ab, line_num_str, (size_t)len);
              abAppend(ab, "|", 1); // Add separator
              abAppend(ab, "\x1b[39m", 5);
      erow *row = &E.row[filerow];
      int rx = 0;
      int current_color_applied = 39;
      int current_invert_applied = 0;
      
      int start_col = E.syntax ? E.coloff : E.coloff;
      if (start_col < 0) start_col = 0;
      
      int screen_width = E.screencols - line_num_area_width;

      for (int j = 0; j < row->rsize; ) {
          int char_len = utf8_char_len_from_byte(row->render[j]);
          int width = 1;
          wchar_t wc;
           if (mbtowc(&wc, &row->render[j], char_len) > 0) {
              width = wcwidth(wc);
              if (width < 0) width = 1;
          } else {
              char_len = 1;
              width = 1;
          }

          if (rx >= start_col) {
              if (rx - start_col >= screen_width) break;
              int chars_idx = (row->render_to_chars_map) ? row->render_to_chars_map[j] : j;
              int is_sel_for_this_char = is_selected(filerow, chars_idx);
              int color_for_this_char = editorSyntaxToColor(row->hl[j]);

              if (is_sel_for_this_char && !current_invert_applied) {
                abAppend(ab, "\x1b[7m", 4);
                current_invert_applied = 1;
              } else if (!is_sel_for_this_char && current_invert_applied) {
                abAppend(ab, "\x1b[27m", 5);
                current_invert_applied = 0;
              }

              if (color_for_this_char != current_color_applied) {
                current_color_applied = color_for_this_char;
                char buf[16];
                int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color_applied);
                abAppend(ab, buf, (size_t)clen);
              }

              if (iscntrl(row->render[j])) {
                char sym = (row->render[j] <= 26) ? '@' + row->render[j] : '?';
                abAppend(ab, "\x1b[m", 3); 
                abAppend(ab, "\x1b[7m", 4); 
                abAppend(ab, &sym, 1);
                abAppend(ab, "\x1b[m", 3);
                current_color_applied = 39; 
                current_invert_applied = 0; 
              } else {
                abAppend(ab, &row->render[j], (size_t)char_len);
              }
          }
          rx += width;
          j += char_len;
      }
      
      if (current_invert_applied) {
        abAppend(ab, "\x1b[27m", 5);
      }
      if (current_color_applied != 39) {
        abAppend(ab, "\x1b[39m", 5);
      }
    }
    abAppend(ab, "\x1b[K", 3);
    abAppend(ab, "\r\n", 2);
  }
}


void editorDrawStatusBar(struct abuf *ab) {
    abAppend(ab, "\x1b[7m", 4);
    char status[80], rstatus[80];
    int len = snprintf(status, sizeof(status), "%.20s - %d 行 %s",
        E.filename ? E.filename : "[無題]", E.numrows, E.dirty ? "(変更済)" : "");
    int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d",
        E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.numrows);
    if (len > E.screencols) len = E.screencols;
    abAppend(ab, status, (size_t)len);
    while (len < E.screencols) {
        if (E.screencols - len == rlen) {
            abAppend(ab, rstatus, (size_t)rlen);
            break;
        } else {
            abAppend(ab, " ", 1);
            len++;
        }
    }
    abAppend(ab, "\x1b[m", 3);
    abAppend(ab, "\r\n", 2);
}


void editorDrawMessageBar(struct abuf *ab) {
  abAppend(ab, "\x1b[K", 3);
  int msglen = strlen(E.statusmsg);
  if (msglen && time(NULL) - E.statusmsg_time < 5) {
    abAppend(ab, "\x1b[7m", 4);
    int display_width = 0;
    int i = 0;
    wchar_t wc;
    while(i < msglen) {
        int char_bytes = mbtowc(&wc, &E.statusmsg[i], msglen - i);
        if (char_bytes <= 0) {
            char_bytes = 1; 
        }

        int col_width = wcwidth(wc);
        if (col_width < 0) col_width = 1;

        if (display_width + col_width > E.screencols) {
            abAppend(ab, "...", 3);
            break;
        }
        
        abAppend(ab, &E.statusmsg[i], (size_t)char_bytes);
        display_width += col_width;
        i += char_bytes;
    }
    abAppend(ab, "\x1b[m", 3);
  }
}

void editorUpdateScreenContent(struct abuf *current_frame, struct abuf *prev_frame);

void editorRefreshScreen() {
  editorScroll();
  
  struct abuf current_frame_ab = ABUF_INIT;

  abAppend(&current_frame_ab, "\x1b[?25l", 6); // Hide cursor
  abAppend(&current_frame_ab, "\x1b[H", 3);    // Cursor to top-left

  editorDrawStatusBar(&current_frame_ab);
  editorDrawRows(&current_frame_ab);
  editorDrawMessageBar(&current_frame_ab);
  
  int line_num_area_width = LINE_NUM_COLUMNS + 1;

  char buf[32];
  snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 2, (E.rx - E.coloff) + 1 + line_num_area_width);
  abAppend(&current_frame_ab, buf, (size_t)strlen(buf));
  
  abAppend(&current_frame_ab, "\x1b[?25h", 6); // Show cursor
  
  editorUpdateScreenContent(&current_frame_ab, &E.prev_screen_abuf);
  // current_frame_ab's buffer is now owned by E.prev_screen_abuf, so don't free it.
  current_frame_ab.b = NULL;
  current_frame_ab.len = 0;
  abFree(&current_frame_ab);
}

void editorUpdateScreenContent(struct abuf *current_frame, struct abuf *prev_frame) {
    // If it's the first draw or the previous frame is empty, draw everything
    if (prev_frame->b == NULL || prev_frame->len == 0) {
        write(STDOUT_FILENO, current_frame->b, current_frame->len);
    } else {
        struct abuf diff_output = ABUF_INIT;

        // Offsets to iterate through the buffers
        size_t curr_offset = 0;
        size_t prev_offset = 0;

        // Line numbers for cursor positioning
        int row = 1; // Terminal rows are 1-indexed

        // Iterate line by line
        // Loop as long as there is content in either buffer
        while (curr_offset < current_frame->len || prev_offset < prev_frame->len) {
            // Determine the start and length of the current line in current_frame
            size_t curr_line_start_offset = curr_offset;
            size_t curr_line_end_offset = curr_offset;
            while (curr_line_end_offset < current_frame->len && current_frame->b[curr_line_end_offset] != '\n') {
                curr_line_end_offset++;
            }
            // Include the newline character in the length for display purposes, but exclude for comparison
            size_t curr_line_total_len = curr_line_end_offset - curr_line_start_offset;
            size_t curr_line_cmp_len = curr_line_total_len;
            if (curr_line_end_offset < current_frame->len && current_frame->b[curr_line_end_offset] == '\n') {
                curr_line_total_len++; // Include newline in total length
            }

            // Determine the start and length of the current line in prev_frame
            size_t prev_line_start_offset = prev_offset;
            size_t prev_line_end_offset = prev_offset;
            while (prev_line_end_offset < prev_frame->len && prev_frame->b[prev_line_end_offset] != '\n') {
                prev_line_end_offset++;
            }
            // Include the newline character in the length for display purposes, but exclude for comparison
            size_t prev_line_total_len = prev_line_end_offset - prev_line_start_offset;
            size_t prev_line_cmp_len = prev_line_total_len;
            if (prev_line_end_offset < prev_frame->len && prev_frame->b[prev_line_end_offset] == '\n') {
                prev_line_total_len++; // Include newline in total length
            }
            
            // Compare lines: check length first, then content
            // If one buffer has run out of lines, they are considered different
            bool lines_are_different = false;
            if (curr_line_cmp_len != prev_line_cmp_len) { // Length difference
                lines_are_different = true;
            } else if (curr_offset >= current_frame->len || prev_offset >= prev_frame->len) { // One buffer exhausted
                lines_are_different = true;
            } else if (curr_line_cmp_len > 0 && strncmp(current_frame->b + curr_line_start_offset,
                                                        prev_frame->b + prev_line_start_offset,
                                                        curr_line_cmp_len) != 0) {
                lines_are_different = true;
            }

            if (lines_are_different) {
                // Lines are different, redraw the current line
                char cmd[32];
                int cmdlen = snprintf(cmd, sizeof(cmd), "\x1b[%d;1H", row); // Move to start of line
                abAppend(&diff_output, cmd, cmdlen);
                abAppend(&diff_output, "\x1b[K", 3); // Clear to end of line
                // Only write content from current_frame if it exists for this line
                if (curr_line_start_offset < current_frame->len) {
                    abAppend(&diff_output, current_frame->b + curr_line_start_offset, curr_line_total_len);
                }
            }

            // Advance offsets to the beginning of the next line
            // Handle case where line is empty (just a newline) or last line without newline
            curr_offset = curr_line_start_offset + curr_line_total_len;
            prev_offset = prev_line_start_offset + prev_line_total_len;
            
            row++;
        }
        write(STDOUT_FILENO, diff_output.b, diff_output.len);
        abFree(&diff_output);
    }

    // After writing, update prev_frame for the next cycle
    abFree(prev_frame); // Free old prev_frame content

    // Transfer ownership of current_frame's buffer to prev_frame
    prev_frame->b = current_frame->b;
    prev_frame->len = current_frame->len;
    current_frame->b = NULL; // Prevent current_frame from freeing this memory
    current_frame->len = 0;
}

void editorSetStatusMessage(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
  va_end(ap);
  E.statusmsg_time = time(NULL);
}

/*** input ***/

char *editorPrompt(char *prompt, void (*callback)(int, int)) {
  int is_fr_prompt = (strstr(prompt, "検索:") && strstr(prompt, "置換:"));
  static int active_field = 0;
  
  if (is_fr_prompt) {
    active_field = 0;
  }

  size_t bufsize = 256;
  char *buf = malloc(bufsize);
  if (!buf) die("malloc failed in editorPrompt");

  if (is_fr_prompt) {
    strncpy(buf, find_buffer, bufsize - 1);
  } else {
    buf[0] = '\0';
  }
  size_t buflen = strlen(buf);

  while (1) {
    if (is_fr_prompt) {
      if (active_field == 0) {
        editorSetStatusMessage("[検索: %s] 置換: %s", buf, replace_buffer);
      } else {
        editorSetStatusMessage("検索: %s [置換: %s]", find_buffer, buf);
      }
    } else {
      editorSetStatusMessage(prompt, buf);
    }
    editorRefreshScreen();
    
    int c = editorReadKey();

    if (c == '\t' && is_fr_prompt) {
      if (active_field == 0) { // from find to replace
        strncpy(find_buffer, buf, sizeof(find_buffer) - 1);
        find_buffer[sizeof(find_buffer) - 1] = '\0';
        strncpy(buf, replace_buffer, bufsize - 1);
      } else { // from replace to find
        strncpy(replace_buffer, buf, sizeof(replace_buffer) - 1);
        replace_buffer[sizeof(replace_buffer) - 1] = '\0';
        strncpy(buf, find_buffer, bufsize - 1);
      }
      buflen = strlen(buf);
      active_field = 1 - active_field;
      continue;
    }

    switch (c) {
      case '\r': // Enter
        if (is_fr_prompt) {
          if (active_field == 0) { // In find field
            strncpy(find_buffer, buf, sizeof(find_buffer) - 1);
            find_buffer[sizeof(find_buffer) - 1] = '\0';
            if (callback) callback(c, active_field);

            editorSetStatusMessage("");
            free(buf);
            return strdup(""); // Exit prompt
          } else { // In replace field
            strncpy(replace_buffer, buf, sizeof(replace_buffer) - 1);
            replace_buffer[sizeof(replace_buffer) - 1] = '\0';
            if (callback) callback(c, active_field);
            
            break; // Keep prompt open
          }
        } else {
           // For other prompts (GoTo, SaveAs), return the buffer content
           editorSetStatusMessage("");
           return buf; // Caller is responsible for freeing this
        }

      case '\x1b': // ESC
        editorSetStatusMessage("");
        if (callback) callback(c, active_field);
        free(buf);
        return NULL;

      case BACKSPACE:
      case CTRL_KEY('h'):
      case DEL_KEY:
        if (buflen > 0) {
          int i = buflen - 1;
          while (i > 0 && (buf[i] & 0xC0) == 0x80) i--;
          buflen = i;
          buf[buflen] = '\0';
        }
        break;

      case ARROW_UP:
      case ARROW_DOWN:
      case ARROW_LEFT:
      case ARROW_RIGHT:
      case PAGE_UP:
      case PAGE_DOWN:
      case HOME_KEY:
      case END_KEY:
        if (is_fr_prompt) {
          if (active_field == 0) {
            strncpy(find_buffer, buf, sizeof(find_buffer) - 1);
            find_buffer[sizeof(find_buffer) - 1] = '\0';
          } else {
            strncpy(replace_buffer, buf, sizeof(replace_buffer) - 1);
            replace_buffer[sizeof(replace_buffer) - 1] = '\0';
          }
        }
        if (callback) callback(c, active_field);
        break;

      default:
        if (buflen < bufsize - 5) { 
          if (c < 0x80) {
            buf[buflen++] = c;
          } else if (c < 0x800) {
            buf[buflen++] = (c >> 6) | 0xC0;
            buf[buflen++] = (c & 0x3F) | 0x80;
          } else if (c < 0x10000) {
            buf[buflen++] = (c >> 12) | 0xE0;
            buf[buflen++] = ((c >> 6) & 0x3F) | 0x80;
            buf[buflen++] = (c & 0x3F) | 0x80;
          } else if (c < 0x110000) {
            buf[buflen++] = (c >> 18) | 0xF0;
            buf[buflen++] = ((c >> 12) & 0x3F) | 0x80;
            buf[buflen++] = ((c >> 6) & 0x3F) | 0x80;
            buf[buflen++] = (c & 0x3F) | 0x80;
          }
          buf[buflen] = '\0';
        }
        break;
    }
  }
}

void editorMoveCursor(int key) {
  int is_shift = 0;
  if (key == SHIFT_ARROW_UP || key == SHIFT_ARROW_DOWN || key == SHIFT_ARROW_LEFT || key == SHIFT_ARROW_RIGHT) {
    is_shift = 1;
    switch (key) {
      case SHIFT_ARROW_UP: key = ARROW_UP; break;
      case SHIFT_ARROW_DOWN: key = ARROW_DOWN; break;
      case SHIFT_ARROW_LEFT: key = ARROW_LEFT; break;
      case SHIFT_ARROW_RIGHT: key = ARROW_RIGHT; break;
    }
  }

  if (is_shift) {
    if (E.selection_start_cy == -1) {
      E.selection_start_cx = E.cx;
      E.selection_start_cy = E.cy;
    }
  } else {
    editorClearSelection();
  }

  erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
  switch (key) {
    case ARROW_LEFT:
      if (E.cx != 0) {
        int prev_char_len = 1;
        int i = E.cx - 1;
        while(i > 0 && (row->chars[i] & 0xC0) == 0x80) {
            i--;
        }
        if (i < E.cx) {
            prev_char_len = utf8_char_len_from_byte(row->chars[i]);
             if (E.cx - i != prev_char_len) prev_char_len = 1;
        }
        E.cx -= prev_char_len;
      } else if (E.cy > 0) {
        E.cy--;
        E.cx = E.row[E.cy].size;
      }
      break;
    case ARROW_RIGHT:
      if (row && E.cx < row->size) {
        E.cx += utf8_char_len_from_byte(row->chars[E.cx]);
      } else if (row && E.cx == row->size) {
        E.cy++;
        E.cx = 0;
      }
      break;
    case CTRL_ARROW_RIGHT:
      if (E.cy < E.numrows) {
        E.cx = E.row[E.cy].size;
      }
      break;
    case ARROW_UP:
      if (E.cy != 0) {
        E.cy--;
      }
      break;
    case ARROW_DOWN:
      if (E.cy < E.numrows) {
        E.cy++;
      }
      break;
    case MOUSE_WHEEL_UP:
      if (E.cy != 0) {
        E.cy -= 3;
        if (E.cy < 0) E.cy = 0;
      }
      break;
    case MOUSE_WHEEL_DOWN:
      if (E.cy < E.numrows) {
        E.cy += 3;
        if (E.cy > E.numrows) E.cy = E.numrows;
      }
      break;
    case PAGE_UP:
    case PAGE_DOWN:
      {
        if (key == PAGE_UP) {
          E.cy = E.rowoff;
        } else if (key == PAGE_DOWN) {
          E.cy = E.rowoff + E.screenrows - 1;
          if (E.cy > E.numrows) E.cy = E.numrows;
        }
        int times = E.screenrows;
        while (times--)
          editorMoveCursor(key == PAGE_UP ? ARROW_UP : ARROW_DOWN);
      }
      break;
  }
  row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
  int rowlen = row ? row->size : 0;
  if (E.cx > rowlen) {
    E.cx = rowlen;
  }
  // Clamp E.cy to prevent it from going past the last actual row
  if (E.numrows == 0) {
      E.cy = 0;
  } else if (E.cy >= E.numrows) {
      E.cy = E.numrows - 1;
  }

  if (is_shift) {
    E.selection_end_cx = E.cx;
    E.selection_end_cy = E.cy;
  }
}

void editorRowInsertString(erow *row, int at, char *s, size_t len) {
  if (at < 0 || at > row->size) at = row->size;
  row->chars = realloc(row->chars, row->size + len + 1);
  memmove(&row->chars[at + len], &row->chars[at], row->size - at + 1);
  memcpy(&row->chars[at], s, len);
  row->size += len;
  editorUpdateRow(row);
  E.dirty++;
}

void editorProcessKeypress() {
  static int quit_times = AYA_QUIT_TIMES;
  int c = editorReadKey();
  switch (c) {
    case '\r':
      editorInsertNewLine();
      break;
    case CTRL_KEY('q'):
      if (E.dirty && quit_times > 0) {
        editorSetStatusMessage("!!!あと %d 回押すと終了!!!", quit_times);
        quit_times--;
        return;
      }
      write(STDOUT_FILENO, "\x1b[2J", 4);
      write(STDOUT_FILENO, "\x1b[H", 3);
      exit(0);
      break;
    case CTRL_KEY('s'):
      editorSave();
      break;
    case CTRL_KEY('y'):
      editorRedo();
      break;
    case CTRL_KEY('z'):
    case '\0':
      editorUndo();
      break;
    case HOME_KEY:
    case CTRL_ARROW_LEFT: // Handle Ctrl + Left Arrow
      E.cx = 0;
      break;
    case END_KEY:
    case THORN_KEY: // Handle Ctrl + Right Arrow as THORN_KEY
      if (E.cy < E.numrows)
        E.cx = E.row[E.cy].size;
      break;
    case CTRL_KEY('f'):
      editorFind();
      break;
    case BACKSPACE:
    case CTRL_KEY('h'):
    case DEL_KEY:
      if (E.selection_start_cy != -1) {
        editorDeleteSelection();
      } else {
        if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
        editorDelChar();
      }
      break;
    case PAGE_UP:
    case PAGE_DOWN:
    case ARROW_UP:
    case ARROW_DOWN:
    case ARROW_LEFT:
    case ARROW_RIGHT:
    case SHIFT_ARROW_UP:
    case SHIFT_ARROW_DOWN:
    case SHIFT_ARROW_LEFT:
    case SHIFT_ARROW_RIGHT:
    case MOUSE_WHEEL_UP:
    case MOUSE_WHEEL_DOWN:
      editorMoveCursor(c);
      break;
    case MOUSE_CLICK:
      if (E.cy >= 1 && E.cy <= E.screenrows) {
        E.cy = E.rowoff + E.cy - 1;
        if (E.cy >= E.numrows) {
          E.cy = E.numrows;
          E.cx = 0;
        } else {
          E.cx = editorRowRxToCx(&E.row[E.cy], E.coloff + E.cx);
        }
      }
      break;
    case CTRL_KEY('a'):
      E.selection_start_cy = 0;
      E.selection_start_cx = 0;
      if (E.numrows > 0) {
        E.selection_end_cy = E.numrows - 1;
        E.selection_end_cx = E.row[E.numrows - 1].size;
      } else {
        E.selection_end_cy = 0;
        E.selection_end_cx = 0;
      }
      break;
    case CTRL_KEY('c'):
      {
        char *selected = editorGetSelectedString();
        if (selected) {
          FILE *p = popen("xclip -selection clipboard", "w");
          if (p) {
            fwrite(selected, 1, strlen(selected), p);
            pclose(p);
            editorSetStatusMessage("クリップボードにコピーしました");
          }
          free(selected);
        }
      }
      break;
    case CTRL_KEY('x'):
      {
        char *selected = editorGetSelectedString();
        if (selected) {
          FILE *p = popen("xclip -selection clipboard", "w");
          if (p) {
            fwrite(selected, 1, strlen(selected), p);
            pclose(p);
            editorDeleteSelection();
            editorSetStatusMessage("クリップボードに切り取りました");
          }
          free(selected);
        }
      }
      break;
    case CTRL_KEY('v'):
      {
        FILE *p = popen("xclip -selection clipboard -o", "r");
        if (p) {
          char *text = NULL;
          size_t len = 0;
          char buffer[1024];
          size_t n;
          while ((n = fread(buffer, 1, sizeof(buffer), p)) > 0) {
            text = realloc(text, len + n + 1);
            memcpy(text + len, buffer, n);
            len += n;
          }
          pclose(p);

          if (text) {
            if (!E.is_undo_redo) {
              undoAction action;
              action.type = ACTION_INSERT_STRING;
              action.cx = E.cx;
              action.cy = E.cy;
              action.data.string.str = text;
              action.data.string.len = len;
              push_undo_action(action);
            }

            editorInsertString(text, len);
            free(text);
            editorSetStatusMessage("クリップボードから貼り付けました");
          }
        }
      }
      break;
    case CTRL_KEY('l'):
      editorGoToLine();
      break;
    case '\x1b':
      break;
    default:
      editorInsertChar(c);
      break;
  }
  quit_times = AYA_QUIT_TIMES;
}

/*** init ***/

void initEditor() {
  E.cx = 0;
  E.cy = 0;
  E.rx = 0;
  E.rowoff = 0;
  E.coloff = 0;
  E.numrows = 0;
  E.row = NULL;
  E.dirty = 0;
  E.filename = NULL;
  E.statusmsg[0] = '\0';
  E.statusmsg_time = 0;
  E.syntax = NULL;
  E.selection_start_cx = -1;
  E.selection_start_cy = -1;
  E.selection_end_cx = -1;
  E.selection_end_cy = -1;
  E.undo_stack = NULL;
  E.undo_count = 0;
  E.undo_capacity = 0;
  E.redo_stack = NULL;
  E.redo_count = 0;
  E.redo_capacity = 0;
  E.is_undo_redo = 0;
  E.initial_line = 0;
  E.prev_screen_abuf.b = NULL;
  E.prev_screen_abuf.len = 0;
  if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
  E.screenrows -= 2;
}

void display_version() {
  printf("Aya version %s\n", AYA_VERSION);
}

void display_help() {
  printf("Aya version %s\n", AYA_VERSION);
  printf("使用法: aya [options] [file]\n");
  printf("オプション:\n");
  printf("  -h, --help     このヘルプメッセージの表示\n");
  printf("  -v, --version  バージョン情報の表示\n\n");
  printf("  -l <line>  指定行でファイルを開く\n");
}

int main(int argc, char *argv[]) {
  setlocale(LC_ALL, "");

  char *filepath = NULL;
  int initial_line = 0;

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0 ) {
      display_help();
      return 0;
    } else if (strcmp(argv[i], "--version") == 0 || strcmp(argv[i], "-v") == 0) {
      display_version();
      return 0;
    } else if (strcmp(argv[i], "-l") == 0) {
      if (i + 1 < argc) {
        initial_line = atoi(argv[i + 1]);
        i++;
      } else {
        fprintf(stderr, "エラー: -l オプションには行番号が必要です。\n");
        return 1;
      }
    } else {
      if (filepath == NULL) {
        filepath = argv[i];
      } else {
        fprintf(stderr, "エラー: 複数のファイルパスが指定されています。\n");
        return 1;
      }
    }
  }

  enableRawMode();
  initEditor();
  E.initial_line = initial_line;

  if (filepath != NULL) {
    editorOpen(filepath);
  }

  editorSetStatusMessage("ヘルプ: ctrl+S = 保存 | ctrl+Q = 終了 | ctrl+F = 検索");
  while (1) {
    editorRefreshScreen();
    editorProcessKeypress();
  }
  return 0;
}
