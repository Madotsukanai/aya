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


  
  // Move the action to the redo stack. Note that we don't free its data.
  push_action(&E.redo_stack, &E.redo_count, &E.redo_capacity, action);
  if (action.type == ACTION_DELETE_STRING || action.type == ACTION_INSERT_STRING ||
      action.type == ACTION_JOIN_LINES || action.type == ACTION_SPLIT_LINE) {
    E.undo_stack[E.undo_count].data.string.str = NULL; // Transfer ownership
  }

  E.cx = action.cx;
  E.cy = action.cy;

  // Perform the INVERSE of the action
  switch (action.type) {
    case ACTION_INSERT_CHAR:
      editorRowDelChar(&E.row[action.cy], action.cx, 1);
      break;
    case ACTION_DELETE_CHAR:
      editorRowInsertChar(&E.row[action.cy], action.cx, action.data.ch);
      E.cx++;
      break;
    case ACTION_SPLIT_LINE: // Undo a split is to join
      editorRowAppendString(&E.row[action.cy], E.row[action.cy + 1].chars, E.row[action.cy + 1].size);
      editorDelRow(action.cy + 1);
      break;
    case ACTION_JOIN_LINES: { // Undo a join is to split
      erow *row = &E.row[action.cy];
      editorInsertRow(action.cy + 1, action.data.string.str, action.data.string.len);
      // Truncate the line that was appended to.
      row->size = action.cx;
      row->chars[row->size] = '\0';
      editorUpdateRow(row);
      E.cy++; E.cx = 0;
      break;
    }
    case ACTION_INSERT_STRING: {
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
      if (action.cy == end_y) { // Single line
          erow *row = &E.row[action.cy];
          memmove(&row->chars[action.cx], &row->chars[end_x], row->size - end_x + 1);
          row->size -= (end_x - action.cx);
          editorUpdateRow(row);
      } else { // Multi-line
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
    case ACTION_DELETE_STRING:
      editorInsertString(action.data.string.str, action.data.string.len);
      break;
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
  
