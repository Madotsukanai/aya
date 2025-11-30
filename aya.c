#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>


#define AYA_VERSION "0.0.4"
#define AYA_TAB_STOP 8
#define AYA_QUIT_TIMES 3

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
  SHIFT_ARROW_DOWN
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

/*** data ***/

typedef struct erow {
  int idx;
  int size;
  int rsize;
  char *chars;
  char *render;
  unsigned char *hl;
  int hl_open_comment;
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
};

struct editorConfig E;

/*** prototypes ***/

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));
void editorMoveCursor(int key);
void editorInsertString(char *s, int len);
void editorUndo();
void editorRedo();
int editorRowRxToCx(erow *row, int rx);
void editorClearSelection();
int is_selected(int filerow, int cx);
char *editorGetSelectedString();
void editorDeleteSelection();
void editorUpdateSyntax(erow *row);
void editorSelectSyntaxHighlight();
void editorRowInsertString(erow *row, int at, char *s, size_t len);
void editorInsertNewLine();
void editorRowDelChar(erow *row, int at);
void editorRowInsertChar(erow *row, int at, int c);
void editorRowAppendString(erow *row, char *s, size_t len);
void editorDelRow(int at);
void editorInsertRow(int at, char *s, size_t len);
void editorUpdateRow(erow *row);
void editorDelChar();
// undo 系関数のプロトタイプ
void push_action(undoAction **stack, int *count, int *capacity, undoAction action);
void push_undo_action(undoAction action);

// editorDeleteSelection
char *editorGetSelectedString();
void editorDeleteSelection();

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
  write(STDOUT_FILENO, "\x1b[?1006l", 8); // Disable SGR mouse mode
  write(STDOUT_FILENO, "\x1b[?1000l", 8); // Disable normal mouse reporting
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
  write(STDOUT_FILENO, "\x1b[?1000h", 8); // Enable normal mouse reporting
  write(STDOUT_FILENO, "\x1b[?1006h", 8); // Enable SGR mouse mode
}

int editorReadKey() {
  int nread;
  char c;
  while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    if (nread == -1 && errno != EAGAIN) die("read");
  }

  if (c != '\x1b') {
    return c;
  }

  char seq[32] = {0};
  size_t i = 0;
  // Read the rest of the escape sequence with a timeout
  while (i < sizeof(seq) - 1) {
    if (read(STDIN_FILENO, &seq[i], 1) != 1) break;
    // Heuristic: stop at terminating character
    if (isalpha(seq[i]) || seq[i] == '~') {
      i++;
      break;
    }
    i++;
  }

  // If we didn't read anything, it was a lone Esc press
  if (i == 0) return '\x1b';

  // Null-terminate for strcmp
  seq[i] = '\0';

  // Parse the sequence
  if (seq[0] == '[') {
    // Standard CSI sequences
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
    } else if (seq[1] == '<') { // SGR Mouse
        int b, x, y;
        char m;
        if (sscanf(seq, "[<%d;%d;%d%c", &b, &x, &y, &m) == 4) {
            if (m == 'M') { // Press
                if (b == 64) return MOUSE_WHEEL_UP;
                if (b == 65) return MOUSE_WHEEL_DOWN;
                E.cx = x - 1;
                E.cy = y - 1;
                return MOUSE_CLICK;
            }
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

  // Unrecognized sequence, but we consumed it.
  return '\x1b';
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
    return 1; // Not a valid start byte, treat as single byte
}

void editorUpdateSyntax(erow *row) {
    if (row->rsize == 0) {
        free(row->hl);
        row->hl = NULL;
        return;
    }
    row->hl = realloc(row->hl, row->rsize);
    if (row->hl == NULL) {
    
        die("realloc failed in editorUpdateSyntax");
    }
    memset(row->hl, HL_NORMAL, row->rsize);
    if (E.syntax == NULL) return;
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
    
            // Handle preprocessor directives (C/C++ specific)
            if (E.syntax != NULL && (strcmp(E.syntax->filetype, "c") == 0 || strcmp(E.syntax->filetype, "cpp") == 0) && c == '#' && i == 0) {
                int start_i = i;
                memset(&row->hl[start_i], HL_PREPROC, row->rsize - start_i); // Default to HL_PREPROC for the whole line
    
                // Find the directive word (e.g., "include", "define")
                int j = start_i + 1; // Skip '#'
                while (j < row->rsize && isspace(row->render[j])) j++; // Skip whitespace
                int directive_start = j;
                while (j < row->rsize && isalpha(row->render[j])) j++; // Read directive word
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
                // This break is crucial for preprocessor directives to be handled fully and not re-parsed as other syntax
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
    case HL_COMMENT:
    case HL_MLCOMMENT: return 36;
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

/*** append buffer ***/

struct abuf {
  char *b;
  int len;
};

#define ABUF_INIT {NULL, 0}

void abAppend(struct abuf *ab, const char *s, int len) {
    char *new = realloc(ab->b, ab->len + len + 1); // +1 for null terminator
    if (new == NULL) die("realloc failed in abAppend");
    memcpy(new + ab->len, s, len);
    new[ab->len + len] = '\0';

    ab->b = new;
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
        undoAction *new_stack = realloc(*stack, sizeof(undoAction) * (*capacity));
        if (!new_stack) die("realloc failed in push_action");
        *stack = new_stack;
    }

    if (action.type == ACTION_DELETE_STRING || action.type == ACTION_INSERT_STRING ||
        action.type == ACTION_JOIN_LINES || action.type == ACTION_SPLIT_LINE) {
        if (action.data.string.str != NULL) {
            char *new_str = strdup(action.data.string.str);
            if (!new_str) die("strdup failed in push_action");
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
      editorRowDelChar(&E.row[action.cy], action.cx);
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
      editorRowDelChar(&E.row[action.cy], action.cx);
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
  int j;
  for (j = 0; j < cx; j++) {
    if (row->chars[j] == '\t')
      rx += (AYA_TAB_STOP - 1) - (rx % AYA_TAB_STOP);
    rx++;
  }
  return rx;
}

int editorRowRxToCx(erow *row, int rx) {
  int cur_rx = 0;
  int cx;
  for (cx = 0; cx < row->size; cx++) {
    if (row->chars[cx] == '\t')
      cur_rx += (AYA_TAB_STOP - 1) - (cur_rx % AYA_TAB_STOP);
    cur_rx++;
    if (cur_rx > rx) return cx;
  }
  return cx;
}

void editorUpdateRow(erow *row) {
  int tabs = 0;
  int j;
  for (j = 0; j < row->size; j++)
    if (row->chars[j] == '\t') tabs++;
  free(row->render);
  row->render = malloc(row->size + tabs * (AYA_TAB_STOP - 1) + 1);
  if (row->render == NULL) {

      die("malloc failed in editorUpdateRow");
  }
  int idx = 0;
  for (j = 0; j < row->size; j++) {
    if (row->chars[j] == '\t') {
      row->render[idx++] = ' ';
      while (idx % AYA_TAB_STOP != 0) row->render[idx++] = ' ';
    } else {
      row->render[idx++] = row->chars[j];
    }
  }
  row->render[idx] = '\0';
  row->rsize = idx;
  editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t len) {
  if (at < 0 || at > E.numrows) return;

  E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
  if (E.row == NULL) {

      die("realloc failed in editorInsertRow");
  }
  if (at < E.numrows) {
    memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));
  }
  for (int j = at + 1; j <= E.numrows; j++) E.row[j].idx++;
  E.row[at].idx = at;
  E.row[at].size = len;
  E.row[at].chars = malloc(len + 1);
  memcpy(E.row[at].chars, s, len);
  E.row[at].chars[len] = '\0';
  E.row[at].rsize = 0;
  E.row[at].render = NULL;
  E.row[at].hl = NULL;
  E.row[at].hl_open_comment = 0;
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
  memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
  row->size++;
  row->chars[at] = c;
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowAppendString(erow *row, char *s, size_t len) {
  row->chars = realloc(row->chars, row->size + len + 1);
  memcpy(&row->chars[row->size], s, len);
  row->size += len;
  row->chars[row->size] = '\0';
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowDelChar(erow *row, int at) {
  if (at < 0 || at >= row->size) return;
  memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
  row->size--;
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
  char *ret = strdup(ab.b ? ab.b : "");
  abFree(&ab); // ab の内部バッファを解放
  return ret;
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
        action.data.string.str = strdup(selected); // 既存 push_undo_action と同等
        action.data.string.len = strlen(selected);
        push_undo_action(action);

        free(selected); // ← 追加: editorGetSelectedString の戻り値は free 必須
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
  if (!E.is_undo_redo) {
    undoAction action;
    action.type = ACTION_INSERT_CHAR;
    action.cx = E.cx;
    action.cy = E.cy;
    action.data.ch = c;
    push_undo_action(action);
  }

  if (E.cy == E.numrows) {
    editorInsertRow(E.numrows, "", 0);
  }
  editorRowInsertChar(&E.row[E.cy], E.cx, c);
  E.cx++;
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
                action.data.string.len = row->size - E.cx;
            } else {
                action.data.string.str = strdup("");
                action.data.string.len = 0;
            }
        } else {
            action.data.string.str = strdup("");
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
    if (!E.is_undo_redo) {
      undoAction action;
      action.type = ACTION_DELETE_CHAR;
      action.cx = E.cx - 1;
      action.cy = E.cy;
      action.data.ch = row->chars[E.cx - 1];
      push_undo_action(action);
    }
    editorRowDelChar(row, E.cx - 1);
    E.cx--;
  } else {
    if (!E.is_undo_redo) {
      undoAction action;
      action.type = ACTION_JOIN_LINES;
      action.cx = E.row[E.cy - 1].size;
      action.cy = E.cy - 1;
      action.data.string.str = strdup(row->chars);
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

char *editorRowsToString(int *buflen) {
  int totlen = 0;
  int j;
  for (j = 0; j < E.numrows; j++) {
    totlen += E.row[j].size + 1;
  }
  *buflen = totlen;
  char *buf = malloc(totlen);
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
  editorSelectSyntaxHighlight();
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
  int len;
  char *buf = editorRowsToString(&len);
  int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
  if (fd != -1) {
    if (ftruncate(fd, len) != -1) {
      if (write(fd, buf, len) == len) {
        close(fd);
        free(buf);
        E.dirty = 0;
        editorSetStatusMessage("%d バイトが書き込まれました。", len);
        return;
      }
    }
    close(fd);
  }
  free(buf);
  editorSetStatusMessage("書き込めません！ I/O エラー: %s", strerror(errno));
}

/*** find ***/
void editorFindCallback(char *query, int key);

void editorFind() {
  int saved_cx = E.cx;
  int saved_cy = E.cy;
  int saved_coloff = E.coloff;
  int saved_rowoff = E.rowoff;

  char *query = editorPrompt("検索: %s (Esc でキャンセル, Enterで次へ)", editorFindCallback);

  if (query == NULL) { // Search was cancelled
    E.cx = saved_cx;
    E.cy = saved_cy;
    E.coloff = saved_coloff;
    E.rowoff = saved_rowoff;
  }
  free(query);
}

void editorFindCallback(char *query, int key) {
    static int last_match_cy = -1;
    static int last_match_cx = -1;
    static char *current_query = NULL;

    if (key == '\x1b') {
        last_match_cy = -1;
        last_match_cx = -1;
        if(current_query) free(current_query);
        current_query = NULL;
        return;
    }

    if (current_query == NULL || strcmp(query, current_query) != 0) {
        if(current_query) free(current_query);
        current_query = strdup(query);
        last_match_cy = -1;
        last_match_cx = -1;
    }

    if (key != '\r') { // new char typed
        last_match_cy = -1;
        last_match_cx = -1;
    }

    int start_cy = last_match_cy == -1 ? 0 : last_match_cy;
    int start_cx = last_match_cx == -1 ? 0 : last_match_cx;

    if (key == '\r') {
        start_cx++;
    }

    for (int i = 0; i < E.numrows; i++) {
        int current_cy = (start_cy + i) % E.numrows;
        erow *row = &E.row[current_cy];

        char *match;
        if (i == 0) {
            if (start_cx >= row->size) continue;
            match = strstr(&row->render[start_cx], query);
        } else {
            match = strstr(row->render, query);
        }

        if (match) {
            last_match_cy = current_cy;
            last_match_cx = match - row->render;
            E.cy = current_cy;
            E.cx = editorRowRxToCx(row, last_match_cx);
            E.rowoff = E.numrows;
            return;
        }
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
  if (E.rx < E.coloff) {
    E.coloff = E.rx;
  }
  if (E.rx >= E.coloff + E.screencols) {
    E.coloff = E.rx - E.screencols + 1;
  }
}

void editorDrawRows(struct abuf *ab) {
  int y;
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
      int len = E.row[filerow].rsize - E.coloff;
      if (len < 0) len = 0;
      if (len > E.screencols) len = E.screencols;
      char *c = &E.row[filerow].render[E.coloff];
      unsigned char *hl = &E.row[filerow].hl[E.coloff];
      
      int current_color_applied = 39; // Start with default foreground color
      int current_invert_applied = 0; // 0 for no invert, 1 for invert
      
      int j = 0;
      while (j < len) {
        int char_len = utf8_char_len_from_byte(c[j]);
        if (j + char_len > len) char_len = 1;

        int is_sel_for_this_char = is_selected(filerow, editorRowRxToCx(&E.row[filerow], E.coloff + j));
        int color_for_this_char = editorSyntaxToColor(hl[j]);

        // Manage inversion (selection)
        if (is_sel_for_this_char && !current_invert_applied) {
          abAppend(ab, "\x1b[7m", 4); // Turn on inversion
          current_invert_applied = 1;
        } else if (!is_sel_for_this_char && current_invert_applied) {
          abAppend(ab, "\x1b[27m", 5); // Turn off inversion
          current_invert_applied = 0;
        }

        // Manage color
        if (color_for_this_char != current_color_applied) {
          current_color_applied = color_for_this_char;
          char buf[16];
          int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color_applied);
          abAppend(ab, buf, clen);
        }

        // Handle control characters (they override styling for their symbol)
        if (iscntrl(c[j])) {
          char sym = (c[j] <= 26) ? '@' + c[j] : '?';
          // Reset all attributes, then apply reverse video for the symbol
          abAppend(ab, "\x1b[m", 3); 
          abAppend(ab, "\x1b[7m", 4); 
          abAppend(ab, &sym, 1);
          abAppend(ab, "\x1b[m", 3); // Reset after symbol
          
          // Force re-evaluation of color and invert state for next character
          current_color_applied = 39; 
          current_invert_applied = 0; 
        } else {
          // Normal character: just append it
          abAppend(ab, &c[j], char_len);
        }
        j += char_len;
      }
      // After the loop, ensure everything is reset to default
      if (current_invert_applied) {
        abAppend(ab, "\x1b[27m", 5); // Turn off inversion
      }
      if (current_color_applied != 39) {
        abAppend(ab, "\x1b[39m", 5); // Reset to default foreground color
      }
    }
    abAppend(ab, "\x1b[K", 3);
    abAppend(ab, "\r\n", 2);
  }
}



void editorDrawStatusBar(struct abuf *ab) {
    abAppend(ab, "\x1b[7m", 4); // 反転表示

    char status[80], rstatus[32];
    int status_len = snprintf(status, sizeof(status), "%.20s - %d 行 %s",
                              E.filename ? E.filename : "[無題]",
                              E.numrows,
                              E.dirty ? "(変更済)" : "");
    if (status_len < 0) status_len = 0;
    if (status_len > E.screencols) status_len = E.screencols;

    int rlen = snprintf(rstatus, sizeof(rstatus), "%d/%d", E.cy + 1, E.numrows);
    if (rlen < 0) rlen = 0;
    if (rlen > E.screencols) rlen = E.screencols;

    // 左側描画
    int len = (status_len > E.screencols ? E.screencols : status_len);
    abAppend(ab, status, len);

    // 左右の間を空白で埋める
    int padding = E.screencols - len - rlen;
    if (padding < 0) padding = 0;
    for (int i = 0; i < padding; i++) abAppend(ab, " ", 1);

    // 右側描画
    abAppend(ab, rstatus, rlen);

    // 右端まで空白で残骸を完全に消す
    int total_len = len + padding + rlen;
    while (total_len < E.screencols) {
        abAppend(ab, " ", 1);
        total_len++;
    }

    abAppend(ab, "\x1b[m", 3); // 反転解除
    abAppend(ab, "\r\n", 2);
}


void editorDrawMessageBar(struct abuf *ab) {
  abAppend(ab, "\x1b[K", 3);
  int msglen = strlen(E.statusmsg);
  if (msglen > E.screencols) msglen = E.screencols;
  if (msglen && time(NULL) - E.statusmsg_time < 5) {
    abAppend(ab, "\x1b[7m", 4);
    abAppend(ab, E.statusmsg, msglen);
  }
}

void editorRefreshScreen() {
  editorScroll();
  struct abuf ab = ABUF_INIT;
  abAppend(&ab, "\x1b[?25l", 6);
  abAppend(&ab, "\x1b[H", 3);
  editorDrawStatusBar(&ab);
  editorDrawRows(&ab);
  editorDrawMessageBar(&ab);
  char buf[32];
  snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 2,
           (E.rx - E.coloff) + 1);
  abAppend(&ab, buf, strlen(buf));
  abAppend(&ab, "\x1b[?25h", 6);
  write(STDOUT_FILENO, ab.b, ab.len);
  abFree(&ab);
}

void editorSetStatusMessage(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
  va_end(ap);
  E.statusmsg_time = time(NULL);
}

/*** input ***/

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
  size_t bufsize = 128;
  char *buf = malloc(bufsize);
  size_t buflen = 0;
  buf[0] = '\0';
  while (1) {
    editorSetStatusMessage(prompt, buf);
    editorRefreshScreen();
    int c = editorReadKey();
    if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
      if (buflen != 0) buf[--buflen] = '\0';
      if (callback) callback(buf, c);
    } else if (c == '\x1b') {
      editorSetStatusMessage("");
      if (callback) callback(buf, c);
      free(buf);
      return NULL;
    } else if (c == '\r') {
      if (callback) {
        callback(buf, c);
      } else {
        if (buflen != 0) {
          editorSetStatusMessage("");
          return buf;
        }
      }
    } else if (!iscntrl(c) && c < 128) {
      if (buflen == bufsize - 1) {
        bufsize *= 2;
        buf = realloc(buf, bufsize);
      }
      buf[buflen++] = c;
      buf[buflen] = '\0';
      if (callback) callback(buf, c);
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
        E.cx--;
      } else if (E.cy > 0) {
        E.cy--;
        E.cx = E.row[E.cy].size;
      }
      break;
    case ARROW_RIGHT:
      if (row && E.cx < row->size) {
        E.cx++;
      } else if (row && E.cx == row->size) {
        E.cy++;
        E.cx = 0;
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
        editorSetStatusMessage("!!!変更は保存されていません!!!"
          "ctrl+Q をあと %d 回押すと終了", quit_times);
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
      E.cx = 0;
      break;
    case END_KEY:
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
            text = NULL;
            }

            size_t sanitized_len = 0;
            for (size_t i = 0; i < len; i++) {
              if (isprint(text[i]) || text[i] == '\t' || text[i] == '\n' || text[i] == '\r') {
                text[sanitized_len++] = text[i];
              }
            }
            text[len] = '\0';
            editorInsertString(text, len);
            free(text);
            editorSetStatusMessage("クリップボードから貼り付けました");
          }
        }
      }
      break;
    case CTRL_KEY('l'):
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
  printf("  --help     このヘルプメッセージの表示\n");
  printf("  --version  バージョン情報の表示\n");
}

int main(int argc, char *argv[]) {
  if (argc >= 2) {
    if (strcmp(argv[1], "--help") == 0) {
      display_help();
      return 0;
    } else if (strcmp(argv[1], "--version") == 0) {
      display_version();
      return 0;
    }
  }

  enableRawMode();
  initEditor();
  if (argc >= 2) {
    editorOpen(argv[1]);
  }
  editorSetStatusMessage("ヘルプ: ctrl+S = 保存 | ctrl+Q = 終了 | ctrl+F = 検索");
  while (1) {
    editorRefreshScreen();
    editorProcessKeypress();
  }
  return 0;
}
