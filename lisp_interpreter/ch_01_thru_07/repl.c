// cc -std=c99 -Wall repl.c -lreadline -o repl.exe
#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32
#include <string.h>

static char buffer[2048];

char *readline(char *prompt) {
  fputs(prompt, stdout);
  fgets(buffer, 2048, stdin);
  char *cpy = malloc(strlen(buffer) + 1);
  strcpy(cpy, buffer);
  cpy[strlen(cpy) - 1] = '\0';
  return cpy;
}

void add_history(char *unused) {}

#else

#include <readline/history.h>
#include <readline/readline.h>

#endif

int main(int argc, char **argv) {
  puts("Lispy Version 0.0.0.0.1");
  puts("Press Ctrl+C to Exit\n");

  while (1) {
    char *input = readline("lispy> ");
    add_history(input);
    printf("No you're a %s", input);
    free(input);
  }
  return 0;
}
