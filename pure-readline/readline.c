
/* Copyright (c) 2009-2012 Albert Graef

   Copying and distribution of this file, with or without modification,
   are permitted in any medium without royalty provided the copyright
   notice and this notice are preserved.  This file is offered as-is,
   without any warranty. */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/* You might have to edit this if readline/editline lives elsewhere on your
   system. */
#ifdef USE_LIBEDIT
#include <editline/readline.h>
#else
#include <readline/readline.h>
#include <readline/history.h>
#endif

#include <pure/runtime.h>

static HISTORY_STATE *my_hist = NULL;

static void my_init_history(void)
{
  my_hist = history_get_history_state();
  /* KLUDGE: At this point, history_get_history_state() may return the most
     recent interpreter history instead of an empty one, so null it out. */
  if (my_hist->entries) {
    my_hist->entries = NULL;
    my_hist->offset = 0;
    my_hist->length = 0;
    my_hist->size = 0;
    my_hist->flags = 0;
  }
}

static HISTORY_STATE *save_hist = NULL;
static int histmax = -1;

static void push_history(void)
{
  save_hist = history_get_history_state();
  histmax = unstifle_history();
  if (!my_hist) my_init_history();
  history_set_history_state(my_hist);
}

static void pop_history(bool save)
{
  if (save) {
    if (my_hist) free(my_hist);
    my_hist = history_get_history_state();
  }
  history_set_history_state(save_hist);
  free(save_hist);
  if (histmax>=0) stifle_history(histmax);
}

extern pure_expr *wrap_readline(const char *prompt)
{
  pure_expr *res;
  const char *save_rl_readline_name = rl_readline_name;
  const char *save_rl_basic_word_break_characters =
    rl_basic_word_break_characters;
  rl_completion_func_t *save_rl_attempted_completion_function =
    rl_attempted_completion_function;
  rl_readline_name = NULL;
  rl_basic_word_break_characters = " \t\n\"\\'`@$><=,;!|&{([";
  rl_attempted_completion_function = NULL;
  push_history();
  res = pure_cstring(readline(prompt));
  pop_history(true);
  rl_readline_name = save_rl_readline_name;
  rl_basic_word_break_characters = save_rl_basic_word_break_characters;
  rl_attempted_completion_function = save_rl_attempted_completion_function;
  return res;
}

extern void wrap_add_history(const char *s)
{
  push_history();
  add_history(s);
  pop_history(true);
}

extern void wrap_clear_history(void)
{
  push_history();
  clear_history();
  pop_history(true);
}

extern int wrap_read_history(const char *fname)
{
  int res;
  push_history();
  res = read_history(fname);
  pop_history(true);
  return res;
}

extern int wrap_write_history(const char *fname)
{
  int res;
  push_history();
  res = write_history(fname);
  pop_history(false);
  return res;
}
