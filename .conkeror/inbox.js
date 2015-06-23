/**
 * (C) Copyright 2008 David Glasser
 * (C) Copyright 2008 Will Farrington
 * (C) Copyright 2008 Jeremy Maitin-Shepard
 * (C) Copyright 2009-2010 John J. Foerch
 *
 * Use, modification, and distribution are subject to the terms specified in the
 * COPYING file.
**/

require("content-buffer.js");

define_keymap("inbox_keymap", $display_name = "inbox");

// Jumping
define_key(inbox_keymap, "g", null, $fallthrough);
define_key(inbox_keymap, "i", null, $fallthrough);
define_key(inbox_keymap, "t", null, $fallthrough);
define_key(inbox_keymap, "d", null, $fallthrough);
define_key(inbox_keymap, "a", null, $fallthrough);
define_key(inbox_keymap, "b", null, $fallthrough);

// Threadlist
define_key(inbox_keymap, "*", null, $fallthrough);

// Navigation
define_key(inbox_keymap, "u", null, $fallthrough);
define_key(inbox_keymap, "j", null, $fallthrough);
define_key(inbox_keymap, "k", null, $fallthrough);
define_key(inbox_keymap, "o", null, $fallthrough);
define_key(inbox_keymap, "n", null, $fallthrough);
define_key(inbox_keymap, "p", null, $fallthrough);

// Application
define_key(inbox_keymap, "c", null, $fallthrough);
define_key(inbox_keymap, "C", null, $fallthrough);
define_key(inbox_keymap, "/", null, $fallthrough);
define_key(inbox_keymap, "q", null, $fallthrough);
define_key(inbox_keymap, "?", null, $fallthrough);

// Actions
define_key(inbox_keymap, "s", null, $fallthrough);
define_key(inbox_keymap, "e", null, $fallthrough);
define_key(inbox_keymap, "x", null, $fallthrough);
define_key(inbox_keymap, "y", null, $fallthrough);
define_key(inbox_keymap, "!", null, $fallthrough);
define_key(inbox_keymap, "m", null, $fallthrough);
define_key(inbox_keymap, "#", null, $fallthrough);
define_key(inbox_keymap, "r", null, $fallthrough);
define_key(inbox_keymap, "f", null, $fallthrough);
define_key(inbox_keymap, "N", null, $fallthrough);
define_key(inbox_keymap, ".", null, $fallthrough);
define_key(inbox_keymap, "I", null, $fallthrough);
define_key(inbox_keymap, "U", null, $fallthrough);
define_key(inbox_keymap, "]", null, $fallthrough);
define_key(inbox_keymap, "[", null, $fallthrough);
define_key(inbox_keymap, "l", null, $fallthrough);
define_key(inbox_keymap, "return", null, $fallthrough);
define_key(inbox_keymap, "tab", null, $fallthrough);


define_keymaps_page_mode("inbox-mode",
    build_url_regexp($domain = "mail.google",
                     $path = new RegExp('(?!support)')),
    { normal: inbox_keymap },
    $display_name = "Inbox");

page_mode_activate(inbox_mode);

provide("inbox");
