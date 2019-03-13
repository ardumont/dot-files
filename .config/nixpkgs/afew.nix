{ pkgs, ... }:

{
  programs.afew = {
    enable = true;

    extraConfig = ''
[global]

# This is the default filter chain
[SpamFilter]             # filter through X-Spam-Flag headers
#[ClassifyingFilter]     # not working yet since no classification has been bootstraped
[KillThreadsFilter]      # Look through the new messages and add a the name of the list as a tag.
                         # Use ListID: header
[ListMailsFilter]        #
[SentMailsFilter]        # Tag all emails sent to others. Uses the From: header.
sent_tag = sent
[ArchiveSentMailsFilter] # Removes inbox tag from sent email
                         #
[InboxFilter]            #

# Let's say you like the SpamFilter, but it is way too polite

# 1. create an filter object and customize it
#[SpamFilter.0] # note the index
#message = meh

# 2. create a new type and...
#[ShitFilter(SpamFilter)]
#message = I hatez teh spam!

#    create an object or two...
#[ShitFilter.0]
#[ShitFilter.1]
#message = Me hatez it too.

# 3. drop a custom filter type in ~/.config/afew/
#[MyCustomFilter]


# To create a custom generic filter, define it inline with
# your above filter chain. E.g.:

# ...
# [ListMailsFilter]

[Filter.0]
query = from:swh-team@inria.fr
tags = -new;-inbox;+unread;+lists/swh-team;+unread
message = swh-team mailing list

[Filter.1]
query = from:swh-devel@inria.fr
tags = -new;-inbox;+unread;+lists/swh-devel;+unread
message = swh-devel mailing list

[Filter.2]
query = 'from:forge@softwareheritage.org and message:swh-public-ci'
tags = -new;-inbox;+lists/swh-forge-ci;+unread
message = swh CI message

[Filter.3]
query = from:forge@softwareheritage.org
tags = -new;-inbox;+unread;+lists/swh-forge
message = swh forge message

[Filter.4]
query = 'to:swh-sysadm@inria.fr or from:@.*softwareheritage.org'
tags = -new;-inbox;+lists/swh-sysadm;+unread
message = swh-sysadm mailing list

[Filter.5]
query = from:@travis-ci.org
tags = -new;-inbox;+unread;+lists/travis-ci
message = travis-ci mailing list

[Filter.6]
query = 'to:debian-devel@lists.debian.org or cc:debian-devel@lists.debian.org'
tags = -new;-inbox;+unread;+lists/debian-devel
message = debian-devel mailing list

[Filter.7]
query = 'to:debian-mentors@lists.debian.org or cc:debian-mentors@lists.debian.org'
tags = -new;-inbox;+unread;+lists/debian-mentors
message = debian mentors mailing list

[Filter.8]
query = 'to:debian-python@lists.debian.org or cc:debian-python@lists.debian.org or X-Debbugs-Cc:debian-python@lists.debian.org'
tags = -new;-inbox;+unread;+lists/debian-python
message = debian python mailing list

[Filter.9]
query = 'from:nixpkgs@noreply.github.com or to:nixpkgs@noreply.github.com'
tags = -new;-inbox;+unread;+lists/nixpkgs
message = nixpkgs stuff

[Filter.10]
query = 'to:nix-dev@lists.science.uu.nl or cc:to:nix-dev@lists.science.uu.nl'
tags = -new;-inbox;+unread;+lists/nix-dev
message = nix-dev stuff

[Filter.11]
query = 'to:emacs-orgmode@gnu.org or cc:emacs-orgmode@gnu.org'
tags = -new;-inbox;+unread;+lists/emacs-orgmode
message = orgmode mailing list

[Filter.12]
query = 'to:xmonad@haskell.org or cc:xmonad@haskell.org'
tags = -new;-inbox;+unread;+lists/xmonad
message = xmonad mailing list

[Filter.13]
query = 'to:stumpwm-devel@nongnu.org or cc:stumpwm-devel@nongnu.org'
tags = -new;-inbox;+unread;+lists/stumpwm-devel
message = stumpwm mailing list

[Filter.14]
query = 'to:Password-Store@lists.zx2c4.com or cc:Password-Store@lists.zx2c4.com'
tags = -new;-inbox;+unread;+lists/password-store
message = password-store mailing list

[Filter.15]
query = 'to:help-gnu-emacs@gnu.org or cc:help-gnu-emacs@gnu.org'
tags = -new;-inbox;-unread;+lists/help-gnu-emacs
message = help gnu emacs mailing list

[Filter.16]
query = from:@twitter.com
tags = -new;-inbox;-unread;+lists/twitter
message = twitter

[Filter.17]
query = from:@quora.com
tags = -new;-inbox;-unread;+lists/quora
message = quora

[Filter.18]
query = from:@youtube.com
tags = -new;-inbox;-unread;+lists/quora
message = youtube

[Filter.19]
query = from:@m.mail.coursera.org
tags = -new;-inbox;-unread;+lists/coursera
message = coursera

[Filter.20]
query = from:@engage.linuxfoundation.org
tags = -new;-inbox;-unread;+lists/linuxfoundation
message = linux foundation

[Filter.21]
query = from:@news.strava.com
tags = -new;-inbox;-unread;+lists/strava
message = strava

[Filter.22]
query = 'from:@email.boulanger.com or from:@meetup.com or from:@sunflowerjewels.com or from:@partoch.com or from:@b.journaldunet.com or from:@infolettre.grosbill.com or from:@getpocket.com or from:@trello.com or from:@online.berklee.edu or from:@info.go-sport.com or donate@wikimedia.org'
tags = -new;-inbox;-unread;+spam
message = uninteresting spams

#
# [ArchiveSentMailsFilter]
# ...

[MailMover]
folders = INBOX junk all
max_age = 1

# rules
INBOX = 'tag:spam and NOT tag:inbox and NOT tag:unread':junk
junk = 'NOT tag:spam and tag:inbox':INBOX 'NOT tag:spam':all
all = 'NOT tag:unread':all
'';
  };
}
