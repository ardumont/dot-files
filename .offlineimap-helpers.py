#!/usr/bin/python

"""Offlineimap helper

This permits to expose authentication functions to retrieve credential
information from a .authinfo.gpg file without leaking those in the
.offlineimaprc configuration file.

"""

import re
import subprocess


def read_cmd_from_shell(command):
    """Given a command to read a file, return its content (beware, limited
       to 1024 bytes)

    """
    return subprocess.Popen(command, shell=True, bufsize=1024,
                            stdout=subprocess.PIPE).stdout.read()


def retrieve_password_regexp(machine, login, port):
    """Compute an authinfo regexp to retrieve a password from a machine,
       login, port entry in authinfo file

    """
    return re.compile("machine %s login %s port %s password ([^ ]*)\n" % (
        machine, login, port))


def retrieve_password(authinfo, machine, login, port):
    """Compute the password from an authinfo content

    """
    return retrieve_password_regexp(
        machine, login, port).search(authinfo).group(1)


def get_secure_credentials(filename, machine, login, port):
    """Retrieve the credentials from an authinfo symmetrically encrypted
       filename

    """
    authinfo = read_cmd_from_shell("gpg2 --quiet --no-tty --decrypt %s " % (
        filename, ))
    return retrieve_password(authinfo, machine, login, port)


def get_plain_credentials(filename, machine, login, port):
    """Retrieve the credentials from a plain authinfo filename

    """
    authinfo = read_cmd_from_shell("cat %s" % filename)
    return retrieve_password(authinfo, machine, login, port)


def oimaptransfolder_account(account_name, foldername):
    """Compute folder per account

    """
    if foldername == "INBOX":
        retval = account_name
    else:
        retval = "%s.%s" % (account_name, foldername)
    return re.sub("/", ".", retval)
