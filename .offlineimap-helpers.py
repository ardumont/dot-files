#!/usr/bin/python
import re, subprocess

# Given a command to read a file, return its content (beware, limited to 1024 bytes)
def read_cmd_from_shell(command):
    return subprocess.Popen(command, shell=True, bufsize=1024, stdout=subprocess.PIPE).stdout.read()

# Compute an authinfo regexp to retrieve a password from a machine, login, port entry in authinfo file
def retrieve_password_regexp(machine, login, port):
    return re.compile("machine %s login %s port %s password ([^ ]*)\n" % (machine, login, port))

# Compute the password from an authinfo content
def retrieve_password(authinfo, machine, login, port):
    return retrieve_password_regexp(machine, login, port).search(authinfo).group(1)

# Retrieve the credentials from an authinfo symmetrically encrypted filename
def get_secure_credentials(filename, machine, login, port):
    authinfo = read_cmd_from_shell("gpg2 --quiet --no-tty --decrypt %s " % filename)
    return retrieve_password(authinfo, machine, login, port)

# Retrieve the credentials from a plain authinfo filename
def get_plain_credentials(filename, machine, login, port):
    authinfo = read_cmd_from_shell("cat %s" % filename)
    return retrieve_password(authinfo, machine, login, port)

# def oimaptransfolder_account(account_name, foldername):
#     retval = account_name if (foldername == "INBOX") else ("%s.%s" % (account_name, foldername))
#     return re.sub("/", ".", retval)
